/*
 * LispThread.java
 *
 * Copyright (C) 2003-2007 Peter Graves
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

public final class LispThread extends LispObject
{
    static boolean use_fast_calls = false;

    // use a concurrent hashmap: we may want to add threads
    // while at the same time iterating the hash
    final static ConcurrentHashMap<Thread,LispThread> map =
       new ConcurrentHashMap<Thread,LispThread>();

    LispObject threadValue = NIL;

    private static ThreadLocal<LispThread> threads = new ThreadLocal<LispThread>(){
        @Override
        public LispThread initialValue() {
            Thread thisThread = Thread.currentThread();
            LispThread thread = LispThread.map.get(thisThread);
            if (thread == null) {
                thread = new LispThread(thisThread);
                LispThread.map.put(thisThread,thread);
            }
            return thread;
        }
    };

    public static final LispThread currentThread()
    {
        return threads.get();
    }

    final Thread javaThread;
    private boolean destroyed;
    final LispObject name;
    public LispObject[] _values;
    private boolean threadInterrupted;
    private LispObject pending = NIL;
    private Symbol wrapper =
        PACKAGE_THREADS.intern("THREAD-FUNCTION-WRAPPER");

    LispThread(Thread javaThread)
    {
        this.javaThread = javaThread;
        name = new SimpleString(javaThread.getName());
    }

    LispThread(final Function fun, LispObject name)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                try {
                    threadValue = funcall(wrapper,
                            new LispObject[] { fun },
                            LispThread.this);
                }
                catch (ThreadDestroyed ignored) {
                      // Might happen.
                }
                catch (Throwable t) { // any error: process thread interrupts
                    if (isInterrupted()) {
                        processThreadInterrupts();
                    }
                }
                finally {
                    // make sure the thread is *always* removed from the hash again
                    map.remove(Thread.currentThread());
                }
            }
        };
        javaThread = new Thread(r);
        this.name = name;
        map.put(javaThread, this);
        if (name != NIL)
            javaThread.setName(name.getStringValue());
        javaThread.setDaemon(true);
        javaThread.start();
    }

    public StackTraceElement[] getJavaStackTrace() {
        return javaThread.getStackTrace();
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.THREAD;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.THREAD;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.THREAD)
            return T;
        if (typeSpecifier == BuiltInClass.THREAD)
            return T;
        return super.typep(typeSpecifier);
    }

    public final synchronized boolean isDestroyed()
    {
        return destroyed;
    }

    final synchronized boolean isInterrupted()
    {
        return threadInterrupted;
    }

    final synchronized void setDestroyed(boolean b)
    {
        destroyed = b;
    }

    final synchronized void interrupt(LispObject function, LispObject args)
    {
        pending = new Cons(args, pending);
        pending = new Cons(function, pending);
        threadInterrupted = true;
        javaThread.interrupt();
    }

    final synchronized void processThreadInterrupts()

    {
        while (pending != NIL) {
            LispObject function = pending.car();
            LispObject args = pending.cadr();
            pending = pending.cddr();
            Primitives.APPLY.execute(function, args);
        }
        threadInterrupted = false;
    }

    public final LispObject[] getValues()
    {
        return _values;
    }

    public final LispObject[] getValues(LispObject result, int count)
    {
        if (_values == null) {
            LispObject[] values = new LispObject[count];
            if (count > 0)
                values[0] = result;
            for (int i = 1; i < count; i++)
                values[i] = NIL;
            return values;
        }
        // If the caller doesn't want any extra values, just return the ones
        // we've got.
        if (count <= _values.length)
            return _values;
        // The caller wants more values than we have. Pad with NILs.
        LispObject[] values = new LispObject[count];
        for (int i = _values.length; i-- > 0;)
            values[i] = _values[i];
        for (int i = _values.length; i < count; i++)
            values[i] = NIL;
        return values;
    }

    // Used by the JVM compiler for MULTIPLE-VALUE-CALL.
    public final LispObject[] accumulateValues(LispObject result,
                                               LispObject[] oldValues)
    {
        if (oldValues == null) {
            if (_values != null)
                return _values;
            LispObject[] values = new LispObject[1];
            values[0] = result;
            return values;
        }
        if (_values != null) {
            if (_values.length == 0)
                return oldValues;
            final int totalLength = oldValues.length + _values.length;
            LispObject[] values = new LispObject[totalLength];
            System.arraycopy(oldValues, 0,
                             values, 0,
                             oldValues.length);
            System.arraycopy(_values, 0,
                             values, oldValues.length,
                             _values.length);
            return values;
        }
        // _values is null.
        final int totalLength = oldValues.length + 1;
        LispObject[] values = new LispObject[totalLength];
        System.arraycopy(oldValues, 0,
                         values, 0,
                         oldValues.length);
        values[totalLength - 1] = result;
        return values;
    }

    public final LispObject setValues()
    {
        _values = new LispObject[0];
        return NIL;
    }

    public final LispObject setValues(LispObject value1)
    {
        _values = null;
        return value1;
    }

    public final LispObject setValues(LispObject value1, LispObject value2)
    {
        _values = new LispObject[2];
        _values[0] = value1;
        _values[1] = value2;
        return value1;
    }

    public final LispObject setValues(LispObject value1, LispObject value2,
                                      LispObject value3)
    {
        _values = new LispObject[3];
        _values[0] = value1;
        _values[1] = value2;
        _values[2] = value3;
        return value1;
    }

    public final LispObject setValues(LispObject value1, LispObject value2,
                                      LispObject value3, LispObject value4)
    {
        _values = new LispObject[4];
        _values[0] = value1;
        _values[1] = value2;
        _values[2] = value3;
        _values[3] = value4;
        return value1;
    }

    public final LispObject setValues(LispObject[] values)
    {
        switch (values.length) {
            case 0:
                _values = values;
                return NIL;
            case 1:
                _values = null;
                return values[0];
            default:
                _values = values;
                return values[0];
        }
    }

    public final void clearValues()
    {
        _values = null;
    }

    public final LispObject nothing()
    {
        _values = new LispObject[0];
        return NIL;
    }

    // Forces a single value, for situations where multiple values should be
    // ignored.
    public final LispObject value(LispObject obj)
    {
        _values = null;
        return obj;
    }



    final static int UNASSIGNED_SPECIAL_INDEX = 0;

    /** Indicates the last special slot which has been assigned.
     * Symbols which don't have a special slot assigned use a slot
     * index of 0 for efficiency reasons: it eliminates the need to
     * check for index validity before accessing the specials array.
     *
     */
    final static AtomicInteger lastSpecial
        = new AtomicInteger(UNASSIGNED_SPECIAL_INDEX);

    /** A list of indices which can be (re)used for symbols to
     * be assigned a special slot index.
     */
    final static ConcurrentLinkedQueue<Integer> freeSpecialIndices
        = new ConcurrentLinkedQueue<Integer>();
    
    /** This array stores the current special binding for every symbol
     * which has been globally or locally declared special.
     *
     * If the array element has a null value, this means there currently
     * is no active binding. If the array element contains a valid
     * SpecialBinding object, but the value field of it is null, that
     * indicates an "UNBOUND VARIABLE" situation.
     */
    SpecialBinding[] specials
        = new SpecialBinding[Integer.valueOf(System.getProperty("abcl.specials.initialSize","4096"))+1];

    /** The number of slots to grow the specials table in
     * case of insufficient storage.
     */
    final int specialsDelta
        = Integer.valueOf(System.getProperty("abcl.specials.grow.delta","1024"));

    /** This variable points to the head of a linked list of saved
     * special bindings. Its main purpose is to allow a mark/reset
     * interface to special binding and unbinding.
     */
    private SpecialBindingsMark savedSpecials = null;

    /** Marks the state of the special bindings,
     * for later rewinding by resetSpecialBindings().
     */
    public final SpecialBindingsMark markSpecialBindings() {
        return savedSpecials;
    }

    /** Restores the state of the special bindings to what
     * was captured in the marker 'mark' by a call to markSpecialBindings().
     */
    public final void resetSpecialBindings(SpecialBindingsMark mark) {
        SpecialBindingsMark c = savedSpecials;
        while (mark != c) {
            specials[c.idx] = c.binding;
            c = c.next;
        }
        savedSpecials = c;
    }

    /** Clears out all active special bindings including any marks
     * previously set. Invoking resetSpecialBindings() with marks
     * set before this call results in undefined behaviour.
     */
    // Package level access: only for Interpreter.run()
    final void clearSpecialBindings() {
        resetSpecialBindings(null);
    }

    /** Assigns a specials array index number to the symbol,
     * if it doesn't already have one.
     */
    private void assignSpecialIndex(Symbol sym)
    {
        if (sym.specialIndex != 0)
            return;

        synchronized (sym) {
            // Don't use an atomic access: we'll be swapping values only once.
            if (sym.specialIndex == 0) {
                Integer next = freeSpecialIndices.poll();
                if (next == null
                        && specials.length < lastSpecial.get()
                        && null == System.getProperty("abcl.specials.grow.slowly")) {
                    // free slots are exhausted; in the middle and at the end.
                    System.gc();
                    next = freeSpecialIndices.poll();
                }
                if (next == null)
                    sym.specialIndex = lastSpecial.incrementAndGet();
                else
                    sym.specialIndex = next.intValue();
            }
        }
    }

    /** Frees up an index previously assigned to a symbol for re-assignment
     * to another symbol. Returns without effect if the symbol has the
     * default UNASSIGNED_SPECIAL_INDEX special index.
     */
    protected static void releaseSpecialIndex(Symbol sym)
    {
        int index = sym.specialIndex;
        if (index != UNASSIGNED_SPECIAL_INDEX) {
            // clear out the values in the
            Iterator<LispThread> it = map.values().iterator();
            while (it.hasNext()) {
                LispThread thread = it.next();

                // clear out the values in the saved specials list
                SpecialBindingsMark savedSpecial = thread.savedSpecials;
                while (savedSpecial != null) {
                    if (savedSpecial.idx == index) {
                        savedSpecial.idx = 0;
                        savedSpecial.binding = null;
                    }
                    savedSpecial = savedSpecial.next;
                }

                thread.specials[index] = null;
            }

            freeSpecialIndices.add(new Integer(index));
        }
    }

    private void growSpecials() {
        SpecialBinding[] newSpecials
                = new SpecialBinding[specials.length + specialsDelta];
        System.arraycopy(specials, 0, newSpecials, 0, specials.length);
        specials = newSpecials;
    }

    private SpecialBinding ensureSpecialBinding(int idx) {
        SpecialBinding binding;
        boolean assigned;
        do {
            try {
                binding = specials[idx];
                assigned = true;
            }
            catch (ArrayIndexOutOfBoundsException e) {
                assigned = false;
                binding = null;  // suppresses 'unassigned' error
                growSpecials();
            }
        } while (! assigned);
        return binding;
    }

    public final SpecialBinding bindSpecial(Symbol name, LispObject value)
    {
        int idx;

        assignSpecialIndex(name);
        SpecialBinding binding = ensureSpecialBinding(idx = name.specialIndex);
        savedSpecials = new SpecialBindingsMark(idx, binding, savedSpecials);
        return specials[idx] = new SpecialBinding(idx, value);
    }

    public final SpecialBinding bindSpecialToCurrentValue(Symbol name)
    {
        int idx;

        assignSpecialIndex(name);
        SpecialBinding binding = ensureSpecialBinding(idx = name.specialIndex);
        savedSpecials = new SpecialBindingsMark(idx, binding, savedSpecials);
        return specials[idx]
            = new SpecialBinding(idx,
                                 (binding == null) ?
                                 name.getSymbolValue() : binding.value);
    }

    /** Looks up the value of a special binding in the context of the
     * given thread.
     *
     * In order to find the value of a special variable (in general),
     * use {@link Symbol#symbolValue}.
     *
     * @param name The name of the special variable, normally a symbol
     * @return The inner most binding of the special, or null if unbound
     *
     * @see Symbol#symbolValue
     */
    public final LispObject lookupSpecial(Symbol name)
    {
        SpecialBinding binding = ensureSpecialBinding(name.specialIndex);
        return (binding == null) ? null : binding.value;
    }

    public final SpecialBinding getSpecialBinding(Symbol name)
    {
        return ensureSpecialBinding(name.specialIndex);
    }

    public final LispObject setSpecialVariable(Symbol name, LispObject value)
    {
        SpecialBinding binding = ensureSpecialBinding(name.specialIndex);
        if (binding != null)
            return binding.value = value;

        name.setSymbolValue(value);
        return value;
    }

    public final LispObject pushSpecial(Symbol name, LispObject thing)

    {
        SpecialBinding binding = ensureSpecialBinding(name.specialIndex);
        if (binding != null)
            return binding.value = new Cons(thing, binding.value);

        LispObject value = name.getSymbolValue();
        if (value != null) {
            LispObject newValue = new Cons(thing, value);
            name.setSymbolValue(newValue);
            return newValue;
        } else
            return error(new UnboundVariable(name));
    }

    // Returns symbol value or NIL if unbound.
    public final LispObject safeSymbolValue(Symbol name)
    {
        SpecialBinding binding = ensureSpecialBinding(name.specialIndex);
        if (binding != null)
            return binding.value;

        LispObject value = name.getSymbolValue();
        return value != null ? value : NIL;
    }

    public final void rebindSpecial(Symbol name, LispObject value)
    {
        SpecialBinding binding = getSpecialBinding(name);
        binding.value = value;
    }

    private LispObject catchTags = NIL;

    public void pushCatchTag(LispObject tag)
    {
        catchTags = new Cons(tag, catchTags);
    }

    public void popCatchTag()
    {
        if (catchTags != NIL)
            catchTags = catchTags.cdr();
        else
            Debug.assertTrue(false);
    }

    public void throwToTag(LispObject tag, LispObject result)

    {
        LispObject rest = catchTags;
        while (rest != NIL) {
            if (rest.car() == tag)
                throw new Throw(tag, result, this);
            rest = rest.cdr();
        }
        error(new ControlError("Attempt to throw to the nonexistent tag " +
                                tag.writeToString() + "."));
    }


    private StackFrame stack = null;

   public final void pushStackFrame(StackFrame frame)
    {
        frame.setNext(stack);
        stack = frame;
    }

    public final void popStackFrame()
    {
        if (stack != null)
            stack = stack.getNext();
    }

    public void resetStack()
    {
        stack = null;
    }

    @Override
    public LispObject execute(LispObject function)
    {
        if (use_fast_calls)
            return function.execute();

        pushStackFrame(new LispStackFrame(function));
        try {
            return function.execute();
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject arg)

    {
        if (use_fast_calls)
            return function.execute(arg);

        pushStackFrame(new LispStackFrame(function, arg));
        try {
            return function.execute(arg);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second)

    {
        if (use_fast_calls)
            return function.execute(first, second);

        pushStackFrame(new LispStackFrame(function, first, second));
        try {
            return function.execute(first, second);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third)

    {
        if (use_fast_calls)
            return function.execute(first, second, third);

        pushStackFrame(new LispStackFrame(function, first, second, third));
        try {
            return function.execute(first, second, third);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth)

    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth);

        pushStackFrame(new LispStackFrame(function, first, second, third, fourth));
        try {
            return function.execute(first, second, third, fourth);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth)

    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth);

        pushStackFrame(new LispStackFrame(function, first, second, third, fourth, fifth));
        try {
            return function.execute(first, second, third, fourth, fifth);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth)

    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth);

        pushStackFrame(new LispStackFrame(function, first, second, 
					  third, fourth, fifth, sixth));
        try {
            return function.execute(first, second, third, fourth, fifth, sixth);
        }
        finally {
            popStackFrame();
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth, LispObject seventh)

    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh);

        pushStackFrame(new LispStackFrame(function, first, second, third, 
					  fourth, fifth, sixth, seventh));
        try {
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh);
        }
        finally {
            popStackFrame();
        }
    }

    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth, LispObject seventh,
                              LispObject eighth)

    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh, eighth);

        pushStackFrame(new LispStackFrame(function, first, second, third, 
					  fourth, fifth, sixth, seventh, eighth));
        try {
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh, eighth);
        }
        finally {
            popStackFrame();
        }
    }

    public LispObject execute(LispObject function, LispObject[] args)

    {
        if (use_fast_calls)
            return function.execute(args);

        pushStackFrame(new LispStackFrame(function, args));
        try {
            return function.execute(args);
        }
        finally {
            popStackFrame();
        }
    }

    public void printBacktrace()
    {
        printBacktrace(0);
    }

    public void printBacktrace(int limit)
    {
        if (stack != null) {
            int count = 0;
            Stream out =
                checkCharacterOutputStream(Symbol.TRACE_OUTPUT.symbolValue());
            out._writeLine("Evaluation stack:");
            out._finishOutput();

            StackFrame s = stack;
            while (s != null) {
                out._writeString("  ");
                out._writeString(String.valueOf(count));
                out._writeString(": ");

                pprint(s.toLispList(), out.getCharPos(), out);
                out.terpri();
                out._finishOutput();
                if (limit > 0 && ++count == limit)
                    break;
                s = s.next;
            }
        }
    }

    public LispObject backtrace(int limit)
    {
        LispObject result = NIL;
        if (stack != null) {
            int count = 0;
            StackFrame s = stack;
            while (s != null) {
                result = result.push(s);
                if (limit > 0 && ++count == limit)
                    break;
                s = s.getNext();
            }
        }
        return result.nreverse();
    }

    public void incrementCallCounts()
    {
        StackFrame s = stack;

        for (int i = 0; i < 8; i++) {
            if (s == null)
                break;
	    if (s instanceof LispStackFrame) {
		LispObject operator = ((LispStackFrame)s).getOperator();
		if (operator != null) {
		    operator.incrementHotCount();
		    operator.incrementCallCount();
		}
		s = s.getNext();
	    }
        }

        while (s != null) {
	    if (s instanceof LispStackFrame) {
		LispObject operator = ((LispStackFrame)s).getOperator();
		if (operator != null)
		    operator.incrementCallCount();
	    }
	    s = s.getNext();
        }
    }

    private static void pprint(LispObject obj, int indentBy, Stream stream)

    {
        if (stream.getCharPos() == 0) {
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i < indentBy; i++)
                sb.append(' ');
            stream._writeString(sb.toString());
        }
        String raw = obj.writeToString();
        if (stream.getCharPos() + raw.length() < 80) {
            // It fits.
            stream._writeString(raw);
            return;
        }
        // Object doesn't fit.
        if (obj instanceof Cons) {
            boolean newlineBefore = false;
            LispObject[] array = obj.copyToArray();
            if (array.length > 0) {
                LispObject first = array[0];
                if (first == Symbol.LET) {
                    newlineBefore = true;
                }
            }
            int charPos = stream.getCharPos();
            if (newlineBefore && charPos != indentBy) {
                stream.terpri();
                charPos = stream.getCharPos();
            }
            if (charPos < indentBy) {
                StringBuffer sb = new StringBuffer();
                for (int i = charPos; i < indentBy; i++)
                    sb.append(' ');
                stream._writeString(sb.toString());
            }
            stream.print('(');
            for (int i = 0; i < array.length; i++) {
                pprint(array[i], indentBy + 2, stream);
                if (i < array.length - 1)
                   stream.print(' ');
            }
            stream.print(')');
        } else {
            stream.terpri();
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i < indentBy; i++)
                sb.append(' ');
            stream._writeString(sb.toString());
            stream._writeString(raw);
            return;
        }
    }

    @Override
    public String writeToString()
    {
        StringBuffer sb = new StringBuffer("THREAD");
        if (name != NIL) {
            sb.append(" \"");
            sb.append(name.getStringValue());
            sb.append("\"");
        }
        return unreadableString(sb.toString());
    }

    @DocString(name="make-thread", args="function &optional &key name")
    private static final Primitive MAKE_THREAD =
        new Primitive("make-thread", PACKAGE_THREADS, true, "function &optional &key name")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            final int length = args.length;
            if (length == 0)
                error(new WrongNumberOfArgumentsException(this));
            LispObject name = NIL;
            if (length > 1) {
                if ((length - 1) % 2 != 0)
                    error(new ProgramError("Odd number of keyword arguments."));
                if (length > 3)
                    error(new WrongNumberOfArgumentsException(this));
                if (args[1] == Keyword.NAME)
                    name = args[2].STRING();
                else
                    error(new ProgramError("Unrecognized keyword argument " +
                                            args[1].writeToString() + "."));
            }
            return new LispThread(checkFunction(args[0]), name);
        }
    };

    @DocString(name="threadp", args="object",
    doc="Boolean predicate testing if OBJECT is a thread.")
    private static final Primitive THREADP =
        new Primitive("threadp", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return arg instanceof LispThread ? T : NIL;
        }
    };

    @DocString(name="thread-alive-p", args="thread",
    doc="Returns T if THREAD is alive.")
    private static final Primitive THREAD_ALIVE_P =
        new Primitive("thread-alive-p", PACKAGE_THREADS, true, "thread",
		      "Boolean predicate whether THREAD is alive.")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            final LispThread lispThread;
            if (arg instanceof LispThread) {
                lispThread = (LispThread) arg;
            }
            else {
                return type_error(arg, Symbol.THREAD);
            }
            return lispThread.javaThread.isAlive() ? T : NIL;
        }
    };

    @DocString(name="thread-name", args="thread",
    doc="Return the name of THREAD, if it has one.")
    private static final Primitive THREAD_NAME =
        new Primitive("thread-name", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
                if (arg instanceof LispThread) {
                return ((LispThread)arg).name;
            }
                 return type_error(arg, Symbol.THREAD);
        }
    };

    private static final Primitive THREAD_JOIN =
        new Primitive("thread-join", PACKAGE_THREADS, true, "thread",
                      "Waits for thread to finish.")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            // join the thread, and returns it's value.  The second return
            // value is T if the thread finishes normally, NIL if its 
            // interrupted. 
            if (arg instanceof LispThread) {                
                final LispThread joinedThread = (LispThread) arg;
                final LispThread waitingThread = currentThread();
                try {
                    joinedThread.javaThread.join();
                    return 
                        waitingThread.setValues(joinedThread.threadValue, T);
                } catch (InterruptedException e) {
                    waitingThread.processThreadInterrupts();
                    return 
                        waitingThread.setValues(joinedThread.threadValue, NIL);
                }
            } else {
                return type_error(arg, Symbol.THREAD);
            } 
        }
    };


    public static final long javaSleepInterval(LispObject lispSleep)

    {
        double d =
            checkDoubleFloat(lispSleep.multiplyBy(new DoubleFloat(1000))).getValue();
        if (d < 0)
            type_error(lispSleep, list(Symbol.REAL, Fixnum.ZERO));

        return (d < Long.MAX_VALUE ? (long) d : Long.MAX_VALUE);
    }

    @DocString(name="sleep", args="seconds",
    doc="Causes the invoking thread to sleep for SECONDS seconds.\n"+
        "SECONDS may be a value between 0 1and 1.")
    private static final Primitive SLEEP = new Primitive("sleep", PACKAGE_CL, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {

            try {
                Thread.sleep(javaSleepInterval(arg));
            }
            catch (InterruptedException e) {
                currentThread().processThreadInterrupts();
            }
            return NIL;
        }
    };

    @DocString(name="mapcar-threads", args= "function",
    doc="Applies FUNCTION to all existing threads.")
    private static final Primitive MAPCAR_THREADS =
        new Primitive("mapcar-threads", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            Function fun = checkFunction(arg);
            final LispThread thread = LispThread.currentThread();
            LispObject result = NIL;
            Iterator it = map.values().iterator();
            while (it.hasNext()) {
                LispObject[] args = new LispObject[1];
                args[0] = (LispThread) it.next();
                result = new Cons(funcall(fun, args, thread), result);
            }
            return result;
        }
    };

    @DocString(name="destroy-thread", args="thread", doc="Mark THREAD as destroyed")
    private static final Primitive DESTROY_THREAD =
        new Primitive("destroy-thread", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            final LispThread thread;
            if (arg instanceof LispThread) {
                thread = (LispThread) arg;
            }
            else {
                return type_error(arg, Symbol.THREAD);
            }
            thread.setDestroyed(true);
            return T;
        }
    };

    // => T
    @DocString(name="interrupt-thread", args="thread function &rest args",
    doc="Interrupts thread and forces it to apply function to args. When the\n"+
        "function returns, the thread's original computation continues. If\n"+
        "multiple interrupts are queued for a thread, they are all run, but the\n"+
        "order is not guaranteed.")
    private static final Primitive INTERRUPT_THREAD =
        new Primitive("interrupt-thread", PACKAGE_THREADS, true,
		      "thread function &rest args",
		      "Interrupts THREAD and forces it to apply FUNCTION to ARGS.\nWhen the function returns, the thread's original computation continues. If  multiple interrupts are queued for a thread, they are all run, but the order is not guaranteed.")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 2)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread;
            if (args[0] instanceof LispThread) {
                thread = (LispThread) args[0];
            }
            else {
                return type_error(args[0], Symbol.THREAD);
            }
            LispObject fun = args[1];
            LispObject funArgs = NIL;
            for (int i = args.length; i-- > 2;)
                funArgs = new Cons(args[i], funArgs);
            thread.interrupt(fun, funArgs);
            return T;
        }
    };

    @DocString(name="current-thread",
    doc="Returns a reference to invoking thread.")
    private static final Primitive CURRENT_THREAD =
        new Primitive("current-thread", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute()
        {
            return currentThread();
        }
    };

    @DocString(name="backtrace",
               doc="Returns a backtrace of the invoking thread.")
    private static final Primitive BACKTRACE =
        new Primitive("backtrace", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject[] args)

        {
            if (args.length > 1)
                return error(new WrongNumberOfArgumentsException(this));
            int limit = args.length > 0 ? Fixnum.getValue(args[0]) : 0;
            return currentThread().backtrace(limit);
        }
    };
    @DocString(name="frame-to-string", args="frame")
    private static final Primitive FRAME_TO_STRING =
        new Primitive("frame-to-string", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject[] args)

        {
            if (args.length != 1)
                return error(new WrongNumberOfArgumentsException(this));
            
            return checkStackFrame(args[0]).toLispString();
        }
    };

    @DocString(name="frame-to-list", args="frame")
    private static final Primitive FRAME_TO_LIST =
        new Primitive("frame-to-list", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject[] args)

        {
            if (args.length != 1)
                return error(new WrongNumberOfArgumentsException(this));

            return checkStackFrame(args[0]).toLispList();
        }
    };


    @DocString(name="use-fast-calls")
    private static final Primitive USE_FAST_CALLS =
        new Primitive("use-fast-calls", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            use_fast_calls = (arg != NIL);
            return use_fast_calls ? T : NIL;
        }
    };

    @DocString(name="synchronized-on", args="form &body body")
    private static final SpecialOperator SYNCHRONIZED_ON =
        new SpecialOperator("synchronized-on", PACKAGE_THREADS, true,
                            "form &body body")
    {
        @Override
        public LispObject execute(LispObject args, Environment env)

        {
          if (args == NIL)
            return error(new WrongNumberOfArgumentsException(this));

          LispThread thread = LispThread.currentThread();
          synchronized (eval(args.car(), env, thread).lockableInstance()) {
              return progn(args.cdr(), env, thread);
          }
        }
    };

    @DocString(name="object-wait", args="object &optional timeout")
    private static final Primitive OBJECT_WAIT =
        new Primitive("object-wait", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject object)

        {
            try {
                object.lockableInstance().wait();
            }
            catch (InterruptedException e) {
                currentThread().processThreadInterrupts();
            }
            catch (IllegalMonitorStateException e) {
                return error(new IllegalMonitorState());
            }
            return NIL;
        }

        @Override
        public LispObject execute(LispObject object, LispObject timeout)

        {
            try {
                object.lockableInstance().wait(javaSleepInterval(timeout));
            }
            catch (InterruptedException e) {
                currentThread().processThreadInterrupts();
            }
            catch (IllegalMonitorStateException e) {
                return error(new IllegalMonitorState());
            }
            return NIL;
        }
    };

    @DocString(name="object-notify", args="object")
    private static final Primitive OBJECT_NOTIFY =
        new Primitive("object-notify", PACKAGE_THREADS, true,
                      "object")
    {
        @Override
        public LispObject execute(LispObject object)

        {
            try {
                object.lockableInstance().notify();
            }
            catch (IllegalMonitorStateException e) {
                return error(new IllegalMonitorState());
            }
            return NIL;
        }
    };

    @DocString(name="object-notify-all", args="object")
    private static final Primitive OBJECT_NOTIFY_ALL =
        new Primitive("object-notify-all", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject object)

        {
            try {
                object.lockableInstance().notifyAll();
            }
            catch (IllegalMonitorStateException e) {
                return error(new IllegalMonitorState());
            }
            return NIL;
        }
    };


}
