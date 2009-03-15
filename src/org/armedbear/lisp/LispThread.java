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

import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;

public final class LispThread extends LispObject
{
    private static boolean use_fast_calls = false;

    // use a concurrent hashmap: we may want to add threads
    // while at the same time iterating the hash
    final private static ConcurrentHashMap<Thread,LispThread> map =
       new ConcurrentHashMap<Thread,LispThread>();

    private static ThreadLocal<LispThread> threads = new ThreadLocal<LispThread>(){
        @Override
        public LispThread initialValue() {
            Thread thisThread = Thread.currentThread();
            LispThread newThread = new LispThread(thisThread);
            LispThread.map.put(thisThread,newThread);
            return newThread;
        }
    };

    public static final LispThread currentThread()
    {
        return threads.get();
    }

   private final Thread javaThread;
    private boolean destroyed;
    private final LispObject name;
    public SpecialBinding lastSpecialBinding;
    public LispObject[] _values;
    private boolean threadInterrupted;
    private LispObject pending = NIL;

    private LispThread(Thread javaThread)
    {
        this.javaThread = javaThread;
        name = new SimpleString(javaThread.getName());
    }

    private LispThread(final Function fun, LispObject name)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                try {
                    funcall(fun, new LispObject[0], LispThread.this);
                }
                catch (ThreadDestroyed ignored) {
                      // Might happen.
                }
                catch (Throwable t) {
                    if (isInterrupted()) {
                        try {
                            processThreadInterrupts();
                        }
                        catch (ConditionThrowable c) {
                            Debug.trace(c);
                        }
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
        javaThread.setDaemon(true);
        javaThread.start();
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
    public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
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

    private final synchronized boolean isInterrupted()
    {
        return threadInterrupted;
    }

    private final synchronized void setDestroyed(boolean b)
    {
        destroyed = b;
    }

    private final synchronized void interrupt(LispObject function, LispObject args)
    {
        pending = new Cons(args, pending);
        pending = new Cons(function, pending);
        threadInterrupted = true;
        javaThread.interrupt();
    }

    private final synchronized void processThreadInterrupts()
        throws ConditionThrowable
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

    public final void bindSpecial(Symbol name, LispObject value)
    {
        lastSpecialBinding = new SpecialBinding(name, value, lastSpecialBinding);
    }

    public final void bindSpecialToCurrentValue(Symbol name)
    {
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name) {
                lastSpecialBinding =
                    new SpecialBinding(name, binding.value, lastSpecialBinding);
                return;
            }
            binding = binding.next;
        }
        // Not found.
        lastSpecialBinding =
            new SpecialBinding(name, name.getSymbolValue(), lastSpecialBinding);
    }

    public final LispObject lookupSpecial(LispObject name)
    {
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name)
                return binding.value;
            binding = binding.next;
        }
        return null;
    }

    public final SpecialBinding getSpecialBinding(LispObject name)
    {
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name)
                return binding;
            binding = binding.next;
        }
        return null;
    }

    public final LispObject setSpecialVariable(Symbol name, LispObject value)
    {
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name) {
                binding.value = value;
                return value;
            }
            binding = binding.next;
        }
        name.setSymbolValue(value);
        return value;
    }

    public final LispObject pushSpecial(Symbol name, LispObject thing)
        throws ConditionThrowable
    {
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name) {
                LispObject newValue = new Cons(thing, binding.value);
                binding.value = newValue;
                return newValue;
            }
            binding = binding.next;
        }
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
        SpecialBinding binding = lastSpecialBinding;
        while (binding != null) {
            if (binding.name == name)
                return binding.value;
            binding = binding.next;
        }
        LispObject value = name.getSymbolValue();
        return value != null ? value : NIL;
    }

    public final void rebindSpecial(Symbol name, LispObject value)
    {
        SpecialBinding binding = getSpecialBinding(name);
        binding.value = value;
    }

    private LispObject catchTags = NIL;

    public void pushCatchTag(LispObject tag) throws ConditionThrowable
    {
        catchTags = new Cons(tag, catchTags);
    }

    public void popCatchTag() throws ConditionThrowable
    {
        if (catchTags != NIL)
            catchTags = catchTags.cdr();
        else
            Debug.assertTrue(false);
    }

    public void throwToTag(LispObject tag, LispObject result)
        throws ConditionThrowable
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

    private static class StackFrame extends LispObject
    {
        public final LispObject operator;
        private final LispObject first;
        private final LispObject second;
        private final LispObject third;
        private final LispObject[] args;

        public StackFrame(LispObject operator)
        {
            this.operator = operator;
            first = null;
            second = null;
            third = null;
            args = null;
        }

        public StackFrame(LispObject operator, LispObject arg)
        {
            this.operator = operator;
            first = arg;
            second = null;
            third = null;
            args = null;
        }

        public StackFrame(LispObject operator, LispObject first,
                          LispObject second)
        {
            this.operator = operator;
            this.first = first;
            this.second = second;
            third = null;
            args = null;
        }

        public StackFrame(LispObject operator, LispObject first,
                          LispObject second, LispObject third)
        {
            this.operator = operator;
            this.first = first;
            this.second = second;
            this.third = third;
            args = null;
        }

        public StackFrame(LispObject operator, LispObject[] args)
        {
            this.operator = operator;
            first = null;
            second = null;
            third = null;
            this.args = new LispObject[args.length];
            for (int i = args.length; i-- > 0;)
                this.args[i] = args[i];
        }

        public LispObject toList() throws ConditionThrowable
        {
            LispObject list = NIL;
            if (args != null) {
                for (int i = 0; i < args.length; i++)
                    list = list.push(args[i]);
            } else {
                do {
                    if (first != null)
                        list = list.push(first);
                    else
                        break;
                    if (second != null)
                        list = list.push(second);
                    else
                        break;
                    if (third != null)
                        list = list.push(third);
                    else
                        break;
                } while (false);
            }
            list = list.nreverse();
            if (operator instanceof Operator) {
                LispObject lambdaName = ((Operator)operator).getLambdaName();
                if (lambdaName != null && lambdaName != NIL)
                    return list.push(lambdaName);
            }
            return list.push(operator);
        }
    }

    private LispObject stack = NIL;

    public LispObject getStack()
    {
        return stack;
    }

    public void setStack(LispObject stack)
    {
        this.stack = stack;
    }

    public void pushStackFrame(LispObject operator)
        throws ConditionThrowable
    {
        stack = new Cons((new StackFrame(operator)), stack);
        if (profiling && sampling) {
            if (sampleNow)
                Profiler.sample(this);
        }
    }

    public void pushStackFrame(LispObject operator, LispObject arg)
        throws ConditionThrowable
    {
        stack = new Cons((new StackFrame(operator, arg)), stack);
        if (profiling && sampling) {
            if (sampleNow)
                Profiler.sample(this);
        }
    }

    public void pushStackFrame(LispObject operator, LispObject first,
                               LispObject second)
        throws ConditionThrowable
    {
        stack = new Cons((new StackFrame(operator, first, second)), stack);
        if (profiling && sampling) {
            if (sampleNow)
                Profiler.sample(this);
        }
    }

    public void pushStackFrame(LispObject operator, LispObject first,
                               LispObject second, LispObject third)
        throws ConditionThrowable
    {
        stack = new Cons((new StackFrame(operator, first, second, third)),
                         stack);
        if (profiling && sampling) {
            if (sampleNow)
                Profiler.sample(this);
        }
    }

    public void pushStackFrame(LispObject operator, LispObject[] args)
        throws ConditionThrowable
    {
        stack = new Cons((new StackFrame(operator, args)), stack);
        if (profiling && sampling) {
            if (sampleNow)
                Profiler.sample(this);
        }
    }

    public void resetStack()
    {
        stack = NIL;
    }

    @Override
    public LispObject execute(LispObject function) throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute();

        LispObject oldStack = stack;
        pushStackFrame(function);
        try {
            return function.execute();
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject arg)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(arg);

        LispObject oldStack = stack;
        pushStackFrame(function, arg);
        try {
            return function.execute(arg);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second);

        LispObject oldStack = stack;
        pushStackFrame(function, first, second);
        try {
            return function.execute(first, second);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third);

        LispObject oldStack = stack;
        pushStackFrame(function, first, second, third);
        try {
            return function.execute(first, second, third);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth);

        LispObject oldStack = stack;
        LispObject[] args = new LispObject[4];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        pushStackFrame(function, args);
        try {
            return function.execute(first, second, third, fourth);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth);

        LispObject oldStack = stack;
        LispObject[] args = new LispObject[5];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        pushStackFrame(function, args);
        try {
            return function.execute(first, second, third, fourth, fifth);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth);

        LispObject oldStack = stack;
        LispObject[] args = new LispObject[6];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        pushStackFrame(function, args);
        try {
            return function.execute(first, second, third, fourth, fifth, sixth);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    @Override
    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth, LispObject seventh)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh);

        LispObject oldStack = stack;
        LispObject[] args = new LispObject[7];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        args[6] = seventh;
        pushStackFrame(function, args);
        try {
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    public LispObject execute(LispObject function, LispObject first,
                              LispObject second, LispObject third,
                              LispObject fourth, LispObject fifth,
                              LispObject sixth, LispObject seventh,
                              LispObject eighth)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh, eighth);

        LispObject oldStack = stack;
        LispObject[] args = new LispObject[8];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        args[6] = seventh;
        args[7] = eighth;
        pushStackFrame(function, args);
        try {
            return function.execute(first, second, third, fourth, fifth, sixth,
                                    seventh, eighth);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    public LispObject execute(LispObject function, LispObject[] args)
        throws ConditionThrowable
    {
        if (use_fast_calls)
            return function.execute(args);

        LispObject oldStack = stack;
        pushStackFrame(function, args);
        try {
            return function.execute(args);
        }
        finally {
            if (profiling && sampling) {
                if (sampleNow)
                    Profiler.sample(this);
            }
            stack = oldStack;
        }
    }

    public void backtrace()
    {
        backtrace(0);
    }

    public void backtrace(int limit)
    {
        if (stack != NIL) {
            try {
                int count = 0;
                Stream out =
                    checkCharacterOutputStream(Symbol.TRACE_OUTPUT.symbolValue());
                out._writeLine("Evaluation stack:");
                out._finishOutput();
                while (stack != NIL) {
                    out._writeString("  ");
                    out._writeString(String.valueOf(count));
                    out._writeString(": ");
                    StackFrame frame = (StackFrame) stack.car();
                    pprint(frame.toList(), out.getCharPos(), out);
                    out.terpri();
                    out._finishOutput();
                    if (limit > 0 && ++count == limit)
                        break;
                    stack = stack.cdr();
                }
            }
            catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    public LispObject backtraceAsList(int limit) throws ConditionThrowable
    {
        LispObject result = NIL;
        if (stack != NIL) {
            int count = 0;
            try {
                LispObject s = stack;
                while (s != NIL) {
                    StackFrame frame = (StackFrame) s.car();
                    if (frame != null) {
                        result = result.push(frame.toList());
                        if (limit > 0 && ++count == limit)
                            break;
                    }
                    s = s.cdr();
                }
            }
            catch (Throwable t) {
                t.printStackTrace();
            }
        }
        return result.nreverse();
    }

    public void incrementCallCounts() throws ConditionThrowable
    {
        LispObject s = stack;
        while (s != NIL) {
            StackFrame frame = (StackFrame) s.car();
            if (frame != null) {
                LispObject operator = frame.operator;
                if (operator != null)
                    operator.incrementCallCount();
            }
            s = s.cdr();
        }
    }

    private static void pprint(LispObject obj, int indentBy, Stream stream)
        throws ConditionThrowable
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
            try {
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
            }
            catch (ConditionThrowable t) {
                Debug.trace(t);
            }
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
    public String writeToString() throws ConditionThrowable
    {
        StringBuffer sb = new StringBuffer("THREAD");
        if (name != NIL) {
            sb.append(" \"");
            sb.append(name.getStringValue());
            sb.append("\"");
        }
        return unreadableString(sb.toString());
    }

    // ### make-thread
    private static final Primitive MAKE_THREAD =
        new Primitive("make-thread", PACKAGE_EXT, true, "function &key name")
    {
        @Override
        public LispObject execute(LispObject[] args) throws ConditionThrowable
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

    // ### threadp
    private static final Primitive THREADP =
        new Primitive("threadp", PACKAGE_EXT, true, "object")
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            return arg instanceof LispThread ? T : NIL;
        }
    };

    // ### thread-alive-p
    private static final Primitive THREAD_ALIVE_P =
        new Primitive("thread-alive-p", PACKAGE_EXT, true, "thread")
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            final LispThread lispThread;
            try {
                lispThread = (LispThread) arg;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.THREAD);
            }
            return lispThread.javaThread.isAlive() ? T : NIL;
        }
    };

    // ### thread-name
    private static final Primitive THREAD_NAME =
        new Primitive("thread-name", PACKAGE_EXT, true, "thread")
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            try {
                return ((LispThread)arg).name;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.THREAD);
            }
        }
    };

    // ### sleep
    private static final Primitive SLEEP = new Primitive("sleep", "seconds")
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            double d =
                ((DoubleFloat)arg.multiplyBy(new DoubleFloat(1000))).getValue();
            if (d < 0)
                return type_error(arg, list(Symbol.REAL, Fixnum.ZERO));
            long millis = d < Long.MAX_VALUE ? (long) d : Long.MAX_VALUE;
            try {
                Thread.sleep(millis);
            }
            catch (InterruptedException e) {
                currentThread().processThreadInterrupts();
            }
            return NIL;
        }
    };

    // ### mapcar-threads
    private static final Primitive MAPCAR_THREADS =
        new Primitive("mapcar-threads", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
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

    // ### destroy-thread
    private static final Primitive DESTROY_THREAD =
        new Primitive("destroy-thread", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            final LispThread thread;
            try {
                thread = (LispThread) arg;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.THREAD);
            }
            thread.setDestroyed(true);
            return T;
        }
    };

    // ### interrupt-thread thread function &rest args => T
    // Interrupts thread and forces it to apply function to args. When the
    // function returns, the thread's original computation continues. If
    // multiple interrupts are queued for a thread, they are all run, but the
    // order is not guaranteed.
    private static final Primitive INTERRUPT_THREAD =
        new Primitive("interrupt-thread", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject[] args) throws ConditionThrowable
        {
            if (args.length < 2)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread;
            try {
                thread = (LispThread) args[0];
            }
            catch (ClassCastException e) {
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

    // ### current-thread
    private static final Primitive CURRENT_THREAD =
        new Primitive("current-thread", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute() throws ConditionThrowable
        {
            return currentThread();
        }
    };

    // ### backtrace-as-list
    private static final Primitive BACKTRACE_AS_LIST =
        new Primitive("backtrace-as-list", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject[] args)
            throws ConditionThrowable
        {
            if (args.length > 1)
                return error(new WrongNumberOfArgumentsException(this));
            int limit = args.length > 0 ? Fixnum.getValue(args[0]) : 0;
            return currentThread().backtraceAsList(limit);
        }
    };

    // ### use-fast-calls
    private static final Primitive USE_FAST_CALLS =
        new Primitive("use-fast-calls", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            use_fast_calls = (arg != NIL);
            return use_fast_calls ? T : NIL;
        }
    };
}
