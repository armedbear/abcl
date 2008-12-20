/*
 * AbstractArray.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public abstract class AbstractArray extends LispObject
{
    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.ARRAY)
            return T;
        if (type == BuiltInClass.ARRAY)
            return T;
        return super.typep(type);
    }

    public boolean equalp(LispObject obj) throws ConditionThrowable
    {
        if (obj instanceof AbstractArray) {
            AbstractArray a = (AbstractArray) obj;
            if (getRank() != a.getRank())
                return false;
            for (int i = getRank(); i-- > 0;) {
                if (getDimension(i) != a.getDimension(i))
                    return false;
            }
            for (int i = getTotalSize(); i--> 0;) {
                if (!AREF(i).equalp(a.AREF(i)))
                    return false;
            }
            return true;
        }
        return false;
    }

    public boolean isDisplaced()
    {
        return false;
    }

    public LispObject arrayDisplacement() throws ConditionThrowable
    {
        return LispThread.currentThread().setValues(NIL, Fixnum.ZERO);
    }

    public boolean hasFillPointer()
    {
        return false;
    }

    public int getFillPointer() throws ConditionThrowable
    {
        noFillPointer();
        return -1; // Not reached.
    }

    public boolean isAdjustable()
    {
        return true;
    }

    public abstract int getRank();

    public abstract LispObject getDimensions();

    public abstract int getDimension(int n) throws ConditionThrowable;

    public abstract LispObject getElementType();

    public abstract int getTotalSize();

    public abstract void aset(int index, LispObject newValue)
        throws ConditionThrowable;

    // FIXME Detect overflow!
    protected static final int computeTotalSize(int[] dimensions)
    {
        int size = 1;
        for (int i = dimensions.length; i-- > 0;)
            size *= dimensions[i];
        return size;
    }

    public int getRowMajorIndex(LispObject[] subscripts)
        throws ConditionThrowable
    {
        int[] subs = new int[subscripts.length];
        for (int i = 0; i < subscripts.length; i++) {
            LispObject subscript = subscripts[i];
            if (subscript instanceof Fixnum)
                subs[i] = ((Fixnum)subscript).value;
            else
                type_error(subscript, Symbol.FIXNUM);
        }
        return getRowMajorIndex(subs);
    }

    public int getRowMajorIndex(int[] subscripts) throws ConditionThrowable
    {
        final int rank = getRank();
        if (rank != subscripts.length) {
            FastStringBuffer sb =
                new FastStringBuffer("Wrong number of subscripts (");
            sb.append(subscripts.length);
            sb.append(") for array of rank ");
            sb.append(rank);
            sb.append('.');
            error(new ProgramError(sb.toString()));
        }
        int sum = 0;
        int size = 1;
        for (int i = rank; i-- > 0;) {
            final int dim = getDimension(i);
            final int lastSize = size;
            size *= dim;
            final int n = subscripts[i];
            if (n < 0 || n >= dim) {
                FastStringBuffer sb = new FastStringBuffer("Invalid index ");
                sb.append(n);
                sb.append(" for array ");
                sb.append(writeToString());
                sb.append('.');
                error(new ProgramError(sb.toString()));
            }
            sum += n * lastSize;
        }
        return sum;
    }

    public LispObject get(int[] subscripts) throws ConditionThrowable
    {
        return AREF(getRowMajorIndex(subscripts));
    }

    public void set(int[] subscripts, LispObject newValue)
        throws ConditionThrowable
    {
        aset(getRowMajorIndex(subscripts), newValue);
    }

    public abstract void fill(LispObject obj) throws ConditionThrowable;

    public String writeToString(int[] dimv) throws ConditionThrowable
    {
        FastStringBuffer sb = new FastStringBuffer();
        LispThread thread = LispThread.currentThread();
        LispObject printReadably = Symbol.PRINT_READABLY.symbolValue(thread);
        if (printReadably != NIL || Symbol.PRINT_ARRAY.symbolValue(thread) != NIL) {
            int maxLevel = Integer.MAX_VALUE;
            if (printReadably != NIL) {
                for (int i = 0; i < dimv.length - 1; i++) {
                    if (dimv[i] == 0) {
                        for (int j = i + 1; j < dimv.length; j++) {
                            if (dimv[j] != 0) {
                                error(new PrintNotReadable(list2(Keyword.OBJECT,
                                                                  this)));
                                return null; // Not reached.
                            }
                        }
                    }
                }
            } else {
                LispObject printLevel = Symbol.PRINT_LEVEL.symbolValue(thread);
                if (printLevel instanceof Fixnum)
                    maxLevel = ((Fixnum)printLevel).value;
            }
            LispObject currentPrintLevel =
                _CURRENT_PRINT_LEVEL_.symbolValue(thread);
            int currentLevel = Fixnum.getValue(currentPrintLevel);
            if (currentLevel >= maxLevel)
                return "#";
            sb.append('#');
            sb.append(dimv.length);
            sb.append('A');
            appendContents(dimv, 0, sb, thread);
            return sb.toString();
        }
        sb.append('(');
        if (this instanceof SimpleArray_T)
            sb.append("SIMPLE-");
        sb.append("ARRAY T (");
        for (int i = 0; i < dimv.length; i++) {
            sb.append(dimv[i]);
            if (i < dimv.length - 1)
                sb.append(' ');
        }
        sb.append("))");
        return unreadableString(sb.toString());
    }

    // Helper for writeToString().
    private void appendContents(int[] dimensions, int index, FastStringBuffer sb,
                                LispThread thread)
        throws ConditionThrowable
    {
        if (dimensions.length == 0) {
            if (Symbol.PRINT_CIRCLE.symbolValue(thread) != NIL) {
                StringOutputStream stream = new StringOutputStream();
                thread.execute(Symbol.OUTPUT_OBJECT.getSymbolFunction(),
                               AREF(index), stream);
                sb.append(stream.getString().getStringValue());
            } else
                sb.append(AREF(index).writeToString());
        } else {
            final LispObject printReadably =
                Symbol.PRINT_READABLY.symbolValue(thread);
            int maxLength = Integer.MAX_VALUE;
            int maxLevel = Integer.MAX_VALUE;
            if (printReadably == NIL) {
                final LispObject printLength =
                    Symbol.PRINT_LENGTH.symbolValue(thread);
                if (printLength instanceof Fixnum)
                    maxLength = ((Fixnum)printLength).value;
                final LispObject printLevel =
                    Symbol.PRINT_LEVEL.symbolValue(thread);
                if (printLevel instanceof Fixnum)
                    maxLevel = ((Fixnum)printLevel).value;
            }
            LispObject currentPrintLevel =
                _CURRENT_PRINT_LEVEL_.symbolValue(thread);
            int currentLevel = Fixnum.getValue(currentPrintLevel);
            if (currentLevel < maxLevel) {
                SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                thread.bindSpecial(_CURRENT_PRINT_LEVEL_, currentPrintLevel.incr());
                try {
                    sb.append('(');
                    int[] dims = new int[dimensions.length - 1];
                    for (int i = 1; i < dimensions.length; i++)
                        dims[i-1] = dimensions[i];
                    int count = 1;
                    for (int i = 0; i < dims.length; i++)
                        count *= dims[i];
                    final int length = dimensions[0];
                    final int limit = Math.min(length, maxLength);
                    for (int i = 0; i < limit; i++) {
                        appendContents(dims, index, sb, thread);
                        if (i < limit - 1 || limit < length)
                            sb.append(' ');
                        index += count;
                    }
                    if (limit < length)
                        sb.append("...");
                    sb.append(')');
                }
                finally {
                    thread.lastSpecialBinding = lastSpecialBinding;
                }
            } else
                sb.append('#');
        }
    }

    // For EQUALP hash tables.
    public int psxhash()
    {
        try {
            long result = 128387; // Chosen at random.
            final int rank = getRank();
            int limit = rank < 4 ? rank : 4;
            for (int i = 0; i < limit; i++)
                result = mix(result, getDimension(i));
            final int length = getTotalSize();
            limit = length < 4 ? length : 4;
            for (int i = 0; i < length; i++)
                result = mix(result, AREF(i).psxhash());
            return (int) (result & 0x7fffffff);
        }
        catch (Throwable t) {
            // Shouldn't happen.
            Debug.trace(t);
            return 0;
        }
    }
}
