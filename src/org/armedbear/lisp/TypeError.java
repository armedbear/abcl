/*
 * TypeError.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

public class TypeError extends LispError
{
    public TypeError() throws ConditionThrowable
    {
        super(StandardClass.TYPE_ERROR);
    }

    protected TypeError(LispClass cls) throws ConditionThrowable
    {
        super(cls);
    }

    public TypeError(LispObject datum, LispObject expectedType)
        throws ConditionThrowable
    {
        super(StandardClass.TYPE_ERROR);
        setDatum(datum);
        setExpectedType(expectedType);
    }

    public TypeError(LispObject initArgs) throws ConditionThrowable
    {
        super(StandardClass.TYPE_ERROR);
        initialize(initArgs);
    }

    protected void initialize(LispObject initArgs) throws ConditionThrowable
    {
        super.initialize(initArgs);
        LispObject datum = null;
        LispObject expectedType = null;
        LispObject first, second;
        while (initArgs != NIL) {
            first = initArgs.car();
            initArgs = initArgs.cdr();
            second = initArgs.car();
            initArgs = initArgs.cdr();
            if (first == Keyword.DATUM) {
                if (datum == null)
                    datum = second;
            } else if (first == Keyword.EXPECTED_TYPE) {
                if (expectedType == null)
                    expectedType = second;
            }
        }
        if (datum != null)
            setDatum(datum);
        if (expectedType != null)
            setExpectedType(expectedType);
    }

    public TypeError(String message) throws ConditionThrowable
    {
        super(StandardClass.TYPE_ERROR);
        setFormatControl(message);
        setDatum(NIL);
        setExpectedType(NIL);
    }

    public TypeError(String message, LispObject datum, LispObject expectedType)
        throws ConditionThrowable
    {
        super(StandardClass.TYPE_ERROR);
        setFormatControl(message);
        setDatum(datum);
        setExpectedType(expectedType);
    }

    public LispObject typeOf()
    {
        return Symbol.TYPE_ERROR;
    }

    public LispObject classOf()
    {
        return StandardClass.TYPE_ERROR;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.TYPE_ERROR)
            return T;
        if (type == StandardClass.TYPE_ERROR)
            return T;
        return super.typep(type);
    }

    public String getMessage()
    {
        // FIXME
        try {
            final LispThread thread = LispThread.currentThread();
            final SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
            thread.bindSpecial(Symbol.PRINT_ESCAPE, T);
            try {
                String s = super.getMessage();
                if (s != null)
                    return s;
                final LispObject datum = getDatum();
                final LispObject expectedType = getExpectedType();
                FastStringBuffer sb = new FastStringBuffer();
                String name = datum != null ? datum.writeToString() : null;
                String type = null;
                if (expectedType != null)
                    type = expectedType.writeToString();
                if (type != null) {
                    if (name != null) {
                        sb.append("The value ");
                        sb.append(name);
                    } else
                        sb.append("Value");
                    sb.append(" is not of type ");
                    sb.append(type);
                } else if (name != null) {
                    sb.append("Wrong type: ");
                    sb.append(name);
                }
                sb.append('.');
                return sb.toString();
            }
            catch (Throwable t) {
                // FIXME
                Debug.trace(t);
                return toString();
            }
            finally {
                thread.lastSpecialBinding = lastSpecialBinding;
            }
        }
        catch (Throwable t) {
            return toString();
        }
    }

    public final LispObject getDatum() throws ConditionThrowable
    {
        return getInstanceSlotValue(Symbol.DATUM);
    }

    private final void setDatum(LispObject datum) throws ConditionThrowable
    {
        setInstanceSlotValue(Symbol.DATUM, datum);
    }

    public final LispObject getExpectedType() throws ConditionThrowable
    {
        return getInstanceSlotValue(Symbol.EXPECTED_TYPE);
    }

    private final void setExpectedType(LispObject expectedType)
        throws ConditionThrowable
    {
        setInstanceSlotValue(Symbol.EXPECTED_TYPE, expectedType);
    }

    // ### type-error-datum
    private static final Primitive TYPE_ERROR_DATUM =
        new Primitive(Symbol.TYPE_ERROR_DATUM, "condition")
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            final StandardObject obj;
            try {
                obj = (StandardObject) arg;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.STANDARD_OBJECT);
            }
            return obj.getInstanceSlotValue(Symbol.DATUM);
        }
    };

    // ### type-error-expected-type
    private static final Primitive TYPE_ERROR_EXPECTED_TYPE =
        new Primitive(Symbol.TYPE_ERROR_EXPECTED_TYPE, "condition")
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            final StandardObject obj;
            try {
                obj = (StandardObject) arg;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.STANDARD_OBJECT);
            }
            return obj.getInstanceSlotValue(Symbol.EXPECTED_TYPE);
        }
    };
}
