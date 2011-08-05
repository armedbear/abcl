/*
 * jclass_name.java
 *
 * Copyright (C) 2005 Peter Graves
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

// ### jclass-name class-ref &optional name
public final class jclass_name extends Primitive
{
    private jclass_name()
    {
        super(Symbol.JCLASS_NAME, "class-ref &optional name",
"When called with one argument, returns the name of the Java class\n" +
"  designated by CLASS-REF. When called with two arguments, tests\n" +
"  whether CLASS-REF matches NAME.");
    }

    // When called with one argument, JCLASS-NAME returns the name of the class
    // referenced by CLASS-REF.
    @Override
    public LispObject execute(LispObject arg)

    {
        if (arg instanceof AbstractString) {
            String s = arg.getStringValue();
            try {
                return new SimpleString((Class.forName(s)).getName());
            }
            catch (ClassNotFoundException e) {
                // Fall through.
            }
        } else if (arg instanceof JavaObject) {
            Object obj = ((JavaObject)arg).getObject();
            if (obj instanceof Class)
                return new SimpleString(((Class)obj).getName());
            // Fall through.
        }
        return error(new LispError(arg.princToString() + " does not designate a Java class."));
    }

    // When called with two arguments, JCLASS-NAME tests whether CLASS-REF
    // matches NAME.
    @Override
    public LispObject execute(LispObject first, LispObject second)

    {
        String className = null;
        if (first instanceof AbstractString) {
            String s = first.getStringValue();
            try {
                className = (Class.forName(s)).getName();
            }
            catch (ClassNotFoundException e) {}
        } else if (first instanceof JavaObject) {
            Object obj = ((JavaObject)first).getObject();
            if (obj instanceof Class)
                className = ((Class)obj).getName();
        }
        if (className == null)
            return error(new LispError(first.princToString() + " does not designate a Java class."));
        final AbstractString name = checkString(second);
        return LispThread.currentThread().setValues(name.getStringValue().equals(className) ? T : NIL,
                                                    new SimpleString(className));
    }

    private static final Primitive JCLASS_NAME = new jclass_name();
}
