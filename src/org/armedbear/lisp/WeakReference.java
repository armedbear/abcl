/*
 * WeakReference.java
 *
 * Copyright (C) 2011 Erik Huelsmann
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

public class WeakReference extends LispObject {

    java.lang.ref.WeakReference<LispObject> ref;

    public WeakReference(LispObject ref) {
        this.ref = new java.lang.ref.WeakReference<LispObject>(ref);
    }

    @Override
    public LispObject typeOf() {
        return Symbol.WEAK_REFERENCE;
    }

    @Override
    public LispObject classOf() {
        return BuiltInClass.WEAK_REFERENCE;
    }

    @Override
    public String printObject() {
        return unreadableString("WEAK-REFERENCE "
                + toString());
    }

    @Override
    public LispObject typep(LispObject typeSpecifier) {
        if (typeSpecifier == Symbol.WEAK_REFERENCE) {
            return T;
        }
        if (typeSpecifier == BuiltInClass.WEAK_REFERENCE) {
            return T;
        }
        return super.typep(typeSpecifier);
    }

    private static final Primitive MAKE_WEAK_REFERENCE =
            new pf_make_weak_reference();
    @DocString(name="make-weak-reference", args="obj",
    doc="Creates a weak reference to 'obj'.")
    private static final class pf_make_weak_reference extends Primitive
    {
        pf_make_weak_reference()
        {
            super("make-weak-reference", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject obj) {
	    return new WeakReference(obj);
        }
    };

    private static final Primitive WEAK_REFERENCE_VALUE =
            new pf_weak_reference_value();
    @DocString(name="weak-reference-value", args="obj",
    doc="Returns two values, the first being the value of the weak ref,"
            + "the second T if the reference is valid, or NIL if it has"
            + "been cleared.")
    private static final class pf_weak_reference_value extends Primitive
    {
        pf_weak_reference_value()
        {
            super("weak-reference-value", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject obj) {
            if (! (obj instanceof WeakReference))
                return Lisp.type_error(obj, Symbol.WEAK_REFERENCE);
            
            LispObject value = ((WeakReference)obj).ref.get();
	    return LispThread.currentThread().setValues(value == null ? NIL : value,
                                                        value == null ? NIL : T);
        }
    };
}
