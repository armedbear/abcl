/*
 * SpecialBinding.java
 *
 * Copyright (C) 2002-2008 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

final public class SpecialBinding
{
    /** The index in the specials array of the symbol
     *  to which this value belongs.
     */
    final int idx;

    /** The value bound */
    public LispObject value;

    SpecialBinding(int idx, LispObject value)
    {
        this.idx = idx;
        this.value = value;
    }

    /** Return the value of the binding,
     * checking a valid binding.
     *
     * If the binding is invalid, an unbound variable error
     * is raised.
     */
    final public LispObject getValue()
    {
        if (value == null)
            // return or not: error doesn't return anyway
            Lisp.error(new UnboundVariable(LispThread.specialNames.get(new Integer(idx)).get()));

        return value;
    }

    /** Sets the value of the binding.
     *
     * Note: this method can only be called when the
     *    binding is the one which is currently visible.
     */
    final public void setValue(LispObject value)
    {
        this.value = value;
    }
}
