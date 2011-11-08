/*
 * Binding.java
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

/** Used by the environment to capture different kinds of bindings:
 * tags, blocks, functions and variables.
 *
 */
// Package accessibility.
final class Binding
{
    /** The symbol in case of a variable, block, symbol-macro or
     * non-SETF function binding, the tag (symbol or
     * integer) in case of a tag binding or the cons
     * in case of a SETF function binding
     */
    final LispObject symbol;

    /** Used only for tags and blocks. Refers to the
     * defining environment.
     *
     */
    Environment env = null;

    /** The value bound.
     *
     * In case of a block binding, it holds the block identifier to be used
     * with the Return to be thrown.
     *
     * In case of a tagbody, it holds the tail subforms of the tagbody, of
     * which the tag is the first subform.
     *
     * In case of a function binding, it holds the function object.
     *
     * In case of a variable binding, it holds the value associated with the
     * variable, unless specialp is true.
     *
     * In case of a symbol macro binding, holds the SymbolMacro instance
     * holding the macro's expansion.
     */
    LispObject value;

    /** Only used for variable bindings. Indicates whether or not the value
     * should be retrieved from the dynamic environment or from this binding.
     */
    boolean specialp;
    final Binding next;

    Binding(LispObject symbol, LispObject value, Binding next)
    {
        this.symbol = symbol;
        this.value = value;
        this.next = next;
    }

    Binding(LispObject symbol, Environment env,
            LispObject value, Binding next)
    {
        this(symbol, value, next);
        this.env = env;
    }
}
