/*
 * adjust_array.java
 *
 * Copyright (C) 2004-2007 Peter Graves
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

import static org.armedbear.lisp.Lisp.*;

// ### %adjust-array array new-dimensions element-type initial-element
// initial-element-p initial-contents initial-contents-p fill-pointer
// displaced-to displaced-index-offset => new-array
public final class adjust_array extends Primitive
{
    public adjust_array()
    {
        super("%adjust-array", PACKAGE_SYS, false);
    }

    @Override
    public LispObject execute(LispObject[] args)
    {
        if (args.length != 10)
            return error(new WrongNumberOfArgumentsException(this, 10));
        AbstractArray array = checkArray(args[0]);
        LispObject dimensions = args[1];
        LispObject elementType = args[2];
        boolean initialElementProvided = args[4] != NIL;
        boolean initialContentsProvided = args[6] != NIL;
        LispObject initialElement = initialElementProvided ? args[3] : null;
        LispObject initialContents = initialContentsProvided ? args[5] : null;
        LispObject fillPointer = args[7];
        LispObject displacedTo = args[8];
        LispObject displacedIndexOffset = args[9];
        if (initialElementProvided && initialContentsProvided) {
            return error(new LispError("ADJUST-ARRAY: cannot specify both initial element and initial contents."));
        }
        if (elementType != array.getElementType() &&
            getUpgradedArrayElementType(elementType) != array.getElementType())
        {
            return error(new LispError("ADJUST-ARRAY: incompatible element type."));
        }
        if (array.getRank() == 0) {
            return array.adjustArray(new int[0], initialElement, initialContents);
        }
        if (!initialElementProvided && array.getElementType() == T)
            initialElement = Fixnum.ZERO;
        if (array.getRank() == 1) {
            final int newSize;
            if (dimensions instanceof Cons && dimensions.length() == 1)
                newSize = Fixnum.getValue(dimensions.car());
            else
                newSize = Fixnum.getValue(dimensions);
            if (array instanceof AbstractVector) {
                AbstractVector v = (AbstractVector) array;
                AbstractArray v2;
                if (displacedTo != NIL) {
                    final int displacement;
                    if (displacedIndexOffset == NIL)
                        displacement = 0;
                    else
                        displacement = Fixnum.getValue(displacedIndexOffset);
                    v2 = v.adjustArray(newSize,
                                        checkArray(displacedTo),
                                        displacement);
                } else {
                    v2 = v.adjustArray(newSize,
                                        initialElement,
                                        initialContents);
                }
                if (fillPointer != NIL)
                    v2.setFillPointer(fillPointer);
                return v2;
            }
        }
        // rank > 1
        final int rank = dimensions.listp() ? dimensions.length() : 1;
        int[] dimv = new int[rank];
        if (dimensions.listp()) {
            for (int i = 0; i < rank; i++) {
                LispObject dim = dimensions.car();
                dimv[i] = Fixnum.getValue(dim);
                dimensions = dimensions.cdr();
            }
        } else
            dimv[0] = Fixnum.getValue(dimensions);

        if (displacedTo != NIL) {
            final int displacement;
            if (displacedIndexOffset == NIL)
                displacement = 0;
            else
                displacement = Fixnum.getValue(displacedIndexOffset);
            return array.adjustArray(dimv,
                                     checkArray(displacedTo),
                                     displacement);
        } else {
            return array.adjustArray(dimv,
                                     initialElement,
                                     initialContents);
        }
    }

    private static final Primitive _ADJUST_ARRAY = new adjust_array();
}
