/*
 * RACFMalformedInputException.java
 *
 * Copyright (C) 2009 Erik Huelsmann
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

package org.armedbear.lisp.util;

import java.nio.charset.MalformedInputException;


/** Class - derived from MalformedInputException -
 * which holds information required to allow higher level
 * systems to invoke a lisp restart function to set replacement characters.
 */
public class RACFMalformedInputException
    extends MalformedInputException {

    final int position;
    final char character;
    final String charsetName;

    public RACFMalformedInputException(int position, char character,
                                       String charsetName) {
        super(1); // 1 == fake length
        this.position = position;
        this.character = character;
        this.charsetName = charsetName;
    }

    @Override
    public String getMessage() {
        return "Input value 0x" + Integer.toHexString(character)
            + " is malformed while recoding with charset " + charsetName;
    }

    public int getPosition() {
        return position;
    }

}