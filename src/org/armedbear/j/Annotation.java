/*
 * Annotation.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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
 */

package org.armedbear.j;

public class Annotation
{
    private final Object userObject;
    private final int integerValue;
    private final char gutterChar;

    public Annotation(Object obj)
    {
        userObject = obj;
        integerValue = 0;
        gutterChar = 0;
    }

    public Annotation(Object obj, char c)
    {
        userObject = obj;
        integerValue = 0;
        gutterChar = c;
    }

    public Annotation(int n)
    {
        userObject = null;
        integerValue = n;
        gutterChar = 0;
    }

    public final Object getUserObject()
    {
        return userObject;
    }

    public final int getIntegerValue()
    {
        return integerValue;
    }

    public final char getGutterChar()
    {
        return gutterChar;
    }
}
