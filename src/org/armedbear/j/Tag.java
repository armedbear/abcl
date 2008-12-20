/*
 * Tag.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

public abstract class Tag
{
    protected final String name;
    protected final String signature;

    protected String canonicalSignature;

    protected Tag(String name, String signature)
    {
        this.name = name;
        this.signature = signature;
    }

    public final String getName()
    {
        return name;
    }

    public final String getSignature()
    {
        return signature;
    }

    public final String getCanonicalSignature()
    {
        return canonicalSignature;
    }

    public abstract String getMethodName();

    public abstract String getLongName();

    public abstract String getClassName();

    public abstract void gotoTag(Editor editor);
}
