/*
 * JavaExpression.java
 *
 * Copyright (C) 2002 Peter Graves
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

/**
 * A JavaExpression represents an instance of a method call in Java source.
 */
public final class JavaExpression extends Expression implements Constants
{
    private final int type; // TAG_METHOD etc.

    public JavaExpression(String name, int arity)
    {
        super(name, arity);
        type = TAG_METHOD;
    }

    public JavaExpression(String name, int arity, int type)
    {
        super(name, arity);
        this.type = type;
    }

    public final int getType()
    {
        return type;
    }

    public boolean matches(LocalTag tag)
    {
        if (!name.equals(tag.getMethodName()))
            return false;
        if (type == TAG_METHOD && tag.getType() != TAG_METHOD)
            return false;
        if (type == TAG_UNKNOWN && tag.getType() == TAG_METHOD)
            return false;
        if (arity >= 0) {
            int n = getArity(tag.getCanonicalSignature());
            if (n < 0 || n == arity)
                return true;
            else
                return false;
        }
        return true;
    }

    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof JavaExpression) {
            JavaExpression expr = (JavaExpression) obj;
            if (arity != expr.arity)
                return false;
            if (type != expr.type)
                return false;
            if (name == expr.name || (name != null && name.equals(expr.name)))
                return true;
        }
        return false;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        switch (type) {
            case TAG_UNKNOWN:
                sb.append("TAG_UNKNOWN ");
                break;
            case TAG_CLASS:
                sb.append("TAG_CLASS ");
                break;
            case TAG_METHOD:
                sb.append("TAG_METHOD ");
                break;
            default:
                sb.append(type);
                sb.append(' ');
                break;
        }
        sb.append(name);
        if (type == TAG_METHOD) {
            sb.append(' ');
            sb.append(arity);
        }
        return sb.toString();
    }
}
