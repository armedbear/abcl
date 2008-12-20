/*
 * Command.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.lang.reflect.Method;

public final class Command
{
    private final String name;
    private final String methodName;

    private String className;
    private Method method;

    public Command(String name, String className, String methodName)
    {
        this.name = name;
        this.className = className;
        this.methodName = methodName;
    }

    // Constructor for commands that are implemented by a method of the same
    // name in the Editor class.
    public Command(String name)
    {
        this.name = name;
        this.methodName = name;
    }

    public final String getName()
    {
        return name;
    }

    public final String getClassName()
    {
        return className;
    }

    public final String getMethodName()
    {
        return methodName;
    }

    public final Method getMethod()
    {
        return method;
    }

    public final void setMethod(Method method)
    {
        this.method = method;
    }
}
