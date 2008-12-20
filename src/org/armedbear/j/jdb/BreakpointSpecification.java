/*
 * BreakpointSpecification.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

package org.armedbear.j.jdb;

import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.xml.sax.Attributes;

public final class BreakpointSpecification
{
    private final String className;
    private final String methodName;
    private final String fileName;
    private final int lineNumber;

    public BreakpointSpecification(Attributes attributes)
    {
        className = attributes.getValue("className");
        methodName = attributes.getValue("methodName");
        fileName = attributes.getValue("fileName");
        int n = 0;
        String s = attributes.getValue("lineNumber");
        if (s != null) {
            try {
                n = Integer.parseInt(s);
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
        lineNumber = n;
    }

    public String getClassName()
    {
        return className;
    }

    public String getMethodName()
    {
        return methodName;
    }

    public String getFileName()
    {
        return fileName;
    }

    public int getLineNumber()
    {
        return lineNumber;
    }

    // Only used to generate meaningful trace output.
    public String toString()
    {
        final String separator = System.getProperty("line.separator");
        FastStringBuffer sb = new FastStringBuffer("BreakpointSpecification: ");
        sb.append(separator);
        sb.append("  className = " + className);
        sb.append(separator);
        sb.append("  methodName = " + methodName);
        sb.append(separator);
        sb.append("  fileName = " + fileName);
        sb.append(separator);
        sb.append("  lineNumber = " + lineNumber);
        return sb.toString();
    }
}
