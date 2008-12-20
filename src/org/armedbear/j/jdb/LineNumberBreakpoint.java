/*
 * LineNumberBreakpoint.java
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

import com.sun.jdi.Location;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import java.util.List;
import org.armedbear.j.Annotation;
import org.armedbear.j.Buffer;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.JavaSource;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class LineNumberBreakpoint extends ResolvableBreakpoint
{
    private Buffer buffer;
    private final int lineNumber;

    public LineNumberBreakpoint(Jdb jdb, Buffer buffer, Line line)
    {
        super(jdb);
        this.buffer = buffer;
        this.line = line;
        file = buffer.getFile();
        final String fileName = file.getName();
        String name = fileName.substring(0, fileName.length()-5);
        String packageName = JavaSource.getPackageName(buffer);
        if (packageName != null)
            className = packageName.concat(".").concat(name);
        else
            className = name;
        Log.debug("LineNumberBreakpoint className = |" + className + "|");
        // Our line numbers are zero-based.
        lineNumber = line.lineNumber() + 1;
    }

    public LineNumberBreakpoint(Jdb jdb, String className, File file,
        int lineNumber)
    {
        super(jdb);
        this.className = className;
        this.file = file;
        this.lineNumber = lineNumber;
    }

    public int getLineNumber()
    {
        return lineNumber;
    }

    public EventRequest resolveEventRequest(ReferenceType refType)
        throws Exception
    {
        Log.debug("LineNumberBreakpoint.resolveEventRequest");
        Location location = findLocation(refType, lineNumber);
        if (location == null) {
            Log.debug("resolveEventRequest location is null");
            return null;
        }
        EventRequestManager erm =
            refType.virtualMachine().eventRequestManager();
        EventRequest er = erm.createBreakpointRequest(location);
        er.setSuspendPolicy(EventRequest.SUSPEND_ALL);
        er.enable();
        return er;
    }

    private Location findLocation(ReferenceType refType, int lineNumber)
        throws Exception
    {
        Location location = null;
        List locations = refType.locationsOfLine(lineNumber);
        if (locations.size() > 0) {
            location = (Location) locations.get(0);
            if (location.method() != null)
                return location;
        }
        return null;
    }

    public void resolved()
    {
        if (line != null) {
            line.setAnnotation(new BreakpointAnnotation(this));
        } else {
            // This only finds existing buffers. We might want to create one
            // if one doesn't already exist for the file in question.
            Buffer buffer = Editor.getBufferList().findBuffer(file);
            if (buffer != null) {
                if (!buffer.initialized())
                    buffer.initialize();
                if (!buffer.isLoaded())
                    buffer.load();
                line = buffer.getLine(lineNumber - 1);
                if (line != null) {
                    line.setAnnotation(new BreakpointAnnotation(this));
                    buffer.repaint();
                }
            }
        }
        if (buffer != null)
            buffer.repaint();
        jdb.log("Breakpoint resolved: " + getLocationString());
    }

    public String getLocationString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (file != null) {
            sb.append(file.getName());
            sb.append(':');
        }
        sb.append(lineNumber);
        if (!isResolved())
            sb.append(' ');
        return sb.toString();
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (file != null) {
            sb.append(file.getName());
            sb.append(':');
        }
        sb.append(lineNumber);
        if (!isResolved()) {
            sb.append(' ');
            sb.append("(deferred)");
        }
        return sb.toString();
    }

    public String toXml()
    {
        int indent = 4;
        final String separator = System.getProperty("line.separator");
        FastStringBuffer sb = new FastStringBuffer(Utilities.spaces(indent));
        sb.append("<breakpoint");
        sb.append(separator);
        if (className == null)
            Debug.bug();
        if (className != null) {
            sb.append(Utilities.spaces(indent+2));
            sb.append("className=\"");
            sb.append(className);
            sb.append('"');
            sb.append(separator);
        }
        if (file!= null) {
            sb.append(Utilities.spaces(indent+2));
            sb.append("fileName=\"");
            sb.append(file.canonicalPath());
            sb.append('"');
            sb.append(separator);
        }
        if (lineNumber > 0) {
            sb.append(Utilities.spaces(indent+2));
            sb.append("lineNumber=\"");
            sb.append(String.valueOf(lineNumber));
            sb.append('"');
            sb.append(separator);
        }
        sb.append(Utilities.spaces(indent));
        sb.append("/>");
        sb.append(separator);
        return sb.toString();
    }
}
