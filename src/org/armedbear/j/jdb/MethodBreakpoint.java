/*
 * MethodBreakpoint.java
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

import com.sun.jdi.InvalidTypeException;
import com.sun.jdi.Location;
import com.sun.jdi.Method;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import java.util.Iterator;
import java.util.List;
import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.JavaSource;
import org.armedbear.j.Line;
import org.armedbear.j.LocalTag;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class MethodBreakpoint extends ResolvableBreakpoint
{
    private final String methodName;

    public MethodBreakpoint(Jdb jdb, String className, String methodName)
    {
        super(jdb);
        this.className = className;
        this.methodName = methodName;
    }

    public String getMethodName()
    {
        return methodName;
    }

    public EventRequest resolveEventRequest(ReferenceType refType) throws Exception
    {
        Method method = findMatchingMethod(refType);
        if (method == null)
            throw new InvalidTypeException();
        Location location = method.location();
        if (location == null)
            throw new InvalidTypeException(); // Abstract or native method.
        EventRequestManager erm =
            refType.virtualMachine().eventRequestManager();
        EventRequest er = erm.createBreakpointRequest(location);
        er.setSuspendPolicy(EventRequest.SUSPEND_ALL);
        er.enable();
        setBreakpointInSource(refType, method);
        return er;
    }

    // BUG! Overloads are not handled correctly.
    private Method findMatchingMethod(ReferenceType refType)
    {
        Iterator iter = refType.methods().iterator();
        while (iter.hasNext()) {
            Method method = (Method) iter.next();
            if (method.name().equals(methodName))
                return method;
        }
        return null;
    }

    private void setBreakpointInSource(ReferenceType refType, Method method)
    {
        file = JavaSource.findSource(refType.name(), jdb.getSourcePath());
        if (file == null) {
            Log.debug("setBreakpointInSource findSource returned null");
            return;
        }
        Buffer buf = Editor.getBuffer(file);
        if (buf == null)
            return;
        if (!buf.initialized())
            buf.initialize();
        if (!buf.isLoaded())
            buf.load();
        List tags = buf.getTags(true);
        if (tags == null)
            return;
        String lookFor = refType.name();
        // Remove package prefix.
        int index = lookFor.lastIndexOf('.');
        if (index >= 0)
            lookFor = lookFor.substring(index+1);
        lookFor += '.';
        lookFor += method.name();
        Log.debug("lookFor = |" + lookFor + "|");
        Line begin = null;
        Line end = null;
        for (int i = 0; i < tags.size(); i++) {
            LocalTag tag = (LocalTag) tags.get(i);
            if (tag.getName() != null) {
                if (tag.getName().equals(lookFor)) {
                    begin = tag.getLine();
                    if (++i < tags.size()) {
                        tag = (LocalTag) tags.get(i);
                        end = tag.getLine();
                    }
                    break;
                }
            }
        }
        if (begin == null)
            return;
        // Look for first line of actual code.
        for (Line ln = begin.next(); ln != end && ln != null; ln = ln.next()) {
            String text = ln.getText().trim();
            if (!text.startsWith("//")) {
                if (text.indexOf('=') >= 0 || text.indexOf('(') >= 0) {
                    // Line looks like code.
                    ln.setAnnotation(new BreakpointAnnotation(this));
                    line = ln;
                    break;
                }
            }
        }
    }

    public void resolved()
    {
        if (file != null) {
            Buffer buffer = Editor.getBufferList().findBuffer(file);
            if (buffer != null)
                buffer.repaint();
        }
        if (line != null)
            line.setAnnotation(new BreakpointAnnotation(this));
        jdb.log("Breakpoint resolved: " + getLocationString());
    }

    public String getLocationString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (className != null) {
            sb.append(className);
            sb.append('.');
        }
        sb.append(methodName);
        if (!isResolved())
            sb.append(' ');
        return sb.toString();
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (className != null) {
            sb.append(className);
            sb.append('.');
        }
        sb.append(methodName);
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
        if (className != null) {
            sb.append(Utilities.spaces(indent+2));
            sb.append("className=\"");
            sb.append(className);
            sb.append('"');
            sb.append(separator);
        }
        if (methodName != null) {
            sb.append(Utilities.spaces(indent+2));
            sb.append("methodName=\"");
            sb.append(methodName);
            sb.append('"');
            sb.append(separator);
        }
        sb.append(Utilities.spaces(indent));
        sb.append("/>");
        sb.append(separator);
        return sb.toString();
    }
}
