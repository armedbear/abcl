/*
 * ResolvableBreakpoint.java
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

import com.sun.jdi.ReferenceType;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import java.util.Iterator;
import org.armedbear.j.Annotation;
import org.armedbear.j.File;
import org.armedbear.j.Line;
import org.armedbear.j.Log;

public abstract class ResolvableBreakpoint
{
    protected final Jdb jdb;

    protected String className;
    protected File file;
    protected Line line;
    protected EventRequest eventRequest;

    private boolean temporary;

    protected ResolvableBreakpoint(Jdb jdb)
    {
        this.jdb = jdb;
    }

    public final String getClassName()
    {
        return className;
    }

    public final File getFile()
    {
        return file;
    }

    public final Line getLine()
    {
        return line;
    }

    public final EventRequest getEventRequest()
    {
        return eventRequest;
    }

    public boolean isTemporary()
    {
        return temporary;
    }

    public void setTemporary()
    {
        temporary = true;
    }

    public final void clear()
    {
        if (eventRequest != null) {
            eventRequest.disable();
            VirtualMachine vm = eventRequest.virtualMachine();
            EventRequestManager mgr = vm.eventRequestManager();
            mgr.deleteEventRequest(eventRequest);
            eventRequest = null;
        }
        if (line != null) {
            Annotation annotation = line.getAnnotation();
            if (annotation instanceof BreakpointAnnotation)
                if (((BreakpointAnnotation)annotation).getBreakpoint() == this)
                    line.setAnnotation(null);
        }
    }

    public final boolean isResolved()
    {
        return eventRequest != null;
    }

    public EventRequest resolveAgainstPreparedClasses() throws Exception
    {
        Log.debug("resolveAgainstPreparedClasses className = |" + className + "|");
        Iterator iter = jdb.getVM().allClasses().iterator();
        while (eventRequest == null && iter.hasNext()) {
            ReferenceType refType = (ReferenceType) iter.next();
            if (refType.isPrepared() && refType.name().equals(className)) {
                eventRequest = resolveEventRequest(refType);
                if (eventRequest != null) {
                    Log.debug("resolved!");
                    resolved();
                    jdb.fireBreakpointChanged();
                    return eventRequest;
                }
            }
        }
        Log.debug("*** not resolved");
        return null;
    }

    public abstract EventRequest resolveEventRequest(ReferenceType refType)
        throws Exception;

    public abstract void resolved();

    public abstract String getLocationString();
}
