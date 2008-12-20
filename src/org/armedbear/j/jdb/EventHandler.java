/*
 * EventHandler.java
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

import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.Method;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.Value;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.ClassUnloadEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.EventIterator;
import com.sun.jdi.event.EventQueue;
import com.sun.jdi.event.EventSet;
import com.sun.jdi.event.ExceptionEvent;
import com.sun.jdi.event.LocatableEvent;
import com.sun.jdi.event.MethodEntryEvent;
import com.sun.jdi.event.MethodExitEvent;
import com.sun.jdi.event.StepEvent;
import com.sun.jdi.event.ThreadDeathEvent;
import com.sun.jdi.event.ThreadStartEvent;
import com.sun.jdi.event.VMDeathEvent;
import com.sun.jdi.event.VMDisconnectEvent;
import com.sun.jdi.event.VMStartEvent;
import com.sun.jdi.event.WatchpointEvent;
import com.sun.jdi.request.BreakpointRequest;
import com.sun.jdi.request.EventRequest;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.SwingUtilities;
import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.File;
import org.armedbear.j.Log;
import org.armedbear.j.Platform;

public final class EventHandler implements Runnable
{
    private final Jdb jdb;
    private final Thread eventHandlerThread;
    private boolean connected = true;
    private boolean completed;

    public EventHandler(Jdb jdb)
    {
        this.jdb = jdb;
        eventHandlerThread = new Thread(this, "jdb event handler");
        eventHandlerThread.start();
    }

    public void run()
    {
        EventQueue queue = jdb.getVM().eventQueue();
        while (connected) {
            try {
                boolean resume = true;
                EventSet eventSet = queue.remove();
                EventIterator iter = eventSet.eventIterator();
                while (iter.hasNext()) {
                    Event event = iter.nextEvent();
                    resume &= handleEvent(event);
                }
                if (resume)
                    eventSet.resume();
                else {
                    jdb.setSuspended(true);
                    // Set current thread and stack frame.
                    if (eventSet.size() > 0) {
                        Event event = (Event) eventSet.iterator().next();
                        ThreadReference threadRef = getThreadForEvent(event);
                        if (threadRef != null) {
                            jdb.setCurrentThread(threadRef);
                            try {
                                List frames = threadRef.frames();
                                if (frames.size() > 0)
                                    jdb.setCurrentStackFrame((StackFrame)frames.get(0));
                            }
                            catch (IncompatibleThreadStateException e) {
                                Log.error(e);
                            }
                        } else {
                            Log.debug("threadRef is null");
                            Log.debug("event = " + event);
                        }
                    }
                    jdb.fireContextChanged();
                    jdb.prompt();
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            if (!Platform.isPlatformWindows())
                                Editor.getCurrentFrame().toFront();
                            jdb.getControlDialog().toFront();
                        }
                    };
                    SwingUtilities.invokeLater(r);
                }
            }
            catch (InterruptedException e) {
                // Do nothing.
            }
            catch (VMDisconnectedException disconnect) {
                break;
            }
        }
        synchronized (this) {
            completed = true;
            notifyAll();
        }
        jdb.setVM(null);
    }

    private static ThreadReference getThreadForEvent(Event event)
    {
        if (event instanceof ClassPrepareEvent)
            return ((ClassPrepareEvent)event).thread();
        else if (event instanceof LocatableEvent)
            return ((LocatableEvent)event).thread();
        else if (event instanceof ThreadStartEvent)
            return ((ThreadStartEvent)event).thread();
        else if (event instanceof ThreadDeathEvent)
            return ((ThreadDeathEvent)event).thread();
        else if (event instanceof VMStartEvent)
            return ((VMStartEvent)event).thread();
        else
            return null;
    }

    // Return true to resume.
    private boolean handleEvent(Event event)
    {
        if (event instanceof ExceptionEvent)
            return handleExceptionEvent(event);
        else if (event instanceof BreakpointEvent)
            return handleBreakpointEvent(event);
        else if (event instanceof WatchpointEvent)
            return handleFieldWatchEvent(event);
        else if (event instanceof StepEvent)
            return handleStepEvent(event);
        else if (event instanceof MethodEntryEvent)
            return handleMethodEntryEvent(event);
        else if (event instanceof MethodExitEvent)
            return handleMethodExitEvent(event);
        else if (event instanceof ClassPrepareEvent)
            return handleClassPrepareEvent(event);
        else if (event instanceof ClassUnloadEvent)
            return handleClassUnloadEvent(event);
        else if (event instanceof ThreadStartEvent)
            return handleThreadStartEvent(event);
        else if (event instanceof ThreadDeathEvent)
            return handleThreadDeathEvent(event);
        else if (event instanceof VMStartEvent)
            return handleVMStartEvent(event);
        else if (event instanceof VMDeathEvent)
            return handleVMDeathEvent(event);
        else if (event instanceof VMDisconnectEvent)
            return handleVMDisconnectEvent(event);
        jdb.log("handleEvent " + event.toString());
        return false;
    }

    private boolean handleExceptionEvent(Event event)
    {
        ExceptionEvent ee = (ExceptionEvent) event;
        if (ee.catchLocation() != null)
            jdb.log("Exception encountered: " + ee.exception());
        else
            jdb.log("Uncaught exception encountered " + ee.exception());
        jdb.printCurrentLocation(ee);
        jdb.setLocation(ee.location());
        jdb.source();
        // Suspend the VM.
        return false;
    }

    private boolean handleBreakpointEvent(Event event)
    {
        jdb.log("Breakpoint hit");
        BreakpointEvent evt = (BreakpointEvent) event;
        jdb.printCurrentLocation(evt);
        jdb.setLocation(evt.location());
        jdb.source();
        // If breakpoint is temporary, delete it.
        EventRequest er = evt.request();
        if (er instanceof BreakpointRequest) {
            BreakpointRequest br = (BreakpointRequest) er;
            for (Iterator it = jdb.getBreakpoints().iterator(); it.hasNext();) {
                ResolvableBreakpoint bp = (ResolvableBreakpoint) it.next();
                if (bp.getEventRequest() == br) {
                    if (bp.isTemporary()) {
                        jdb.deleteBreakpoint(bp);
                        File file = bp.getFile();
                        if (file != null) {
                            Buffer buffer =
                                Editor.getBufferList().findBuffer(file);
                            if (buffer != null)
                                buffer.repaint();
                        }
                        jdb.fireBreakpointChanged();
                    }
                    break;
                }
            }
        }
        // Suspend the VM.
        return false;
    }

    private boolean handleFieldWatchEvent(Event event)
    {
        jdb.log("handleFieldWatchEvent");
        return true;
    }

    private boolean handleStepEvent(Event event)
    {
        StepEvent evt = (StepEvent) event;
        jdb.printCurrentLocation(evt);
        jdb.setLocation(evt.location());
        jdb.source();
        // Suspend the VM.
        return false;
    }

    private boolean handleMethodEntryEvent(Event event)
    {
        MethodEntryEvent evt = (MethodEntryEvent) event;
        Method method = evt.method();
        FastStringBuffer sb = new FastStringBuffer("Method entered: ");
        sb.append(method.declaringType().name());
        sb.append('.');
        sb.append(method.name());
        jdb.log(sb.toString());
        return true;
    }

    private boolean handleMethodExitEvent(Event event)
    {
        MethodExitEvent evt = (MethodExitEvent) event;
        Method method = evt.method();
        FastStringBuffer sb = new FastStringBuffer("Method exited: ");
        sb.append(method.declaringType().name());
        sb.append('.');
        sb.append(method.name());
        jdb.log(sb.toString());
        return true;
    }

    private boolean handleClassPrepareEvent(Event event)
    {
        ClassPrepareEvent evt = (ClassPrepareEvent) event;
        jdb.log("Class loaded: ".concat(evt.referenceType().name()));
        jdb.resolveDeferredRequests(evt);
        return true;
    }

    private boolean handleClassUnloadEvent(Event event)
    {
        ClassUnloadEvent evt = (ClassUnloadEvent) event;
        jdb.log("Class unloaded: ".concat(evt.className()));
        return true;
    }

    private boolean handleThreadStartEvent(Event event)
    {
        ThreadStartEvent evt = (ThreadStartEvent) event;
        ThreadReference threadRef = evt.thread();
        jdb.log("Thread started: ".concat(threadRef.name()));
        return true;
    }

    private boolean handleThreadDeathEvent(Event event)
    {
        ThreadDeathEvent evt = (ThreadDeathEvent) event;
        ThreadReference threadRef = evt.thread();
        jdb.log("Thread exited: ".concat(threadRef.name()));
        return true;
    }

    private boolean handleVMStartEvent(Event event)
    {
        jdb.log("VM loaded");
        if (jdb.isSuspended()) {
            jdb.log("VM suspended");
            jdb.prompt();
        }
        return true;
    }

    private boolean handleVMDeathEvent(Event event)
    {
        jdb.log("VM exited");
        return true;
    }

    private boolean handleVMDisconnectEvent(Event event)
    {
        jdb.log("VM disconnected");
        return true;
    }
}
