/*
 * Jdb.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.ArrayReference;
import com.sun.jdi.Field;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.Location;
import com.sun.jdi.Method;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StackFrame;
import com.sun.jdi.StringReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.Value;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.LocatableEvent;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.ExceptionRequest;
import com.sun.jdi.request.StepRequest;
import com.sun.jdi.request.ThreadDeathRequest;
import com.sun.jdi.request.ThreadStartRequest;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import org.armedbear.j.Annotation;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.EditorList;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.JavaMode;
import org.armedbear.j.JavaSource;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.Platform;
import org.armedbear.j.Position;
import org.armedbear.j.ReaderThread;
import org.armedbear.j.SimpleEdit;
import org.armedbear.j.Utilities;

public final class Jdb extends Buffer implements JdbConstants
{
    private static int catchMode = CATCH_UNCAUGHT;

    private JdbSession session;
    private VirtualMachine vm;
    private ThreadReference currentThread;
    private StackFrame currentStackFrame;
    private Location location;
    private String mainClass;
    private String mainClassArgs;
    private String classPath;
    private String javaHome;
    private String javaExecutable;
    private String vmArgs;
    private boolean startSuspended;
    private boolean isSuspended = true;
    private String sourcePath;
    private JdbControlDialog controlDialog;
    private Position posEndOfBuffer;
    private ArrayList breakpointListeners = new ArrayList();
    private ArrayList contextListeners = new ArrayList();
    private int lastCommand;
    private final List breakpoints = new ArrayList();

    public static synchronized void jdb()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        Jdb jdb = findJdb();
        if (jdb != null) {
            if (!jdb.equals(buffer)) {
                editor.getFrame().unsplitWindow();
                editor.makeNext(jdb);
                editor.activateInOtherWindow(jdb);
                editor.updateDisplay();
            }
            if (jdb.getVM() != null)
                return; // Debugger is running.
        }
        JdbDialog d = new JdbDialog(editor);
        editor.centerDialog(d);
        d.show();
        editor.getFrame().setWaitCursor();
        editor.repaintNow();
        if (!d.cancelled()) {
            JdbSession session = d.getSession();
            if (jdb != null) {
                jdb.setSession(session);
                jdb.startProcess();
            } else {
                jdb = new Jdb(session);
                if (jdb != null) {
                    JavaMode.setJdb(jdb);
                    editor.getFrame().unsplitWindow();
                    editor.makeNext(jdb);
                    editor.activateInOtherWindow(jdb);
                    jdb.showControlDialog();
                    jdb.startProcess();
                }
            }
        }
        editor.getFrame().setDefaultCursor();
    }

    private Jdb(JdbSession session)
    {
        super();
        supportsUndo = false;
        mode = JdbMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        setSession(session);
    }

    public VirtualMachine getVM()
    {
        return vm;
    }

    public void setVM(VirtualMachine vm)
    {
        this.vm = vm;
        if (vm == null) {
            isSuspended = true;
            currentThread = null;
            currentStackFrame = null;
        }
    }

    public String getMainClass()
    {
        return mainClass;
    }

    public String getMainClassArgs()
    {
        return mainClassArgs;
    }

    public String getClassPath()
    {
        return classPath;
    }

    public String getJavaHome()
    {
        return javaHome;
    }

    public String getJavaExecutable()
    {
        return javaExecutable;
    }

    public String getVMArgs()
    {
        return vmArgs;
    }

    public boolean getStartSuspended()
    {
        return startSuspended;
    }

    public String getSourcePath()
    {
        return sourcePath;
    }

    public int getLastCommand()
    {
        return lastCommand;
    }

    public void setLocation(Location location)
    {
        this.location = location;
    }

    public void setCurrentThread(ThreadReference threadRef)
    {
        currentThread = threadRef;
    }

    public ThreadReference getCurrentThread()
    {
        return currentThread;
    }

    public boolean isSuspended()
    {
        return isSuspended;
    }

    public void setSuspended(boolean b)
    {
        isSuspended = b;
    }

    public synchronized void setCurrentStackFrame(StackFrame stackFrame)
    {
        currentStackFrame = stackFrame;
    }

    public synchronized StackFrame getCurrentStackFrame()
    {
        return currentStackFrame;
    }

    public List getBreakpoints()
    {
        return breakpoints;
    }

    public void addBreakpointListener(BreakpointListener listener)
    {
        synchronized(breakpointListeners) {
            breakpointListeners.add(listener);
        }
    }

    public void fireBreakpointChanged()
    {
        synchronized(breakpointListeners) {
            Iterator iter = breakpointListeners.iterator();
            while (iter.hasNext())
                ((BreakpointListener)iter.next()).breakpointChanged();
        }
    }

    public void addContextListener(ContextListener listener)
    {
        synchronized(contextListeners) {
            contextListeners.add(listener);
        }
    }

    private final Runnable fireContextChangedRunnable = new Runnable() {
        public void run()
        {
            synchronized (contextListeners) {
                for (Iterator it = contextListeners.iterator(); it.hasNext();)
                    ((ContextListener)it.next()).contextChanged();
            }
        }
    };

    public void fireContextChanged()
    {
        if (SwingUtilities.isEventDispatchThread())
            fireContextChangedRunnable.run();
        else
            SwingUtilities.invokeLater(fireContextChangedRunnable);
    }

    public void initialize()
    {
        // Nothing to do.
    }

    public synchronized int load()
    {
        if (!isLoaded()) {
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.debug(e);
                return LOAD_FAILED; // Shouldn't happen.
            }
            try {
                appendLine("");
                setLoaded(true);
                posEndOfBuffer = new Position(getFirstLine(), 0);
            }
            finally {
                unlockWrite();
            }
        }
        return LOAD_COMPLETED;
    }

    private void showControlDialog()
    {
        if (controlDialog == null) {
            controlDialog = new JdbControlDialog(this);
            controlDialog.show();
        }
    }

    public JdbControlDialog getControlDialog()
    {
        return controlDialog;
    }

    public void doCommand(String input)
    {
        String s = input.trim();
        String cmd, args;
        int index = s.indexOf(' ');
        if (index >= 0) {
            cmd = s.substring(0, index);
            args = s.substring(index+1).trim();
        } else {
            cmd = s;
            args = null;
        }
        // Command may be abbreviated.
        int command = JdbCommands.findCommand(cmd);
        if (command < 0) {
            // Not found.
            logCommand(cmd);
            log("Command not found");
        } else {
            doCommand(command, args);
            lastCommand = command;
        }
    }

    public void doCommand(int command, String args)
    {
        switch (command) {
            case JDB_BREAK:
                logCommand("break", args);
                doBreak(args, false);
                break;
            case JDB_CATCH:
                logCommand("catch", args);
                doCatch(args);
                break;
            case JDB_CLEAR:
                logCommand("clear", args);
                doClear(args);
                break;
            case JDB_FINISH:
                logCommand("finish");
                doFinish();
                break;
            case JDB_LOCALS:
                logCommand("locals");
                doLocals();
                break;
            case JDB_NEXT:
                logCommand("next");
                doNext(args);
                break;
            case JDB_PRINT:
                logCommand("print", args);
                doPrint(args);
                break;
            case JDB_QUIT:
                logCommand("quit");
                quit();
                break;
            case JDB_RESTART:
                logCommand("restart");
                restart();
                break;
            case JDB_CONTINUE:
                logCommand("continue");
                doContinue();
                break;
            case JDB_STDIN:
                doStdin(args);
                break;
            case JDB_STEP:
                logCommand("step", args);
                doStep(args);
                break;
            case JDB_SUSPEND:
                logCommand("suspend");
                doSuspend();
                break;
            case JDB_TBREAK:
                logCommand("tbreak", args);
                doBreak(args, true);
                break;
            default:
                Log.error("Jdb.doCommand unknown command " + command);
                Debug.bug();
                break;
        }
    }

    private static final String prompt = "jdb> ";

    public static final String getPrompt()
    {
        return prompt;
    }

    public void prompt()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                appendString(prompt, true, JdbFormatter.JDB_FORMAT_PROMPT);
            }
        };
        if (SwingUtilities.isEventDispatchThread())
            r.run();
        else
            SwingUtilities.invokeLater(r);
    }

    private void logCommand(String command)
    {
        log(prompt.concat(command));
    }

    private void logCommand(String command, String remainder)
    {
        FastStringBuffer sb = new FastStringBuffer(prompt);
        sb.append(command);
        if (remainder != null && remainder.length() > 0) {
            sb.append(' ');
            sb.append(remainder);
        }
        log(sb.toString());
    }

    public void log(String s)
    {
        log(s, true);
    }

    private void log(String s, boolean forceNewLine)
    {
        log(s, forceNewLine, JdbFormatter.JDB_FORMAT_LOG);
    }

    private void log(final String s, final boolean forceNewLine, final int flags)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                Log.debug(s);
                appendString(s.concat("\n"), forceNewLine,
                    flags);
            }
        };
        if (SwingUtilities.isEventDispatchThread())
            r.run();
        else
            SwingUtilities.invokeLater(r);
    }

    private void appendString(String s, boolean forceNewLine, int flags)
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            Position posStart = posEndOfBuffer.copy();
            if (forceNewLine) {
                if (posEndOfBuffer.getOffset() > 0) {
                    if (!posEndOfBuffer.getLine().getText().equals(prompt)) {
                        insertLineSeparator(posEndOfBuffer);
                        posEndOfBuffer.getLine().setFlags(flags);
                    }
                }
            }
            if (posEndOfBuffer.getOffset() > 0 && s.startsWith(prompt)) {
                if (posEndOfBuffer.getLine().getText().equals(prompt))
                    s = s.substring(prompt.length());
            }
            insertString(posEndOfBuffer, s);
            if (needsRenumbering())
                renumber();
            Line line = posStart.getLine();
            if (posStart.getOffset() > 0)
                line = line.next();
            while (line != null) {
                line.setFlags(flags);
                line = line.next();
            }
        }
        finally {
            unlockWrite();
        }
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.eob();
                ed.getDisplay().setReframe(-2);
                ed.setUpdateFlag(REPAINT);
                ed.updateDisplay();
            }
        }
    }

    public void printCurrentLocation(LocatableEvent event)
    {
        printCurrentLocation(event.thread(), event.location());
    }

    public void printCurrentLocation(ThreadReference threadRef, Location location)
    {
        FastStringBuffer sb = new FastStringBuffer("[");
        sb.append(threadRef.name());
        sb.append("] ");
        sb.append(location.declaringType().name());
        sb.append('.');
        Method method = location.method();
        sb.append(method.name());
        try {
            sb.append(" (");
            if (method.isNative()) {
                sb.append("native method");
            } else {
                String sourceName = location.sourceName();
                sb.append(location.sourceName());
                int lineNumber = location.lineNumber();
                if (lineNumber > 0) {
                    sb.append(':');
                    sb.append(lineNumber);
                }
            }
            sb.append(')');
        }
        catch (AbsentInformationException e) {
            Log.error(e);
        }
        log(sb.toString());
    }

    public void displayRemoteOutput(InputStream inputStream) {
        ReaderThread readerThread = new ReaderThread(inputStream) {
            public void update(final String s)
            {
                Runnable runnable = new Runnable() {
                    public void run()
                    {
                        appendString(s, false, JdbFormatter.JDB_FORMAT_OUTPUT);
                    }
                };
                SwingUtilities.invokeLater(runnable);
            }
        };
        readerThread.setPriority(Thread.MAX_PRIORITY-1);
        readerThread.start();
    }

    private void addBreakpoint(ResolvableBreakpoint bp)
    {
        breakpoints.add(bp);
        FastStringBuffer sb = new FastStringBuffer();
        if (bp.isTemporary())
            sb.append("Temporary b");
        else
            sb.append('B');
        sb.append("reakpoint added: ");
        sb.append(bp.getLocationString());
        log(sb.toString());
        if (isSuspended())
            prompt();
    }

    public void deleteBreakpoint(ResolvableBreakpoint bp)
    {
        bp.clear();
        breakpoints.remove(bp);
        FastStringBuffer sb = new FastStringBuffer();
        if (bp.isTemporary())
            sb.append("Temporary b");
        else
            sb.append('B');
        sb.append("reakpoint deleted: ");
        sb.append(bp.getLocationString());
        log(sb.toString());
        if (isSuspended())
            prompt();
    }

    public static void jdbToggleBreakpoint()
    {
        Jdb jdb = findJdb();
        if (jdb == null)
            return;
        final Editor editor = Editor.currentEditor();
        final Line line = editor.getDotLine();
        Annotation annotation = line.getAnnotation();
        if (annotation instanceof BreakpointAnnotation)
            jdbDeleteBreakpoint();
        else
            jdbSetBreakpoint();
    }

    public static void jdbSetBreakpoint()
    {
        setBreakpointAtCurrentLine(false);
    }

    public static void jdbRunToCurrentLine()
    {
        Jdb jdb = findJdb();
        if (jdb == null)
            return;
        setBreakpointAtCurrentLine(true);
        jdb.logCommand("continue");
        jdb.doContinue();
    }

    private static void setBreakpointAtCurrentLine(boolean temporary)
    {
        Jdb jdb = findJdb();
        if (jdb == null)
            return;
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        final File file = buffer.getFile();
        if (file != null && file.getName().toLowerCase().endsWith(".java")) {
            final Line line = editor.getDotLine();
            FastStringBuffer sb = new FastStringBuffer();
            if (temporary)
                sb.append('t');
            sb.append("break ");
            sb.append(file.getName());
            sb.append(':');
            sb.append(line.lineNumber() + 1);
            jdb.logCommand(sb.toString());
            LineNumberBreakpoint bp =
                new LineNumberBreakpoint(jdb, buffer, editor.getDotLine());
            if (temporary)
                bp.setTemporary();
            try {
                EventRequest eventRequest = bp.resolveAgainstPreparedClasses();
                if (eventRequest != null) {
                    eventRequest.enable();
                } else {
                    EventRequestManager mgr = jdb.getVM().eventRequestManager();
                    ClassPrepareRequest cpr = mgr.createClassPrepareRequest();
                    String packageName = JavaSource.getPackageName(buffer);
                    String classFilter;
                    if (packageName != null) {
                        classFilter =
                            packageName.concat(".").concat(file.getName());
                    } else {
                        classFilter = file.getName();
                    }
                    if (classFilter.toLowerCase().endsWith(".java")) {
                        classFilter =
                            classFilter.substring(0, classFilter.length()-5);
                    }
                    cpr.addClassFilter(classFilter);
                    cpr.enable();
                }
                jdb.addBreakpoint(bp);
                jdb.saveSession();
                jdb.fireBreakpointChanged();
            }
            catch (AbsentInformationException absent) {
                jdb.log("Line number information is not available.");
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    public static void jdbDeleteBreakpoint()
    {
        Jdb jdb = findJdb();
        if (jdb == null)
            return;
        final Editor editor = Editor.currentEditor();
        final Line line = editor.getDotLine();
        Annotation annotation = line.getAnnotation();
        if (annotation instanceof BreakpointAnnotation) {
            ResolvableBreakpoint bp =
                ((BreakpointAnnotation)annotation).getBreakpoint();
            jdb.log("clear " + bp.getLocationString());
            jdb.deleteBreakpoint(bp);
            File file = bp.getFile();
            if (file != null) {
                Buffer buffer = Editor.getBufferList().findBuffer(file);
                if (buffer != null)
                    buffer.repaint();
            }
            jdb.saveSession();
            jdb.fireBreakpointChanged();
        }
    }

    public synchronized void doContinue()
    {
        if (vm != null) {
            currentThread = null;
            currentStackFrame = null;
            vm.resume();
            isSuspended = false;
            fireContextChanged();
        }
    }

    public synchronized void doSuspend()
    {
        if (vm != null && !isSuspended) {
            vm.suspend();
            isSuspended = true;
            log("VM suspended");
            ThreadReference threadRef = null;
            List threads = vm.allThreads();
            for (int i = 0; i < threads.size(); i++) {
                ThreadReference tr = (ThreadReference) threads.get(i);
                if ("main".equals(tr.name()))
                    threadRef = tr;
            }
            if (threadRef == null) {
                if (threads.size() > 0)
                    threadRef = (ThreadReference) threads.get(0);
            }
            setCurrentThread(threadRef);
            fireContextChanged();
            prompt();
        }
    }

    public static Jdb findJdb()
    {
        return (Jdb) JavaMode.getJdb();
    }

    public void startProcess()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                startProcessInternal();
            }
        };
        new Thread(r).start();
    }

    private void startProcessInternal()
    {
        VMConnection connection = VMConnection.getConnection(this);
        if (connection != null) {
            vm = connection.open(this);
            if (vm != null) {
                EventRequestManager mgr = vm.eventRequestManager();
                if (catchMode != CATCH_NONE) {
                    ExceptionRequest exceptionRequest =
                        mgr.createExceptionRequest(null,
                            catchMode == CATCH_ALL, true);
                    exceptionRequest.enable();
                }
                ThreadStartRequest tsr = mgr.createThreadStartRequest();
                tsr.enable();
                ThreadDeathRequest tdr = mgr.createThreadDeathRequest();
                tdr.enable();
                if (breakpoints.size() > 0) {
                    Iterator iter = breakpoints.iterator();
                    while (iter.hasNext()) {
                        Object obj = iter.next();
                        if (obj instanceof ResolvableBreakpoint) {
                            ResolvableBreakpoint bp =
                                (ResolvableBreakpoint) obj;
                            String className = bp.getClassName();
                            if (className != null) {
                                Log.debug("adding class prepare request for |" + className + "|");
                                ClassPrepareRequest cpr =
                                    mgr.createClassPrepareRequest();
                                cpr.addClassFilter(className);
                                cpr.enable();
                            }
                        }
                    }
                } else {
                    Log.debug("startProcessInternal adding default breakpoint");
                    breakpoints.add(new MethodBreakpoint(this, mainClass,
                        "main"));
                    fireBreakpointChanged();
                }
                ClassPrepareRequest cpr = mgr.createClassPrepareRequest();
                cpr.addClassFilter(mainClass);
                cpr.enable();
                if (!startSuspended) {
                    vm.resume();
                    isSuspended = false;
                    fireContextChanged();
                }
            }
        }
    }

    public void resolveDeferredRequests(ClassPrepareEvent event)
    {
        synchronized (breakpoints) {
            Iterator iter = breakpoints.iterator();
            while (iter.hasNext()) {
                ResolvableBreakpoint bp = (ResolvableBreakpoint) iter.next();
                if (!bp.isResolved()) {
                    try {
                        Log.debug("bp.getClassName() = " + bp.getClassName());
                        EventRequest eventRequest = bp.resolveAgainstPreparedClasses();
                        if (eventRequest != null) {
                            Log.debug("bp was resolved");
                            eventRequest.enable();
                        } else
                            Log.debug("bp was NOT resolved");
                    }
                    catch (Exception e) {
                        Log.error(e);
                    }
                }
            }
        }
    }

    public JdbSession getSession()
    {
        return session;
    }

    private void setSession(JdbSession session)
    {
        this.session = session;
        mainClass = session.getMainClass();
        mainClassArgs = session.getMainClassArgs();
        classPath = session.getClassPath();
        javaHome = session.getJavaHome();
        javaExecutable = session.getJavaExecutable();
        vmArgs = session.getVMArgs();
        startSuspended = session.getStartSuspended();
        sourcePath = session.getSourcePath();
        initializeBreakpoints();
    }

    private void initializeBreakpoints()
    {
        breakpoints.clear();
        List breakpointSpecifications = session.getBreakpointSpecifications();
        if (breakpointSpecifications != null) {
            Iterator iter = breakpointSpecifications.iterator();
            while (iter.hasNext()) {
                BreakpointSpecification spec =
                    (BreakpointSpecification) iter.next();
                Log.debug(spec.toString());
                int lineNumber = spec.getLineNumber();
                if (spec.getLineNumber() > 0){
                    File file = File.getInstance(spec.getFileName());
                    if (file != null && file.isFile()) {
                        LineNumberBreakpoint bp =
                            new LineNumberBreakpoint(this, spec.getClassName(),
                                file, lineNumber);
                        breakpoints.add(bp);
                    }
                } else {
                    String className = spec.getClassName();
                    String methodName = spec.getMethodName();
                    if (className != null && methodName != null) {
                        MethodBreakpoint bp =
                            new MethodBreakpoint(this, className, methodName);
                        breakpoints.add(bp);
                    }
                }
            }
        }
        fireBreakpointChanged();
    }

    public void saveSession()
    {
        session.setBreakpoints(breakpoints);
        session.saveDefaults();
    }

    public void source()
    {
        source(Editor.currentEditor());
    }

    public void source(final Editor editor)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                if (location == null)
                    return;
                Method method = location.method();
                if (method != null && method.isNative())
                    return;
                String className = location.declaringType().name();
                String sourceName = null;
                try {
                    sourceName = location.sourceName();
                    Log.debug("sourceName = |" + sourceName + "|");
                }
                catch (AbsentInformationException e) {
                    Log.error(e);
                }
                int lineNumber = location.lineNumber();
                Log.debug("lineNumber = " + lineNumber);
                Log.debug(location.declaringType().name());
                if (sourceName != null)
                    follow(editor, className, sourceName, lineNumber - 1);
            }
        };
        if (SwingUtilities.isEventDispatchThread())
            r.run();
        else
            SwingUtilities.invokeLater(r);
    }

    private boolean follow(Editor editor, String className, String fileName,
        int lineNumber)
    {
        int index = className.indexOf('$');
        if (index >= 0)
            className = className.substring(0, index);
        File file = JavaSource.findSource(className, sourcePath);
        if (file == null) {
            file = Utilities.findFileInPath(fileName, sourcePath,
                getCurrentDirectory());
        }
        if (file == null)
            return false;
        if (!file.exists())
            return false;
        final Buffer buf = Editor.getBuffer(file);
        if (buf == null)
            return false;
        Editor ed = null;
        final Buffer currentBuffer = editor.getBuffer();
        if (buf == currentBuffer) {
            ed = editor;
        } else {
            editor.makeNext(buf);
            if (currentBuffer instanceof Jdb) {
                ed = editor.activateInOtherWindow(buf);
            } else {
                // Re-use current editor.
                ed = editor;
                ed.activate(buf);
            }
        }
        Line line = buf.getLine(lineNumber);
        if (line == null) {
            ed.eob();
        } else {
            ed.addUndo(SimpleEdit.MOVE);
            ed.unmark();
            ed.update(ed.getDotLine());
            ed.setDot(line, 0);
            ed.update(ed.getDotLine());
            ed.moveCaretToDotCol();
            ed.updateDisplay();
        }
        return true;
    }

    public void dispose()
    {
        killVM();
        if (controlDialog != null) {
            controlDialog.dispose();
            controlDialog = null;
        }
        saveSession();
        removeAnnotations();
        synchronized (Jdb.class) {
            if (JavaMode.getJdb() != this)
                Debug.bug();
            JavaMode.setJdb(null);
        }
    }

    private void removeAnnotations()
    {
        if (breakpoints != null) {
            for (Iterator it = breakpoints.iterator(); it.hasNext();) {
                Object obj = it.next();
                if (obj instanceof ResolvableBreakpoint) {
                    ResolvableBreakpoint bp = (ResolvableBreakpoint) obj;
                    Line line = bp.getLine();
                    if (line != null)
                        line.setAnnotation(null);
                }
            }
            // Repaint editors with buffers in Java mode.
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getModeId() == JAVA_MODE)
                    ed.repaint();
            }
        }
    }

    private void quit()
    {
        killVM();
        removeAnnotations();
        // Copy editor list since unsplitWindow() may close an editor.
        ArrayList editors = new ArrayList();
        for (EditorIterator it = new EditorIterator(); it.hasNext();)
            editors.add(it.next());
        EditorList editorList = Editor.getEditorList();
        for (Iterator it = editors.iterator(); it.hasNext();) {
            Editor ed = (Editor) it.next();
            if (editorList.contains(ed)) {
                if (ed.getBuffer() == this) {
                    Editor other = ed.getOtherEditor();
                    if (other != null) {
                        Editor.setCurrentEditor(other);
                        ed.getFrame().unsplitWindow();
                    }
                }
            }
        }
        kill();
    }

    private void restart()
    {
        killVM();
        saveSession();
        removeAnnotations();
        session.loadDefaults();
        initializeBreakpoints();
        startProcess();
        fireContextChanged();
    }

    private synchronized void killVM()
    {
        if (vm != null) {
            try {
                vm.exit(0);
            }
            catch (VMDisconnectedException e) {
                // Already exited.
            }
            setVM(null);
        }
    }

    private void doBreak(String arg, boolean temporary)
    {
        try {
            if (vm == null)
                return;
            if (arg == null) {
                log("No location specified");
                return;
            }
            int index = arg.indexOf(':');
            if (index >= 0) {
                String fileName = arg.substring(0, index);
                try {
                    int lineNumber =
                        Integer.parseInt(arg.substring(index + 1).trim());
                    doBreakAtLineNumber(fileName, lineNumber, temporary);
                }
                catch (NumberFormatException e) {
                    log("Invalid line number");
                }
            } else {
                index = arg.lastIndexOf('.');
                if (index < 0) {
                    log("No class specified");
                    return;
                }
                String className = arg.substring(0, index);
                String methodName = arg.substring(index + 1);
                doBreakAtMethod(className, methodName, temporary);
            }
        }
        finally {
            if (isSuspended())
                prompt();
        }
    }

    private void doBreakAtLineNumber(String fileName, int lineNumber,
        boolean temporary)
    {
        File file = findSourceFile(fileName);
        if (file == null) {
            log("File not found: ".concat(fileName));
            return;
        }
        String className = file.getName();
        if (className.toLowerCase().endsWith(".java"))
            className = className.substring(0, className.length() - 5);
        Buffer buffer = Editor.getBuffer(file);
        if (buffer != null) {
            if (!buffer.initialized())
                buffer.initialize();
            if (!buffer.isLoaded())
                buffer.load();
            String packageName = JavaSource.getPackageName(buffer);
            if (packageName != null)
                className = packageName.concat(".").concat(className);
            LineNumberBreakpoint bp =
                new LineNumberBreakpoint(this, className, file, lineNumber);
            if (temporary)
                bp.setTemporary();
            try {
                EventRequest eventRequest = bp.resolveAgainstPreparedClasses();
                if (eventRequest != null) {
                    eventRequest.enable();
                } else {
                    EventRequestManager mgr = vm.eventRequestManager();
                    ClassPrepareRequest cpr = mgr.createClassPrepareRequest();
                    String classFilter = className;
                    cpr.addClassFilter(classFilter);
                    cpr.enable();
                }
                addBreakpoint(bp);
                saveSession();
                fireBreakpointChanged();
            }
            catch (AbsentInformationException absent) {
                log("Line number information is not available.");
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    private File findSourceFile(String fileName)
    {
        String canonicalPath = null;
        if (Utilities.isFilenameAbsolute(fileName)) {
            File file = File.getInstance(fileName);
            if (file != null && file.isFile())
                return file;
            // File does not exist.
            return null;
        }
        // Filename is not absolute.
        File mainFile = JavaSource.findSource(mainClass, sourcePath);
        if (mainFile != null && mainFile.isFile()) {
            File dir = mainFile.getParentFile();
            if (dir != null) {
                File file = File.getInstance(dir, fileName);
                if (file != null && file.isFile())
                    return file;
            }
        }
        // Try current directory.
        File dir = Editor.currentEditor().getCurrentDirectory();
        Log.debug("trying dir = " + dir);
        File file = File.getInstance(dir, fileName);
        if (file != null && file.isFile())
            return file;
        // Look for match in buffer list.
        List dirs = Utilities.getDirectoriesInPath(sourcePath);
        for (BufferIterator iter = new BufferIterator(); iter.hasNext();) {
            Buffer b = iter.nextBuffer();
            file = b.getFile();
            if (file.getName().equals(fileName)) {
                File rootDir = JavaSource.getPackageRootDirectory(b);
                for (Iterator it = dirs.iterator(); it.hasNext();) {
                    String dirname = (String) it.next();
                    if (dirname.equals(rootDir.canonicalPath()))
                        return file;
                }
            }
            if (Platform.isPlatformWindows()) {
                if (file.getName().equalsIgnoreCase(fileName)) {
                    File rootDir = JavaSource.getPackageRootDirectory(b);
                    for (Iterator it = dirs.iterator(); it.hasNext();) {
                        String dirname = (String) it.next();
                        if (dirname.equalsIgnoreCase(rootDir.canonicalPath()))
                            return file;
                    }
                }
            }
        }
        // Not found.
        return null;
    }

    private void doBreakAtMethod(String className, String methodName,
        boolean temporary)
    {
        if (className.indexOf(".") < 0) {
            // No package prefix.
            String fileName = className.concat(".java");
            File file = findSourceFile(fileName);
            if (file != null) {
                Buffer buffer = Editor.getBuffer(file);
                if (buffer != null) {
                    if (!buffer.initialized())
                        buffer.initialize();
                    if (!buffer.isLoaded())
                        buffer.load();
                    String packageName = JavaSource.getPackageName(buffer);
                    if (packageName != null)
                        className = packageName.concat(".").concat(className);
                }
            }
        }
        MethodBreakpoint bp = new MethodBreakpoint(this, className, methodName);
        if (temporary)
            bp.setTemporary();
        try {
            EventRequest eventRequest = bp.resolveAgainstPreparedClasses();
            if (eventRequest != null) {
                eventRequest.enable();
            } else {
                EventRequestManager mgr = vm.eventRequestManager();
                ClassPrepareRequest cpr = mgr.createClassPrepareRequest();
                cpr.addClassFilter(className);
                cpr.enable();
            }
            addBreakpoint(bp);
            saveSession();
            fireBreakpointChanged();
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    // e.g. "clear Jdb.java:877"
    private void doClear(String arg)
    {
        if (arg == null) {
            log("No breakpoint specified");
            return;
        }
        if (arg.equals("all"))
            doClearAll();
        else if (arg.indexOf(':') >= 0)
            doClearLineNumberBreakpoint(arg);
        else
            doClearMethodBreakpoint(arg);
        // Repaint editors with buffers in Java mode.
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getModeId() == JAVA_MODE)
                ed.repaint();
        }
    }

    private void doClearAll()
    {
        // Disable resolved breakpoints.
        for (Iterator it = breakpoints.iterator(); it.hasNext();) {
            Object obj = it.next();
            if (obj instanceof ResolvableBreakpoint)
                ((ResolvableBreakpoint)obj).clear();
        }
        // Clear the list.
        breakpoints.clear();
        fireBreakpointChanged();
        if (isSuspended())
            prompt();
    }

    private void doClearLineNumberBreakpoint(String arg)
    {
        int index = arg.indexOf(':');
        String fileName = arg.substring(0, index);
        if (!fileName.toLowerCase().endsWith(".java"))
            fileName = fileName.concat(".java");
        int lineNumber = -1;
        try {
            lineNumber = Integer.parseInt(arg.substring(index + 1));
        }
        catch (NumberFormatException e) {}
        if (lineNumber < 1) {
            log("Invalid breakpoint");
            return;
        }
        for (Iterator it = breakpoints.iterator(); it.hasNext();) {
            Object obj = it.next();
            if (obj instanceof LineNumberBreakpoint) {
                LineNumberBreakpoint bp = (LineNumberBreakpoint) obj;
                File file = bp.getFile();
                if (file != null) {
                    if (fileName.equals(file.getName())) {
                        if (lineNumber == bp.getLineNumber()) {
                            // Found it.
                            deleteBreakpoint(bp);
                            fireBreakpointChanged();
                            return;
                        }
                    }
                }
            }
        }
        log("No breakpoint at " + arg);
    }

    private void doClearMethodBreakpoint(String arg)
    {
        String className, methodName;
        int index = arg.lastIndexOf('.');
        if (index >= 0) {
            className = arg.substring(0, index);
            methodName = arg.substring(index + 1);
        } else {
            className = null;
            methodName = arg;
        }
        for (Iterator it = breakpoints.iterator(); it.hasNext();) {
            Object obj = it.next();
            if (obj instanceof MethodBreakpoint) {
                MethodBreakpoint bp = (MethodBreakpoint) obj;
                if (className != null) {
                    if (className.equals(bp.getClassName())) {
                        if (methodName.equals(bp.getMethodName())) {
                            // Found it.
                            deleteBreakpoint(bp);
                            fireBreakpointChanged();
                            return;
                        }
                    }
                } else {
                    // No class name specified.
                    if (methodName.equals(bp.getMethodName())) {
                        // Found it.
                        deleteBreakpoint(bp);
                        fireBreakpointChanged();
                        return;
                    }
                }
            }
        }
        log("No breakpoint at " + arg);
    }

    private synchronized void doCatch(String arg)
    {
        try {
            if (vm == null)
                return;
            int newCatchMode = -1;
            if (arg.equals("none"))
                newCatchMode = CATCH_NONE;
            else if (arg.equals("uncaught"))
                newCatchMode = CATCH_UNCAUGHT;
            else if (arg.equals("all"))
                newCatchMode = CATCH_ALL;
            else {
                log("Invalid argument");
                return;
            }
            if (newCatchMode == catchMode)
                return; // No change.
            EventRequestManager mgr = vm.eventRequestManager();
            if (catchMode != CATCH_NONE) {
                List list = mgr.exceptionRequests();
                Log.debug("exception request count = " + list.size());
                mgr.deleteEventRequests(list);
            }
            if (newCatchMode != CATCH_NONE) {
                ExceptionRequest exceptionRequest =
                    mgr.createExceptionRequest(null,
                        newCatchMode == CATCH_ALL, true);
                exceptionRequest.enable();
            }
            catchMode = newCatchMode;
        }
        finally {
            if (isSuspended())
                prompt();
        }
    }

    private void doNext(String args)
    {
        if (vm == null)
            return;
        if (currentThread == null) {
            Log.debug("currentThread is null");
            return;
        }
        Log.debug("currentThread = " + currentThread.name());
        int count = 1;
        if (args != null) {
            try {
                count = Integer.parseInt(args);
            }
            catch (NumberFormatException e) {
                log("Invalid argument");
                return;
            }
        }
        clearStepForThread(currentThread);
        EventRequestManager erm = vm.eventRequestManager();
        StepRequest request = erm.createStepRequest(currentThread,
            StepRequest.STEP_LINE, StepRequest.STEP_OVER);
        request.addCountFilter(count);
        request.enable();
        doContinue();
    }

    private void doStep(String args)
    {
        if (vm == null)
            return;
        if (currentThread == null) {
            Log.debug("currentThread is null");
            return;
        }
        Log.debug("currentThread = " + currentThread.name());
        boolean out = false;
        int count = 1;
        if (args != null) {
            if (args.equals("out")) {
                out = true;
            } else {
                try {
                    count = Integer.parseInt(args);
                }
                catch (NumberFormatException e) {
                    log("Invalid argument");
                    return;
                }
            }
        }
        clearStepForThread(currentThread);
        EventRequestManager erm = vm.eventRequestManager();
        StepRequest request =
            erm.createStepRequest(currentThread, StepRequest.STEP_LINE,
                out ? StepRequest.STEP_OUT : StepRequest.STEP_INTO);
        request.addCountFilter(count);
        request.enable();
        doContinue();
    }

    private void doFinish()
    {
        if (vm == null)
            return;
        if (currentThread == null) {
            Log.debug("currentThread is null");
            return;
        }
        clearStepForThread(currentThread);
        EventRequestManager erm = vm.eventRequestManager();
        StepRequest request =
            erm.createStepRequest(currentThread, StepRequest.STEP_LINE,
                StepRequest.STEP_OUT);
        request.addCountFilter(1);
        request.enable();
        doContinue();
    }

    private void doPrint(String what)
    {
        if (!isSuspended()) {
            log("VM is not suspended");
            return;
        }
        try {
            if (what == null || what.length() < 1) {
                log("Missing argument");
                return;
            }
            if (currentStackFrame == null) {
                log("No stack frame");
                return;
            }
            Value value = getValue(what, currentStackFrame);
            if (value == null) {
                log("null");
            } else if (value instanceof StringReference) {
                log(value.toString());
            } else if (value instanceof ArrayReference) {
                log(value.toString());
                log(getStringValueOfArray(what, (ArrayReference)value));
            } else {
                log(value.toString());
                if (value instanceof ObjectReference) {
                    String s = getStringValueOfObject((ObjectReference)value,
                        currentThread);
                    if (s != null) {
                        Log.debug(s);
                        log(s);
                    }
                    // getStringValueOfObject() resumes the current thread, so
                    // the context has changed...
                    Log.debug("doPrint calling fireContextChanged");
                    fireContextChanged();
                }
            }
        }
        catch (AbsentInformationException e) {
            Log.debug(e);
            log("Local variable information is not available.");
            log("Compile with -g to generate local variable information.");
        }
        catch (NoSuchFieldException e) {
            log("No such field");
        }
        catch (Exception e) {
            log(e.toString());
            Log.error(e);
        }
        finally {
            if (isSuspended())
                prompt();
        }
    }

    private void doStdin(String s)
    {
        Process process = vm.process();
        if (process != null) {
            OutputStream out = process.getOutputStream();
            try {
                if (s != null) {
                    out.write(s.getBytes());
                    // Format stdin like stdout. JDB_FORMAT_INPUT is for
                    // debugger commands.
                    log(s, false, JdbFormatter.JDB_FORMAT_OUTPUT);
                }
                out.write('\n');
                out.flush();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
    }

    private void doLocals()
    {
        if (vm == null)
            return;
        if (currentThread == null) {
            Log.debug("currentThread is null");
            return;
        }
        boolean contextChanged = false;
        try {
            StackFrame stackFrame = currentStackFrame;
            if (stackFrame == null && currentThread.frameCount() > 0)
                stackFrame = currentThread.frame(0);
            List variables = stackFrame.visibleVariables();
            Map map = stackFrame.getValues(variables);
            Set entrySet = map.entrySet();
            Iterator iter = entrySet.iterator();
            while (iter.hasNext()) {
                Map.Entry entry = (Map.Entry) iter.next();
                LocalVariable variable = (LocalVariable) entry.getKey();
                Value value = (Value) entry.getValue();
                FastStringBuffer sb = new FastStringBuffer(variable.typeName());
                sb.append(' ');
                sb.append(variable.name());
                sb.append(" = ");
                sb.append(value);
                if (value instanceof StringReference) {
                    ;
                } else if (value instanceof ArrayReference) {
                    String s = getStringValueOfArray(variable.name(),
                        (ArrayReference)value);
                    if (s.length() > 0) {
                        sb.append('\n');
                        sb.append(s);
                    }
                } else if (value instanceof ObjectReference) {
                    String s = getStringValueOfObject((ObjectReference)value,
                        currentThread);
                    if (s != null) {
                        sb.append(' ');
                        sb.append(s);
                    }
                    // getStringValueOfObject() resumes the current thread, so
                    // the context has changed...
                    contextChanged = true;
                }
                log(sb.toString());
            }
        }
        catch (AbsentInformationException e) {
            Log.debug(e);
            log("Local variable information is not available.");
            log("Compile with -g to generate local variable information.");
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (contextChanged)
            fireContextChanged();
        if (isSuspended())
            prompt();
    }

    private String getStringValueOfObject(ObjectReference objRef,
        ThreadReference threadRef)
    {
        try {
            // Get index of current stack frame so we can restore it later.
            List frames = threadRef.frames();
            int index = -1;
            if (frames.size() > 0) {
                for (int i = 0; i < frames.size(); i++) {
                    StackFrame frame = (StackFrame) frames.get(i);
                    if (frame != null && frame.equals(currentStackFrame)) {
                        index = i;
                        break;
                    }
                }
            }

            ReferenceType refType = objRef.referenceType();
            List methods =
                refType.methodsByName("toString", "()Ljava/lang/String;");
            Method method = (Method) methods.get(0);
            Value value = objRef.invokeMethod(threadRef, method,
                new ArrayList(), ObjectReference.INVOKE_SINGLE_THREADED);

            // Restore current stack frame if possible.
            frames = threadRef.frames();
            if (frames != null && index >= 0 && index < frames.size())
                currentStackFrame = (StackFrame) frames.get(index);

            if (value != null)
                return value.toString();
        }
        catch (Exception e) {
            Log.error(e);
        }
        return null;
    }

    private static String getStringValueOfArray(String name, ArrayReference ar)
    {
        FastStringBuffer sb = new FastStringBuffer();
        final int limit = ar.length();
        for (int i = 0; i < limit; i++) {
            sb.append("    ");
            sb.append(name);
            sb.append('[');
            sb.append(i);
            sb.append("]: ");
            Value v = ar.getValue(i);
            sb.append(v == null ? "null" : v.toString());
            if (i < limit-1)
                sb.append('\n');
        }
        return sb.toString();
    }

    private void clearStepForThread(ThreadReference threadRef)
    {
        EventRequestManager erm = vm.eventRequestManager();
        List requests = erm.stepRequests();
        Iterator iter = requests.iterator();
        while (iter.hasNext()) {
            StepRequest request = (StepRequest) iter.next();
            if (request.thread().equals(threadRef)) {
                erm.deleteEventRequest(request);
                break; // There should be only one!
            }
        }
    }

    private static Value getValue(String expression, StackFrame frame)
        throws Exception
    {
        Log.debug("getValue");
        StringTokenizer st = new StringTokenizer(expression, "[].");
        if (!st.hasMoreTokens()) {
            Log.debug("no more tokens");
            throw new NoSuchFieldException();
        }
        String token = st.nextToken();
        Log.debug("token = |" + token + "|");
        Value currentValue = null;
        Field currentField = null;
        ObjectReference obj = null;
        LocalVariable local = null;
        if (token.equals("this")) {
            currentValue = frame.thisObject();
            if (currentValue == null)
                throw new NoSuchFieldException(token);
        } else {
            Log.debug("calling visibleVariableByName");
            local = frame.visibleVariableByName(token);
            Log.debug("local = " + local);
            if (local != null) {
                currentValue = frame.getValue(local);
                Log.debug("currentValue = " + currentValue);
            } else {
                ReferenceType refType = frame.location().declaringType();
                Log.debug("refType = " + refType);
                obj = frame.thisObject();
                if (obj == null) {
                    // Static method.
                    Log.debug("static method");
                    currentField = refType.fieldByName(token);
                    if (currentField != null && currentField.isStatic())
                        currentValue = refType.getValue(currentField);
                    else
                        throw new NoSuchFieldException();
                } else {
                    currentField = refType.fieldByName(token);
                    if (currentField != null)
                        currentValue = obj.getValue(currentField);
                    else {
                        Log.debug("throwing NoSuchFieldException ...");
                        throw new NoSuchFieldException();
                    }
                }
            }
        }
        while (st.hasMoreTokens() && currentValue != null) {
            String prevToken = token;
            token = st.nextToken();
            Log.debug("while loop token = |" + token + "|");
            Object arg;
            try {
                arg = new Integer(token);
            } catch (NumberFormatException e) {
                arg = token;
            }
            if (currentValue instanceof ArrayReference) {
                int count = -1;
                if (arg instanceof Integer)
                    count = ((Integer)arg).intValue();
                if (count >= 0 && count < ((ArrayReference)currentValue).length())
                    currentValue = ((ArrayReference)currentValue).getValue(count);
                else
                    throw new ArrayIndexOutOfBoundsException();
            } else if (currentValue instanceof ObjectReference &&
                arg instanceof String) {
                Log.debug("object reference, string");
                obj = (ObjectReference) currentValue;
                ReferenceType refType = obj.referenceType();
                currentField = refType.fieldByName(token);
                if (currentField != null)
                    currentValue = obj.getValue(currentField);
            } else
                throw new Exception();
        }
        Log.debug("getValue returning currentValue = " + currentValue);
        return currentValue;
    }

    public boolean isModified()
    {
        return false;
    }

    // For the buffer list.
    public String toString()
    {
        return "jdb";
    }

    public String getTitle()
    {
        return "jdb";
    }

    public Icon getIcon()
    {
        return Utilities.getIconFromFile("jpty.png");
    }
}
