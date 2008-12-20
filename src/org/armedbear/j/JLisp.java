/*
 * JLisp.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.ServerSocket;
import java.net.Socket;
import javax.swing.SwingUtilities;
import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.LispThread;

public final class JLisp extends LispShell
{
    private Thread thread;
    private final File initialDir;

    private int port;
    private Socket socket;
    private Interpreter interpreter;

    private JLisp(File initialDir)
    {
        super();
        this.initialDir = initialDir;
        title = "jlisp";
        mode = LispShellMode.getMode();
        formatter = mode.getFormatter(this);
        setInitialized(true);
    }

    protected void initializeHistory()
    {
        history = new History("jlisp.history", 30);
    }

    public String toString()
    {
        return "jlisp";
    }

    // Returns true if underlying process is alive and well.
    protected boolean checkProcess()
    {
        return true;
    }

    protected void startProcess()
    {
        thread = new Thread("JLisp interpreter") {
            public void run()
            {
                try {
                    startServer();
                    if (interpreter != null) {
                        Editor.setLispInitialized(true);
                        interpreter.run();
                    }
                }
                catch (Exception e) {
                    Log.error(e);
                }
                Log.debug("interpreter thread exiting");
                Lisp.resetIO();
                Runnable processExitedRunnable = new Runnable() {
                    public void run()
                    {
                        appendString("\nProcess exited\n");
                        setBusy(false);
                        updateDisplayInAllFrames();
                    }
                };
                SwingUtilities.invokeLater(processExitedRunnable);
                LispThread.remove(thread);
            }
        };
        thread.setDaemon(true);
        thread.start();

        synchronized (this) {
            if (port == 0) {
                try {
                    wait(3000);
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
                if (port == 0)
                    return;
            }
        }

        try {
            socket = new Socket("localhost", port);
            stdin  = new OutputStreamWriter(socket.getOutputStream());
            stdoutThread = new StdoutThread(socket.getInputStream());
            stdoutThread.setName("JLisp reader");
            stdoutThread.setDaemon(true);
            stdoutThread.start();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private void startServer()
    {
        try {
            ServerSocket serverSocket = new ServerSocket(0);
            port = serverSocket.getLocalPort();
            synchronized (this) {
                notify();
            }
            Socket socket = serverSocket.accept(); // Blocks.
            interpreter =
                Interpreter.createJLispInstance(socket.getInputStream(),
                                                socket.getOutputStream(),
                                                initialDir.canonicalPath(),
                                                Version.getLongVersionString());
        }
        catch (Throwable t) {
            Log.error(t);
        }
    }

    public synchronized void dispose()
    {
        Thread disposeThread = new Thread("JLisp dispose") {
            public void run()
            {
                Log.debug("JLisp.dispose");
                if (interpreter != null)
                    interpreter.kill();
                if (socket != null) {
                    try {
                        Log.debug("closing socket");
                        socket.close();
                        Log.debug("back from closing socket");
                    }
                    catch (IOException e) {
                        Log.debug(e);
                    }
                    socket = null;
                }
                if (interpreter != null) {
                    interpreter.dispose();
                    interpreter = null;
                }
            }
        };
        disposeThread.setDaemon(true);
        disposeThread.start();
    }

    public static void jlisp()
    {
        final Editor editor = Editor.currentEditor();
        // Look for existing jlisp buffer.
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof JLisp) {
                editor.makeNext(buf);
                editor.activateInOtherWindow(buf);
                return;
            }
        }
        // Not found.
        editor.setWaitCursor();
        File initialDir = editor.getCurrentDirectory();
        if (initialDir == null || initialDir.isRemote())
            initialDir = Directories.getUserHomeDirectory();
        JLisp jlisp = new JLisp(initialDir);
        jlisp.startProcess();
        editor.makeNext(jlisp);
        Editor ed;
        Buffer b = editor.getBuffer();
        if (b != null && b.isPaired()) {
            Frame frame = editor.getFrame();
            editor.switchToBuffer(jlisp);
            ed = frame.getCurrentEditor();
        } else
            ed = editor.activateInOtherWindow(jlisp);
        ed.eob();
        editor.setDefaultCursor();
    }

    public static void runStartupScript(File file) throws ConditionThrowable
    {
        if (!Editor.isLispInitialized()) {
            Interpreter.initializeJLisp();
            Editor.setLispInitialized(true);
        }
        FastStringBuffer sb = new FastStringBuffer("(load \"");
        if (Platform.isPlatformWindows()) {
            String cp = file.canonicalPath();
            final int limit = cp.length();
            for (int i = 0; i < limit; i++) {
                char c = cp.charAt(i);
                if (c == '\\')
                    sb.append('\\'); // Double backslash.
                sb.append(c);
            }
        } else
            sb.append(file.canonicalPath());
        sb.append("\")");
        Interpreter.evaluate(sb.toString());
    }

    public static LispObject runLispCommand(String command)
        throws ConditionThrowable
    {
        if (!Editor.isLispInitialized()) {
            Interpreter.initializeJLisp();
            Editor.setLispInitialized(true);
        }
        return Interpreter.evaluate(command);
    }
}
