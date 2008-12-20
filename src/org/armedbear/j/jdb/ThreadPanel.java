/*
 * ThreadPanel.java
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
import com.sun.jdi.Location;
import com.sun.jdi.Method;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.VirtualMachine;
import java.awt.Component;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.JavaSource;
import org.armedbear.j.Log;

public final class ThreadPanel implements ContextListener, MouseListener
{
    private final Jdb jdb;
    private final JdbControlDialog dialog;
    private final JList list;
    private final JScrollPane scrollPane;

    private List threads;

    public ThreadPanel(Jdb jdb, JdbControlDialog dialog)
    {
        this.jdb = jdb;
        this.dialog = dialog;
        Vector v = new Vector();
        list = new JList(v);
        scrollPane = new JScrollPane(list);
        jdb.addContextListener(this);
        list.addMouseListener(this);
    }

    public Component getComponent()
    {
        return scrollPane;
    }

    public void contextChanged()
    {
        final Vector v = new Vector();
        int index = -1;
        VirtualMachine vm = jdb.getVM();
        if (vm != null) {
            threads = vm.allThreads();
            for (Iterator iter = threads.iterator(); iter.hasNext();) {
                ThreadReference threadRef = (ThreadReference) iter.next();
                FastStringBuffer sb = new FastStringBuffer();
                sb.append(threadRef.name());
                sb.append(" (id=");
                sb.append(threadRef.uniqueID());
                sb.append(") ");
                switch (threadRef.status()) {
                    case ThreadReference.THREAD_STATUS_UNKNOWN:
                        sb.append("UNKNOWN");
                        break;
                    case ThreadReference.THREAD_STATUS_ZOMBIE:
                        sb.append("ZOMBIE");
                        break;
                    case ThreadReference.THREAD_STATUS_RUNNING:
                        sb.append("RUNNING");
                        break;
                    case ThreadReference.THREAD_STATUS_SLEEPING:
                        sb.append("SLEEPING");
                        break;
                    case ThreadReference.THREAD_STATUS_MONITOR:
                        sb.append("MONITOR");
                        break;
                    case ThreadReference.THREAD_STATUS_WAIT:
                        sb.append("WAIT");
                        break;
                    case ThreadReference.THREAD_STATUS_NOT_STARTED:
                        sb.append("NOT_STARTED");
                        break;
                }
                if (threadRef == jdb.getCurrentThread())
                    index = v.size();
                v.add(sb.toString());
            }
        }
        // Update UI in event dispatch thread.
        final int finalIndex = index;
        Runnable r = new Runnable() {
            public void run()
            {
                list.setListData(v);
                list.setSelectedIndex(finalIndex);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void mousePressed(MouseEvent e)
    {
        if (!jdb.isSuspended())
            return;
        // Mask off the bits we don't care about (Java 1.4).
        int modifiers = e.getModifiers() & 0x1f;
        if (modifiers == InputEvent.BUTTON1_MASK ||
            modifiers == InputEvent.BUTTON2_MASK) {
            if (modifiers == InputEvent.BUTTON2_MASK)
                list.setSelectedIndex(list.locationToIndex(e.getPoint()));
            list.paintImmediately(0, 0, list.getWidth(), list.getHeight());
            int index = list.getSelectedIndex();
            if (threads != null && index >= 0 && index < threads.size()) {
                ThreadReference threadRef = (ThreadReference) threads.get(index);
                jdb.setCurrentThread(threadRef);
                try {
                    List frames = threadRef.frames();
                    if (frames.size() > 0) {
                        StackFrame stackFrame = threadRef.frame(0);
                        Location location = stackFrame.location();
                        Method method = location.method();
                        if (method != null && !method.isNative()) {
                            String className = location.declaringType().name();
                            final int lineNumber = location.lineNumber();
                            Log.debug(className.concat(":").concat(String.valueOf(lineNumber)));
                            File file =
                                JavaSource.findSource(className, jdb.getSourcePath());
                            if (file != null) {
                                Buffer buffer = Editor.getBuffer(file);
                                if (buffer != null) {
                                    Editor editor = null;
                                    for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                                        Editor ed = it.nextEditor();
                                        if (ed.getBuffer() instanceof Jdb) {
                                            editor = ed;
                                            break;
                                        }
                                    }
                                    if (editor != null) {
                                        editor.makeNext(buffer);
                                        editor = editor.activateInOtherWindow(buffer);
                                    } else {
                                        editor = Editor.currentEditor();
                                        editor.makeNext(buffer);
                                        editor.activate(buffer);
                                    }
                                    if (lineNumber > 0) {
                                        editor.jumpToLine(lineNumber - 1);
                                        editor.updateDisplay();
                                    }
                                }
                            }
                        }
                    }
                    // Update stack panel.
                    jdb.fireContextChanged();
                }
                catch (IncompatibleThreadStateException ex) {
                    Log.error(ex);
                }
            }
        }
        dialog.requestDefaultFocus();
    }

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e) {}

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e) {}
}
