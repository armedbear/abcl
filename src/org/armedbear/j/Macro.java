/*
 * Macro.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.util.ArrayList;
import javax.swing.undo.CompoundEdit;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.LispThread;

public final class Macro implements Constants
{
    private static Macro macro;

    public static synchronized void recordMacro()
    {
        if (Editor.isRecordingMacro())
            Editor.setRecordingMacro(false);
        else {
            final Editor editor = Editor.currentEditor();
            if (macro != null && !macro.isEmpty()) {
                if (!editor.confirm("Record Macro",
                                    "Overwrite existing keyboard macro?"))
                    return;
            }
            macro = new Macro(editor);
            Editor.setRecordingMacro(true);
        }
    }

    public static synchronized void startMacro()
    {
        if (Editor.isRecordingMacro()) {
            MessageDialog.showMessageDialog(
                Editor.currentEditor(),
                "Command ignored (already recording a macro)",
                "Start Macro");
        } else {
            final Editor editor = Editor.currentEditor();
            if (macro != null && !macro.isEmpty()) {
                if (!editor.confirm("Start Macro",
                                    "Overwrite existing keyboard macro?"))
                    return;
            }
            macro = new Macro(editor);
            Editor.setRecordingMacro(true);
        }
    }

    public static synchronized void endMacro()
    {
        if (Editor.isRecordingMacro()) {
            Editor.setRecordingMacro(false);
        } else {
            MessageDialog.showMessageDialog(
                Editor.currentEditor(),
                "Command ignored (not recording a macro)",
                "End Macro");
        }
    }

    public static synchronized void playbackMacro()
    {
        final Editor editor = Editor.currentEditor();
        if (Editor.isRecordingMacro()) {
            MessageDialog.showMessageDialog(editor,
                                            "Command ignored (playbackMacro is not allowed while recording a macro)",
                                            "Record Macro");
            return;
        }
        if (macro == null || macro.isEmpty()){
            editor.status("No keyboard macro defined");
            return;
        }
        macro.playback();
    }

    private final Editor editor;
    private ArrayList list = new ArrayList();

    private Macro(Editor editor)
    {
        this.editor = editor;
    }

    public Editor getEditor()
    {
        return editor;
    }

    private synchronized boolean isEmpty()
    {
        return list.isEmpty();
    }

    public static synchronized void record(Editor editor, Object command)
    {
        if (command == "recordMacro" || command == "playbackMacro" ||
            command == "startMacro" || command == "endMacro")
            return;
        if (macro != null && macro.getEditor() == editor)
            macro.record(command);
    }

    public static synchronized void record(Editor editor, char c)
    {
        if (macro != null && macro.getEditor() == editor)
            macro.record(c);
    }

    private synchronized void record(Object command)
    {
        list.add(command);
    }

    private synchronized void record(char c)
    {
        list.add(new Character(c));
    }

    private synchronized void playback()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = buffer.beginCompoundEdit();
            final int size = list.size();
            for (int i = 0; i < size; i++) {
                editor.setCurrentCommand(COMMAND_NOTHING);
                Object object = list.get(i);
                if (object instanceof String) {
                    editor.executeCommand((String)object);
                } else if (object instanceof LispObject) {
                    try {
                        LispThread.currentThread().execute((LispObject)object);
                    }
                    catch (Throwable t) {
                        Log.error(t);
                    }
                } else if (object instanceof Character) {
                    editor.insertNormalChar(((Character)object).charValue());
                }
                editor.setLastCommand(editor.getCurrentCommand());
            }
            buffer.endCompoundEdit(compoundEdit);
        }
        finally {
            buffer.unlockWrite();
        }
    }
}
