/*
 * CompilationCommands.java
 *
 * Copyright (C) 2003-2004 Peter Graves
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

import java.awt.AWTEvent;
import java.awt.event.MouseEvent;

public final class CompilationCommands implements Constants
{
    private static CompilationBuffer lastCompilationBuffer;

    public static CompilationBuffer getCompilationBuffer()
    {
        return lastCompilationBuffer;
    }

    public static void compile()
    {
        if (!checkPlatform("Compile"))
            return;
        final Editor editor = Editor.currentEditor();
        CompileDialog d = new CompileDialog(editor);
        editor.centerDialog(d);
        d.show();
        editor.repaintNow();
        final String command = d.getCommand();
        if (command != null && command.length() > 0)
            compile(command, editor);
    }

    public static void compile(String args)
    {
        if (!checkPlatform("Compile"))
            return;
        if (args != null && args.length() > 0) {
            History history = new History("compile.command");
            history.append(args);
            history.save();
            compile(args, Editor.currentEditor());
        }
    }

    public static void recompile()
    {
        if (!checkPlatform("Recompile"))
            return;
        final History history = new History("compile.command");
        final String command = history.getPrevious();
        if (command != null && command.length() > 0)
            compile(command, Editor.currentEditor());
        else
            compile();
    }

    private static boolean checkPlatform(String command)
    {
        if (Platform.isPlatformWindows()) {
            if (Platform.isPlatformWindows5())
                ; // OK (Windows 2000, Windows XP)
            else if (Platform.isPlatformWindowsNT4())
                ; // OK (NT 4)
            else {
                MessageDialog.showMessageDialog(
                    "This feature requires Windows NT 4, Windows 2000 or Windows XP.",
                    command);
                return false;
            }
        }
        return true;
    }

    private static void compile(final String command, final Editor editor)
    {
        IdleThread.killFollowContextTask();
        Editor.getTagFileManager().setEnabled(false);
        saveCompilableBuffers(editor);

        CompilationBuffer cb = null;
        boolean visible = false;

        if (lastCompilationBuffer != null) {
            // Re-use existing compilation buffer.
            cb = lastCompilationBuffer;
            if (!Editor.getBufferList().contains(cb))
                cb.relink();
            cb.empty();
            cb.setCommand(command);
            cb.setParentBuffer(editor.getBuffer());
            cb.setCurrentDirectory(editor.getCurrentDirectory());
            // Is it visible?
            EditorIterator it = new EditorIterator();
            while (it.hasNext()) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == cb) {
                    ed.updateLocation();
                    ed.repaintNow();
                    visible = true;
                }
            }
        } else {
            cb = new CompilationBuffer(command, editor.getCurrentDirectory());
            cb.setParentBuffer(editor.getBuffer());
            lastCompilationBuffer = cb;
        }

        // Call initialize() before starting the thread so we can put the
        // expanded command in the location bar when the compilation buffer is
        // activated.
        cb.initialize();
        // Don't keep a reference to the parent buffer indefinitely!
        cb.setParentBuffer(null);
        new Thread(cb).start();
        if (!visible) {
            Editor otherEditor = editor.getOtherEditor();
            if (otherEditor != null) {
                cb.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                otherEditor.makeNext(cb);
            } else
                cb.setUnsplitOnClose(true);
            editor.displayInOtherWindow(cb);
        }
    }

    public static void thisError()
    {
        final Editor editor = Editor.currentEditor();
        // If this method is invoked via a mouse event mapping, move dot to
        // location of mouse click first.
        AWTEvent e = editor.getDispatcher().getLastEvent();
        if (e instanceof MouseEvent)
            editor.mouseMoveDotToPoint((MouseEvent)e);
        CompilationError error =
            CompilationError.parseLineAsErrorMessage(editor.getDotLine());
        if (error != null) {
            final Buffer buffer = editor.getBuffer();
            if (buffer instanceof CompilationErrorBuffer)
                ((CompilationErrorBuffer)buffer).setCurrentError(error);
            String errorFileName = error.getFileName();
            int errorLineNumber = error.getLineNumber();
            if (errorFileName != null && errorLineNumber != 0) {
                Buffer buf =
                    getSourceBuffer(buffer.getCurrentDirectory(),
                        errorFileName);
                if (buf == null)
                    return;
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null)
                    otherEditor.makeNext(buf);
                Editor ed = editor.activateInOtherWindow(buf);
                int lineNumber = errorLineNumber - 1;
                if (lineNumber < 0)
                    lineNumber = 0;
                Position pos = buf.findOriginal(lineNumber, error.getOffset());
                ed.moveDotTo(pos);
                ed.setUpdateFlag(REFRAME);
                ed.updateDisplay();
                Sidebar sidebar = editor.getSidebar();
                if (sidebar != null)
                    sidebar.setUpdateFlag(SIDEBAR_BUFFER_LIST_ALL);
            }
        }
    }

    public static void nextError()
    {
        nextOrPreviousError(true);
    }

    public static void previousError()
    {
        nextOrPreviousError(false);
    }

    private static void nextOrPreviousError(boolean next)
    {
        final Editor editor = Editor.currentEditor();
        CompilationErrorBuffer errorBuffer;
        if (editor.getModeId() == XML_MODE)
            errorBuffer = XmlMode.getErrorBuffer();
        else
            errorBuffer = lastCompilationBuffer;
        if (errorBuffer == null)
            return;
        if (!Editor.getBufferList().contains(errorBuffer)) {
            errorBuffer.relink();
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
        }
        CompilationError error =
            next ? errorBuffer.nextError() : errorBuffer.previousError();
        if (error == null) {
            editor.status("No more errors");
            return;
        }
        boolean useOtherWindow = false;
        // Find editor displaying error buffer (if any).
        Editor ed = null;
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            ed = it.nextEditor();
            if (ed.getBuffer() == errorBuffer)
                break;
        }
        if (ed.getBuffer() != errorBuffer) {
            // The compilation buffer is not currently displayed.
            ed = editor.displayInOtherWindow(errorBuffer);
        } else if (ed == editor) {
            // This command was invoked from the window displaying the
            // compilation buffer.
            useOtherWindow = true;
        }
        // Move caret to relevant line of error buffer.
        Line errorLine = error.getErrorLine();
        if (errorLine != null) {
            Debug.assertTrue(ed.getBuffer() == errorBuffer);
            ed.addUndo(SimpleEdit.MOVE);
            ed.update(ed.getDotLine());
            ed.setDot(errorLine, 0);
            ed.update(ed.getDotLine());
            ed.moveCaretToDotCol();
            ed.getDisplay().setUpdateFlag(REFRAME);
            ed.updateDisplay();
        }
        String errorFileName = error.getFileName();
        int errorLineNumber = error.getLineNumber();
        if (errorFileName != null && errorLineNumber != 0) {
            // Find or create buffer for source file containing the error.
            Buffer buf =
                getSourceBuffer(errorBuffer.getCurrentDirectory(),
                    errorFileName);
            if (buf == null)
                return;
            Debug.assertTrue(ed.getBuffer() == errorBuffer);
            if (useOtherWindow) {
                Debug.assertTrue(ed == editor);
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null)
                    otherEditor.makeNext(buf);
                ed = editor.activateInOtherWindow(buf);
            } else {
                ed = editor;
                ed.makeNext(buf);
                ed.activate(buf);
            }
            int lineNumber = errorLineNumber - 1;
            if (lineNumber < 0)
                lineNumber = 0;
            Position pos = buf.findOriginal(lineNumber, error.getOffset());
            ed.moveDotTo(pos);
            ed.setUpdateFlag(REFRAME);
            ed.updateDisplay();
            Sidebar sidebar = editor.getSidebar();
            if (sidebar != null)
                sidebar.setUpdateFlag(SIDEBAR_BUFFER_LIST_ALL);
        } else
            editor.status("No more errors");
    }

    private static void saveCompilableBuffers(Editor editor)
    {
        editor.setWaitCursor();
        int numModified = 0;
        int numErrors = 0;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (!buf.isModified())
                continue;
            if (buf.isUntitled())
                continue;
            final int modeId = buf.getModeId();
            if (modeId == SEND_MAIL_MODE)
                continue;
            if (modeId == CHECKIN_MODE)
                continue;
            if (buf.getFile() != null && buf.getFile().isLocal()) {
                editor.status("Saving modified buffers...");
                ++numModified;
                if (buf.getBooleanProperty(Property.REMOVE_TRAILING_WHITESPACE))
                    buf.removeTrailingWhitespace();
                if (!buf.save())
                    ++numErrors;
            }
        }
        if (numModified == 0)
            ;
        else if (numErrors == 0)
            editor.status("Saving modified buffers...done");
        else {
            // User will already have seen detailed error information from
            // Buffer.save().
            editor.status("Unable to save all compilable buffers");
        }
        editor.setDefaultCursor();
    }

    private static Buffer getSourceBuffer(File currentDirectory,
        String errorFileName)
    {
        File file = File.getInstance(currentDirectory, errorFileName);
        if (!file.isFile()) {
            // Strip path prefix.
            file = File.getInstance(errorFileName);
            String name = file.getName();
            // Look in current directory.
            file = File.getInstance(currentDirectory, name);
            if (!file.isFile())
                return null;
        }
        return Editor.getBuffer(file);
    }

    public static void showMessage()
    {
        final Editor editor = Editor.currentEditor();
        CompilationErrorBuffer errorBuffer;
        if (editor.getModeId() == XML_MODE)
            errorBuffer = XmlMode.getErrorBuffer();
        else
            errorBuffer = lastCompilationBuffer;
        if (errorBuffer != null) {
            CompilationError error = errorBuffer.getCurrentError();
            if (error != null) {
                String message = error.getMessage();
                if (message != null) {
                    int lineNumber = error.getLineNumber();
                    int columnNumber = -1;
                    int offset = error.getOffset();
                    if (offset >= 0)
                        columnNumber = offset + 1;
                    String title = "Line " + lineNumber;
                    if (columnNumber > 0)
                        title += "   Col " + columnNumber;
                    if (message.length() > 65)
                        message = Utilities.wrap(message, 65, 8);
                    MessageDialog.showMessageDialog(editor, message, title);
                }
            }
        }
    }
}
