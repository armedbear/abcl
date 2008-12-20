/*
 * IList.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Stack;
import javax.swing.SwingUtilities;

public final class IList implements BackgroundProcess, Constants
{
    private final HashSet searchedFiles = new HashSet(256);
    private final Stack stack = new Stack();
    private final Editor editor;
    private final Buffer sourceBuffer;
    private final Search search;
    private final boolean verbose;
    private final String path;
    private final File currentDirectory;

    private ListOccurrencesInFiles outputBuffer;
    private boolean cancelled;

    public IList(Editor editor, Search search, boolean verbose)
    {
        this.editor = editor;
        sourceBuffer = editor.getBuffer();
        this.search = search;
        this.verbose = verbose;
        path = sourceBuffer.getStringProperty(Property.INCLUDE_PATH);
        currentDirectory = editor.getCurrentDirectory();
    }

    private Buffer getSourceBuffer()
    {
        return sourceBuffer;
    }

    private ListOccurrences getOutputBuffer()
    {
        return outputBuffer;
    }

    private ListOccurrencesInFiles createOutputBuffer()
    {
        ListOccurrencesInFiles buf = new ListOccurrencesInFiles(search);
        FastStringBuffer sb = new FastStringBuffer(sourceBuffer.getFile().getName());
        sb.append(" \"");
        sb.append(search.getPattern());
        sb.append('"');
        buf.setTitle(sb.toString());
        return buf;
    }

    public void run()
    {
        if (SwingUtilities.isEventDispatchThread())
            Debug.bug();
        try {
            final Mode mode = sourceBuffer.getMode();
            for (Line line = sourceBuffer.getFirstLine(); line != null; line = line.next()) {
                Position pos = new Position(line, 0);
                if ((pos = search.findInLine(mode, pos)) != null)
                    found(sourceBuffer.getFile(), line.getText(), line.lineNumber()+1);
                String s = Utilities.extractInclude(line.getText());
                if (s != null)
                    searchFile(s, search);
                if (cancelled)
                    break;
            }
            if (outputBuffer != null) {
                if (cancelled)
                    outputBuffer.appendStatusLine("Search cancelled by user");
                outputBuffer.renumber();
                outputBuffer.setLoaded(true);
            }
        }
        finally {
            Log.debug("calling sourceBuffer.unlockRead");
            sourceBuffer.unlockRead();
            sourceBuffer.setBusy(false);
            SwingUtilities.invokeLater(completionRunnable);
        }
    }

    public void cancel()
    {
        cancelled = true;
    }

    private void searchFile(final String s, Search search)
    {
        File file = Utilities.findInclude(s, path, currentDirectory);
        if (file == null) {
            // Not found.
            if (verbose) {
                if (outputBuffer == null)
                    outputBuffer = createOutputBuffer();
                outputBuffer.appendLine(s.concat(" not found"));
            }
            return;
        }
        if (searchedFiles.contains(file)) {
            // Already searched.
            if (verbose) {
                if (outputBuffer == null)
                    outputBuffer = createOutputBuffer();
                outputBuffer.appendLine(s.concat(" already searched"));
            }
            return;
        }
        searchedFiles.add(file);
        if (verbose) {
            if (outputBuffer == null)
                outputBuffer = createOutputBuffer();
            outputBuffer.appendLine(s.concat(": searching ".concat(file.toString())));
        }
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(file.getInputStream()));
            String line;
            int lineNumber = 0;
            while ((line = reader.readLine()) != null) {
                ++lineNumber;
                if (search.find(line)) {
                    found(file, line, lineNumber);
                }
                String name = Utilities.extractInclude(line);
                if (name != null) {
                    // Recurse!
                    stack.push(file);
                    searchFile(name, search);
                    stack.pop();
                }
            }
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private File currentFile;

    private void found(File file, String s, int lineNumber)
    {
        if (outputBuffer == null)
            outputBuffer = createOutputBuffer();
        if (!file.equals(currentFile)) {
            outputBuffer.appendFileLine(file, true);
            currentFile = file;
        }
        outputBuffer.appendOccurrenceLine(s, lineNumber);
    }

    public static void iList()
    {
        iList(false);
    }

    public static void iList(String arg)
    {
        iList(arg != null && arg.trim().equals("-v"));
    }

    private static void iList(boolean verbose)
    {
        final Editor editor = Editor.currentEditor();
        int modeId = editor.getModeId();
        if (modeId == C_MODE || modeId == CPP_MODE) {
            final Search search = editor.getSearchAtDot();
            if (search != null) {
                editor.setLastSearch(search);
                editor.setWaitCursor();
                IList ilist = new IList(editor, search, verbose);
                Buffer buffer = ilist.getSourceBuffer();
                try {
                    buffer.lockRead();
                }
                catch (InterruptedException e) {
                    Log.error(e);
                    return;
                }
                buffer.setBusy(true);
                new Thread(ilist).start();
            }
        }
    }

    private Runnable completionRunnable = new Runnable() {
        public void run()
        {
            Log.debug("completionRunnable.run");
            editor.setDefaultCursor();
            Buffer buf = getOutputBuffer();
            if (buf != null) {
                editor.makeNext(buf);
                Editor ed = editor.activateInOtherWindow(buf);
                ed.setDot(buf.getInitialDotPos());
                ed.moveCaretToDotCol();
                ed.updateDisplay();
            } else if (!cancelled)
                search.notFound(editor);
            if (cancelled)
                editor.status("Search cancelled");
        }
    };
}
