/*
 * ListOccurrences.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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

public class ListOccurrences extends Buffer
{
    protected final Search search;
    private final Buffer sourceBuffer;

    protected ListOccurrences(Search search)
    {
        this.search = search;
        sourceBuffer = null;
        init();
    }

    private ListOccurrences(Search search, Buffer sourceBuffer)
    {
        this.search = search;
        this.sourceBuffer = sourceBuffer;
        init();
        if (sourceBuffer.getFile() != null)
            title = sourceBuffer.getFile().getName() + " \"" + search.getPattern() + "\"";
        else
            title = sourceBuffer.getTitle() + " \"" + search.getPattern() + "\"";
        setTabWidth(sourceBuffer.getTabWidth());
        if (sourceBuffer.getFile() != null)
            appendLine("File: " + sourceBuffer.getFile().netPath());
        else
            appendLine("Buffer: " + sourceBuffer.getTitle());
    }

    private void init()
    {
        initializeUndo();
        type = TYPE_LIST_OCCURRENCES;
        mode = ListOccurrencesMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        setTransient(true);
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            if (search.isRegularExpression())
                appendLine("Regular expression: \"" + search.getPattern() + '"');
            else
                appendLine("Pattern: \"" + search.getPattern() + '"');
            if (search instanceof Replacement) {
                String replaceWith = ((Replacement)search).getReplaceWith();
                if (replaceWith != null)
                    appendLine("Replace with: \"" + replaceWith + '"');
            }
            appendLine("Options: " + getOptions());
        }
        finally {
            unlockWrite();
        }
        setInitialized(true);
    }

    public Mode getParentMode()
    {
        if (search instanceof FindInFiles)
            return ((FindInFiles)search).getMode();
        if (sourceBuffer != null)
            return sourceBuffer.getMode();
        return null;
    }

    public static ListOccurrences findBuffer(Buffer sourceBuffer, Search search)
    {
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof ListOccurrences) {
                ListOccurrences lo = (ListOccurrences) buf;
                if (lo.search.equals(search) && lo.sourceBuffer == sourceBuffer)
                    return lo;
            }
        }
        return null;
    }

    private static ListOccurrences createBuffer(Buffer sourceBuffer, Search search)
    {
        ListOccurrences listOccurrences = null;
        Position pos = new Position(sourceBuffer.getFirstLine(), 0);
        int count = 0;
        while ((pos = search.find(sourceBuffer.getMode(), pos)) != null) {
            if (listOccurrences == null)
                listOccurrences = new ListOccurrences(search, sourceBuffer);
            listOccurrences.appendOccurrenceLine(pos.getLine());
            ++count;
            Line next = pos.getNextLine();
            if (next != null)
                pos.moveTo(next, 0);
            else
                break;
        }
        if (count > 0) {
            FastStringBuffer sb = new FastStringBuffer("Pattern found in ");
            sb.append(count);
            sb.append(" line");
            if (count > 1)
                sb.append('s');
            listOccurrences.appendStatusLine(sb.toString());
        }
        if (listOccurrences != null) {
            listOccurrences.renumber();
            listOccurrences.setLoaded(true);
        }
        return listOccurrences;
    }

    public static ListOccurrences getBuffer(Buffer sourceBuffer, Search search)
    {
        ListOccurrences lo = findBuffer(sourceBuffer, search);
        if (lo != null)
            return lo;
        else
            return createBuffer(sourceBuffer, search);
    }

    public final Buffer getSourceBuffer()
    {
        return sourceBuffer;
    }

    public File getCurrentDirectory()
    {
        if (sourceBuffer != null)
            return sourceBuffer.getCurrentDirectory();
        else
            return Directories.getUserHomeDirectory();
    }

    public void findOccurrenceAtDot(Editor editor, boolean killList)
    {
        Position pos = editor.getDotCopy();
        if (pos == null)
            return;
        final Line line = pos.getLine();
        if (!(line instanceof OccurrenceLine))
            return;
        Buffer buf = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b == sourceBuffer) {
                buf = b;
                break;
            }
        }
        if (buf == null) {
            sourceBuffer.relink();
            buf = sourceBuffer;
        }
        if (!buf.isLoaded()) {
            Log.debug("findOccurrence reloading source buffer");
            buf.load();
            if (!buf.isLoaded()) {
                editor.status("Unable to load buffer");
                return;
            }
        }
        final Line sourceLine = ((OccurrenceLine)line).getSourceLine();
        Debug.assertTrue(sourceLine != null);
        Line target;
        if (buf.contains(sourceLine))
            target = sourceLine;
        else
            target = buf.getLine(sourceLine.lineNumber());
        if (target != null)
            gotoSource(editor, buf, target, killList);
    }

    protected void gotoSource(Editor editor, Buffer buf, Line target,
                              boolean killList)
    {
        if (target != null) {
            Editor ed;
            if (editor.getFrame().getEditorCount() > 1) {
                editor.getOtherEditor().makeNext(buf);
                ed = editor.activateInOtherWindow(buf);
            } else {
                ed = editor;
                ed.makeNext(buf);
                ed.activate(buf);
            }
            ed.setLastSearch(search);
            ed.addUndo(SimpleEdit.MOVE);
            ed.unmark();
            ed.update(ed.getDotLine());
            ed.setDot(target, 0);
            Position found = search.find(buf.getMode(), ed.getDot());
            if (found != null) {
                ed.setDot(found);
                ed.markFoundPattern(search);
            } else {
                ed.moveCaretToDotCol();
            }
            ed.update(ed.getDotLine());
            ed.updateDisplay();

            if (killList) {
                if (ed.getFrame().getEditorCount() > 1) {
                    Editor otherEditor = ed.getOtherEditor();
                    if (otherEditor != null)
                        ed.getFrame().closeEditor(otherEditor);
                    kill();
                }
            }
        }
    }

    public final void appendOccurrenceLine(Line sourceLine)
    {
        appendLine(new OccurrenceLine(sourceLine));
    }

    public final void appendOccurrenceLine(String s, int lineNumber)
    {
        appendLine(new OccurrenceLine(s, lineNumber));
    }

    public final void appendStatusLine(String s)
    {
        appendLine(new ListOccurrencesStatusLine(s));
    }

    protected String getOptions()
    {
        FastStringBuffer sb = new FastStringBuffer(search.ignoreCase() ? "ignore case" : "case sensitive");
        if (search.wholeWordsOnly())
            sb.append(", whole words only");
        return sb.toString();
    }

    public final Search getSearch()
    {
        return search;
    }

    public Position getInitialDotPos()
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof OccurrenceLine)
                return new Position(line, 0);
        }
        return new Position(getFirstLine(), 0);
    }

    public Position getInitialDotPos(Line sourceLine, int sourceOffs)
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof OccurrenceLine) {
                if (((OccurrenceLine)line).getSourceLine() == sourceLine) {
                    int index = line.getText().indexOf(':');
                    if (index >= 0)
                        return new Position(line, index + 1 + sourceOffs);
                    else
                        return new Position(line, sourceOffs);
                }
            }
        }
        return getInitialDotPos();
    }

    public String getFileNameForDisplay()
    {
        if (sourceBuffer == null || sourceBuffer.getFile() == null)
            return "";
        else
            return sourceBuffer.getFile().getName();
    }

    public static final void listOccurrences()
    {
        listOccurrences(Editor.currentEditor());
    }

    public static void listOccurrences(Editor editor)
    {
        final Search search = editor.getLastSearch();
        if (search != null) {
            editor.setWaitCursor();
            ListOccurrences buf = getBuffer(editor.getBuffer(), search);
            editor.setDefaultCursor();
            if (buf != null) {
                editor.makeNext(buf);
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null) {
                    buf.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                    otherEditor.makeNext(buf);
                } else
                    buf.setUnsplitOnClose(true);
                Editor ed = editor.activateInOtherWindow(buf);

                ed.setDot(buf.getInitialDotPos());
                ed.moveCaretToDotCol();
                ed.updateDisplay();
            } else
                search.notFound(editor);
        }
    }

    public static void listOccurrencesOfPatternAtDot()
    {
        final Editor editor = Editor.currentEditor();
        final Search search = editor.getSearchAtDot();
        if (search != null) {
            editor.setLastSearch(search);
            editor.setWaitCursor();
            ListOccurrences buf = getBuffer(editor.getBuffer(), search);
            editor.setDefaultCursor();
            if (buf != null) {
                editor.makeNext(buf);
                boolean shrink = false;
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null) {
                    buf.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                    otherEditor.makeNext(buf);
                } else {
                    buf.setUnsplitOnClose(true);
                    shrink = true;
                }
                Editor ed = editor.activateInOtherWindow(buf);
                if (shrink)
                    ed.shrinkWindowIfLargerThanBuffer();
                ed.setDot(buf.getInitialDotPos(editor.getDotLine(),
                                               editor.getDotOffset()));
                ed.moveCaretToDotCol();
                ed.updateDisplay();
            } else
                search.notFound(editor);
        }
    }

    public static void findOccurrenceAtDot()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListOccurrences)
            ((ListOccurrences)buffer).findOccurrenceAtDot(editor, false);
    }

    public static void findOccurrenceAtDotAndKillList()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListOccurrences)
            ((ListOccurrences)buffer).findOccurrenceAtDot(editor, true);
    }

    public static void mouseFindOccurrence()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListOccurrences) {
            AWTEvent e = editor.getDispatcher().getLastEvent();
            if (e instanceof MouseEvent) {
                editor.mouseMoveDotToPoint((MouseEvent)e);
                ((ListOccurrences)buffer).findOccurrenceAtDot(editor, false);
            }
        }
    }
}
