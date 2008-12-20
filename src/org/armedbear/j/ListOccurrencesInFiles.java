/*
 * ListOccurrencesInFiles.java
 *
 * Copyright (C) 2000-2004 Peter Graves
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

public final class ListOccurrencesInFiles extends ListOccurrences
{
    private Position lastDotPos;

    // Find in files.
    public ListOccurrencesInFiles(Search search)
    {
        super(search);
        if (search instanceof FindInFiles) {
            appendLine("Files: " + ((FindInFiles)search).getFiles());
            if (search.wholeWordsOnly())
                appendLine("Mode: " + ((FindInFiles)search).getMode().getDisplayName());
            renumber();
        }
        title = "\"" + search.getPattern() + "\"";
        setLoaded(true);
    }

    private final void setLastDotPos(Position pos)
    {
        lastDotPos = pos.copy();
    }

    public final Position getLastDotPos()
    {
        return lastDotPos != null ? lastDotPos : getInitialDotPos();
    }

    public final File getCurrentDirectory()
    {
        return Directories.getUserHomeDirectory();
    }

    public void findOccurrenceAtDot(Editor editor, boolean killList)
    {
        Position pos = editor.getDotCopy();
        if (pos == null)
            return;
        Line sourceLine = null;
        int sourceLineNumber = 0;
        Buffer buf = null;
        String canonicalPath = null;
        setLastDotPos(pos);
        final Line line = pos.getLine();
        if ((line instanceof OccurrenceLine)) {
            sourceLine = ((OccurrenceLine)line).getSourceLine();
            Line ln = line;
            if (!ln.getText().startsWith("File: "))
                sourceLineNumber = Utilities.parseInt(ln.getText()) - 1;
            while (ln != null) {
                if (ln.getText().startsWith("File: ")) {
                    canonicalPath = ln.getText().substring(6);
                    break;
                }
                ln = ln.previous();
            }
        } else if (line instanceof FileLine)
            canonicalPath = ((FileLine)line).getCanonicalPath();
        if (buf == null && canonicalPath != null)
            buf = Editor.getBuffer(File.getInstance(canonicalPath));
        if (buf != null) {
            if (!buf.isLoaded()) {
                if (!buf.initialized())
                    buf.initialize();
                buf.load();
                if (!buf.isLoaded()) {
                    editor.status("Unable to load buffer");
                    return;
                }
            }
            if (line instanceof FileLine) {
                // Mark file visited.
                ((FileLine)line).markVisited();
                // Advance dot to next line.
                Position dot = editor.getDot();
                if (dot.equals(pos) && dot.getNextLine() != null) {
                    dot.moveTo(dot.getNextLine(), 0);
                    editor.moveCaretToDotCol();
                }
            }
            Line target;
            if (sourceLine != null) {
                if (buf.contains(sourceLine))
                    target = sourceLine;
                else
                    target = buf.getLine(sourceLine.lineNumber());
            } else
                target = buf.getLine(sourceLineNumber);
            gotoSource(editor, buf, target, killList);
        }
    }

    public void findNextOccurrence(Editor editor)
    {
        Line line;
        for (line = editor.getDotLine().next(); line != null; line = line.next()) {
            if (line instanceof OccurrenceLine)
                break;
        }
        if (line instanceof OccurrenceLine)
            findOccurrence(editor, line);
    }

    public void findPreviousOccurrence(Editor editor)
    {
        Line line;
        for (line = editor.getDotLine().previous(); line != null; line = line.previous()) {
            if (line instanceof OccurrenceLine)
                break;
        }
        if (line instanceof OccurrenceLine)
            findOccurrence(editor, line);
    }

    // Move dot to line (in all editors) and call findOccurrenceAtDot().
    private void findOccurrence(Editor editor, Line line)
    {
        if (line instanceof OccurrenceLine) {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.moveDotTo(line, 0);
                    ed.updateDisplay();
                }
            }
            findOccurrenceAtDot(editor, false);
        }
    }

    public final void appendFileLine(File file, boolean listEachOccurrence)
    {
        appendLine(new FileLine(file, listEachOccurrence));
    }

    protected String getOptions()
    {
        String s = super.getOptions();
        if (search instanceof FindInFiles)
            if (((FindInFiles)search).getIncludeSubdirs())
                s = s.concat(", include subdirectories");
        return s;
    }

    public Position getInitialDotPos()
    {
        final boolean listEachOccurrence;
        if (search instanceof FindInFiles)
            listEachOccurrence = ((FindInFiles)search).getListEachOccurrence();
        else
            listEachOccurrence = true;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof OccurrenceLine)
                return new Position(line, 0);
            if (!listEachOccurrence && line instanceof FileLine)
                return new Position(line, 0);
        }
        return new Position(getFirstLine(), 0);
    }

    public void follow(File sourceFile, Line sourceLine)
    {
        Line line = findLineForOccurrence(sourceFile, sourceLine);
        if (line == null)
            return;
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.moveDotTo(line, 0);
                ed.updateDisplay();
            }
        }
    }

    private Line findLineForOccurrence(File sourceFile, Line sourceLine)
    {
        if (sourceFile == null)
            return null;
        if (sourceLine == null)
            return null;
        final String cp = sourceFile.canonicalPath();
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof FileLine) {
                if (((FileLine)line).getCanonicalPath().equals(cp))
                    break;
            }
        }
        if (line == null)
            return null;
        Debug.assertTrue(line instanceof FileLine);
        Debug.assertTrue(((FileLine)line).getCanonicalPath().equals(cp));
        int target = sourceLine.lineNumber() + 1;
        Line best = null;
        for (line = line.next(); line instanceof OccurrenceLine; line = line.next()) {
            int candidate = ((OccurrenceLine)line).getSourceLineNumber();
            if (candidate == target)
                return line;
            if (best != null && candidate > target)
                return best;
            best = line;
        }
        return best;
    }

    public String getFileNameForDisplay()
    {
        return "";
    }
}
