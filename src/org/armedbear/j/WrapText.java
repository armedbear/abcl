/*
 * WrapText.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import javax.swing.undo.CompoundEdit;
import org.armedbear.j.mail.SendMail;

public final class WrapText implements Constants
{
    private final Editor editor;
    private final Buffer buffer;
    private Position dot;
    private Position mark;
    private final int wrapCol;
    private final int tabWidth;
    private final boolean isHtml;

    public WrapText(Editor editor)
    {
        this.editor = editor;
        buffer = editor.getBuffer();
        dot = editor.getDot();
        mark = editor.getMark();
        wrapCol = buffer.getIntegerProperty(Property.WRAP_COL);
        tabWidth = buffer.getTabWidth();
        isHtml = buffer.getModeId() == HTML_MODE;
    }

    public void wrapRegion()
    {
        wrapRegion(new Region(buffer, dot, mark));
    }

    public void wrapParagraphsInRegion()
    {
        final Region r;
        if (dot != null && mark != null) {
            r = new Region(buffer, dot, mark);
        } else {
            Line line = buffer.getFirstLine();
            if (buffer instanceof SendMail) {
                // Skip headers and header separator line.
                while (line != null) {
                    String s = line.trim();
                    line = line.next();
                    if (s.equals(SendMail.getHeaderSeparator()))
                        break;
                }
            }
            if (line == null)
                return;
            r = new Region(buffer, new Position(line, 0), buffer.getEnd());
        }
        Position savedDot = dot.copy();
        boolean seenDot = false;
        CompoundEdit compoundEdit = buffer.beginCompoundEdit();
        editor.moveDotTo(new Position(r.getBeginLine(), 0));
        while (true) {
            while (dot.getLine().isBlank() && dot.getNextLine() != null)
                dot.setLine(dot.getNextLine());
            Position start = dot.copy();
            Position end = findEndOfParagraph(dot);
            if (!seenDot && !savedDot.isBefore(start) && savedDot.isBefore(end)) {
                editor.moveDotTo(savedDot);
                wrapParagraph();
                savedDot = dot.copy();
                seenDot = true;
            } else
                wrapParagraph();
            if (buffer.needsRenumbering())
                buffer.renumber();
            Position pos = findEndOfParagraph(dot);
            if (!pos.getLine().isBefore(r.getEndLine()))
                break;
            if (pos.getLine() == dot.getLine())
                break;
            editor.moveDotTo(pos);
        }
        if (buffer.contains(savedDot.getLine()))
            editor.moveDotTo(savedDot);
        buffer.endCompoundEdit(compoundEdit);
    }

    public void wrapLine()
    {
        // Don't try to wrap header lines in mail composition buffers!
        if (buffer instanceof SendMail)
            if (((SendMail)buffer).isHeaderLine(dot.getLine()))
                return;
        Position begin = new Position(dot.getLine(), 0);
        Position end;
        if (dot.getNextLine() != null)
            end = new Position(dot.getNextLine(), 0);
        else
            end = new Position(dot.getLine(), dot.getLineLength());
        Region r = new Region(buffer, begin, end);
        wrapRegion(r);
    }

    public void wrapParagraph()
    {
        String prefix = getPrefix(dot.getLine());
        final Position begin, end;
        if (prefix != null) {
            int prefixLength = prefix.length();
            begin = findStartOfQuotedText(dot, prefix, prefixLength);
            end = findEndOfQuotedText(dot, prefix, prefixLength);
        } else {
            begin = findStartOfParagraph(dot);
            end = findEndOfParagraph(dot);
        }
        if (begin != null && end != null) {
            Region r = new Region(buffer, begin, end);
            wrapRegion(r, prefix);
        }
    }

    public void unwrapParagraph()
    {
        Position begin = findStartOfParagraph(dot);
        Position end = findEndOfParagraph(dot);
        if (begin != null && end != null) {
            Region r = new Region(buffer, begin, end);
            unwrapRegion(r);
        }
    }

    private void wrapCommentInternal()
    {
        String commentStart = null;
        final Line dotLine = dot.getLine();
        final String trim = dotLine.trim();

        switch (buffer.getModeId()) {
            case JAVA_MODE:
            case JAVASCRIPT_MODE:
            case C_MODE:
            case CPP_MODE:
            case VERILOG_MODE:
                if (trim.startsWith("// "))
                    commentStart = "// ";
                else if (trim.startsWith("* "))
                    commentStart = "* ";
                break;
            case PERL_MODE:
            case PROPERTIES_MODE:
                if (trim.startsWith("# "))
                    commentStart = "# ";
                break;
            default:
                commentStart = buffer.getMode().getCommentStart();
                break;
        }

        if (commentStart != null) {
            int index = dotLine.getText().indexOf(commentStart);
            if (index < 0)
                return;
            String prefix =
                dotLine.getText().substring(0, index + commentStart.length());
            Position begin = findStartOfComment(dot, commentStart);
            Position end = findEndOfComment(dot, commentStart);
            if (begin != null && end != null) {
                try {
                    buffer.lockWrite();
                }
                catch (InterruptedException e) {
                    Log.error(e);
                    return;
                }
                try {
                    processRegion(new Region(buffer, begin, end), prefix, true);
                }
                finally {
                    buffer.unlockWrite();
                }
            }
        }
    }

    private void wrapRegion(Region r)
    {
        wrapRegion(r, null);
    }

    private void wrapRegion(Region r, String prefix)
    {
        try {
            r.getBuffer().lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            processRegion(r, prefix, true);
        }
        finally {
            r.getBuffer().unlockWrite();
        }
    }

    private void unwrapRegion(Region r)
    {
        try {
            r.getBuffer().lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            processRegion(r, null, false);
        }
        finally {
            r.getBuffer().unlockWrite();
        }
    }

    private void processRegion(Region r, String prefix, boolean wrap)
    {
        // Remember original contents of region.
        final String before = r.toString();

        if (before.length() == 0)
            return;

        int offsetBefore = buffer.getAbsoluteOffset(dot);

        int originalModificationCount = buffer.getModCount();

        // Save undo information before detabbing region (which may also move
        // dot).
        CompoundEdit compoundEdit = new CompoundEdit();
        compoundEdit.addEdit(new UndoMove(editor));
        editor.setMark(null);

        // Detab region (may move dot).
        detab(r);

        int savedOffset = buffer.getAbsoluteOffset(dot);

        final String detabbed = r.toString();

        // Working copy.
        String s = detabbed;

        // Remove trailing '\n'.
        if (s.charAt(s.length() - 1) == '\n')
            s = s.substring(0, s.length() - 1);

        FastStringBuffer sb = new FastStringBuffer();

        if (prefix != null) {
            prefix = Utilities.detab(prefix, tabWidth);
        } else {
            // If not specified, prefix is whitespace at start of first line.
            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                if (c == '\t') {
                    // String should be detabbed at this point.
                    Log.error("tab found unexpectedly at offset " + i);
                    Debug.assertTrue(false);
                }
                if (c == ' ')
                    sb.append(c);
                else
                    break;
            }
            prefix = sb.toString();
        }

        final int prefixLength = prefix.length();

        if (prefixLength > 0)
            s = s.substring(prefixLength);

        if (!Utilities.isWhitespace(prefix)) {
            // Replace prefix with spaces.
            sb.setLength(0);
            int index;
            int begin = 0;
            while ((index = s.indexOf("\n" + prefix, begin)) >= 0) {
                sb.append(s.substring(begin, index));
                sb.append('\n');
                sb.append(Utilities.spaces(prefixLength));
                begin = index + prefixLength + 1;
            }
            sb.append(s.substring(begin));
            s = sb.toString();
        }

        sb.setLength(0);
        sb.append(prefix);
        final int start = buffer.getAbsoluteOffset(r.getBegin());
        char lastChar = '\0';
        int numSkipped = 0;
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (c == '\n') {
                if (lastChar == '-') {
                    // Line ends with '-'. We want to remove the '\n' and skip
                    // leading spaces on the next line. We can accomplish this
                    // by pretending the '-' is a space...
                    lastChar = ' ';
                }
                c = ' ';
            }
            if (c == ' ') {
                if (lastChar == ' ') {
                    if (start + prefixLength + i <= savedOffset)
                        ++numSkipped;
                } else {
                    sb.append(c);
                    lastChar = c;
                }
            } else {
                // Not a space.
                sb.append(c);
                lastChar = c;
            }
        }

        savedOffset -= numSkipped;

        final String unwrapped = sb.toString();

        String toBeInserted = null;

        if (wrap) {
            if (getCol(prefix, prefixLength) >= wrapCol) {
                editor.status("Can't wrap (indentation extends beyond wrap column)");
                return;
            }

            sb.setLength(0);
            String remaining = unwrapped;
            int where = start;
            int startCol = 0;
            int adjust = 0;

            // All the tabs have been replaced with spaces, so we can just use
            // the length of the remaining string.
            while (remaining.length() > wrapCol - startCol) {
                int breakOffset = findBreak(remaining, wrapCol - startCol);
                sb.append(remaining.substring(0, breakOffset));
                where += breakOffset;
                sb.append('\n');
                if (where <= savedOffset)
                    ++adjust;
                remaining = remaining.substring(breakOffset);
                if (remaining.length() > 0 && remaining.charAt(0) == ' ') {
                    remaining = remaining.substring(1);
                    if (where <= savedOffset)
                        --adjust;
                    ++where;
                }
                if (prefix.length() > 0) {
                    sb.append(prefix);
                    if (where <= savedOffset)
                        adjust += prefixLength;
                    startCol = getCol(prefix, prefixLength);
                }
            }
            sb.append(remaining);
            toBeInserted = sb.toString();
            savedOffset += adjust;
        } else
            toBeInserted = unwrapped;

        toBeInserted += "\n";

        if (toBeInserted.equals(detabbed)) {
            // No change. Restore status quo.
            dot.moveTo(r.getBegin());
            r.delete();
            buffer.insertString(dot, before);
            Position pos = buffer.getPosition(offsetBefore);
            Debug.assertTrue(pos != null);
            if (pos != null)
                editor.getDot().moveTo(pos);
            editor.getDisplay().setShift(0);

            // Buffer has not been modified.
            buffer.setModCount(originalModificationCount);
            return;
        }

        // Commit undo information.
        buffer.addEdit(compoundEdit);

        dot.moveTo(r.getBegin());
        editor.addUndoDeleteRegion(r);
        r.delete();
        editor.addUndo(SimpleEdit.INSERT_STRING);

        // This leaves dot at the end of the inserted string.
        buffer.insertString(dot, toBeInserted);

        // If we don't need to entab, we no longer need r. If we do need to
        // entab, the bounds of the region may have changed, so we reconstruct
        // it here.
        r = buffer.getUseTabs() ? new Region(buffer, r.getBegin(), dot) : null;

        // Move dot where it needs to go before entabbing, so the entabbing
        // code can update it correctly.
        Position pos = buffer.getPosition(savedOffset);
        Debug.assertTrue(pos != null);
        if (pos != null)
            editor.moveDotTo(pos);

        if (r != null)
            entab(r);

        editor.getDisplay().setShift(0);
        editor.moveCaretToDotCol();
        buffer.endCompoundEdit(compoundEdit);
    }

    private int getCol(String s, int offset)
    {
        if (offset > s.length())
            offset = s.length();
        int col = 0;
        for (int i = 0; i < offset; i++) {
            if (s.charAt(i) == '\t')
                col += tabWidth - col % tabWidth;
            else
                ++col;
        }
        return col;
    }

    private int findBreak(String s, int maxLength)
    {
        int breakOffset = 0;
        final int limit = Math.min(s.length(), maxLength);
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (c == ' ')
                breakOffset = i;
            else if (c == '-') {
                if (i < limit-1)
                    breakOffset = i+1;
            } else if (isHtml && c == '<') {
                // Avoid end tags on the first pass.
                if (i < s.length()-1 && (c = s.charAt(i+1)) != '/') {
                    // Start tag.
                    breakOffset = i;
                    // Avoid breaks within <a> tags if possible.
                    if (c == 'a' || c == 'A') {
                        if (s.regionMatches(true, i, "<a ", 0, 3)) {
                            // It's an <a> tag. Look for end tag.
                            int index = s.toLowerCase().indexOf("</a>", i+3);
                            if (index >= 0 && index+4 < limit) {
                                breakOffset = index+4;
                                i = breakOffset;
                            } else {
                                // Don't break at the space after "<a".
                                i += 2;
                            }
                        }
                    }
                }
            }
        }
        if (breakOffset == 0 && isHtml) {
            // No break found. Now we'll settle for an end tag.
            for (int i = 0; i < limit; i++) {
                if (s.charAt(i) == '<')
                    breakOffset = i;
            }
        }
        if (breakOffset == 0) // No tabs or spaces fouund.
            breakOffset = maxLength;
        return breakOffset;
    }

    private static Position findStartOfParagraph(Position startingPoint)
    {
        Position pos = new Position(startingPoint);
        while (true) {
            Line previousLine = pos.getPreviousLine();
            if (previousLine == null || previousLine.isBlank())
                break;
            String s = previousLine.trim();
            if (s.equals(SendMail.getHeaderSeparator()))
                break;
            if (s.endsWith(">"))
                break;
            pos.setLine(previousLine);
            s = s.toLowerCase();
            if (s.startsWith("<p>") || s.startsWith("<br>"))
                break;
        }
        pos.setOffset(0);
        return pos;
    }

    private static Position findEndOfParagraph(Position startingPoint)
    {
        Line line = startingPoint.getLine();
        while (true) {
            if (line.next() == null)
                return new Position(line, line.length());
            line = line.next();
            if (line.isBlank())
                return new Position(line, 0);
            String s = line.trim().toLowerCase();
            // Honor HTML breaks.
            if (s.startsWith("<p>") ||
                s.startsWith("</p>") ||
                s.startsWith("<br>") ||
                s.startsWith("<li>") ||
                s.startsWith("</li>") ||
                s.startsWith("<dl>") ||
                s.startsWith("</dl>") ||
                s.startsWith("</body>") ||
                s.startsWith("<pre>")) {
                return new Position(line, 0);
            }
            if (s.endsWith("<p>") ||
                s.endsWith("</p>") ||
                s.endsWith("<br>") ||
                s.endsWith("</li>")) {
                if (line.next() != null)
                    return new Position(line.next(), 0);
                return new Position(line, line.length());
            }
        }
    }

    private static Position findStartOfComment(Position startingPoint, String commentStart)
    {
        Line beginLine = null;
        for (Line line = startingPoint.getLine(); line != null; line = line.previous()) {
            if (!line.trim().startsWith(commentStart))
                break;
            int index = line.getText().indexOf(commentStart);
            String remaining = line.substring(index + commentStart.length()).trim();
            if (remaining.endsWith("</pre>"))
                break;
            if (remaining.endsWith("<p>"))
                break;
            if (remaining.startsWith("<p>")) {
                beginLine = line;
                break;
            }
            beginLine = line;
        }
        if (beginLine != null)
            return new Position(beginLine, 0);
        return null;
    }

    private static Position findEndOfComment(Position startingPoint, String commentStart)
    {
        Line endLine = null;
        for (Line line = startingPoint.getLine(); line != null; line = line.next()) {
            if (!line.trim().startsWith(commentStart))
                break;
            int index = line.getText().indexOf(commentStart);
            String remaining = line.substring(index + commentStart.length()).trim();
            if (remaining.startsWith("<p>"))
                break;
            if (remaining.startsWith("<pre>"))
                break;
            endLine = line;
        }
        if (endLine == null)
            return null;
        if (endLine.next() != null)
            return new Position(endLine.next(), 0);
        return new Position(endLine, endLine.length());
    }

    private static final RE prefixRE = new UncheckedRE("^>[> ]*");

    private static String getPrefix(Line line)
    {
        REMatch match = prefixRE.getMatch(line.getText());
        return match != null ? match.toString() : null;
    }

    private static Position findStartOfQuotedText(Position pos, String prefix,
        int prefixLength)
    {
        Line start = pos.getLine();
        for (Line line = start.previous(); line != null; line = line.previous()) {
            if (!prefix.equals(getPrefix(line)))
                break;
            if (line.substring(prefixLength).trim().length() == 0)
                break; // Blank line (except for prefix string).
            start = line;
        }
        return new Position(start, 0);
    }

    private static Position findEndOfQuotedText(Position pos, String prefix,
        int prefixLength)
    {
        Line end = pos.getLine();
        for (Line line = end.next(); line != null; line = line.next()) {
            if (!prefix.equals(getPrefix(line)))
                break;
            if (line.substring(prefixLength).trim().length() == 0)
                break; // Blank line (except for prefix string).
            end = line;
        }
        if (end.next() != null)
            return new Position(end.next(), 0);
        return new Position(end, end.length());
    }

    private void detab(Region r)
    {
        Debug.assertTrue(r.getBeginOffset() == 0);
        Debug.assertTrue(r.getEndOffset() == 0 || r.getEndOffset() == r.getEndLine().length());
        Debug.assertTrue(buffer == r.getBuffer());
        int dotCol = buffer.getCol(dot);
        Line line = r.getBeginLine();
        while (line != null && line != r.getEndLine()) {
            line.setText(Utilities.detab(line.getText(), tabWidth));
            line = line.next();
        }
        if (line == r.getEndLine() && r.getEndOffset() == r.getEndLine().length())
            line.setText(Utilities.detab(line.getText(), tabWidth));
        // Don't assume dot.line has been detabbed (dot might not be in the
        // detabbed region).
        dot.moveToCol(dotCol, tabWidth);
    }

    private void entab(Region r)
    {
        Debug.assertTrue(r.getBeginOffset() == 0);
        Debug.assertTrue(r.getEndOffset() == 0 ||
            r.getEndOffset() == r.getEndLine().length());
        Debug.assertTrue(buffer == r.getBuffer());
        int dotCol = buffer.getCol(dot);
        Line line = r.getBeginLine();
        while (line != null && line != r.getEndLine()) {
            line.setText(Utilities.entab(line.getText(), tabWidth));
            line = line.next();
        }
        if (line == r.getEndLine() && r.getEndOffset() == r.getEndLine().length())
            line.setText(Utilities.entab(line.getText(), tabWidth));
        dot.moveToCol(dotCol, tabWidth);
    }

    public static void wrapComment()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        new WrapText(editor).wrapCommentInternal();
    }

    public static void toggleWrap()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        boolean b = !buffer.getBooleanProperty(Property.WRAP);
        buffer.setProperty(Property.WRAP, b);
        buffer.saveProperties();
    }
}
