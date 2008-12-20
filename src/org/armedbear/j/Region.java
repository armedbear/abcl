/*
 * Region.java
 *
 * Copyright (C) 1998-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j;

import java.util.List;

public final class Region implements Constants
{
    private final Buffer buffer;
    private final Position begin;
    private final Line beginLine;
    private final int beginOffset;
    private Position end;
    private Line endLine;
    private int endOffset;
    private boolean isColumnRegion;
    private int beginCol = -1;
    private int endCol = -1;

    // It is not necessary that pos1 come before pos2; we figure that out on
    // the fly.
    public Region(Buffer buffer, Position pos1, Position pos2)
    {
        this.buffer = buffer;
        if (buffer.needsRenumbering())
            buffer.renumber();
        if (pos1.isBefore(pos2)) {
            begin = new Position(pos1);
            end = new Position(pos2);
        } else {
            begin = new Position(pos2);
            end = new Position(pos1);
        }
        beginLine = begin.getLine();
        beginOffset = begin.getOffset();
        endLine = end.getLine();
        endOffset = end.getOffset();
    }

    public Region(Editor editor)
    {
        this(editor.getBuffer(), editor.getMark(), editor.getDot());
        if (editor.isColumnSelection()) {
            isColumnRegion = true;
            if (begin.equals(editor.getMark())) {
                beginCol = buffer.getCol(begin);
                endCol = editor.getDisplay().getAbsoluteCaretCol();
            } else {
                beginCol = editor.getDisplay().getAbsoluteCaretCol();
                endCol = buffer.getCol(end);
            }
        }
    }

    public final Buffer getBuffer()
    {
        return buffer;
    }

    public final Position getBegin()
    {
        return begin;
    }

    public final Line getBeginLine()
    {
        return beginLine;
    }

    public final int getBeginLineNumber()
    {
        return beginLine.lineNumber();
    }

    public final int getBeginOffset()
    {
        return beginOffset;
    }

    public final int getBeginCol()
    {
        if (beginCol < 0)
            beginCol = buffer.getCol(begin);
        return beginCol;
    }

    public final Position getEnd()
    {
        return end;
    }

    public final void setEnd(Position pos)
    {
        end = pos;
        endLine = end.getLine();
        endOffset = end.getOffset();
        endCol = -1;
    }

    public final Line getEndLine()
    {
        return endLine;
    }

    public final int getEndLineNumber()
    {
        return endLine.lineNumber();
    }

    public final int getEndOffset()
    {
        return endOffset;
    }

    public final void setEndOffset(int offset)
    {
        end.setOffset(offset);
        endOffset = offset;
    }

    public final int getEndCol()
    {
        if (endCol < 0)
            endCol = buffer.getCol(end);
        return endCol;
    }

    public final boolean isColumnRegion()
    {
        return isColumnRegion;
    }

    public final boolean isLineRegion()
    {
        return beginOffset == 0 && endOffset == 0;
    }

    public String toString()
    {
        if (beginLine == endLine)
            return beginLine.substring(beginOffset, endOffset);

        FastStringBuffer sb = new FastStringBuffer();
        if (isColumnRegion) {
            for (Line line = beginLine; line != null; line = line.next()) {
                sb.append(getTextInRegion(line));
                sb.append('\n');
                if (line == getEndLine())
                    break;
            }
        } else {
            sb.append(beginLine.substring(beginOffset));
            sb.append('\n');
            Line line = beginLine.next();
            while (line != endLine && line != null) {
                sb.append(line.getText());
                sb.append('\n');
                line = line.next();
            }
            if (line == endLine)
                sb.append(line.substring(0, endOffset));
        }
        return sb.toString();
    }

    private String getTextInRegion(Line line)
    {
        String text = Utilities.detab(line.getText(), buffer.getTabWidth());
        if (text.length() > getEndCol())
            return text.substring(getBeginCol(), getEndCol());
        else if (text.length() > getBeginCol())
            return text.substring(getBeginCol());
        else
            return "";
    }

    public boolean adjustMarker(Position pos)
    {
        if (pos == null)
            return false;
        final Line line = pos.getLine();
        final int offset = pos.getOffset();
        if (line == null)
            return false;
        if (line == beginLine) {
            if (line == endLine && offset > endOffset) {
                pos.setOffset(offset - (endOffset - beginOffset));
                return true;
            }
            if (offset > beginOffset) {
                pos.setOffset(beginOffset);
                return true;
            }
            return false;
        }
        if (line == endLine) {
            if (offset <= endOffset)
                pos.moveTo(begin);
            else
                pos.moveTo(beginLine, beginOffset + offset - endOffset);
            return true;
        }
        if (pos.lineNumber() > begin.lineNumber() && pos.lineNumber() < end.lineNumber()) {
            pos.moveTo(begin);
            return true;
        }
        return false;
    }

    private void adjustMarkers()
    {
        if (Editor.getEditorCount() > 1) {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed == Editor.currentEditor())
                    continue;
                if (ed.getBuffer() == buffer) {
                    // Buffer is displayed in another frame.
                    if (adjustMarker(ed.getDot()))
                        ed.moveCaretToDotCol();
                    if (adjustMarker(ed.getMark()))
                        ed.setMark(null); // Not taking any chances.
                    Position top = new Position(ed.getTopLine(), 0);
                    if (adjustMarker(top)) {
                        ed.setTopLine(top.getLine());
                        ed.setUpdateFlag(REPAINT);
                    }
                } else {
                    // Not presently displayed, but possibly in a stored view...
                    View view = (View) ed.views.get(buffer);
                    if (view != null) {
                        adjustMarker(view.dot);
                        if (adjustMarker(view.mark))
                            view.mark = null;
                        if (view.topLine != null) {
                            Position top = new Position(view.topLine, 0);
                            if (adjustMarker(top))
                                view.topLine = top.getLine();
                        }
                    }
                }
            }
        }
        List markers = Marker.getAllMarkers();
        for (int i = markers.size(); i-- > 0;) {
            Marker m = (Marker) markers.get(i);
            if (m != null && m.getBuffer() == buffer)
                adjustMarker(m.getPosition());
        }
    }

    public void delete()
    {
        adjustMarkers();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            final String head = beginLine.substring(0, beginOffset);
            final String tail = endLine.substring(endOffset);
            if (beginLine == endLine) {
                beginLine.setText(head.concat(tail));
            } else {
                FastStringBuffer sb = new FastStringBuffer();
                for (Line line = beginLine; line != null; line = line.next()) {
                    // Skip new lines since we just want the original text.
                    if (line != endLine && !line.isNew()) {
                        if (sb.length() > 0)
                            sb.append('\n');
                        String s = line.getOriginalText();
                        sb.append(s != null ? s : line.getText());
                    } else if (line == endLine) {
                        if (!line.isNew()) {
                            if (sb.length() > 0)
                                sb.append('\n');
                            String s = line.getOriginalText();
                            sb.append(s != null ? s : line.getText());
                        }
                        break;
                    }
                }
                beginLine.setText(head.concat(tail));
                beginLine.setOriginalText(sb.toString());
                // Make adjustments so revertLines() will work correctly...
                if (beginLine.isNew()) {
                    if (beginLine.getText().equals(beginLine.getOriginalText())) {
                        beginLine.setNew(false);
                        beginLine.setOriginalText(null);
                    }
                }
                Line nextLine = endLine.next();
                beginLine.setNext(nextLine);
                if (nextLine != null)
                    nextLine.setPrevious(beginLine);
                buffer.needsRenumbering = true;
            }
            buffer.modified();
        }
        finally {
            buffer.unlockWrite();
        }
    }

    // used by Buffer.enforceOutputLimit()
    // deletes whole lines at beginning of buffer
    public void deleteLines()
    {
        adjustMarkers();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            beginLine.setText(endLine.getText());
            beginLine.setOriginalText(null);
            Line nextLine = endLine.next();
            Line line = beginLine;
            while (line != endLine) {
                Line next = line.next();
                line.setPrevious(null);
                line.setNext(null);
                line = next;
            }
            endLine.setPrevious(null);
            endLine.setNext(null);
            beginLine.setNext(nextLine);
            if (nextLine != null)
                nextLine.setPrevious(beginLine);
            buffer.needsRenumbering = true;
            buffer.modified();
        }
        finally {
            buffer.unlockWrite();
        }
    }
}
