/*
 * Marker.java
 *
 * Copyright (C) 1998-2007 Peter Graves <peter@armedbear.org>
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

import java.util.ArrayList;
import java.util.List;

public final class Marker implements Constants
{
  private Buffer buffer;
  private Position pos;
  private final File file;
  private int lineNumber;
  private int offset;

  public Marker(Buffer buffer, Position pos)
  {
    this.buffer = buffer;
    this.pos = new Position(pos);
    file = buffer.getFile();
    lineNumber = pos.lineNumber();
    offset = pos.getOffset();
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  // Returns an alias, not a copy.
  public Position getPosition()
  {
    return pos;
  }

  public void setPosition(Position pos)
  {
    this.pos = new Position(pos);
  }

  public Line getLine()
  {
    return pos != null ? pos.getLine() : null;
  }

  public void invalidate()
  {
    if (pos != null)
      {
        lineNumber = pos.lineNumber();
        offset = pos.getOffset();
      }
    pos = null;
    buffer = null;
  }

  public void gotoMarker(Editor editor)
  {
    if (buffer == editor.getBuffer()
        || (file != null && file.equals(editor.getBuffer().getFile())))
      {
        // Marker is in current buffer.
        editor.addUndo(SimpleEdit.MOVE);
        editor.unmark();
        editor.updateDotLine();
        if (pos != null && editor.getBuffer().contains(pos.getLine()))
          {
            editor.getDot().moveTo(pos);
          }
        else
          {
            editor.gotoline(lineNumber);
            editor.getDot().setOffset(offset);
          }
        if (editor.getDotOffset() > editor.getDotLine().length())
          editor.getDot().setOffset(editor.getDotLine().length());
        editor.moveCaretToDotCol();
        editor.updateDotLine();
        editor.setUpdateFlag(REFRAME);
      }
    else
      {
        // Marker is not in current buffer.
        BufferList buffer_list = Editor.getBufferList();
        Buffer buf = null;
        if (file != null)
          buf = buffer_list.findBuffer(file);
        else if (buffer_list.contains(buffer))
          buf = buffer;
        if (buf != null)
          {
            editor.makeNext(buf);
            editor.activate(buf);
            editor.addUndo(SimpleEdit.MOVE);
            editor.updateDotLine();
            if (pos != null && buf.contains(pos.getLine()))
              {
                editor.getDot().moveTo(pos);
              }
            else
              {
                editor.gotoline(lineNumber);
                editor.getDot().setOffset(offset);
              }
            if (editor.getDotOffset() > editor.getDotLine().length())
              editor.getDot().setOffset(editor.getDotLine().length());
            editor.moveCaretToDotCol();
            editor.updateDotLine();
          }
        else if (file != null)
          {
            buf = Buffer.createBuffer(file);
            editor.makeNext(buf);
            editor.activate(buf);
            editor.gotoline(lineNumber);
            editor.getDot().setOffset(offset);
            if (editor.getDotOffset() > editor.getDotLine().length())
              editor.getDot().setOffset(editor.getDotLine().length());
            editor.moveCaretToDotCol();
          }
        else
          return;
      }
    pos = new Position(editor.getDot());
    buffer = editor.getBuffer();
  }

  public static void selectToMarker()
  {
    selectToMarker(InputDialog.showInputDialog(Editor.currentEditor(),
                                               "Marker:",
                                               "Select To Marker"));
  }

  public static void selectToTemporaryMarker()
  {
    selectToMarker("10");
  }

  public static void selectToMarker(String s)
  {
    if (s == null)
      return;
    s = s.trim();
    if (s.length() == 0)
      return;
    final Editor editor = Editor.currentEditor();
    Marker m = null;
    try
      {
        final int index = Integer.parseInt(s);
        final Marker[] bookmarks = Editor.getBookmarks();
        if (index >= 0 && index < bookmarks.length)
          m = bookmarks[index];
      }
    catch (NumberFormatException e) {}
    if (m == null)
      {
        MessageDialog.showMessageDialog(editor, "No such marker",
                                        "Select To Marker");
        return;
      }
    m.selectToMarker(editor);
  }

  private void selectToMarker(Editor editor)
  {
    if (buffer == editor.getBuffer()
        || (file != null && file.equals(editor.getBuffer().getFile())))
      {
        // Marker is in current buffer.
        editor.addUndo(SimpleEdit.MOVE);
        editor.unmark();
        editor.setMarkAtDot();
        editor.updateDotLine();
        if (pos != null && editor.getBuffer().contains(pos.getLine()))
          {
            editor.getDot().moveTo(pos);
          }
        else
          {
            editor.gotoline(lineNumber);
            editor.getDot().setOffset(offset);
          }
        if (editor.getDotOffset() > editor.getDotLine().length())
          editor.getDot().setOffset(editor.getDotLine().length());
        editor.moveCaretToDotCol();
        editor.updateDotLine();
        editor.setUpdateFlag(REFRAME | REPAINT);
      }
    else
      {
        // Marker is not in current buffer.
        MessageDialog.showMessageDialog(editor,
                                        "Marker is not in this buffer.",
                                        "Select To Marker");
      }
  }

  public boolean equals(Object object)
  {
    if (this == object)
      return true;
    if (object instanceof Marker)
      {
        Marker m = (Marker) object;
        if (buffer != null && buffer == m.buffer)
          if (pos != null && pos.equals(m.pos))
            return true;
        if (file != null && file.equals(m.file))
          if (lineNumber == m.lineNumber && offset == m.offset)
            return true;
      }
    return false;
  }

  public static void invalidateAllMarkers()
  {
    List markers = getAllMarkers();
    for (int i = markers.size(); i-- > 0;)
      {
        Marker m = (Marker) markers.get(i);
        if (m != null)
          m.invalidate();
      }
  }

  public static void invalidateMarkers(Buffer buf)
  {
    List markers = getAllMarkers();
    for (int i = markers.size(); i-- > 0;) {
      Marker m = (Marker) markers.get(i);
      if (m != null && m.getBuffer() == buf)
        m.invalidate();
    }
  }

  public static List getAllMarkers()
  {
    Marker[] bookmarks = Editor.getBookmarks();
    List positionStack = Editor.getPositionStack();
    ArrayList list = new ArrayList(bookmarks.length + positionStack.size());
    for (int i = bookmarks.length; i-- > 0;) {
      Marker m = bookmarks[i];
      if (m != null)
        list.add(m);
    }
    for (int i = positionStack.size(); i-- > 0;)
      list.add(positionStack.get(i));
    return list;
  }
}
