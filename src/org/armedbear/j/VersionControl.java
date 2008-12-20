/*
 * VersionControl.java
 *
 * Copyright (C) 2005 Peter Graves
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

public class VersionControl implements Constants
{
  protected static void diffCompleted(Editor editor, Buffer parentBuffer,
                                      String title, String output, int vcType)
  {
    if (output.length() == 0)
      {
        parentBuffer.setBusy(false);
        MessageDialog.showMessageDialog(editor,
                                        "No changes since latest version",
                                        parentBuffer.getFile().getName());
      }
    else
      {
        DiffOutputBuffer buf =
          new DiffOutputBuffer(parentBuffer, output, vcType);
        buf.setTitle(title);
        editor.makeNext(buf);
        editor.activateInOtherWindow(buf);
        parentBuffer.setBusy(false);
        for (EditorIterator it = new EditorIterator(); it.hasNext();)
          {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == parentBuffer)
              ed.setDefaultCursor();
          }
      }
  }

  protected static void processCompleted(Buffer buffer, String output)
  {
    buffer.setText(output);
    buffer.setBusy(false);
    for (EditorIterator it = new EditorIterator(); it.hasNext();)
      {
        Editor ed = it.nextEditor();
        if (ed.getBuffer() == buffer)
          {
            ed.setDot(buffer.getFirstLine(), 0);
            ed.setTopLine(buffer.getFirstLine());
            ed.setUpdateFlag(REPAINT);
            ed.updateDisplay();
          }
      }
  }
}
