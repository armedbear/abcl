/*
 * VerticalScrollBarListener.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import javax.swing.JScrollBar;

public final class VerticalScrollBarListener implements AdjustmentListener
{
    private final Editor editor;
    private final JScrollBar scrollBar;

    public VerticalScrollBarListener(Editor editor, JScrollBar scrollBar)
    {
        this.editor = editor;
        this.scrollBar = scrollBar;
    }

    public void adjustmentValueChanged(AdjustmentEvent e)
    {
        if (editor.inScrollBarUpdate)
            return;
        final Display display = editor.getDisplay();
        if (display.getTopLine() == null) {
            // Image buffer.
            int value = e.getValue();
            if (value != display.getPixelsAboveTopLine()) {
                display.setPixelsAboveTopLine(value);
                display.repaint();
            }
            return;
        }
        Line topLine = display.getTopLine();
        int oldValue = editor.getBuffer().getY(topLine) + display.getPixelsAboveTopLine();
        int newValue = e.getValue();
        int change = newValue - oldValue;
        final int charHeight = Display.getCharHeight();
        if (Math.abs(change) < charHeight) {
            if (change > 0 && newValue + scrollBar.getVisibleAmount() == scrollBar.getMaximum())
                change = charHeight;
            else
                return; // No visible change.
        }
        if (topLine instanceof ImageLine) {
            int value = e.getValue();
            int y = editor.getBuffer().getY(topLine);
            if (value > y && value < y + topLine.getHeight()) {
                display.setPixelsAboveTopLine(value - y);
                display.repaint();
                editor.updateScrollBars();
                return;
            }
            // Fall through...
        }
        // Find the right top line.
        Line line = editor.getBuffer().getFirstLine();
        int y = 0;
        while (line != null && y + line.getHeight() <= newValue) {
            y += line.getHeight();
            line = line.nextVisible();
        }
        if (line == display.getTopLine())
            return; // No change.
        if (line != null) {
            display.setTopLine(line);
            if (line instanceof ImageLine)
                display.setPixelsAboveTopLine(newValue - y);
            else if (newValue > y) {
                line = line.nextVisible();
                if (line != null)
                    display.setTopLine(line);
            }
            editor.maybeScrollCaret();
            display.repaint();
        }
    }
}
