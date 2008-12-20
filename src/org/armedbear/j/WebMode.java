/*
 * WebMode.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.awt.event.KeyEvent;
import java.util.StringTokenizer;

public final class WebMode extends AbstractMode implements Constants, Mode
{
    private static final WebMode mode = new WebMode();

    private WebMode()
    {
        super(WEB_MODE, WEB_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
        setProperty(Property.P4_AUTO_EDIT, false);
    }

    public static final WebMode getMode()
    {
        return mode;
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(VK_MOUSE_1, 0, "mouseFollowLink");
        km.mapKey(KeyEvent.VK_ENTER, 0, "followLink");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "followLink");
        km.mapKey('s', "viewSource");
        km.mapKey(KeyEvent.VK_BACK_SPACE, 0, "webBack");
        km.mapKey(KeyEvent.VK_B, 0, "webBack");
        km.mapKey(KeyEvent.VK_F, 0, "webForward");
        km.mapKey(KeyEvent.VK_R, 0, "webReload");
    }

    public void populateMenu(Editor editor, Menu menu)
    {
        final String text = menu.getText();
        if (text == "File") {
            menu.add(editor, "New", 'N', "newBuffer");
            menu.add(editor, "Open...", 'O', "openFile");
            menu.add(editor, "Recent Files...", 'R', "recentFiles");
            menu.addSeparator();
            menu.add(editor, "Save As...", 'E', "saveAs");
            menu.add(editor, "Save a Copy...", 'Y', "saveCopy");
            menu.add(editor, "Save All", 'A', "saveAll");
            menu.add(editor, "Close", 'C', "killBuffer");
            menu.add(editor, "Close All", 'L', "closeAll");
            menu.add(editor, "Close Others", 'H', "closeOthers");
            menu.addSeparator();
            menu.add(editor, "Properties", 'I', "properties");
            menu.addSeparator();
            menu.add(editor, "Next Buffer", 'T', "nextBuffer");
            menu.add(editor, "Previous Buffer", 'R', "prevBuffer");
            menu.addSeparator();
            menu.add(editor, "New Frame", 'M', "newFrame");
            menu.add(editor, "Execute Command...", 'D', "executeCommand");
            menu.addSeparator();
            menu.add(editor, "Print...", 'P', "print");
            menu.addSeparator();
            menu.add(editor, "Save All/Exit", '/', "saveAllExit");
            menu.add(editor, "Exit", 'X', "quit");
        } else if (text == "Edit") {
            menu.add(editor, "Copy", 'C', "copyRegion");
            menu.add(editor, "Copy Append", 'D', "copyAppend");
        } else if (text == "Search") {
            menu.add(editor, "Find...", 'F', "find");
            menu.add(editor, "Find Next", 'T', "findNext");
            menu.add(editor, "Find Previous", 'R', "findPrev");
        } else if (text == "Go") {
            menu.add(editor, "Go Back", 'B', "webBack");
            menu.add(editor, "Go Forward", 'F', "webForward");
            menu.addSeparator();
            menu.add(editor, "Go To Next Occurrence of Word", 'T', "findNextWord");
            menu.add(editor, "Go To Previous Occurrence of Word", 'R', "findPrevWord");
        } else
            super.populateMenu(editor, menu);
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new WebFormatter(buffer);
    }

    protected ToolBar getDefaultToolBar(Frame frame)
    {
        return new WebModeToolBar(frame);
    }

    public final String getContextString(Editor editor, boolean verbose /*ignored*/)
    {
        return getContextString(editor.getDot());
    }

    public final String getMouseMovedContextString(Editor editor, Position pos)
    {
        // We want to clear the status text if the mouse is not over a link, so
        // return "" instead of null.
        final String s = getContextString(pos);
        return s != null ? s : "";
    }

    private String getContextString(Position pos)
    {
        if (pos != null && pos.getLine() instanceof WebLine) {
            HtmlLineSegment segment =
                ((WebLine)pos.getLine()).findSegment(pos.getOffset());
            if (segment != null) {
                Link link = segment.getLink();
                if (link != null)
                    return link.getTarget();
            }
        }
        return null;
    }

    public static void google()
    {
        final Editor editor = Editor.currentEditor();
        InputDialog d = new InputDialog(editor, "Search for:", "Google Search", null);
        d.setHistory(new History("google.search"));
        editor.centerDialog(d);
        d.show();
        String s = d.getInput();
        if (s == null || s.length() == 0)
            return;
        editor.repaintNow();
        google(s);
    }

    public static void google(String s)
    {
        query("http://www.google.com/search?q=", s);
    }

    public static void query(String prefix, String s)
    {
        s = s.trim();
        // Strip enclosing quotes if any.
        int length = s.length();
        if (length > 1 &&
            ((s.charAt(0) == '"' && s.charAt(length-1) == '"') ||
             (s.charAt(0) == '\'' && s.charAt(length-1) == '\''))) {
            s = s.substring(1, length-1).trim();
        }
        FastStringBuffer sb = new FastStringBuffer(prefix);
        StringTokenizer st = new StringTokenizer(s);
        final int count = st.countTokens();
        for (int i = 0; i < count; i++) {
            if (i != 0)
                sb.append('+');
            sb.append(st.nextToken());
        }
        HttpFile file = HttpFile.getHttpFile(sb.toString());
        if (file != null) {
            Buffer buf = null;
            // Look for existing buffer.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer b = it.nextBuffer();
                if (b instanceof WebBuffer && b.getFile().equals(file)) {
                    buf = b;
                    break;
                }
            }
            if (buf == null)
                buf = WebBuffer.createWebBuffer(file, null, null);
            if (buf != null) {
                final Editor editor = Editor.currentEditor();
                editor.makeNext(buf);
                editor.activate(buf);
            }
        }
    }
}
