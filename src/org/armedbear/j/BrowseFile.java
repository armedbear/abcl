/*
 * BrowseFile.java
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.io.IOException;

public final class BrowseFile implements Constants
{
    public static void browseFileAtDot()
    {
        String browser =
            Editor.preferences().getStringProperty(Property.BROWSER);
        if (browser == null)
            browser = "j";
        final Editor editor = Editor.currentEditor();
        String filename = browseFileGetFilename(editor);
        if (filename == null)
            return;
        File file = null;
        if (!filename.startsWith("http://") && !filename.startsWith("https://")) {
            final Buffer buffer = editor.getBuffer();
            if (buffer.getFile() != null) {
                String prefix = buffer.getFile().netPath();
                if (prefix.startsWith("http://") || prefix.startsWith("https://"))
                    filename = File.appendNameToPath(prefix, filename, '/');
            }
            if (!filename.startsWith("http://") && !filename.startsWith("https://")) {
                file = File.getInstance(editor.getCurrentDirectory(), filename);
                if (file != null && file.isLocal() && file.isFile())
                    filename = "file://" + file.canonicalPath();
                else
                    return;
            }
        }
        if (browser.equals("j")) {
            if (file != null)
                WebBuffer.browse(editor, file, null);
            else
                WebBuffer.browse(editor, File.getInstance(filename), null);
            return;
        }
        // External browser.
        String browserOpts =
            Editor.preferences().getStringProperty(Property.BROWSER_OPTS);
        try {
            if (browserOpts != null) {
                String[] cmdarray = {browser, browserOpts, filename};
                Runtime.getRuntime().exec(cmdarray);
            } else {
                String[] cmdarray = {browser, filename};
                Runtime.getRuntime().exec(cmdarray);
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private static String browseFileGetFilename(Editor editor)
    {
        if (editor.getMark() != null && editor.getMarkLine() == editor.getDotLine()) {
            // Use selection.
            return new Region(editor).toString();
        }
        if (editor.getModeId() == HTML_MODE) {
            RE re = new UncheckedRE("(href|src)=\"([^\"]+)\"", RE.REG_ICASE);
            REMatch match = null;
            final String text = editor.getDotLine().getText();
            final int dotOffset = editor.getDotOffset();
            REMatch m;
            int index = 0;
            while ((m = re.getMatch(text, index)) != null) {
                match = m;
                if (match.getEndIndex() > dotOffset)
                    break; // All subsequent matches will be further away.
                index = match.getEndIndex();
            }
            if (match != null)
                return match.toString(2);
        }
        return editor.getFilenameAtDot();
    }
}
