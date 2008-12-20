/*
 * ChooseFolderDialog.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

package org.armedbear.j.mail;

import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.History;
import org.armedbear.j.InputDialog;
import org.armedbear.j.MessageDialog;

public final class ChooseFolderDialog extends InputDialog
{
    private ChooseFolderDialog(Editor editor, String prompt, String title)
    {
        super(editor, prompt, title, null);
        History history = new History("chooseFolder");
        setHistory(history);
        setDefaultValue(history.getPrevious());
    }

    public static String chooseFolder(Editor editor, String title)
    {
        return chooseFolder(editor, "Folder:", title);
    }

    // Returns null if user cancels.
    // Returns null if user input (or dereferenced alias) is empty string.
    public static String chooseFolder(Editor editor, String prompt,
        String title)
    {
        String errorText = null;
        while (true) {
            if (errorText != null)
                MessageDialog.showMessageDialog(editor, errorText, title);
            ChooseFolderDialog dialog =
                new ChooseFolderDialog(editor, prompt, title);
            editor.centerDialog(dialog);
            dialog.show();
            editor.repaintNow();
            final String input = dialog.getInput();
            if (input == null || input.length() == 0)
                return null;
            final String value = editor.getAlias(input);
            final String destination = value != null ? value : input;
            Debug.assertTrue(destination != null);
            if (destination.length() == 0) // Shouldn't happen.
                return null;
            if (destination.startsWith("mailbox:")) {
                // Local folder. Do a little validation.
                File file = File.getInstance(destination.substring(8));
                if (file == null) {
                    errorText = "Invalid path";
                    continue;
                }
                if (file.isDirectory()) {
                    return "mailbox:".concat(File.getInstance(file, "mbox").canonicalPath());
                }
                if (file.isFile()) {
                    // Assume it's really a mailbox...
                    return "mailbox:".concat(file.canonicalPath());
                }
                errorText = "Folder does not exist";
                continue;
            }
            // Not local.
            return destination;
        }
    }
}
