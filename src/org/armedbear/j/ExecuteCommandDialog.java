/*
 * ExecuteCommandDialog.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.util.List;

public final class ExecuteCommandDialog extends InputDialog
{
    private ExecuteCommandDialog(Editor editor, String title, String historyKey)
    {
        super(editor, "Command:", title, null);
        History history = new History(historyKey);
        setHistory(history);
        setDefaultValue(history.getPrevious());
    }

    protected final List getCompletions(String prefix)
    {
        return CommandTable.getCompletionsForPrefix(prefix);
    }

    public static void whereIs()
    {
        final Editor editor = Editor.currentEditor();
        ExecuteCommandDialog d =
            new ExecuteCommandDialog(editor, "Where is...", "whereIs.input");
        editor.centerDialog(d);
        d.show();
        editor.repaintNow();
        String input = d.getInput();
        if (input == null)
            return;
        input = input.trim();
        if (input.length() == 0)
            return;
        whereIs(input);
    }

    public static void whereIs(String s)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        editor.setWaitCursor();
        // If it's a known command, use its canonical name.
        Command command = CommandTable.getCommand(s);
        final String commandName = command != null ? command.getName() : s;
        List list = KeyMap.getGlobalKeyMap().listKeys(commandName);
        list.addAll(buffer.getKeyMapForMode().listKeys(commandName));
        FastStringBuffer sb = new FastStringBuffer(commandName);
        if (list.size() == 0) {
            sb.append(" is not mapped");
        } else if (list.size() == 1) {
            sb.append(" is mapped to ");
            sb.append((String)list.get(0));
        } else {
            sb.append(" is mapped to:");
            sb.append(System.getProperty("line.separator"));
            for (int i = 0; i < list.size(); i++) {
                sb.append(System.getProperty("line.separator"));
                sb.append("    ");
                sb.append((String) list.get(i));
            }
        }
        editor.setDefaultCursor();
        MessageDialog.showMessageDialog(editor, sb.toString(), "Where is...");
    }
}
