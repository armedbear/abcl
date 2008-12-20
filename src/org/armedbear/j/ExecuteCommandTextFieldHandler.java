/*
 * ExecuteCommandTextFieldHandler.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

public final class ExecuteCommandTextFieldHandler extends DefaultTextFieldHandler
{
    public ExecuteCommandTextFieldHandler(Editor editor, HistoryTextField textField)
    {
        super(editor, textField);
    }

    public void enter()
    {
        String input = textField.getText();
        if (input == null)
            return;
        input = input.trim();
        if (input.length() == 0)
            return;
        // Save history.
        History history = textField.getHistory();
        if (history != null) {
            history.append(input);
            history.save();
        }
        if (!input.equals("inbox")) {
            // Aliases.
            String value = editor.getAlias(input);
            if (value != null)
                input = value;
        }
        editor.ensureActive();
        editor.setFocusToDisplay();
        editor.updateLocation();
        editor.executeCommand(input, true);
        editor.getDispatcher().eventHandled();
    }

    public boolean wantTab()
    {
        return true;
    }

    public final List getCompletions(String prefix)
    {
        return CommandTable.getCompletionsForPrefix(prefix);
    }
}
