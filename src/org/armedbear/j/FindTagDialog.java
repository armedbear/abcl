/*
 * FindTagDialog.java
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

import java.util.List;

public final class FindTagDialog extends InputDialog
{
    private final FindTagTextFieldHandler handler;

    public FindTagDialog(Editor editor, String title)
    {
        super(editor, "Tag:", title, null);
        setHistory(new History("findTag.tag"));
        handler = new FindTagTextFieldHandler(editor, textField);
    }

    protected List getCompletions(String prefix)
    {
        return handler.getCompletions(prefix);
    }
}
