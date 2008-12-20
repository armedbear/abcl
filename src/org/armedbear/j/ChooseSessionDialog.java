/*
 * ChooseSessionDialog.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.util.ArrayList;
import java.util.List;

public final class ChooseSessionDialog extends InputDialog
{
    public ChooseSessionDialog(String title)
    {
        super(Editor.currentEditor(), "Name:", title,
            Editor.getSessionName());
        setHistory(new History("chooseSession"));
        editor.centerDialog(this);
    }

    protected List getCompletions(String prefix)
    {
        ArrayList list = null;
        if (prefix != null) {
            final int prefixLength = prefix.length();
            File dir = Session.getSessionDirectory();
            if (dir != null) {
                String[] names = dir.list();
                for (int i = 0; i < names.length; i++) {
                    String name = names[i];
                    if (name.regionMatches(true, 0, prefix, 0, prefixLength)) {
                        if (list == null)
                            list = new ArrayList();
                        list.add(name);
                    }
                }
            }
        }
        return list;
    }
}
