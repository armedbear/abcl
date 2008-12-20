/*
 * SelectRegisterDialog.java
 *
 * Copyright (C) 2006 Peter Graves
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

public final class SelectRegisterDialog extends InputDialog
{
  public SelectRegisterDialog(Editor editor, String prompt, String title, String defaultValue)
  {
    super(editor, "Register:", title, null);
    setHistory(new History("selectRegister.register"));
  }

  protected List getCompletions(String prefix)
  {
    String lower = prefix.toLowerCase();
    String[] names = null;
    File directory = Directories.getRegistersDirectory();
    if (directory != null)
      names = directory.list();
    ArrayList list = new ArrayList();
    final int limit = names.length;
    for (int i = 0; i < limit; i++)
      {
        String name = names[i];
        if (name.toLowerCase().startsWith(lower))
          list.add(name);
      }
    return list;
  }
}
