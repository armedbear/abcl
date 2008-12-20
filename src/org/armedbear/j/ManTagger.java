// ManTagger.java
//
// Copyright (C) 2006 Peter Graves <peter@armedbear.org>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

// Contributed by Vasile Rotaru.

package org.armedbear.j;

import java.util.ArrayList;
import java.util.List;

public final class ManTagger extends Tagger
{
  // Heuristic: options are usally aligned in the same column.
  private int optionColumn = -1;

  // Heuristic: option cannot be too far to the right.
  private static final int STOP = 12;

  public ManTagger(SystemBuffer buffer)
  {
    super(buffer);
  }

  public void run()
  {
    List tags = new ArrayList();
    Line line = buffer.getFirstLine();
    int stop = 0;
    while (line != null)
      {
        String s = line.getText();
        // We are interested only in the left-most dashes.
        int start = 0;
        int end = s.length();
        if (end < STOP)
          stop = end;
        else
          stop = STOP;
        while (start < stop && Character.isWhitespace(s.charAt(start)))
          ++start;
        if (end > 0 && s.charAt(start) == '-' && optionColumn < 0)
          // found option column
          optionColumn = start;
        if (end > 0 && start < end && start == optionColumn
            && s.charAt(start) == '-')
          {
            end = s.indexOf(". ", start + 1);
            if (end < 0)
              end = s.length();
            String name = s.substring(start, end);
            tags.add(new LocalTag(name, line));
          }
        line = line.next();
      }
    buffer.setTags(tags);
  }
}
