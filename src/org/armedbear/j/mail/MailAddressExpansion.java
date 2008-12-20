/*
 * MailAddressExpansion.java
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

import java.util.ArrayList;
import org.armedbear.j.Debug;
import org.armedbear.j.Expansion;
import org.armedbear.j.Line;
import org.armedbear.j.Position;
import org.armedbear.j.Utilities;

public final class MailAddressExpansion extends Expansion
{
    public MailAddressExpansion(Position dot)
    {
        savedDot = new Position(dot);
        final Line dotLine = dot.getLine();
        final int dotOffset = dot.getOffset();
        savedText = dotLine.getText();
        // Get word before caret.
        int begin = dotOffset - 1;
        if (begin < 0)
            return;
        while (begin > 0) {
            char c = dotLine.charAt(begin);
            if (c == ',' || c ==':') {
                ++begin;
                break;
            }
            --begin;
        }
        Debug.assertTrue(begin >= 0);
        while (begin < dotLine.length()) {
            char c = dotLine.charAt(begin);
            if (c ==',' || c == ':' || c == ' ' || c == '\t')
                ++begin;
            else
                break;
        }
        int end = dotOffset;
        if (begin >= end)
            return;
        prefix = dotLine.substring(begin, end);
        prefixOffset = begin;
        int length = prefix.length();
        boolean ignoreCase = Utilities.isLowerCase(prefix);
        candidates = new ArrayList();
        AddressBook addressBook = AddressBook.getGlobalAddressBook();
        if (addressBook != null) {
            final int limit = addressBook.size();
            for (int i = 0; i < limit; i++) {
                AddressBookEntry entry = addressBook.getEntry(i);
                String personal = entry.getPersonal();
                if (personal != null) {
                    if (personal.regionMatches(ignoreCase, 0, prefix, 0, length)) {
                        candidates.add(entry);
                        continue;
                    }
                }
                // Not added yet.
                if (entry.getAddress().regionMatches(ignoreCase, 0, prefix, 0, length))
                    candidates.add(entry);
            }
        }
    }

    public String getNextCandidate()
    {
        if (candidates == null || candidates.size() == 0)
            return null;
        int index = last + 1;
        if (index == candidates.size())
            index = 0;
        last = index;
        AddressBookEntry entry = (AddressBookEntry) candidates.get(index);
        return entry.toString();
    }
}
