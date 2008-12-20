/*
 * AddressBookEntry.java
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

import org.armedbear.j.FastStringBuffer;

public final class AddressBookEntry
{
    private String personal;
    private String address;

    public AddressBookEntry(String personal, String address)
    {
        this.personal = personal;
        this.address = address;
    }

    // Constructs an address book entry from a line from ~/.j/addresses.
    public static AddressBookEntry parseAddressBookEntry(String s)
    {
        int index = s.lastIndexOf('<');
        if (index >= 0) {
            String personal = s.substring(0, index).trim();
            if (personal.length() == 0)
                personal = null;
            String address = s.substring(index);
            // Strip '<' and '>'.
            address = address.substring(1, address.length()-1);
            return new AddressBookEntry(personal, address);
        } else
            return new AddressBookEntry(null, s);
    }

    public String getPersonal()
    {
        return personal;
    }

    public void setPersonal(String s)
    {
        personal = s;
    }

    // Returns canonical form of personal name.
    public static String canonicalizePersonal(String s)
    {
        if (s == null)
            return null;
        // Reject the name if it looks like an address.
        if (s.indexOf('@') >= 0)
            return null;
        s = s.trim();
        // Strip single quotes.
        if (s.length() >= 2 && s.charAt(0) == '\'' && s.charAt(s.length()-1) == '\'')
            s = s.substring(1, s.length()-1);
        if (s.length() == 0)
            return null;
        // Strip any bogus text starting with '<'.
        int index = s.indexOf('<');
        if (index >= 0)
            s = s.substring(0, index).trim();
        if (s.length() == 0)
            return null;
        // First name should come first.
        index = s.indexOf(',');
        if (index >= 0) {
            // Last name first.
            String lastName = s.substring(0, index);
            String firstName = s.substring(index+1).trim();
            return firstName + ' ' + lastName;
        }
        return s;
    }

    public static String canonicalizeAddress(String s)
    {
        if (s == null)
            return null;
        if (s.length() == 0)
            return null;
        return s;
    }

    public String getAddress()
    {
        return address;
    }

    public void setAddress(String s)
    {
        address = s;
    }

    public boolean equals(Object object)
    {
        if (this == object)
            return true;
        if (object instanceof AddressBookEntry) {
            AddressBookEntry entry = (AddressBookEntry) object;
            if (personal != null) {
                if (!personal.equals(entry.personal))
                    return false;
            } else if (entry.personal != null)
                return false;
            if (address != null) {
                if (!address.equals(entry.address))
                    return false;
            } else if (entry.address != null)
                return false;
            return true;
        }
        return false;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (personal != null) {
            sb.append(personal);
            sb.append(' ');
        }
        if (address != null) {
            if (sb.length() > 0) {
                sb.append('<');
                sb.append(address);
                sb.append('>');
            } else {
                // No personal name. Don't use angle brackets.
                sb.append(address);
            }
        }
        return sb.toString();
    }
}
