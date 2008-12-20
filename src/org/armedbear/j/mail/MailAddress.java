/*
 * MailAddress.java
 *
 * Copyright (C) 2000-2006 Peter Graves
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

import java.io.Serializable;
import java.util.ArrayList;
import org.armedbear.j.FastStringBuffer;

public final class MailAddress implements Serializable
{
    private final String personal;
    private final String encodedPersonal;
    private final String address;

    public MailAddress(String encodedPersonal, String address)
    {
        if (encodedPersonal != null && encodedPersonal.length() > 0) {
            // Remove enclosing quotes if any.
            // Handle cases like "'mikol@onebox.com'" where the actual string
            // is enclosed in both single and double quotes.
            char c = encodedPersonal.charAt(0);
            if (c == '"' || c =='\'') {
                int length = encodedPersonal.length();
                if (length >= 2) {
                    if (encodedPersonal.charAt(length-1) == c) {
                        encodedPersonal =
                            encodedPersonal.substring(1, length-1);
                        length -= 2;
                        if (length >= 2) {
                            c = encodedPersonal.charAt(0);
                            if (c == '"' || c == '\'') {
                                if (encodedPersonal.charAt(length-1) == c) {
                                    encodedPersonal =
                                        encodedPersonal.substring(1, length-1);
                                }
                            }
                        }
                    }
                }
            }
            this.encodedPersonal = encodedPersonal.intern();
            personal = RFC2047.decode(encodedPersonal).trim().intern();
        } else {
            this.encodedPersonal = null;
            personal = null;
        }
        if (address != null && address.length() > 0) {
            if (address.charAt(0) == '<') {
                final int length = address.length();
                if (length >= 2) {
                    if (address.charAt(length-1) == '>')
                        address = address.substring(1, length-1);
                }
            }
            this.address = address.intern();
        } else
            this.address = null;
    }

    public final String getPersonal()
    {
        return personal;
    }

    public final String getEncodedPersonal()
    {
        return encodedPersonal;
    }

    public final String getAddress()
    {
        return address;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (personal != null && personal.length() > 0) {
            if (personal.indexOf(',') >= 0 || personal.indexOf('.') >= 0) {
                sb.append('"');
                sb.append(personal);
                sb.append('"');
            } else
                sb.append(personal);
            if (address != null && address.length() > 0) {
                sb.append(" <");
                sb.append(address);
                sb.append('>');
            }
        } else if (address != null && address.length() > 0)
            sb.append(address);
        return sb.toString();
    }

    public String toEncodedString()
    {
        if (encodedPersonal == null || encodedPersonal.length() == 0)
            return toString();
        FastStringBuffer sb = new FastStringBuffer();
        if (encodedPersonal.indexOf(',') >= 0 || encodedPersonal.indexOf('.') >= 0) {
            sb.append('"');
            sb.append(encodedPersonal);
            sb.append('"');
        } else
            sb.append(encodedPersonal);
        if (address != null && address.length() > 0) {
            sb.append(" <");
            sb.append(address);
            sb.append('>');
        }
        return sb.toString();
    }

    public boolean equals(Object o)
    {
        if (this == o)
            return true;
        if (o instanceof MailAddress) {
            MailAddress ma = (MailAddress) o;
            if (personal != ma.personal)
                return false;
            if (encodedPersonal != ma.encodedPersonal)
                return false;
            if (address != ma.address)
                return false;
            return true;
        } else
            return false;
    }

    public final boolean matches(String pattern)
    {
        if (personal != null)
            if (personal.indexOf(pattern) >= 0)
                return true; // Personal name matches.
        if (address != null)
            if (address.indexOf(pattern) >= 0)
                return true; // Address matches.
        return false;
    }

    // Pattern is already lower case.
    public final boolean matchesIgnoreCase(String pattern)
    {
        if (personal != null)
            if (personal.toLowerCase().indexOf(pattern) >= 0)
                return true; // Personal name matches.
        if (address != null)
            if (address.toLowerCase().indexOf(pattern) >= 0)
                return true; // Address matches.
        return false;
    }

    public final boolean matches(MailAddress a)
    {
        if (a != null) {
            // Personal name must be exact match.
            if (personal != null && personal.equals(a.personal))
                return true;
            else if (address != null && address.equalsIgnoreCase(a.address))
                return true;
        }
        return false;
    }

    public final boolean addressMatches(MailAddress a)
    {
        if (address != null && address.equalsIgnoreCase(a.address))
            return true;
        else
            return false;
    }

    public static MailAddress parseAddress(String s)
    {
        int index = s.lastIndexOf('<');
        if (index >= 0) {
            String encodedPersonal = s.substring(0, index).trim();
            String address = s.substring(index);
            return new MailAddress(encodedPersonal, address);
        }
        index = s.indexOf('(');
        if (index >= 0) {
            String address = s.substring(0, index).trim();
            if (address.indexOf('@') >= 0) {
                int begin = index + 1;
                int end = s.indexOf(')', begin);
                if (end >= 0) {
                    String encodedPersonal = s.substring(begin, end).trim();
                    return new MailAddress(encodedPersonal, address);
                }
            }
        }
        return new MailAddress(null, s);
    }

    public static MailAddress[] parseAddresses(String input)
    {
        if (input == null)
            return null;
        input = input.trim();
        if (input.length() == 0)
            return null;
        ArrayList addresses = new ArrayList();
        FastStringBuffer sb = new FastStringBuffer();
        boolean inQuote = false;
        final int limit = input.length();
        for (int i = 0; i < limit; i++) {
            char c = input.charAt(i);
            switch (c) {
                case '"':
                    inQuote = !inQuote;
                    sb.append(c);
                    break;
                case ',':
                    if (inQuote) {
                        // A comma inside a quoted string is just an ordinary
                        // character.
                        sb.append(c);
                    } else {
                        // Otherwise a comma marks the end of the address.
                        String s = sb.toString().trim();
                        if (s.length() > 0) {
                            MailAddress address = parseAddress(s);
                            if (address != null)
                                addresses.add(address);
                        }
                        sb.setLength(0);
                    }
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        if (sb.length() > 0) {
            String s = sb.toString().trim();
            if (s.length() > 0) {
                MailAddress address = parseAddress(s);
                if (address != null)
                    addresses.add(address);
            }
        }
        if (addresses.size() == 0)
            return null;
        MailAddress[] array = new MailAddress[addresses.size()];
        return (MailAddress[]) addresses.toArray(array);
    }
}
