/*
 * MailboxFilter.java
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

package org.armedbear.j.mail;

import java.util.Stack;
import org.armedbear.j.Debug;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Log;

public abstract class MailboxFilter
{
    public static MailboxFilter getMailboxFilter(String input)
    {
        input = input.trim();
        if (input.indexOf('~') < 0)
            return new GenericMailboxFilter(input);
        FastStringReader reader = new FastStringReader(input);
        try {
            return parse(reader);
        }
        catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

    private static MailboxFilter parse(FastStringReader reader) throws Exception
    {
        Stack stack = new Stack();
        while (parseNextTerm(reader, stack))
            ;
        Debug.assertTrue(stack.size() == 1);
        return (MailboxFilter) stack.pop();
    }

    // Returns false if end of input is reached, true otherwise.
    private static boolean parseNextTerm(FastStringReader reader, Stack stack)
        throws Exception
    {
        reader.skipWhitespace();
        char c = reader.readChar();
        switch (c) {
            case '~': {
                MailboxFilter filter = parseTilde(reader);
                if (filter != null) {
                    if (stack.size() > 0) {
                        MailboxFilter existing = (MailboxFilter) stack.pop();
                        if (existing instanceof AndTerm) {
                            ((AndTerm) existing).add(filter);
                            stack.push(existing);
                        } else
                            stack.push(new AndTerm(existing, filter));
                    } else
                        stack.push(filter);
                }
                break;
            }
            case '!': {
                MailboxFilter filter = parseNot(reader);
                if (filter != null) {
                    if (stack.size() > 0) {
                        MailboxFilter existing = (MailboxFilter) stack.pop();
                        if (existing instanceof AndTerm) {
                            ((AndTerm) existing).add(filter);
                            stack.push(existing);
                        } else
                            stack.push(new AndTerm(existing, filter));
                    } else
                        stack.push(filter);
                }
                break;
            }
            case '(': {
                MailboxFilter filter = parse(reader);
                break;
            }
            case ')':
                return false;
            case '|': {
                MailboxFilter filter = parseOr(reader);
                if (filter != null) {
                    if (stack.size() > 0) {
                        MailboxFilter existing = (MailboxFilter) stack.pop();
                        if (existing instanceof OrTerm) {
                            ((OrTerm) existing).add(filter);
                            stack.push(existing);
                        } else
                            stack.push(new OrTerm(existing, filter));
                    } else
                        stack.push(filter);
                }
                break;
            }
            case '\0':
                return false; // End of input.
            default:
                throw new Exception();
        }
        return true;
    }

    // We've just seen '|'.
    private static MailboxFilter parseOr(FastStringReader reader) throws Exception
    {
        reader.skipWhitespace();
        MailboxFilter filter = null;
        char c = reader.readChar();
        switch (c) {
            case '~':
                filter = parseTilde(reader);
                break;
            case '!':
                filter = parseNot(reader);
                break;
            case '(':
                filter = parse(reader);
                break;
            default:
                Log.error("char = " + c);
                throw new Exception();
        }
        Debug.assertTrue(filter != null);
        return filter;
    }

    private static NotTerm parseNot(FastStringReader reader) throws Exception
    {
        reader.skipWhitespace();
        MailboxFilter filter = null;
        char c = reader.readChar();
        switch (c) {
            case '~':
                filter = parseTilde(reader);
                break;
            case '(':
                filter = parse(reader);
                break;
            default:
                throw new Exception();
        }
        Debug.assertTrue(filter != null);
        return new NotTerm(filter);
    }

    private static MailboxFilter parseTilde(FastStringReader reader) throws Exception
    {
        MailboxFilter filter = null;
        char c = reader.readChar();
        switch (c) {
            case 'C':
                filter = new ToOrCcMailboxFilter(reader);
                break;
            case 'D':
                filter = new DeletedMailboxFilter();
                break;
            case 'F':
                filter = new FlaggedMailboxFilter();
                break;
            case 'N':
                filter = new NewMailboxFilter();
                break;
            case 'R':
                filter = new ReadMailboxFilter();
                break;
            case 'U':
                filter = new UnreadMailboxFilter();
                break;
            case 'T':
                filter = new TaggedMailboxFilter();
                break;
            case 'f':
                filter = new FromMailboxFilter(reader);
                break;
            case 't':
                filter = new ToMailboxFilter(reader);
                break;
            case 'd':
                filter = DateSentMailboxFilter.getMailboxFilter(reader);
                break;
            default:
                Log.error("parseTilde() returning null, remainder = |" + reader.remainder() + "|");
                throw new Exception();
        }
        return filter;
    }

    public abstract boolean accept(MailboxEntry entry);
}
