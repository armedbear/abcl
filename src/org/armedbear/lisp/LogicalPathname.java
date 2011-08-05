/*
 * LogicalPathname.java
 *
 * Copyright (C) 2004-2005 Peter Graves
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.util.HashMap;
import java.util.StringTokenizer;

public final class LogicalPathname extends Pathname
{
    private static final String LOGICAL_PATHNAME_CHARS =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-;*.";

    private static final HashMap map = new HashMap();

    protected LogicalPathname()
    {
    }

    protected LogicalPathname(Pathname p) {
        super(p);
    }

    public LogicalPathname(String host, String rest)
    {
        final int limit = rest.length();
        for (int i = 0; i < limit; i++) {
            char c = rest.charAt(i);
            if (LOGICAL_PATHNAME_CHARS.indexOf(c) < 0) {
                error(new ParseError("The character #\\" + c + " is not valid in a logical pathname."));
                return;
            }
        }

        this.host = new SimpleString(host);

        // "The device component of a logical pathname is always :UNSPECIFIC;
        // no other component of a logical pathname can be :UNSPECIFIC."
        device = Keyword.UNSPECIFIC;

        int semi = rest.lastIndexOf(';');
        if (semi >= 0) {
            // Directory.
            String d = rest.substring(0, semi + 1);
            directory = parseDirectory(d);
            rest = rest.substring(semi + 1);
        } else {
            // "If a relative-directory-marker precedes the directories, the
            // directory component parsed is as relative; otherwise, the
            // directory component is parsed as absolute."
            directory = new Cons(Keyword.ABSOLUTE);
        }

        int dot = rest.indexOf('.');
        if (dot >= 0) {
            String n = rest.substring(0, dot);
            if (n.equals("*"))
                name = Keyword.WILD;
            else
                name = new SimpleString(n.toUpperCase());
            rest = rest.substring(dot + 1);
            dot = rest.indexOf('.');
            if (dot >= 0) {
                String t = rest.substring(0, dot);
                if (t.equals("*"))
                    type = Keyword.WILD;
                else
                    type = new SimpleString(t.toUpperCase());
                // What's left is the version.
                String v = rest.substring(dot + 1);
                if (v.equals("*"))
                    version = Keyword.WILD;
                else if (v.equals("NEWEST") || v.equals("newest"))
                    version = Keyword.NEWEST;
                else
                    version = PACKAGE_CL.intern("PARSE-INTEGER").execute(new SimpleString(v));
            } else {
                String t = rest;
                if (t.equals("*"))
                    type = Keyword.WILD;
                else
                    type = new SimpleString(t.toUpperCase());
            }
        } else {
            String n = rest;
            if (n.equals("*"))
                name = Keyword.WILD;
            else if (n.length() > 0)
                name = new SimpleString(n.toUpperCase());
        }
    }

    private static final String LOGICAL_PATHNAME_COMPONENT_CHARS =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-";

    public static final SimpleString canonicalizeStringComponent(AbstractString s)

    {
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (LOGICAL_PATHNAME_COMPONENT_CHARS.indexOf(c) < 0) {
                error(new ParseError("Invalid character #\\" + c +
                                      " in logical pathname component \"" + s +
                                      '"'));
                // Not reached.
                return null;
            }
        }
        return new SimpleString(s.getStringValue().toUpperCase());
    }

    public static Pathname translateLogicalPathname(LogicalPathname pathname)

    {
        return (Pathname) Symbol.TRANSLATE_LOGICAL_PATHNAME.execute(pathname);
    }

    private static final LispObject parseDirectory(String s)

    {
        LispObject result;
        if (s.charAt(0) == ';') {
            result = new Cons(Keyword.RELATIVE);
            s = s.substring(1);
        } else
            result = new Cons(Keyword.ABSOLUTE);
        StringTokenizer st = new StringTokenizer(s, ";");
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            LispObject obj;
            if (token.equals("*"))
                obj = Keyword.WILD;
            else if (token.equals("**"))
                obj = Keyword.WILD_INFERIORS;
            else if (token.equals("..")) {
                if (result.car() instanceof AbstractString) {
                    result = result.cdr();
                    continue;
                }
                obj= Keyword.UP;
            } else
                obj = new SimpleString(token.toUpperCase());
            result = new Cons(obj, result);
        }
        return result.nreverse();
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.LOGICAL_PATHNAME;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.LOGICAL_PATHNAME;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.LOGICAL_PATHNAME)
            return T;
        if (type == BuiltInClass.LOGICAL_PATHNAME)
            return T;
        return super.typep(type);
    }

    @Override
    protected String getDirectoryNamestring()
    {
        StringBuilder sb = new StringBuilder();
        // "If a pathname is converted to a namestring, the symbols NIL and
        // :UNSPECIFIC cause the field to be treated as if it were empty. That
        // is, both NIL and :UNSPECIFIC cause the component not to appear in
        // the namestring." 19.2.2.2.3.1
        if (directory != NIL) {
            LispObject temp = directory;
            LispObject part = temp.car();
            if (part == Keyword.ABSOLUTE) {
            } else if (part == Keyword.RELATIVE)
                sb.append(';');
            else
                error(new FileError("Unsupported directory component " + part.princToString() + ".",
                                     this));
            temp = temp.cdr();
            while (temp != NIL) {
                part = temp.car();
                if (part instanceof AbstractString)
                    sb.append(part.getStringValue());
                else if (part == Keyword.WILD)
                    sb.append('*');
                else if (part == Keyword.WILD_INFERIORS)
                    sb.append("**");
                else if (part == Keyword.UP)
                    sb.append("..");
                else
                    error(new FileError("Unsupported directory component " + part.princToString() + ".",
                                         this));
                sb.append(';');
                temp = temp.cdr();
            }
        }
        return sb.toString();
    }

    @Override
    public String printObject()
    {
        final LispThread thread = LispThread.currentThread();
        boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
        boolean printEscape = (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
        StringBuilder sb = new StringBuilder();
        if (printReadably || printEscape)
            sb.append("#P\"");
        sb.append(host.getStringValue());
        sb.append(':');
        if (directory != NIL)
            sb.append(getDirectoryNamestring());
        if (name != NIL) {
            if (name == Keyword.WILD)
                sb.append('*');
            else
                sb.append(name.getStringValue());
        }
        if (type != NIL) {
            sb.append('.');
            if (type == Keyword.WILD)
                sb.append('*');
            else
                sb.append(type.getStringValue());
        }
        if (version.integerp()) {
            sb.append('.');
            int base = Fixnum.getValue(Symbol.PRINT_BASE.symbolValue(thread));
            if (version instanceof Fixnum)
                sb.append(Integer.toString(((Fixnum)version).value, base).toUpperCase());
            else if (version instanceof Bignum)
                sb.append(((Bignum)version).value.toString(base).toUpperCase());
        } else if (version == Keyword.WILD) {
            sb.append(".*");
        } else if (version == Keyword.NEWEST) {
            sb.append(".NEWEST");
        }
        if (printReadably || printEscape)
            sb.append('"');
        return sb.toString();
    }

    // ### canonicalize-logical-host host => canonical-host
    private static final Primitive CANONICALIZE_LOGICAL_HOST = new canonicalize_logical_host();
    private static class canonicalize_logical_host extends Primitive {
        canonicalize_logical_host() {
            super("canonicalize-logical-host", PACKAGE_SYS, true, "host");
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            AbstractString s = checkString(arg);
            if (s.length() == 0) {
                // "The null string, "", is not a valid value for any
                // component of a logical pathname." 19.3.2.2
                return error(new LispError("Invalid logical host name: \"" +
                                           s.getStringValue() + '"'));
            }
            return canonicalizeStringComponent(s);
        }
    }

    // ### %make-logical-pathname namestring => logical-pathname
    private static final Primitive _MAKE_LOGICAL_PATHNAME = new _make_logical_pathname();
    private static class _make_logical_pathname extends Primitive {
        _make_logical_pathname() {
            super("%make-logical-pathname", PACKAGE_SYS, true, "namestring");
        }
        @Override
        public LispObject execute(LispObject arg)

        {
            // Check for a logical pathname host.
            String s = arg.getStringValue();
            String h = getHostString(s);
            if (h != null) {
                if (h.length() == 0) {
                    // "The null string, "", is not a valid value for any
                    // component of a logical pathname." 19.3.2.2
                    return error(new LispError("Invalid logical host name: \"" +
                                                h + '"'));
                }
                if (Pathname.LOGICAL_PATHNAME_TRANSLATIONS.get(new SimpleString(h)) != null) {
                    // A defined logical pathname host.
                    return new LogicalPathname(h, s.substring(s.indexOf(':') + 1));
                }
            }
            return error(new TypeError("Logical namestring does not specify a host: \"" + s + '"'));
        }
    }
}
