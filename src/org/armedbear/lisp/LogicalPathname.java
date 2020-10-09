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
import java.text.MessageFormat;

public final class LogicalPathname extends Pathname
{
  public static final String LOGICAL_PATHNAME_CHARS
    = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-;*.";
  private static final HashMap map
    = new HashMap();

  // A logical host is represented as the string that names it.
  // (defvar *logical-pathname-translations* (make-hash-table :test 'equal))
  public static HashTable TRANSLATIONS
    = HashTable.newEqualHashTable(LOGICAL_PATHNAME_CHARS.length(), NIL, NIL);
  private static final Symbol _TRANSLATIONS_
    = exportSpecial("*LOGICAL-PATHNAME-TRANSLATIONS*", PACKAGE_SYS, TRANSLATIONS);

  static public boolean isValidLogicalPathname(String namestring) {
    if (!isValidURL(namestring)) {
      String host = getHostString(namestring);
      if (host != null
       && LogicalPathname.TRANSLATIONS.get(new SimpleString(host)) != null) {
        return true;
      }
    }
    return false;
  }

  protected LogicalPathname() {
  }
  
  // Used in Pathname._makePathname to indicate type for namestring 
  public static LogicalPathname create() {
    return new LogicalPathname();
  }

  public static LogicalPathname create(LogicalPathname p) {
    Pathname pathname = new Pathname();
    pathname.copyFrom(p);
    LogicalPathname result = new LogicalPathname();
    Pathname.ncoerce(pathname, result);
    return result;
  }



  public static LogicalPathname create(String namestring) {
    // parse host out then call create(host, rest);
    LogicalPathname result = null;
    if (LogicalPathname.isValidLogicalPathname(namestring)) {
      String h = LogicalPathname.getHostString(namestring);
      result
        = LogicalPathname.create(h, namestring.substring(namestring.indexOf(':') + 1));
      return result;
    }
    error(new FileError("Failed to find a valid logical Pathname host in '" + namestring + "'",
                        NIL));  // ??? return NIL as we don't have a
                                // PATHNAME.  Maybe signal a different
                                // condition?
    return (LogicalPathname)UNREACHED;
  }

  public static LogicalPathname create(String host, String rest) {
    // This may be "too late" in the creation chain to be meaningful?
    SimpleString h = new SimpleString(host);
    if (LogicalPathname.TRANSLATIONS.get(h) == null) {
      // Logical pathnames are only valid when it's host exists
      String message = MessageFormat.format("'{0}' is not a defined logical host", host);
      error(new SimpleError(message));
    }
    LogicalPathname result = new LogicalPathname();
    final int limit = rest.length();
    for (int i = 0; i < limit; i++) {
      char c = rest.charAt (i);
      if (LOGICAL_PATHNAME_CHARS.indexOf(c) < 0) {
        error(new ParseError("The character #\\" + c + " is not valid in a logical pathname."));

      }
    }

    result.setHost(h);

    // "The device component of a logical pathname is always :UNSPECIFIC;
    // no other component of a logical pathname can be :UNSPECIFIC."
    result.setDevice(Keyword.UNSPECIFIC);

    int semi = rest.lastIndexOf(';');
    if (semi >= 0) {
      // Directory.
      String d = rest.substring(0, semi + 1);
      result.setDirectory(parseDirectory(d));
      rest = rest.substring(semi + 1);
    } else {
      // "If a relative-directory-marker precedes the directories, the
      // directory component parsed is as relative; otherwise, the
            // directory component is parsed as absolute."
          result.setDirectory(new Cons(Keyword.ABSOLUTE));
    }

    int dot = rest.indexOf('.');
    if (dot >= 0) {
      String n = rest.substring(0, dot);
      if (n.equals("*")) {
        result.setName(Keyword.WILD);
      } else {
        result.setName(new SimpleString(n.toUpperCase()));
      }
      rest = rest.substring(dot + 1);
      dot = rest.indexOf('.');
      if (dot >= 0) {
        String t = rest.substring(0, dot);
        if (t.equals("*")) {
          result.setType(Keyword.WILD);
        } else {
          result.setType(new SimpleString(t.toUpperCase()));
        }
        // What's left is the version.
        String v = rest.substring(dot + 1);
        if (v.equals("*")) {
          result.setVersion(Keyword.WILD);
        } else if (v.equals("NEWEST") || v.equals("newest")) {
          result.setVersion(Keyword.NEWEST);
        } else {
          result.setVersion(PACKAGE_CL.intern("PARSE-INTEGER").execute(new SimpleString(v)));
        }
      } else {
        String t = rest;
        if (t.equals("*")) {
          result.setType(Keyword.WILD);
        } else {
          result.setType(new SimpleString(t.toUpperCase()));
        }
      }
    } else {
      String n = rest;
      if (n.equals("*")) {
        result.setName(Keyword.WILD);
      } else if (n.length() > 0) {
        result.setName(new SimpleString(n.toUpperCase()));
      }
    }
    return result;
  }

  public static final SimpleString canonicalizeStringComponent(AbstractString s) {
    final int limit = s.length();
    for (int i = 0; i < limit; i++) {
      char c = s.charAt(i);
      if (LOGICAL_PATHNAME_CHARS.indexOf(c) < 0) {
        error(new ParseError("Invalid character #\\" + c +
                             " in logical pathname component \"" + s +
                             '"'));
        // Not reached.
        return null;
      }
    }
    return new SimpleString(s.getStringValue().toUpperCase());
  }

  public static Pathname translateLogicalPathname(LogicalPathname pathname) {
    return (Pathname) Symbol.TRANSLATE_LOGICAL_PATHNAME.execute(pathname);
  }

  private static final LispObject parseDirectory(String s) {
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
  public LispObject typeOf() {
    return Symbol.LOGICAL_PATHNAME;
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.LOGICAL_PATHNAME;
  }

  @Override
  public LispObject typep(LispObject type) {
    if (type == Symbol.LOGICAL_PATHNAME)
      return T;
    if (type == BuiltInClass.LOGICAL_PATHNAME)
      return T;
    return super.typep(type);
  }
  
  @Override
  protected String getDirectoryNamestring() {
    StringBuilder sb = new StringBuilder();
    // "If a pathname is converted to a namestring, the symbols NIL and
    // :UNSPECIFIC cause the field to be treated as if it were empty. That
    // is, both NIL and :UNSPECIFIC cause the component not to appear in
    // the namestring." 19.2.2.2.3.1
    if (getDirectory() != NIL) {
      LispObject temp = getDirectory();
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
  public String printObject() {
    final LispThread thread = LispThread.currentThread();
    boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
    boolean printEscape = (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
    StringBuilder sb = new StringBuilder();
    if (printReadably || printEscape)
      sb.append("#P\"");
    sb.append(getHost().getStringValue());
    sb.append(':');
    if (getDirectory() != NIL)
      sb.append(getDirectoryNamestring());
    if (getName() != NIL) {
      if (getName() == Keyword.WILD)
        sb.append('*');
      else
        sb.append(getName().getStringValue());
    }
    if (getType() != NIL) {
      sb.append('.');
      if (getType() == Keyword.WILD)
        sb.append('*');
      else
        sb.append(getType().getStringValue());
    }
    if (getVersion().integerp()) {
      sb.append('.');
      int base = Fixnum.getValue(Symbol.PRINT_BASE.symbolValue(thread));
      if (getVersion() instanceof Fixnum)
        sb.append(Integer.toString(((Fixnum)getVersion()).value, base).toUpperCase());
      else if (getVersion() instanceof Bignum)
        sb.append(((Bignum)getVersion()).value.toString(base).toUpperCase());
    } else if (getVersion() == Keyword.WILD) {
      sb.append(".*");
    } else if (getVersion() == Keyword.NEWEST) {
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
                if (LogicalPathname.TRANSLATIONS.get(new SimpleString(h)) != null) {
                    // A defined logical pathname host.
                    return LogicalPathname.create(h, s.substring(s.indexOf(':') + 1));
                }
            }
            return error(new TypeError("Logical namestring does not specify a host: \"" + s + '"'));
        }
    }

  // "one or more uppercase letters, digits, and hyphens"
  protected static String getHostString(String s) {
    int colon = s.indexOf(':');
    if (colon >= 0) {
      return s.substring(0, colon).toUpperCase();
    } else {
      return null;
    }
  }

    public long getLastModified() {
        Pathname p = translateLogicalPathname(this);
        return p.getLastModified();
    }
}
