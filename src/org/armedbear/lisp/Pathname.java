/* 
 * Pathname.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

public class Pathname extends LispObject
  implements Serializable
{
  protected static Pathname create() {
    return new Pathname();
  }

  public static Pathname create(Pathname p) {
    if (p instanceof JarPathname) {
      return JarPathname.create((JarPathname)p);
    } else if (p instanceof URLPathname) {
      return URLPathname.create((URLPathname)p);
    } else if (p instanceof LogicalPathname) {
      return LogicalPathname.create((LogicalPathname)p);
    } else {
      return new Pathname((Pathname)p);
    }
  }

  public static Pathname create(String s) {
    // TODO distinguish between logical hosts and schemes for URLs
    // which we can meaningfully parse.

    if (s.startsWith(JarPathname.JAR_URI_PREFIX)) {
        return JarPathname.create(s);
    }
    if (isValidURL(s)) {
      return URLPathname.create(s);
    } else {
      if (LogicalPathname.isValidLogicalPathname(s)) {
        return LogicalPathname.create(s);
      }
    }
    Pathname result = Pathname.init(s);

    return result;
  }

  public static Pathname create(String s, String host) {
    return LogicalPathname.create(s, host);
  }

  protected LispObject host = NIL;
  public LispObject getHost() {
    return host;
  }
  public Pathname setHost(LispObject host) {
    this.host = host;
    return this;
  }

  protected LispObject device = NIL;
  public final LispObject getDevice() {
    return device;
  }
  public Pathname setDevice(LispObject device) {
    this.device = device;
    return this;
  }

  protected LispObject directory = NIL;
  public LispObject getDirectory() {
    return directory;
  }
  public Pathname setDirectory(LispObject directory) {
    this.directory = directory;
    return this;
  }

  protected LispObject name = NIL;
  public LispObject getName() {
    return name;
  }
  public Pathname setName(LispObject name) {
    this.name = name;
    return this;
  }

  /**  A string, NIL, :WILD or :UNSPECIFIC. */
  protected LispObject type = NIL;
  public LispObject getType() {
    return type;
  }
  public Pathname setType(LispObject type) {
    this.type = type;
    return this;
  }

  /** A positive integer, or NIL, :WILD, :UNSPECIFIC, or :NEWEST. */
  protected LispObject version = NIL;
  public LispObject getVersion() {
    return version;
  }

  public Pathname setVersion(LispObject version) {
    this.version = version;
    return this;
  }

  /** 
   * The path component separator used by internally generated
   * path namestrings.
   */
  public final static char directoryDelimiter = '/';
    

  // If we don't declare the no-arg constructor protected, then
  // inheriting classes cannot invoke their constructors !?!
  protected Pathname() {}

  private Pathname(Pathname p) {
    /** Copy constructor which shares no structure with the original. */
      copyFrom(p);
    }

  /** 
   *  Coerces type between descendents of Pathname types by copying structure
   */
  static public LispObject ncoerce(Pathname orig, Pathname dest) {
    return dest.copyFrom(orig); 
  }

  /**
   *  Create a deep copy of all the information referenced by p
   */
  Pathname copyFrom(Pathname p) {
        if (p.host != NIL) {
          LispObject pHost = p.getHost();
          if (pHost instanceof SimpleString) {
              setHost(new SimpleString(pHost.getStringValue()));
            } else if (pHost instanceof Symbol) {
              setHost(pHost);
          } else if (pHost instanceof Cons) {
              LispObject newHost = NIL;
              LispObject components = pHost.reverse();
              while (!components.car().equals(NIL)) {
                LispObject copy = components.car(); // TODO actually make a copy?
                newHost = newHost.push(copy);
                components = components.cdr();
              }
              setHost(newHost);
            } else {
              simple_error("Failed to copy host in pathname ~a", p);
            }
        }
        if (p.device != NIL) {
            if (p.device instanceof SimpleString) {
                device = new SimpleString(((SimpleString)p.getDevice()).getStringValue());
            } else if (p.getDevice() instanceof Cons) {
              LispObject jars = p.getDevice();
              setDevice(NIL);
              URLPathname root = null;
              Pathname rootPathname = (Pathname) jars.car();
              if (rootPathname instanceof URLPathname) {
                root = URLPathname.create((URLPathname)rootPathname); 
              } else {
                root = URLPathname.create((Pathname)rootPathname);
              }
              device = device.push(root);
              jars = jars.cdr();
              while (jars.car() != NIL) {
                Pathname jar
                  = (Pathname) Pathname.create(((Pathname)jars.car()).getNamestring());
                device = device.push(jar);
                jars = jars.cdr();
              }
              device.nreverse();
            } else if (p.device instanceof Symbol) { // When is this the case?
                device = p.device;
            } else {
              simple_error("Failed to copy device in pathname ~a", p);
            }                
        }
        if (p.directory != NIL) {
            if (p.directory instanceof Cons) {
                directory = NIL;
                for (LispObject list = p.directory; list != NIL; list = list.cdr()) {
                    LispObject o = list.car();
                    if (o instanceof Symbol) {
                        directory = directory.push(o);
                    } else if (o instanceof SimpleString) {
                        directory = directory.push(new SimpleString(((SimpleString)o).getStringValue()));
                    } else {
                        Debug.assertTrue(false);
                    }
                }
                directory.nreverse();
            } else {
              simple_error("Failed to copy directory in pathname ~a", p);
            }
        }
        if (p.name != NIL) {
            if (p.name instanceof SimpleString) {
                name = new SimpleString(((SimpleString)p.getName()).getStringValue());
            } else if (p.name instanceof Symbol) {
                name = p.name;
            } else {
              simple_error("Failed to copy name in pathname ~a", p);
            }
        } 
        if (p.type != NIL) {
            if (p.type instanceof SimpleString) {
                type = new SimpleString(((SimpleString)p.getType()).getStringValue());
            } else if (p.type instanceof Symbol) {
                type = p.type;
            } else {
              simple_error("Failed to copy type in pathname ~a", p);
            }
        }
    if (p.version != NIL) {
        if (p.version instanceof Symbol) {
        version = p.version;
        } else if (p.version instanceof LispInteger) {
        version = p.version;
        } else {
          simple_error("Failed to copy version in pathname ~a", p);
        }
    }
    return this;
  }

    public static boolean isSupportedProtocol(String protocol) {
        // There is no programmatic way to know what protocols will
        // sucessfully construct a URL, so we check for well known ones...
        if ("jar".equals(protocol) 
            || "file".equals(protocol))
            //            || "http".equals(protocol))  XXX remove this as an optimization
            {
                return true;
            }
        // ... and try the entire constructor with some hopefully
        // reasonable parameters for everything else.
        try {
            new URL(protocol, "example.org", "foo");
            return true;
        }  catch (MalformedURLException e) {
            return false;
        }
    }

  private static final Pathname init(String s) { 
    Pathname result = new Pathname();
    if (s == null) {
      return (Pathname)parse_error("Refusing to create a PATHNAME for the null reference.");
    }
    if (s.equals(".") || s.equals("./")
        || (Utilities.isPlatformWindows && s.equals(".\\"))) {
      result.setDirectory(new Cons(Keyword.RELATIVE));
      return result;
    } 
    if (s.startsWith("./"))
      { s = s.substring(2); }
    if (s.equals("..") || s.equals("../")) {
      result.setDirectory(list(Keyword.RELATIVE, Keyword.UP));
      return result;
    }
    // UNC Windows shares
    if (Utilities.isPlatformWindows) {
      if (s.startsWith("\\\\") || s.startsWith("//")) { 
        // UNC path support
        int shareIndex;
        int dirIndex;
        // match \\<server>\<share>\[directories-and-files]
        if (s.startsWith("\\\\")) {
          shareIndex = s.indexOf('\\', 2);
          dirIndex = s.indexOf('\\', shareIndex + 1);
          // match //<server>/<share>/[directories-and-files]
        } else {
          shareIndex = s.indexOf('/', 2);
          dirIndex = s.indexOf('/', shareIndex + 1);
        }
        if (shareIndex == -1 || dirIndex == -1) {
          return (Pathname)parse_error("Unsupported UNC path format: \"" + s + '"');
        }
        
        result
          .setHost(new SimpleString(s.substring(2, shareIndex)))
          .setDevice(new SimpleString(s.substring(shareIndex + 1, dirIndex)));

        Pathname p = (Pathname)Pathname.create(s.substring(dirIndex));
        result
          .setDirectory(p.getDirectory())
          .setName(p.getName())
          .setType(p.getType())
          .setVersion(p.getVersion());
        return result;
      }
    }
        
    // A JAR file
    if (s.startsWith(JarPathname.JAR_URI_PREFIX)
        && s.endsWith(JarPathname.JAR_URI_SUFFIX)) {
      return (JarPathname)JarPathname.create(s);
    }

    // An entry in a JAR file
    final int separatorIndex = s.lastIndexOf(JarPathname.JAR_URI_SUFFIX);
    if (separatorIndex > 0 && s.startsWith(JarPathname.JAR_URI_PREFIX)) {
      return (JarPathname)JarPathname.create(s);
    }
    
    // A URL (anything with a scheme that is not a logical
    // pathname, and not a JAR file or an entry in a JAR file)
    if (isValidURL(s)) {
      return (URLPathname)URLPathname.create(s);
    }

    // Normalize path separators to forward slashes
    if (Utilities.isPlatformWindows) {
      if (s.contains("\\")) {
        s = s.replace("\\", "/");
      } 
    }

    // Expand user home directories
    if (Utilities.isPlatformUnix) {
      if (s.equals("~")) {
        s = System.getProperty("user.home").concat("/");
      } else if (s.startsWith("~/")) {
        s = System.getProperty("user.home").concat(s.substring(1));
      }
    }

    // possible MSDOS device
    if (Utilities.isPlatformWindows) {
      if (s.length() >= 2 && s.charAt(1) == ':') {
        result.setDevice(new SimpleString(s.charAt(0)));
        s = s.substring(2);
      }
    }

    String d = null;
    // Find last file separator char.
    for (int i = s.length(); i-- > 0;) {
      if (s.charAt(i) == '/') {
        d = s.substring(0, i + 1);
        s = s.substring(i + 1);
        break;
      }
    }

    if (d != null) {
      if (s.equals("..")) {
        d = d.concat(s);
        s = "";
      }
      result.setDirectory(parseDirectory(d));
    }

    int index = s.lastIndexOf('.');
    String name = null;
    String type = null;
    if (index > 0) {
      name = s.substring(0, index);
      type = s.substring(index + 1);
    } else if (s.length() > 0) {
      name = s;
    }
    if (name != null) {
      if (name.equals("*")) {
        result.setName(Keyword.WILD);
      } else {
        result.setName(new SimpleString(name));
      }
    }
    if (type != null) {
      if (type.equals("*")) {
        result.setType(Keyword.WILD);
      } else {
        result.setType(new SimpleString(type));
      }
    }
    return result;
  }

    private static final LispObject parseDirectory(String d) {
        if (d.equals("/") || (Utilities.isPlatformWindows && d.equals("\\"))) {
            return new Cons(Keyword.ABSOLUTE);
        }
        LispObject result;
        if (d.startsWith("/") || (Utilities.isPlatformWindows && d.startsWith("\\"))) {
            result = new Cons(Keyword.ABSOLUTE);
        } else {
            result = new Cons(Keyword.RELATIVE);
        }
        StringTokenizer st = new StringTokenizer(d, "/\\");
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            LispObject obj;
            if (token.equals("*")) {
                obj = Keyword.WILD;
            } else if (token.equals("**")) {
                obj = Keyword.WILD_INFERIORS;
            } else if (token.equals("..")) {
                if (result.car() instanceof AbstractString) {
                    result = result.cdr();
                    continue;
                }
                obj = Keyword.UP;
            } else {
                obj = new SimpleString(token);
            }
            result = new Cons(obj, result);
        }
        return result.nreverse();
    }

    @Override
    public LispObject getParts() {
        LispObject parts 
          = list(new Cons("HOST", getHost()),
                 new Cons("DEVICE", getDevice()),
                 new Cons("DIRECTORY", getDirectory()),
                 new Cons("NAME", getName()),
                 new Cons("TYPE", getType()),
                 new Cons("VERSION", getVersion()));
        return parts; 
    }

    @Override
    public LispObject typeOf() {
      if (isJar()) {
        return Symbol.JAR_PATHNAME;
      }
      if (isURL()) {
        return Symbol.URL_PATHNAME;
      } 
      return Symbol.PATHNAME;
    }

    @Override
    public LispObject classOf() {
      if (isJar()) {
        return BuiltInClass.JAR_PATHNAME;
      }
      if (isURL()) {
        return BuiltInClass.URL_PATHNAME;
      } 
      return BuiltInClass.PATHNAME;
    }

    @Override
    public LispObject typep(LispObject type) {
        if (type == Symbol.PATHNAME) {
            return T;
        }
        if (type == Symbol.JAR_PATHNAME && isJar()) {
            return T;
        }
        if (type == Symbol.URL_PATHNAME && isURL()) {
            return T;
        }
        if (type == BuiltInClass.PATHNAME) {
            return T;
        }
        if (type == BuiltInClass.JAR_PATHNAME && isJar()) {
            return T;
        }
        if (type == BuiltInClass.URL_PATHNAME && isURL()) {
            return T;
        }
        return super.typep(type);
    }

    public String getNamestring() {
        if (getDirectory() instanceof AbstractString) {
            Debug.assertTrue(false);
        }
        StringBuilder sb = new StringBuilder();
        // "If a pathname is converted to a namestring, the symbols NIL and
        // :UNSPECIFIC cause the field to be treated as if it were empty. That
        // is, both NIL and :UNSPECIFIC cause the component not to appear in
        // the namestring." 19.2.2.2.3.1
        if (getHost() != NIL) {
            Debug.assertTrue(getHost() instanceof AbstractString 
                             || isURL());
            if (isURL()) {
                LispObject scheme = Symbol.GETF.execute(getHost(), URLPathname.SCHEME, NIL);
                LispObject authority = Symbol.GETF.execute(getHost(), URLPathname.AUTHORITY, NIL);
                Debug.assertTrue(scheme != NIL);
                sb.append(scheme.getStringValue());
                sb.append(":");
                if (authority != NIL) {
                    sb.append("//");
                    sb.append(authority.getStringValue());
                }
            } else if (this instanceof LogicalPathname) {
                sb.append(getHost().getStringValue());
                sb.append(':');
            } else { 
              // A UNC path
              sb.append("//").append(getHost().getStringValue()).append("/");
            }
        }

        if (getDevice().equals(NIL)
            || getDevice().equals(Keyword.UNSPECIFIC)) {
          // nothing emitted for device
        } else if (getDevice() instanceof AbstractString) {
            sb.append(getDevice().getStringValue());
            if (this instanceof LogicalPathname
                || getHost() == NIL) {
              sb.append(':'); // non-UNC paths
            }
        } else {
          simple_error("Transitional error in pathname: should be a JAR-PATHNAME", this);
        }

        String directoryNamestring = getDirectoryNamestring();
        sb.append(directoryNamestring);

        if (getName() instanceof AbstractString) {
            String n = getName().getStringValue();
            if (n.indexOf('/') >= 0) {
                return null;
            }
            sb.append(n);
        } else if (getName() == Keyword.WILD) {
            sb.append('*');
        }
        
        if (getType() != NIL && getType() != Keyword.UNSPECIFIC) {
            sb.append('.');
            if (getType() instanceof AbstractString) {
                String t = getType().getStringValue();
                // Allow Windows shortcuts to include TYPE
                if (!(t.endsWith(".lnk") && Utilities.isPlatformWindows)) {
                    if (t.indexOf('.') >= 0) {
                        return null;
                    }
                }
                sb.append(t);
            } else if (getType() == Keyword.WILD) {
                sb.append('*');
            } else {
                Debug.assertTrue(false);
            }
        }
            
        if (this instanceof LogicalPathname) {
            if (getVersion().integerp()) {
                sb.append('.');
                int base = Fixnum.getValue(Symbol.PRINT_BASE.symbolValue());
                if (getVersion() instanceof Fixnum) {
                    sb.append(Integer.toString(((Fixnum) getVersion()).value, base).toUpperCase());
                } else if (getVersion() instanceof Bignum) {
                    sb.append(((Bignum) getVersion()).value.toString(base).toUpperCase());
                }
            } else if (getVersion() == Keyword.WILD) {
                sb.append(".*");
            } else if (getVersion() == Keyword.NEWEST) {
                sb.append(".NEWEST");
            }
        }
        return sb.toString();
    }

    protected String getDirectoryNamestring() {
        validateDirectory(true);
        StringBuilder sb = new StringBuilder();
        // "If a pathname is converted to a namestring, the symbols NIL and
        // :UNSPECIFIC cause the field to be treated as if it were empty. That
        // is, both NIL and :UNSPECIFIC cause the component not to appear in
        // the namestring." 19.2.2.2.3.1
        if (getDirectory() != NIL && getDirectory() != Keyword.UNSPECIFIC) {
            LispObject temp = getDirectory();
            LispObject part = temp.car();
            temp = temp.cdr();
            if (part == Keyword.ABSOLUTE) {
                sb.append(directoryDelimiter);
            } else if (part == Keyword.RELATIVE) {
                if (temp == NIL) {
                    // #p"./"
                    sb.append('.');
                    sb.append(directoryDelimiter);
                }
                // else: Nothing to do.
            } else {
                error(new FileError("Unsupported directory component "
                  + part.printObject() + ".",
                  this));
            }
            while (temp != NIL) {
                part = temp.car();
                if (part instanceof AbstractString) {
                    sb.append(part.getStringValue());
                } else if (part == Keyword.WILD) {
                    sb.append('*');
                } else if (part == Keyword.WILD_INFERIORS) {
                    sb.append("**");
                } else if (part == Keyword.UP) {
                    sb.append("..");
                }
                sb.append(directoryDelimiter);
                temp = temp.cdr();
            }
        }
        return sb.toString();
    }


    @Override
    public boolean equal(LispObject obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof Pathname) {
            Pathname p = (Pathname) obj;
            if (Utilities.isPlatformWindows) {
                if (!host.equalp(p.host)) {
                    return false;
                }
                if (!device.equalp(p.device)) {
                    return false;
                }
                if (!directory.equalp(p.directory)) {
                    return false;
                }
                if (!name.equalp(p.name)) {
                    return false;
                }
                if (!type.equalp(p.type)) {
                    return false;
                }
                // Ignore version component.
                //if (!version.equalp(p.version))
                //    return false;
            } else {
                // Unix.
                if (!host.equal(p.host)) {
                    return false;
                }
                if (!device.equal(p.device)) {
                    return false;
                }
                if (!directory.equal(p.directory)) {
                    return false;
                }
                if (!name.equal(p.name)) {
                    return false;
                }
                if (!type.equal(p.type)) {
                    return false;
                }
                // Ignore version component.
                //if (!version.equal(p.version))
                //    return false;
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean equalp(LispObject obj) {
        return equal(obj);
    }

    public boolean equals(Object o) {
      if (!(this.getClass().isAssignableFrom(o.getClass()))) {
        return super.equals(o);
      }
      return equal((Pathname)o);
    }

    public int hashCode() {
      return sxhash();
    }

    @Override
    public int sxhash() {
        return ((getHost().sxhash()
          ^ getDevice().sxhash()
          ^ getDirectory().sxhash()
          ^ getName().sxhash()
          ^ getType().sxhash()) & 0x7fffffff);
    }

    @Override
    public String printObject() {
        final LispThread thread = LispThread.currentThread();
        final boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
        final boolean printEscape = (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
        boolean useNamestring;
        String s = null;
        s = getNamestring();
        if (s != null) {
            useNamestring = true;
            if (printReadably) {
                // We have a namestring. Check for pathname components that
                // can't be read from the namestring.
                if ((getHost() != NIL && !isURL())
                    || getVersion() != NIL) 
                {
                    useNamestring = false;
                } else if (getName() instanceof AbstractString) {
                    String n = getName().getStringValue();
                    if (n.equals(".") || n.equals("..")) {
                        useNamestring = false;
                        // ??? File.separatorChar is platform dependent.  Does this help on Windows?
                    } else if (n.indexOf(File.separatorChar) >= 0) {
                        useNamestring = false;
                    }
                }
            }
        } else { 
            useNamestring = false;
        }
        StringBuilder sb = new StringBuilder();

        if (useNamestring) {
            if (printReadably || printEscape) {
                sb.append("#P\"");
            }
            final int limit = s.length();
            for (int i = 0; i < limit; i++) {
                char c = s.charAt(i);
                if (printReadably || printEscape) {
                    if (c == '\"' || c == '\\') {
                        sb.append('\\');
                    }
                }
                sb.append(c);
            }
            if (printReadably || printEscape) {
                sb.append('"');
            }
            return sb.toString();
        } 

        sb.append("PATHNAME (with no namestring) ");
        if (getHost() != NIL) {
            sb.append(":HOST ")
              .append(getHost().printObject())
              .append(" ");
        }
        if (getDevice() != NIL) {
            sb.append(":DEVICE ")
              .append(getDevice().printObject())
              .append(" ");
        }
        if (getDirectory() != NIL) {
            sb.append(":DIRECTORY ")
              .append(getDirectory().printObject())
              .append(" ");
        }
        if (getName() != NIL) {
            sb.append(":NAME ")
              .append(getName().printObject())
              .append(" ");
        }
        if (getType() != NIL) {
            sb.append(":TYPE ")
              .append(getType().printObject())
              .append(" ");
        }
        if (getVersion() != NIL) {
            sb.append(":VERSION ")
              .append(getVersion().printObject())
              .append(" ");
        }
        if (sb.charAt(sb.length() - 1) == ' ') { 
            sb.setLength(sb.length() - 1);
        }

        return unreadableString(sb.toString());
    }

    public static Pathname parseNamestring(String s) {
      return (Pathname)Pathname.create(s);
    }

    public static boolean isValidURL(String s) {
        // On Windows, the scheme "[A-Z]:.*" is ambiguous; reject as urls
        // This special case reduced exceptions while compiling Maxima by 90%+
        if (Utilities.isPlatformWindows && s.length() >= 2 && s.charAt(1) == ':') {
            char c = s.charAt(0);
            if (('A' <= s.charAt(0) && s.charAt(0) <= 'Z')
                    || ('a' <= s.charAt(0) && s.charAt(0) <= 'z'))
                return false;
        }

        if (s.indexOf(':') == -1) // no schema separator; can't be valid
            return false;
        
        try {
            URL url = new URL(s);
        } catch (MalformedURLException e) {
          return false; 
        }
        return true;
    }

    public static LispObject parseNamestring(AbstractString namestring) {
        // Check for a logical pathname host.
        String s = namestring.getStringValue();
        if (!isValidURL(s)) {
            String h = LogicalPathname.getHostString(s);
            if (h != null
                && LogicalPathname.TRANSLATIONS.get(new SimpleString(h)) != null) {
                // A defined logical pathname host.
                return LogicalPathname.create(h, s.substring(s.indexOf(':') + 1));
            }
        }
        return Pathname.create(s);
    }

    public static LogicalPathname parseNamestring(AbstractString namestring,
                                                  AbstractString host) 
    {
        String s = namestring.getStringValue();

        // Look for a logical pathname host in the namestring.        
        String h = LogicalPathname.getHostString(s);
        if (h != null) {
            if (!h.equals(host.getStringValue())) {
                error(new LispError("Host in " + s
                  + " does not match requested host "
                  + host.getStringValue()));
                // Not reached.
                return null;
            }
            // Remove host prefix from namestring.
            s = s.substring(s.indexOf(':') + 1);
        }
        if (LogicalPathname.TRANSLATIONS.get(host) != null) {
            // A defined logical pathname host.
            return LogicalPathname.create(host.getStringValue(), s);
        }
        error(new LispError(host.princToString() + " is not defined as a logical pathname host."));
        // Not reached.
        return null;
    }

    static final void checkCaseArgument(LispObject arg) {
        if (arg != Keyword.COMMON && arg != Keyword.LOCAL) {
            type_error(arg, list(Symbol.MEMBER,
                                 Keyword.COMMON, Keyword.LOCAL));
        }
    }

    private static final Primitive _PATHNAME_HOST = new pf_pathname_host();
    @DocString(name="%pathname-host")
    private static class pf_pathname_host extends Primitive {
        pf_pathname_host() {
            super("%pathname-host", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second); // FIXME Why is this ignored?
            return coerceToPathname(first).getHost();
        }
    }
    private static final Primitive _PATHNAME_DEVICE = new pf_pathname_device(); 
    @DocString(name="%pathname-device")
    private static class pf_pathname_device extends Primitive {
        pf_pathname_device() {
            super("%pathname-device", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second); // FIXME Why is this ignored?
            return coerceToPathname(first).getDevice();
        }
    }
    private static final Primitive _PATHNAME_DIRECTORY = new pf_pathname_directory();
    @DocString(name="%pathname-directory")
    private static class pf_pathname_directory extends Primitive {
        pf_pathname_directory() {
            super("%pathname-directory", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second); // FIXME Why is this ignored?
            return coerceToPathname(first).getDirectory();
        }
    }
    private static final Primitive _PATHNAME_NAME = new pf_pathname_name();
    @DocString(name="%pathname-name")
    private static class  pf_pathname_name extends Primitive {
        pf_pathname_name() {
            super ("%pathname-name", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second); // FIXME Why is this ignored?
            return coerceToPathname(first).getName();
        }
    }
    private static final Primitive _PATHNAME_TYPE = new pf_pathname_type();
    @DocString(name="%pathname-type")
    private static class pf_pathname_type extends Primitive {
        pf_pathname_type() {
            super("%pathname-type", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second); // FIXME Why is this ignored?
            return coerceToPathname(first).getType();
        }
    }
    
    private static final Primitive PATHNAME_VERSION = new pf_pathname_version();
    @DocString(name="pathname-version",
               args="pathname",
               returns="version",
               doc="Return the version component of PATHNAME.")
    private static class pf_pathname_version extends Primitive {
        pf_pathname_version() {
            super("pathname-version", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPathname(arg).getVersion();
        }
    }
    private static final Primitive NAMESTRING = new pf_namestring();
    @DocString(name="namestring",
               args="pathname",
               returns="namestring",
    doc="Returns the NAMESTRING of PATHNAME if it has one.\n"
      + "\n"
      + "If PATHNAME is of type url-pathname or jar-pathname the NAMESTRING is encoded\n"
      + "according to the uri percent escape rules.\n"
      + "\n"
      + "Signals an error if PATHNAME lacks a printable NAMESTRING representation.\n")
    private static class pf_namestring extends Primitive {
        pf_namestring() {
            super("namestring", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname pathname = coerceToPathname(arg);
            String namestring = pathname.getNamestring();
            if (namestring == null) {
                error(new SimpleError("Pathname has no namestring: "
                                      + pathname.princToString()));
            }
            return new SimpleString(namestring);
        }
    }
    
    private static final Primitive DIRECTORY_NAMESTRING = new pf_directory_namestring();
    // TODO clarify uri encoding rules in implementation, then document
    @DocString(name="directory-namestring",
               args="pathname",
               returns="namestring",
    doc="Returns the NAMESTRING of directory porition of PATHNAME if it has one.")
    private static class pf_directory_namestring extends Primitive {
        pf_directory_namestring() {
            super("directory-namestring", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return new SimpleString(coerceToPathname(arg).getDirectoryNamestring());
        }
    }
    private static final Primitive PATHNAME = new pf_pathname();
    @DocString(name="pathname",
               args="pathspec",
               returns="pathname",
               doc="Returns the PATHNAME denoted by PATHSPEC.")
    private static class pf_pathname extends Primitive {
        pf_pathname() {
            super("pathname", "pathspec");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPathname(arg);
        }
    }
    private static final Primitive _PARSE_NAMESTRING = new pf_parse_namestring();
    @DocString(name="%parse-namestring",
               args="namestring host default-pathname",
               returns="pathname, position")
    private static class pf_parse_namestring extends Primitive {
        pf_parse_namestring() {
            super("%parse-namestring", PACKAGE_SYS, false,
                  "namestring host default-pathname");
        }
        @Override
        public LispObject execute(LispObject first, LispObject second, LispObject third) {
            final LispThread thread = LispThread.currentThread();
            final AbstractString namestring = checkString(first);
            // The HOST parameter must be a string or NIL.
            if (second == NIL) {
                // "If HOST is NIL, DEFAULT-PATHNAME is a logical pathname, and
                // THING is a syntactically valid logical pathname namestring
                // without an explicit host, then it is parsed as a logical
                // pathname namestring on the host that is the host component
                // of DEFAULT-PATHNAME."
                third = coerceToPathname(third);
                if (third instanceof LogicalPathname) {
                    second = ((LogicalPathname) third).getHost();
                } else {
                    return thread.setValues(parseNamestring(namestring),
                                            namestring.LENGTH());
                }
            }
            Debug.assertTrue(second != NIL);
            final AbstractString host = checkString(second);
            return thread.setValues(parseNamestring(namestring, host),
                                    namestring.LENGTH());
        }
    }
    private static final Primitive MAKE_PATHNAME = new pf_make_pathname();
    @DocString(name="make-pathname",
               args="&key host device directory name type version defaults case",
               returns="pathname",
    doc="Constructs and returns a pathname from the supplied keyword arguments.")
    private static class pf_make_pathname extends Primitive {
        pf_make_pathname() {
            super("make-pathname",
                  "&key host device directory name type version defaults case");
        }
        @Override
        public LispObject execute(LispObject[] args) {
          LispObject result = _makePathname(args);
          return result;
        }
    }

    // Used by the #p reader.
    public static final Pathname makePathname(LispObject args) {
      return (Pathname) _makePathname(args.copyToArray());
    }

    public static final Pathname makePathname(File file) {
        String namestring = null;
        try {
            namestring = file.getCanonicalPath();
        } catch (IOException e) {
            Debug.trace("Failed to make a Pathname from "
              + "." + file + "'");
            return null;
        }
        return (Pathname)Pathname.create(namestring);
    }

    static final LispObject _makePathname(LispObject[] args) {
        if (args.length % 2 != 0) {
            program_error("Odd number of keyword arguments.");
        }
        LispObject host = NIL;
        LispObject device = NIL;
        LispObject directory = NIL;
        LispObject name = NIL;
        LispObject type = NIL;
        LispObject version = NIL;
        Pathname defaults = null;
        boolean hostSupplied = false;
        boolean deviceSupplied = false;
        boolean nameSupplied = false;
        boolean typeSupplied = false;
        boolean directorySupplied = false;
        boolean versionSupplied = false;
        for (int i = 0; i < args.length; i += 2) {
            LispObject key = args[i];
            LispObject value = args[i + 1];
            if (key == Keyword.HOST) {
                host = value;
                hostSupplied = true;
            } else if (key == Keyword.DEVICE) {
                device = value;
                deviceSupplied = true;
                if (!(value instanceof AbstractString
                      || value.equals(Keyword.UNSPECIFIC)
                      || value.equals(NIL)
                      || value instanceof Cons)) {
                  return type_error("DEVICE is not a string, :UNSPECIFIC, NIL, or a list.",
                                    value,
                                    list(Symbol.OR,
                                         Symbol.STRING, Keyword.UNSPECIFIC, NIL, Symbol.CONS));
                }
            } else if (key == Keyword.DIRECTORY) {
                directorySupplied = true;
                if (value instanceof AbstractString) {
                    directory = list(Keyword.ABSOLUTE, value);
                } else if (value == Keyword.WILD) {
                    directory = list(Keyword.ABSOLUTE, Keyword.WILD);
                } else {
                  // a valid pathname directory is a string, a list of
                  // strings, nil, :wild, :unspecific
                  //
                  // ??? would be nice to (deftype pathname-arg ()
                  // '(or (member :wild :unspecific) string (and cons
                  // ,(mapcar ...  Is this possible?
                  if ((value instanceof Cons 
                       // XXX check that the elements of a list are themselves valid
                       || value == Keyword.UNSPECIFIC
                       || value.equals(NIL))) {
                      directory = value;
                  } else {
                    return
                      type_error("DIRECTORY argument not a string, list of strings, nil, :WILD, or :UNSPECIFIC.",
                                 value,
                                 list(Symbol.OR,
                                      NIL, Symbol.STRING, Symbol.CONS, Keyword.WILD, Keyword.UNSPECIFIC));
                  }
                }
            } else if (key == Keyword.NAME) {
                name = value;
                nameSupplied = true;
            } else if (key == Keyword.TYPE) {
                type = value;
                typeSupplied = true;
            } else if (key == Keyword.VERSION) {
                version = value;
                versionSupplied = true;
            } else if (key == Keyword.DEFAULTS) {
                defaults = coerceToPathname(value);
            } else if (key == Keyword.CASE) {
                // Ignored.
            }
        }
        if (defaults != null) {
            if (!hostSupplied) {
                host = defaults.getHost();
            }
            if (!directorySupplied) {
                directory = defaults.getDirectory();
            }
            if (!deviceSupplied) {
                device = defaults.getDevice();
            }
            if (!nameSupplied) {
                name = defaults.getName();
            }
            if (!typeSupplied) {
                type = defaults.getType();
            }
            if (!versionSupplied) {
                version = defaults.getVersion();
            }
        }
        Pathname p; // Pathname is always created in following
                    // resolution for values of HOST
        LispObject logicalHost = NIL;
        if (host != NIL) {
            if (host instanceof AbstractString) {
                logicalHost = LogicalPathname.canonicalizeStringComponent((AbstractString) host);
            }
            if (LogicalPathname.TRANSLATIONS.get(logicalHost) == null) {
                // Not a defined logical pathname host -- A UNC path
                //warning(new LispError(host.printObject() + " is not defined as a logical pathname host."));
                p = Pathname.create();
                p.setHost(host);
            } else { 
                p = LogicalPathname.create();
                p.setHost(logicalHost);
            }
            if (!Utilities.isPlatformWindows) {
              p.setDevice(Keyword.UNSPECIFIC);
            }
        } else {
            p = Pathname.create();
        }
        
        if (device != NIL) {
            if (p instanceof LogicalPathname) {
                // "The device component of a logical pathname is always :UNSPECIFIC."
                if (device != Keyword.UNSPECIFIC) {
                  return type_error("The device component of a logical pathname must be :UNSPECIFIC.",
                                    p.getDevice(), Keyword.UNSPECIFIC);
                }
            } else {
              if (device instanceof Cons) {
                LispObject normalizedDevice = NIL;
                if (device.car() instanceof SimpleString) {
                  String rootNamestring = device.car().getStringValue();
                  URLPathname root = new URLPathname();
                  if (!isValidURL(rootNamestring)) {
                    Pathname rootPathname = Pathname.create(rootNamestring);
                    root = URLPathname.createFromFile(rootPathname);
                  } else {
                    root = URLPathname.create(rootNamestring);
                  }
                  normalizedDevice = normalizedDevice.push(root);
                } else {
                  normalizedDevice = normalizedDevice.push(device.car());
                }
                LispObject o = device.cdr();
                while (!o.car().equals(NIL)) {
                  Pathname next = coerceToPathname(o.car());
                  normalizedDevice = normalizedDevice.push(next);
                  o = o.cdr();
                }
                normalizedDevice = normalizedDevice.nreverse();
                p.setDevice(normalizedDevice);
              } else {
                p.setDevice(device);
              }
            }
        }
        if (directory != NIL) {
            if (p instanceof LogicalPathname) {
                if (directory.listp()) {
                    LispObject d = NIL;
                    while (directory != NIL) {
                        LispObject component = directory.car();
                        if (component instanceof AbstractString) {
                            d = d.push(LogicalPathname.canonicalizeStringComponent((AbstractString) component));
                        } else {
                            d = d.push(component);
                        }
                        directory = directory.cdr();
                    }
                    p.setDirectory(d.nreverse());
                } else if (directory == Keyword.WILD || directory == Keyword.WILD_INFERIORS) {
                  p.setDirectory(directory);
                } else {
                    error(new LispError("Invalid directory component for logical pathname: " + directory.princToString()));
                }
            } else {
              p.setDirectory(directory);
            }
        }
        if (name != NIL) {
            if (p instanceof LogicalPathname && name instanceof AbstractString) {
              p.setName(LogicalPathname.canonicalizeStringComponent((AbstractString) name));
            } else if (name instanceof AbstractString) {
              p.setName(validateStringComponent((AbstractString) name));
            } else {
              p.setName(name);
            }
        }
        if (type != NIL) {
            if (p instanceof LogicalPathname && type instanceof AbstractString) {
              p.setType(LogicalPathname.canonicalizeStringComponent((AbstractString) type));
            } else {
              p.setType(type);
            }
        }
        
        p.setVersion(version);
        p.validateDirectory(true);

        // Possibly downcast type to JarPathname
        if (p.getDevice() instanceof Cons) {
          JarPathname result = new JarPathname();
          result.copyFrom(p);
          Pathname root = (Pathname)result.getDevice().car();
          URLPathname rootDevice = null;
          if (root instanceof URLPathname) {
            rootDevice = URLPathname.create((URLPathname)root);
          } else {
            rootDevice = URLPathname.create(root);
          }
          result.setDevice(new Cons(rootDevice, result.getDevice().cdr()));

          if (result.getDirectory().equals(NIL)
              && (!result.getName().equals(NIL)
                  || !result.getType().equals(NIL))) {
            result.setDirectory(NIL.push(Keyword.ABSOLUTE));
          }

          // sanity check that the pathname has been constructed correctly
          result.validateComponents();
          return result;
        }

        // Possibly downcast to URLPathname
        if (p.isURL()) {
          URLPathname result = new URLPathname();
          result.copyFrom(p);

          return result;
        }

        return p;
    }
        


    private static final AbstractString validateStringComponent(AbstractString s) {
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            // XXX '\\' should be illegal in all Pathnames at this point?
            if (c == '/' || c == '\\' && Utilities.isPlatformWindows) {
                error(new LispError("Invalid character #\\" + c
                  + " in pathname component \"" + s
                  + '"'));
                // Not reached.
                return null;
            }
        }
        return s;
    }

    private final boolean validateDirectory(boolean signalError) {
        LispObject temp = getDirectory();
        if (temp == Keyword.UNSPECIFIC) {
            return true;
        }
        while (temp != NIL) {
            LispObject first = temp.car();
            temp = temp.cdr();
            if (first == Keyword.ABSOLUTE || first == Keyword.WILD_INFERIORS) {
                LispObject second = temp.car();
                if (second == Keyword.UP || second == Keyword.BACK) {
                    if (signalError) {
                        StringBuilder sb = new StringBuilder();
                        sb.append(first.printObject());
                        sb.append(" may not be followed immediately by ");
                        sb.append(second.printObject());
                        sb.append('.');
                        error(new FileError(sb.toString(), this));
                    }
                    return false;
                }
            } else if (first != Keyword.RELATIVE
                       && first != Keyword.WILD
                       && first != Keyword.UP
                       && first != Keyword.BACK
                       && !(first instanceof AbstractString)) {
                if (signalError) {
                    error(new FileError("Unsupported directory component " + first.princToString() + ".",
                      this));
                }
                return false;
            }
        }
        return true;
    }
    private static final Primitive PATHNAMEP = new pf_pathnamep();
    @DocString(name="pathnamep",
               args="object",
               returns="generalized-boolean",
    doc="Returns true if OBJECT is of type pathname; otherwise, returns false.")
    private static class pf_pathnamep extends Primitive  {
        pf_pathnamep() {
            super("pathnamep", "object");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Pathname ? T : NIL;
        }
    }
    private static final Primitive LOGICAL_PATHNAME_P = new pf_logical_pathname_p();
    @DocString(name="logical-pathname-p",
               args="object",
               returns="generalized-boolean",

    doc="Returns true if OBJECT is of type logical-pathname; otherwise, returns false.")
    private static class pf_logical_pathname_p extends Primitive {
        pf_logical_pathname_p() {
            super("logical-pathname-p", PACKAGE_SYS, true, "object");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof LogicalPathname ? T : NIL;
        }
    }

    private static final Primitive USER_HOMEDIR_PATHNAME = new pf_user_homedir_pathname();
    @DocString(name="user-homedir-pathname",
               args="&optional host",
               returns="pathname",
    doc="Determines the pathname that corresponds to the user's home directory.\n"
      + "The value returned is obtained from the JVM system propoerty 'user.home'.\n"
      + "If HOST is specified, returns NIL.")
    private static class pf_user_homedir_pathname extends Primitive {
        pf_user_homedir_pathname() {
            super("user-homedir-pathname", "&optional host");
        }
        @Override
        public LispObject execute(LispObject[] args) {
            switch (args.length) {
            case 0: {
                String s = System.getProperty("user.home");
                if (!s.endsWith(File.separator)) {
                    s = s.concat(File.separator);
                }
                return Pathname.create(s);
            }
            case 1:
                return NIL; 
            default:
                return error(new WrongNumberOfArgumentsException(this, 0, 1));
            }
        }
    }

    private static final Primitive LIST_DIRECTORY = new pf_list_directory();
    @DocString(name="list-directory",
               args="directory &optional (resolve-symlinks nil)",
               returns="pathnames",
               doc="Lists the contents of DIRECTORY, optionally resolving symbolic links.")
    private static class pf_list_directory extends Primitive {
        pf_list_directory() {
            super("list-directory", PACKAGE_SYS, true, "directory &optional (resolve-symlinks t)");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return execute(arg, T);
        }
        @Override
        public LispObject execute(LispObject arg, LispObject resolveSymlinks) {
          Pathname pathname = coerceToPathname(arg);
          if (pathname instanceof LogicalPathname) {
            pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
          }

          LispObject result = NIL;
          if (pathname.isJar()) {
            return JarPathname.listDirectory((JarPathname)pathname);
          }

          File f = pathname.getFile();
          if (f.isDirectory()) {
            try {
              File[] files = f.listFiles();
              if (files == null) {
                return error(new FileError("Unable to list directory "
                                           + pathname.princToString() + ".",
                                           pathname));
              }
              for (int i = files.length; i-- > 0;) {
                File file = files[i];
                String path;
                if (resolveSymlinks == NIL) {
                  path = file.getAbsolutePath();
                } else {
                  path = file.getCanonicalPath();
                }
                if (file.isDirectory()
                    && !path.endsWith("/")) {
                  path += "/";
                }
                Pathname p;
                p = (Pathname)Pathname.create(path);
                result = new Cons(p, result);
              }
            } catch (IOException e) {
              return error(new FileError("Unable to list directory " 
                                         + pathname.princToString() + ".",
                                         pathname));
            } catch (SecurityException e) {
              return error(new FileError("Unable to list directory: " + e, pathname));
            }
          }
          return result;
        }
    };

    public boolean isAbsolute()  {
        if (!directory.equals(NIL) || !(directory == null)) {
            if (getDirectory() instanceof Cons) {
                if (((Cons)getDirectory()).car().equals(Keyword.ABSOLUTE)) {
                    return true;
                }
            }
        }
        return false;
    }

  // FIXME This should be named JAR-PATHNAME-P
    @DocString(name="pathname-jar-p",
               args="pathname",
               returns="generalized-boolean",
    doc="Predicate functionfor whether PATHNAME references a jar.")
    private static final Primitive PATHNAME_JAR_P = new pf_pathname_jar_p();
    private static class pf_pathname_jar_p extends Primitive {
        pf_pathname_jar_p() {
            super("pathname-jar-p", PACKAGE_EXT, true);
        }
        @Override
        public LispObject execute(LispObject arg) {
          if (arg instanceof Pathname) {
            Pathname p = coerceToPathname(arg);
            return p.isJar() ? T : NIL;
          } else {
            return NIL;
          }
        }
    }

    public boolean isJar() {
        return (getDevice() instanceof Cons);
    }

  /// FIXME should be named URL-PATHNAME-P
    @DocString(name="pathname-url-p",
               args="pathname",
               returns="generalized-boolean",
    doc="Predicate function for whether PATHNAME references a jaurl.")
    private static final Primitive PATHNAME_URL_P = new pf_pathname_url_p();
    private static class pf_pathname_url_p extends Primitive {
        pf_pathname_url_p() {
            super("pathname-url-p", PACKAGE_EXT, true, "pathname",
                  "Predicate for whether PATHNAME references a URL.");
        }
        @Override
        public LispObject execute(LispObject arg) {
          if (arg instanceof Pathname) {
            Pathname p = coerceToPathname(arg);
            return p.isURL() ? T : NIL;
          } else {
            return NIL;
          }
        }
    }

    public boolean isURL() {
      return (getHost() instanceof Cons);
    }

    public boolean isWild() {
        if (getHost() == Keyword.WILD || getHost() == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (getDevice() == Keyword.WILD || getDevice() == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (getDirectory() instanceof Cons) {
            if (memq(Keyword.WILD, getDirectory())) {
                return true;
            }
            if (memq(Keyword.WILD_INFERIORS, getDirectory())) {
                return true;
            }
            Cons d = (Cons) getDirectory();
            while (true) {
                if (d.car() instanceof AbstractString) {
                    String s = d.car().printObject();
                    if (s.contains("*")) {
                        return true;
                    }
                }
                if (d.cdr() == NIL || ! (d.cdr() instanceof Cons)) {
                    break;
                }
                d = (Cons)d.cdr();
            }
        }
        if (getName() == Keyword.WILD || getName() == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (getName() instanceof AbstractString) {
            if (getName().printObject().contains("*")) {
                return true;
            }
        }
        if (getType() == Keyword.WILD || getType() == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (getType() instanceof AbstractString) {
            if (getType().printObject().contains("*")) {
                return true;
            }
        }
        if (getVersion() == Keyword.WILD || getVersion() == Keyword.WILD_INFERIORS) {
            return true;
        }
        return false;
    }

    private static final Primitive _WILD_PATHNAME_P = new pf_wild_pathname_p();
    @DocString(name="%wild-pathname-p",
               args="pathname keyword",
               returns="generalized-boolean",
    doc="Predicate for determing whether PATHNAME contains wild components.\n"
      + "KEYWORD, if non-nil, should be one of :directory, :host, :device,\n"
      + ":name, :type, or :version indicating that only the specified component\n"
      + "should be checked for wildness.")
    static final class pf_wild_pathname_p extends Primitive {
        pf_wild_pathname_p() {
            super("%wild-pathname-p", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            Pathname pathname = coerceToPathname(first);
            if (second == NIL) {
                return pathname.isWild() ? T : NIL;
            }
            if (second == Keyword.DIRECTORY) {
                if (pathname.getDirectory() instanceof Cons) {
                    if (memq(Keyword.WILD, pathname.getDirectory())) {
                        return T;
                    }
                    if (memq(Keyword.WILD_INFERIORS, pathname.getDirectory())) {
                        return T;
                    }
                }
                return NIL;
            }
            LispObject value;
            if (second == Keyword.HOST) {
                value = pathname.getHost();
            } else if (second == Keyword.DEVICE) {
                value = pathname.getDevice();
            } else if (second == Keyword.NAME) {
                value = pathname.getName();
            } else if (second == Keyword.TYPE) {
                value = pathname.getType();
            } else if (second == Keyword.VERSION) {
                value = pathname.getVersion();
            } else {
                return program_error("Unrecognized keyword "
                                     + second.princToString() + ".");
            }
            if (value == Keyword.WILD || value == Keyword.WILD_INFERIORS) {
                return T;
            } else {
                return NIL;
            }
        }
    }
  
    static final Primitive MERGE_PATHNAMES = new pf_merge_pathnames();
    @DocString(name="merge-pathnames",
               args="pathname &optional default-pathname default-version",
               returns="pathname",
    doc="Constructs a pathname from PATHNAME by filling in any unsupplied components\n"
     +  "with the corresponding values from DEFAULT-PATHNAME and DEFAULT-VERSION.")
    static final class pf_merge_pathnames extends Primitive {
        pf_merge_pathnames() {
            super(Symbol.MERGE_PATHNAMES, "pathname &optional default-pathname default-version");
        }
        @Override
        public LispObject execute(LispObject arg) {
          Pathname pathname = coerceToPathname(arg);
          Pathname defaultPathname
            = coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue());
          LispObject defaultVersion = Keyword.NEWEST;
          return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            Pathname pathname = coerceToPathname(first);
            Pathname defaultPathname = coerceToPathname(second);
            LispObject defaultVersion = Keyword.NEWEST;
            return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third) {
            Pathname pathname = coerceToPathname(first);
            Pathname defaultPathname = coerceToPathname(second);
            LispObject defaultVersion = third;
            return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
    }

  public static final Pathname mergePathnames(Pathname pathname, Pathname defaultPathname) {
    return mergePathnames(pathname, defaultPathname, Keyword.NEWEST);
  }
    
  public static final Pathname mergePathnames(final Pathname pathname,
                                              final Pathname defaultPathname,
                                              final LispObject defaultVersion) {
    Pathname result;
    Pathname p = Pathname.create(pathname);
    Pathname d;

    if (pathname instanceof LogicalPathname) {
      result = LogicalPathname.create();
      d = Pathname.create(defaultPathname);
    } else {
      if (pathname instanceof JarPathname
          // If the defaults contain a JAR-PATHNAME, and the pathname
          // to be be merged is not a JAR-PATHNAME, does not have a
          // specified DEVICE or a specified HOST and has a NIL or
          // relative directory then the result will be a JAR-PATHNAME.
          || (defaultPathname instanceof JarPathname
              && !(pathname instanceof JarPathname)
              && pathname.getHost().equals(NIL)
              && pathname.getDevice().equals(NIL)
              && (pathname.getDirectory().equals(NIL)
                  || pathname.getDirectory().car().equals(Keyword.RELATIVE)))) {
        result = JarPathname.create();
      } else if (pathname instanceof URLPathname) {
        result = URLPathname.create();
      } else {
        result = Pathname.create();
      }
              
      if (defaultPathname instanceof LogicalPathname) {
        d = LogicalPathname.translateLogicalPathname((LogicalPathname) defaultPathname);
      } else {
        if (defaultPathname instanceof JarPathname) {
          d = JarPathname.create((JarPathname)defaultPathname);
        } else if (defaultPathname instanceof URLPathname) {
          d = URLPathname.create(defaultPathname);
        } else {
          d = Pathname.create(defaultPathname);
        }
      }
    }

    if (pathname.getHost().equals(NIL)) {
      result.setHost(d.getHost());
    } else {
      result.setHost(p.getHost());
    }

      if (!pathname.getDevice().equals(NIL)) {
        if (!Utilities.isPlatformWindows) {
          result.setDevice(p.getDevice());
        } else {
          if (d instanceof JarPathname
              && p instanceof JarPathname) {
            result.setDevice(d.getDevice());
          } else {
            result.setDevice(p.getDevice());
          }
        }
      } else {
        // If the defaults contain a JAR-PATHNAME, and the pathname
        // to be be merged is not a JAR-PATHNAME, does not have a
        // specified DEVICE, a specified HOST, and doesn't contain a
        // relative DIRECTORY, then on non-MSDOG, set its device to
        // :UNSPECIFIC.
        if ((d instanceof JarPathname)
            && !(result instanceof JarPathname)) {
          if (!Utilities.isPlatformWindows) {
            result.setDevice(Keyword.UNSPECIFIC);
          } else {
            result.setDevice(d.getDevice());
          }
        } else {
          if (p.isLocalFile()) {
            result.setDevice(d.getDevice());
          } else {
            result.setDevice(p.getDevice());
          }
        }
      }

      if (pathname.isJar()) {
        result.setDirectory(p.getDirectory());
      } else {
        result.setDirectory(mergeDirectories(p.getDirectory(), d.getDirectory()));
        // Directories are always absolute in a JarPathname
        if (result instanceof JarPathname) {
          LispObject directories = result.getDirectory();
          if ((!directories.car().equals(NIL))
              && directories.car().equals(Keyword.RELATIVE)) {
            directories = directories.cdr().push(Keyword.ABSOLUTE);
            result.setDirectory(directories);
          }
        }
      }
      
      if (pathname.getName() != NIL) {
        result.setName(p.getName());
      } else {
        result.setName(d.getName());
      }
      if (pathname.getType() != NIL) {
        result.setType(p.getType());
      } else {
        result.setType(d.getType());
      }

      // JAR-PATHNAME directories are always absolute
      if ((result instanceof JarPathname)
          && (!result.getName().equals(NIL)
              || !result.getType().equals(NIL))
          && result.getDirectory().equals(NIL)) {
        result.setDirectory(NIL.push(Keyword.ABSOLUTE));
      }
            
      //  CLtLv2 MERGE-PATHNAMES 
    
      // "[T]he missing components in the given pathname are filled
      // in from the defaults pathname, except that if no version is
      // specified the default version is used."

      // "The merging rules for the version are more complicated and
      // depend on whether the pathname specifies a name. If the
      // pathname doesn't specify a name, then the version, if not
      // provided, will come from the defaults, just like the other
      // components. However, if the pathname does specify a name,
      // then the version is not affected by the defaults. The
      // reason is that the version ``belongs to'' some other file
      // name and is unlikely to have anything to do with the new
      // one. Finally, if this process leaves the
      // version missing, the default version is used."
      if (p.getVersion() != NIL) {
        result.setVersion(p.getVersion());
      } else if (p.getName() == NIL) {
        if (defaultPathname.getVersion() == NIL) {
          result.setVersion(defaultVersion);
        } else {
          result.setVersion(defaultPathname.getVersion());
        }
      } else if (defaultVersion == NIL) {
        result.setVersion(p.getVersion());
      } 
      if (result.getVersion() == NIL) {
        result.setVersion(defaultVersion);
      }

      if (pathname instanceof LogicalPathname) {
        // When we're returning a logical
        result.setDevice(Keyword.UNSPECIFIC);
        if (result.getDirectory().listp()) {
          LispObject original = result.getDirectory();
          LispObject canonical = NIL;
          while (original != NIL) {
            LispObject component = original.car();
            if (component instanceof AbstractString) {
              component = LogicalPathname.canonicalizeStringComponent((AbstractString) component);
            }
            canonical = canonical.push(component);
            original = original.cdr();
          }
          result.setDirectory(canonical.nreverse());
        }
        if (result.getName() instanceof AbstractString) {
          result.setName(LogicalPathname.canonicalizeStringComponent((AbstractString) result.getName()));
        }
        if (result.getType() instanceof AbstractString) {
          result.setType(LogicalPathname.canonicalizeStringComponent((AbstractString) result.getType()));
        }
      }
      // Downcast to URLPathname if resolving a URLPathname
      if (result instanceof Pathname
          && URLPathname.hasExplicitFile(result)) {
        URLPathname downcastResult = new URLPathname();
        downcastResult.copyFrom(result);
        result = downcastResult;
      }
          
      return result;
    }

    private static final LispObject mergeDirectories(LispObject dir,
                                                     LispObject defaultDir) {
        if (dir == NIL) {
            return defaultDir;
        }
        if (dir.car() == Keyword.RELATIVE && defaultDir != NIL) {
            LispObject result = NIL;
            while (defaultDir != NIL) {
                result = new Cons(defaultDir.car(), result);
                defaultDir = defaultDir.cdr();
            }
            dir = dir.cdr(); // Skip :RELATIVE.
            while (dir != NIL) {
                result = new Cons(dir.car(), result);
                dir = dir.cdr();
            }
            LispObject[] array = result.copyToArray();
            for (int i = 0; i < array.length - 1; i++) {
                if (array[i] == Keyword.BACK) {
                    if (array[i + 1] instanceof AbstractString || array[i + 1] == Keyword.WILD) {
                        array[i] = null;
                        array[i + 1] = null;
                    }
                }
            }
            result = NIL;
            for (int i = 0; i < array.length; i++) {
                if (array[i] != null) {
                    result = new Cons(array[i], result);
                }
            }
            return result;
        }
        return dir;
    }

    public static LispObject truename(Pathname pathname) {
        return truename(pathname, false);
    }

    public static LispObject truename(LispObject arg) {
        return truename(arg, false);
    }

    public static LispObject truename(LispObject arg, boolean errorIfDoesNotExist) {
        final Pathname pathname = coerceToPathname(arg);
        return truename(pathname, errorIfDoesNotExist);
    }

    /** @return The canonical TRUENAME as a Pathname if the pathname
     * exists, otherwise returns NIL or possibly a subtype of
     * LispError if there are logical problems with the input.
     */
    public static LispObject truename(Pathname pathname,
                                      boolean errorIfDoesNotExist) {
      if (pathname == null || pathname.equals(NIL)) {  
        return doTruenameExit(pathname, errorIfDoesNotExist); 
      }
      if (pathname instanceof LogicalPathname) {
        pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
      }
      if (pathname.isWild()) {
        return error(new FileError("Fundamentally unable to find a truename for any wild pathname.",
                                   pathname));
      }
      Pathname result 
        = (Pathname)mergePathnames(pathname,
                                   coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
                                   NIL);
      final File file = result.getFile();
      if (file != null
          && file.exists()) {
        if (file.isDirectory()) {
          result = Pathname.getDirectoryPathname(file);
        } else {
          try {
            result = (Pathname)Pathname.create(file.getCanonicalPath());
          } catch (IOException e) {
            return error(new FileError(e.getMessage(), pathname));
          }
        }
        if (Utilities.isPlatformUnix) {
          result.setDevice(Keyword.UNSPECIFIC);
        }
        return result;
      }
      return doTruenameExit(pathname, errorIfDoesNotExist);
    }
    
    static LispObject doTruenameExit(Pathname pathname, boolean errorIfDoesNotExist) {
        if (errorIfDoesNotExist) {
            StringBuilder sb = new StringBuilder("The file ");
            sb.append(pathname.princToString());
            sb.append(" does not exist.");
            return error(new FileError(sb.toString(), pathname));
        }
        return NIL;
    }

  public static final Primitive GET_INPUT_STREAM = new pf_get_input_stream();
  @DocString(name="get-input-stream",
             args="pathname",
             doc="Returns a java.io.InputStream for resource denoted by PATHNAME.")
  private static final class pf_get_input_stream extends Primitive {
    pf_get_input_stream() {
      super(Symbol.GET_INPUT_STREAM, "pathname");
    }
    @Override
    public LispObject execute(LispObject pathname) {
      Pathname p = (Pathname) coerceToPathname(pathname);
      return new JavaObject(p.getInputStream());
    }
  };

  public InputStream getInputStream() {
    InputStream result = null;
    File file = getFile();
    try { 
      result = new FileInputStream(file);
    } catch (IOException e) {
      simple_error("Failed to get InputStream from ~a because ~a", this,  e);
    }
    return result;
  }

  /** @return Time in milliseconds since the UNIX epoch at which the
   * resource was last modified, or 0 if the time is unknown.
   */
  public long getLastModified() {
    File f = getFile();
    return f.lastModified();
  }

  private static final Primitive MKDIR = new pf_mkdir();
  @DocString(name="mkdir",
             args="pathname",
             returns="generalized-boolean",
             doc="Attempts to create directory at PATHNAME returning the success or failure.")
  private static class pf_mkdir extends Primitive {
    pf_mkdir() {
      super("mkdir", PACKAGE_SYS, false, "pathname");
    }

    @Override
    public LispObject execute(LispObject arg) {
      final Pathname pathname = coerceToPathname(arg);
      if (pathname.isWild()) {
        error(new FileError("Bad place for a wild pathname.", pathname));
      }
      Pathname defaultedPathname
        = (Pathname)mergePathnames(pathname,
                                   coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
                                   NIL);
      if (defaultedPathname.isURL() || defaultedPathname.isJar()) {
        return new FileError("Cannot mkdir with a " 
                             + (defaultedPathname.isURL() ? "URL" : "jar")
                             + " Pathname.",
                             defaultedPathname);
      }
      
      File file = defaultedPathname.getFile();
      return file.mkdir() ? T : NIL;
    }
  }

  private static final Primitive RENAME_FILE = new pf_rename_file();
  @DocString(name="rename-file",
               args="filespec new-name",
               returns="defaulted-new-name, old-truename, new-truename",
               doc = "Modifies the file system in such a way that the file indicated by FILESPEC is renamed to DEFAULTED-NEW-NAME.\n"
               + "\n"
               + "Returns three values if successful. The primary value, DEFAULTED-NEW-NAME, is \n"
               + "the resulting name which is composed of NEW-NAME with any missing components filled in by \n"
               + "performing a merge-pathnames operation using filespec as the defaults. The secondary \n" 
               + "value, OLD-TRUENAME, is the truename of the file before it was renamed. The tertiary \n"
               + "value, NEW-TRUENAME, is the truename of the file after it was renamed.\n")
  private static class pf_rename_file extends Primitive {
    pf_rename_file() {
      super("rename-file", "filespec new-name");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second) {
      Pathname oldPathname = coerceToPathname(first);
      Pathname oldTruename = (Pathname) Symbol.TRUENAME.execute(oldPathname);
      Pathname newName = coerceToPathname(second);
      if (newName.isWild()) {
        error(new FileError("Bad place for a wild pathname.", newName));
      }
      if (oldTruename.isJar()) {
        error(new FileError("Bad place for a jar pathname.", oldTruename));
      }
      if (newName.isJar()) {
        error(new FileError("Bad place for a jar pathname.", newName));
      }
      if (oldTruename.isURL()) {
        error(new FileError("Bad place for a URL pathname.", oldTruename));
      }
      if (newName.isURL()) {
        error(new FileError("Bad place for a jar pathname.", newName));
      }
      
      Pathname defaultedNewName = (Pathname)mergePathnames(newName, oldTruename, NIL);
      
      File source = oldTruename.getFile();
      File destination = null;
      if (defaultedNewName instanceof LogicalPathname) {
        destination = LogicalPathname.translateLogicalPathname((LogicalPathname)defaultedNewName)
          .getFile();
      } else {
        destination = defaultedNewName.getFile();
      }
      if (Utilities.isPlatformWindows) {
        if (destination.isFile()) {
          //if (destination.isJar()) {
            // By default, MSDOG doesn't allow one to remove files that are open, so we need to close
            // any open jar references
            // FIXME
            //            ZipCache.remove(destination);
          //          }
          destination.delete();
        }
      }
      if (source.renameTo(destination)) { // Success!
        Pathname newTruename = (Pathname)truename(defaultedNewName, true);
        return LispThread.currentThread().setValues(defaultedNewName, 
                                                    oldTruename,
                                                    newTruename);
      }
      return error(new FileError("Unable to rename "
                                 + oldTruename.princToString()
                                 + " to " + newName.princToString()
                                 + ".",
                                 oldTruename));
    }
  }
    
  // TODO clarify uri encoding cases in implementation and document
  private static final Primitive FILE_NAMESTRING = new pf_file_namestring();
  @DocString(name="file-namestring",
             args="pathname",
             returns="namestring",
             doc="Returns just the name, type, and version components of PATHNAME.")
  private static class pf_file_namestring extends Primitive {
    pf_file_namestring() {
      super(Symbol.FILE_NAMESTRING, "pathname");
    }
    @Override
    public LispObject execute(LispObject arg) {
      Pathname p = coerceToPathname(arg);
      StringBuilder sb = new StringBuilder();
      if (p.getName() instanceof AbstractString) {
        sb.append(p.getName().getStringValue());
      } else if (p.getName() == Keyword.WILD) {
        sb.append('*');
      } else {
        return NIL;
      }
      if (p.getType() instanceof AbstractString) {
        sb.append('.');
        sb.append(p.getType().getStringValue());
      } else if (p.getType() == Keyword.WILD) {
        sb.append(".*");
      }
      return new SimpleString(sb);
    }
  }

  private static final Primitive HOST_NAMESTRING = new pf_host_namestring();
  @DocString(name="host-namestring",
             args="pathname",
             returns="namestring",
             doc="Returns the host name of PATHNAME.")
  private static class pf_host_namestring extends Primitive {
    pf_host_namestring() {
      super("host-namestring", "pathname");
    }
    @Override
    public LispObject execute(LispObject arg) {
      return coerceToPathname(arg).getHost(); // XXX URL-PATHNAME
    }
  }

  static {
    LispObject obj = Symbol.DEFAULT_PATHNAME_DEFAULTS.getSymbolValue();
    Symbol.DEFAULT_PATHNAME_DEFAULTS.setSymbolValue(coerceToPathname(obj));
  }

    
  File getFile() {
    String namestring = getNamestring(); // XXX UNC pathnames currently have no namestring
    if (namestring != null) {
      return new File(namestring);
    }
    error(new FileError("Pathname has no namestring: " + princToString(),
                        this));
    return (File)UNREACHED;
  }

  public static Pathname getDirectoryPathname(File file) {
        try {
            String namestring = file.getCanonicalPath();
            if (namestring != null && namestring.length() > 0) {
              // ??? do we really want the platform dependent separatorChar?
                if (namestring.charAt(namestring.length() - 1) != File.separatorChar) {
                    namestring = namestring.concat(File.separator);
                }
            }
            return (Pathname)Pathname.create(namestring);
        } catch (IOException e) {
            error(new LispError(e.getMessage()));
            // Not reached.
            return null;
        }
    }

  // Whether this pathname represents a file on the filesystem, not
  // addressed as a JAR-PATHNAME
  public boolean isLocalFile() {
    if (getHost().equals(NIL)
        || Symbol.GETF.execute(getHost(), URLPathname.SCHEME, NIL).equals(URLPathname.FILE)) {
      return true;
    }
    return false;
  }

  Pathname getEntryPath() {
    return Pathname.create(asEntryPath());
  }

  /** @return The representation of the DIRECTORY/NAME/TYPE elements
   *  of pathname suitable for referencing an entry in a Zip/JAR file.
   *
   *  This representation is always a relative path.
   */
  String asEntryPath() {
    Pathname p = Pathname.create();
    p.setDirectory(getDirectory())
      .setName(getName())
      .setType(getType());
    String path = p.getNamestring();
    
    StringBuilder result = new StringBuilder();
    result.append(path);

    // ZipEntry syntax is always relative
    if (result.length() > 1
        && result.substring(0, 1).equals("/")) {
      return result.substring(1);
    }
    return result.toString();
  }

  boolean isRemote() {
    if (this instanceof URLPathname) {
      URLPathname p = (URLPathname) this;
      LispObject scheme = Symbol.GETF.execute(p.getHost(), URLPathname.SCHEME, NIL);
      if (scheme.equals(NIL)
          || p.getHost().getStringValue().equals("file")) {
        return false;
      }
      return true;
    } else if (this instanceof JarPathname) {
      Pathname root = (Pathname) ((JarPathname)this).getRootJar();
      return root.isRemote();
    } else {
      return false;
    }
  }
}
