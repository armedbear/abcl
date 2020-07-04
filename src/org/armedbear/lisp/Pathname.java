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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileInputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.text.MessageFormat;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

public class Pathname extends LispObject {

  protected static Pathname create(Pathname p) {
    return new Pathname(p);
  }

  protected static Pathname create() {
    return new Pathname();
  }

  public static Pathname create(String s) {
    // TODO distinguish between logical hosts and schemes for URLs
    // which we can meaningfully parse.
    if (!isValidURL(s)) {
      if (LogicalPathname.isValidLogicalPathname(s)) {
        return LogicalPathname.create(s);
      }
    }
    return new Pathname(s);
  }

  public static Pathname create(String s, String host) {
    return LogicalPathname.create(s, host);
  }

  public static Pathname create(URL url) {
    return new Pathname(url);
  }

  public static Pathname create(URI uri) {
    return new Pathname(uri);
  }
  protected LispObject host = NIL;
  public LispObject getHost() {
    return host;
  }
  public LispObject setHost(LispObject host) {
    this.host = host;
    return this;
  }

  protected LispObject device = NIL;
  public final LispObject getDevice() {
    return device;
  }
  public LispObject setDevice(LispObject device) {
    this.device = device;
    return this;
  }

  protected LispObject directory = NIL;
  public LispObject getDirectory() {
    return directory;
  }
  public LispObject setDirectory(LispObject directory) {
    this.directory = directory;
    return this;
  }

  protected LispObject name = NIL;
  public LispObject getName() {
    return name;
  }
  public LispObject setName(LispObject name) {
    this.name = name;
    return this;
  }

  /**  A string, NIL, :WILD or :UNSPECIFIC. */
  protected LispObject type = NIL;
  public LispObject getType() {
    return type;
  }
  public LispObject setType(LispObject type) {
    this.type = type;
    return this;
  }

  /** A positive integer, or NIL, :WILD, :UNSPECIFIC, or :NEWEST. */
  protected LispObject version = NIL;
  public LispObject getVersion() {
    return version;
  }

  public LispObject setVersion(LispObject version) {
    this.version = version;
    return this;
  }


    /** The path component separator used by internally generated
     * path namestrings.
     */
    public final static char separator = '/';

  private volatile String namestring;

    /** The protocol for changing any instance field (i.e. 'host',
     *  'type', etc.)  is to call this method after changing the field
     *  to recompute the namestring.  We could do this with
     *  setter/getters, but that choose not to in order to avoid the
     *  performance indirection penalty.
     *
     *  TODO There is no "perfomance penalty" in contemporary
     *  compilers which inline such access, so it would be better to
     *  implement this as setter/getter ME 20110622
     * 
     *  Although, given the number of bugs that crop up when this
     *  protocol is not adhered to, maybe we should consider it.
     */
    public void invalidateNamestring() {
        namestring = null;
    }
    
    private static final Primitive _INVALIDATE_NAMESTRING 
        = new pf_invalidate_namestring();
    @DocString(name="%invalidate-namestring",
               args="pathname",
               returns="pathname")
    private static class pf_invalidate_namestring extends Primitive {
        pf_invalidate_namestring() {
            super("%invalidate-namestring", PACKAGE_EXT, false);
        }
        @Override
        public LispObject execute(LispObject first) {
            ((Pathname)coerceToPathname(first)).invalidateNamestring();
            return first;
        }
    }

  // If not protected, then inheriting classes cannot invoke their constructors
    protected Pathname() {}

    /** Copy constructor which shares no structure with the original. */
    private Pathname(Pathname p) {
        if (p.host != NIL) {
            if (p.host instanceof SimpleString) {
                host = new SimpleString(((SimpleString)p.getHost()).getStringValue());
            } else  if (p.host instanceof Symbol) {
                host = p.host;
            } else if (p.host instanceof Cons) {
                host = new Cons((Cons)p.getHost());
            } else {
                Debug.assertTrue(false);
            }
        }
        if (p.device != NIL) {
            if (p.device instanceof SimpleString) {
                device = new SimpleString(((SimpleString)p.getDevice()).getStringValue());
            } else if (p.device instanceof Cons) {
                Cons jars = (Cons)p.device;
                device = new Cons(NIL, NIL);
                LispObject first = jars.car();
                if (first instanceof Pathname) {
                    ((Cons)device).car = Pathname.create((Pathname)first);
                } else {
                    Debug.assertTrue(false);
                }
                if (!jars.cdr().equals(NIL)) {
                    if (jars.cdr() instanceof Cons) {
                        ((Cons)device).cdr = new Cons(Pathname.create((Pathname)jars.cdr().car()), NIL);
                    } else { 
                        Debug.assertTrue(false);
                    }
                }
            } else if (p.device instanceof Symbol) {
                device = p.device;
            } else {
                Debug.assertTrue(false);
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
                Debug.assertTrue(false);
            }
        }
        if (p.name != NIL) {
            if (p.name instanceof SimpleString) {
                name = new SimpleString(((SimpleString)p.getName()).getStringValue());
            } else if (p.name instanceof Symbol) {
                name = p.name;
            } else {
                Debug.assertTrue(false);
            }
        } 
        if (p.type != NIL) {
            if (p.type instanceof SimpleString) {
                type = new SimpleString(((SimpleString)p.getType()).getStringValue());
            } else if (p.type instanceof Symbol) {
                type = p.type;
            } else {
                Debug.assertTrue(false);
            }
        }
    if (p.version != NIL) {
        if (p.version instanceof Symbol) {
        version = p.version;
        } else if (p.version instanceof LispInteger) {
        version = p.version;
        } else {
        Debug.assertTrue(false);
        }
    }
    }

    private Pathname(String s) {
        init(s);
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

    private Pathname(URL url) {
         // URL handling is now buried in init(String), as the URI
         // escaping mechanism didn't interact well with '+' and other
         // characters. 
        init(url.toString());
    }
    
    private Pathname(URI uri) {
        init(uri.toString());
    }

    static final Symbol SCHEME = internKeyword("SCHEME");
    static final Symbol AUTHORITY = internKeyword("AUTHORITY");
    static final Symbol QUERY = internKeyword("QUERY");
    static final Symbol FRAGMENT = internKeyword("FRAGMENT");

    static final private String jarSeparator = "!/";
    private final void init(String s) {
        if (s == null) {
            return;
        }
        if (s.equals(".") || s.equals("./")
          || (Utilities.isPlatformWindows && s.equals(".\\"))) {
            setDirectory(new Cons(Keyword.RELATIVE));
            return;
        }
        if (s.equals("..") || s.equals("../")) {
            setDirectory(list(Keyword.RELATIVE, Keyword.UP));
            return;
        }
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
              error(new LispError("Unsupported UNC path format: \"" + s + '"'));
            }

            setHost(new SimpleString(s.substring(2, shareIndex)));
            setDevice(new SimpleString(s.substring(shareIndex + 1, dirIndex)));

            Pathname p = Pathname.create(s.substring(dirIndex));
            setDirectory(p.getDirectory());
            setName(p.getName());
            setType(p.getType());
            setVersion(p.getVersion());
            invalidateNamestring();
            return;
          }
        }
        
        // A JAR file
        if (s.startsWith("jar:") && s.endsWith(jarSeparator)) {
            LispObject jars = NIL;
            int i = s.lastIndexOf(jarSeparator, s.length() - jarSeparator.length() - 1);
            String jar = null;
            if (i == -1) {
                jar = s;
            } else {
                // There can be no more than two jar references and the
                // inner one must be a file reference within the outer.
                jar = "jar:file:" + s.substring(i + jarSeparator.length());
                s = s.substring("jar:".length(), i + jarSeparator.length());
                Pathname p = Pathname.create(s);
                jars = jars.push(p.getDevice().car());
            }
            if (jar.startsWith("jar:file:")) {
                String file
                    = jar.substring("jar:file:".length(),
                                    jar.length() - jarSeparator.length());
                Pathname jarPathname;
                if (file.length() > 0) {
                    URL url = null;
                    URI uri = null;
                    try {
                        url = new URL("file:" + file);
                        uri = url.toURI();
                    } catch (MalformedURLException e1) {
                        error(new SimpleError("Failed to create URI from "
                                            + "'" + file + "'"
                                            + ": " + e1.getMessage()));
                    } catch (URISyntaxException e2) {
                        error(new SimpleError("Failed to create URI from "
                                            + "'" + file + "'"
                                            + ": " + e2.getMessage()));
                    }
                    String path = uri.getPath();
                    if (path == null) {
                        // We allow "jar:file:baz.jar!/" to construct a relative
                        // path for jar files, so MERGE-PATHNAMES means something.
                        jarPathname = Pathname.create(uri.getSchemeSpecificPart());
                    } else {
                        jarPathname = Pathname.create((new File(path)).getPath());
                    }
                } else {
                    jarPathname = Pathname.create("");
                }
                jars = jars.push(jarPathname);
            } else {
                URL url = null;
                try {
                    url = new URL(jar.substring("jar:".length(), jar.length() - 2));
                    Pathname p = Pathname.create(url);
                    jars = jars.push(p);
                } catch (MalformedURLException e) {
                    error(new LispError("Failed to parse URL "
                                        + "'" + url + "'"
                                        + e.getMessage()));
                }
            }
            jars = jars.nreverse();
            setDevice(jars);
            invalidateNamestring();
            return;
        }

        // An entry in a JAR file
        final int separatorIndex = s.lastIndexOf(jarSeparator);
        if (separatorIndex > 0 && s.startsWith("jar:")) {
            final String jarURL = s.substring(0, separatorIndex + jarSeparator.length());
            URL url = null;
            try {
                url = new URL(jarURL);
            } catch (MalformedURLException ex) {
                error(new LispError("Failed to parse URL "
                                    + "'" + jarURL + "'"
                                    + ex.getMessage()));
            }
            Pathname d = Pathname.create(url);
            if (getDevice() instanceof Cons) {
                LispObject[] jars = d.copyToArray();
                //  XXX Is this ever reached?  If so, need to append lists
                Debug.assertTrue(false);
            } else {
                setDevice(d.getDevice());
            }
            s = "/" + s.substring(separatorIndex + jarSeparator.length());
            Pathname p = Pathname.create("file:" + s); // Use URI escaping rules
            setDirectory(p.getDirectory());
            setName(p.getName());
            setType(p.getType());
            setVersion(p.getVersion());
            return;
        }

        // A URL 
        if (isValidURL(s)) {
            URL url = null;
            try {
                url = new URL(s);
            } catch (MalformedURLException e) {
                Debug.assertTrue(false);
            }
            String scheme = url.getProtocol();
            if (scheme.equals("file")) {
                URI uri = null;
                try {
                    uri = new URI(s);
                } catch (URISyntaxException ex) {
                    error(new SimpleError("Improper URI syntax for "
                                    + "'" + url.toString() + "'"
                                    + ": " + ex.toString()));
                }
            
                String uriPath = uri.getPath();
                if (null == uriPath) {
                                  // Under Windows, deal with pathnames containing
                                  // devices expressed as "file:z:/foo/path"
                                  uriPath = uri.getSchemeSpecificPart();
                                  if (uriPath == null || uriPath.equals("")) {
                    error(new LispError("The URI has no path: " + uri));
                  }
                }
                final File file = new File(uriPath);
                String path = file.getPath();
                if (uri.toString().endsWith("/") && !path.endsWith("/")) {
                  path += "/";
                }
                final Pathname p = Pathname.create(path);
                this.setHost(p.getHost());
                this.setDevice(p.getDevice());
                this.setDirectory(p.getDirectory());
                this.setName(p.getName());
                this.setType(p.getType());
                this.setVersion(p.getVersion());
                return;
            }
            Debug.assertTrue(scheme != null);
            URI uri = null;
            try { 
                uri = url.toURI().normalize();
            } catch (URISyntaxException e) {
                error(new LispError("Couldn't form URI from "
                                    + "'" + url + "'"
                                    + " because: " + e));
            }
            String authority = uri.getAuthority();
        if (authority == null) {
          authority = url.getAuthority();
          if (authority == null) {
            Debug.trace(MessageFormat.format("{0} has a null authority.", url));
          }
        }

        setHost(NIL);
        setHost(getHost().push(SCHEME));
        setHost(getHost().push(new SimpleString(scheme)));

        if (authority != null) {
          setHost(getHost().push(AUTHORITY));
          setHost(getHost().push(new SimpleString(authority)));
        }

        setDevice(NIL);
            
        // URI encode necessary characters
        String path = uri.getRawPath();
        if (path == null) {
          path = "";
        } 
        String query = uri.getRawQuery();
        if (query != null) {
          setHost(getHost().push(QUERY));
                setHost(getHost().push(new SimpleString(query)));
            }
            String fragment = uri.getRawFragment();
            if (fragment != null) {
                setHost(getHost().push(FRAGMENT));
                setHost(getHost().push(new SimpleString(fragment)));
            }
            Pathname p = Pathname.create(path != null ? path : ""); 

            setDirectory(p.getDirectory());
            setName(p.getName());
            setType(p.getType());
            
            setHost(getHost().nreverse());
            invalidateNamestring();
            return;
        }

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
        namestring = s;
        if (Utilities.isPlatformWindows) {
            if (s.length() >= 2 && s.charAt(1) == ':') {
                setDevice(new SimpleString(s.charAt(0)));
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
            setDirectory(parseDirectory(d));
        }
        if (s.startsWith(".") 
            // No TYPE can be parsed
            && (s.indexOf(".", 1) == -1 
                || s.substring(s.length() -1).equals("."))) {
            setName(new SimpleString(s));
            return;
        }
        int index = s.lastIndexOf('.');
        String n = null;
        String t = null;
        if (index > 0) {
            n = s.substring(0, index);
            t = s.substring(index + 1);
        } else if (s.length() > 0) {
            n = s;
        }
        if (n != null) {
            if (n.equals("*")) {
                setName(Keyword.WILD);
            } else {
                setName(new SimpleString(n));
            }
        }
        if (t != null) {
            if (t.equals("*")) {
                setType(Keyword.WILD);
            } else {
                setType(new SimpleString(t));
            }
        }
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
        LispObject parts = NIL;
        parts = parts.push(new Cons("HOST", getHost()));
        parts = parts.push(new Cons("DEVICE", getDevice()));
        parts = parts.push(new Cons("DIRECTORY", getDirectory()));
        parts = parts.push(new Cons("NAME", getName()));
        parts = parts.push(new Cons("TYPE", getType()));
        parts = parts.push(new Cons("VERSION", getVersion()));
        return parts.nreverse();
    }

    @Override
    public LispObject typeOf() {
        if (isURL()) {
            return Symbol.URL_PATHNAME;
        } 
        if (isJar()) {
            return Symbol.JAR_PATHNAME;
        }
        return Symbol.PATHNAME;
    }

    @Override
    public LispObject classOf() {
        if (isURL()) {
            return BuiltInClass.URL_PATHNAME;
        } 
        if (isJar()) {
            return BuiltInClass.JAR_PATHNAME;
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
        if (namestring != null) {
            return namestring;
        }
        if (getName() == NIL && getType() != NIL) {
            Debug.assertTrue(namestring == null);
            return null;
        }
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
                LispObject scheme = Symbol.GETF.execute(getHost(), SCHEME, NIL);
                LispObject authority = Symbol.GETF.execute(getHost(), AUTHORITY, NIL);
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
        boolean uriEncoded = false;
        if (getDevice() == NIL) {
        } else if (getDevice() == Keyword.UNSPECIFIC) {
        } else if (isJar()) {
            LispObject[] jars = ((Cons) getDevice()).copyToArray();
            StringBuilder prefix = new StringBuilder();
            for (int i = 0; i < jars.length; i++) {
                prefix.append("jar:");
                LispObject component = jars[i];
                if (!(component instanceof Pathname)) {
                  return null; // If DEVICE is a CONS, it should only contain Pathname 
                }
                if (! ((Pathname)component).isURL() && i == 0) {
                  sb.append("file:");
                  uriEncoded = true;
                }
                Pathname jar = (Pathname) component;
                String encodedNamestring;
                if (uriEncoded) {
                  encodedNamestring = uriEncode(jar.getNamestring());
                } else { 
                  encodedNamestring = jar.getNamestring();
                }
                sb.append(encodedNamestring);
                sb.append("!/");
            }
            sb = prefix.append(sb);
        } else if (getDevice() instanceof AbstractString) {
            sb.append(getDevice().getStringValue());
            if (this instanceof LogicalPathname
                || getHost() == NIL) {
              sb.append(':'); // non-UNC paths
            }
        } else {
            Debug.assertTrue(false);
        }
        String directoryNamestring = getDirectoryNamestring();
        if (uriEncoded) {
            directoryNamestring = uriEncode(directoryNamestring);
        }
        if (isJar()) {
            if (directoryNamestring.startsWith("/")) {
                sb.append(directoryNamestring.substring(1));
            } else {
                sb.append(directoryNamestring);
            }
        } else {
            sb.append(directoryNamestring);
        }
        if (getName() instanceof AbstractString) {
            String n = getName().getStringValue();
            if (n.indexOf('/') >= 0) {
                Debug.assertTrue(namestring == null);
                return null;
            }
            if (uriEncoded) {
                sb.append(uriEncode(n));
            } else {
                sb.append(n);
            }
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
                        Debug.assertTrue(namestring == null);
                        return null;
                    }
                }
                if (uriEncoded) {
                    sb.append(uriEncode(t));
                } else {
                    sb.append(t);
                }
            } else if (getType() == Keyword.WILD) {
                sb.append('*');
            } else {
                Debug.assertTrue(false);
            }
        }
        
        if (isURL()) {
            LispObject o = Symbol.GETF.execute(getHost(), QUERY, NIL);
            if (o != NIL) {
                sb.append("?");
                sb.append(o.getStringValue());
            }
            o = Symbol.GETF.execute(getHost(), FRAGMENT, NIL);
            if (o != NIL) {
                sb.append("#");
                sb.append(o.getStringValue());
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
        namestring = sb.toString();
        // XXX Decide if this is necessary
        // if (isURL()) { 
        //     namestring = Utilities.uriEncode(namestring);
        // }
        return namestring;
    }

    protected String getDirectoryNamestring() {
        validateDirectory(true);
        StringBuilder sb = new StringBuilder();
        // "If a pathname is converted to a namestring, the symbols NIL and
        // :UNSPECIFIC cause the field to be treated as if it were empty. That
        // is, both NIL and :UNSPECIFIC cause the component not to appear in
        // the namestring." 19.2.2.2.3.1
        if (getDirectory() != NIL && getDirectory() != Keyword.UNSPECIFIC) {
            final char separatorChar = '/';
            LispObject temp = getDirectory();
            LispObject part = temp.car();
            temp = temp.cdr();
            if (part == Keyword.ABSOLUTE) {
                sb.append(separatorChar);
            } else if (part == Keyword.RELATIVE) {
                if (temp == NIL) {
                    // #p"./"
                    sb.append('.');
                    sb.append(separatorChar);
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
                sb.append(separatorChar);
                temp = temp.cdr();
            }
        }
        return sb.toString();
    }

    /** @return The representation of this pathname suitable for
     *  referencing an entry in a Zip/JAR file 
     */
    protected String asEntryPath() {
        Pathname p = Pathname.create();
        p.setDirectory(getDirectory());
        p.setName(getName());
        p.setType(getType());
        p.invalidateNamestring();
        String path = p.getNamestring();
        StringBuilder result = new StringBuilder();
        result.append(path);

        // Entries in jar files are always relative, but Pathname
        // directories are :ABSOLUTE.
        if (result.length() > 1
          && result.substring(0, 1).equals("/")) {
            return result.substring(1);
        }
        return result.toString();
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
            sb.append(":HOST ");
            sb.append(getHost().printObject());
            sb.append(" ");
        }
        if (getDevice() != NIL) {
            sb.append(":DEVICE ");
            sb.append(getDevice().printObject());
            sb.append(" ");
        }
        if (getDirectory() != NIL) {
            sb.append(":DIRECTORY ");
            sb.append(getDirectory().printObject());
            sb.append(" ");
        }
        if (getName() != NIL) {
            sb.append(":NAME ");
            sb.append(getName().printObject());
            sb.append(" ");
        }
        if (getType() != NIL) {
            sb.append(":TYPE ");
            sb.append(getType().printObject());
            sb.append(" ");
        }
        if (getVersion() != NIL) {
            sb.append(":VERSION ");
            sb.append(getVersion().printObject());
            sb.append(" ");
        }
        if (sb.charAt(sb.length() - 1) == ' ') { 
            sb.setLength(sb.length() - 1);
        }

        return unreadableString(sb.toString());
    }
    // A logical host is represented as the string that names it.
    // (defvar *logical-pathname-translations* (make-hash-table :test 'equal))
    public static HashTable LOGICAL_PATHNAME_TRANSLATIONS =
      HashTable.newEqualHashTable(64, NIL, NIL);
    private static final Symbol _LOGICAL_PATHNAME_TRANSLATIONS_ =
      exportSpecial("*LOGICAL-PATHNAME-TRANSLATIONS*", PACKAGE_SYS,
      LOGICAL_PATHNAME_TRANSLATIONS);

    public static Pathname parseNamestring(String s) {
        return Pathname.create(s);
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

//    public static URL toURL(Pathname p) {
 //       try {
//            return p.toURL();
//        } catch (MalformedURLException e) {
//            Debug.assertTrue(false);
//            return null; // not reached
//        }
//    }

    URLConnection getURLConnection() {
        Debug.assertTrue(isURL());
        URL url = this.toURL();
        URLConnection result = null;
        try {
            result = url.openConnection();
        } catch (IOException e) {
            error(new FileError("Failed to open URL connection.",
                                this));
        }
        return result;
    }

    public static Pathname parseNamestring(AbstractString namestring) {
        // Check for a logical pathname host.
        String s = namestring.getStringValue();
        if (!isValidURL(s)) {
            String h = LogicalPathname.getHostString(s);
            if (h != null && LOGICAL_PATHNAME_TRANSLATIONS.get(new SimpleString(h)) != null) {
                // A defined logical pathname host.
                return LogicalPathname.create(h, s.substring(s.indexOf(':') + 1));
            }
        }
        return Pathname.create(s);
    }

    // XXX was @return Pathname
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
        if (LOGICAL_PATHNAME_TRANSLATIONS.get(host) != null) {
            // A defined logical pathname host.
            return LogicalPathname.create(host.getStringValue(), s);
        }
        error(new LispError(host.princToString() + " is not defined as a logical pathname host."));
        // Not reached.
        return null;
    }

    static final void checkCaseArgument(LispObject arg) {
        if (arg != Keyword.COMMON && arg != Keyword.LOCAL) {
            type_error(arg, list(Symbol.MEMBER, Keyword.COMMON,
              Keyword.LOCAL));
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
        return Pathname.create(namestring);
    }


  // ??? changed to return a LispObject, as we wish to return a
  // meaningful Java type (Pathname for files, LogicalPathnames for you know…)
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
                      || value instanceof Cons))
                  error(new TypeError("DEVICE is not a string, :UNSPECIFIC, NIL, or a list.", value, NIL));
            } else if (key == Keyword.DIRECTORY) {
                directorySupplied = true;
                if (value instanceof AbstractString) {
                    directory = list(Keyword.ABSOLUTE, value);
                } else if (value == Keyword.WILD) {
                    directory = list(Keyword.ABSOLUTE, Keyword.WILD);
                } else {
                  // a valid pathname directory is a string, a list of strings, nil, :wild, :unspecific
                  // ??? would be nice to (deftype pathname-arg ()
                  // '(or (member :wild :unspecific) string (and cons ,(mapcar ...
                  // Is this possible?
                  if ((value instanceof Cons 
                       // XXX check that the elements of a list are themselves valid
                       || value == Keyword.UNSPECIFIC
                       || value.equals(NIL))) {
                      directory = value;
                  } else {
                      error(new TypeError("DIRECTORY argument not a string, list of strings, nil, :WILD, or :UNSPECIFIC.", value, NIL));
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
        Pathname p;
        LispObject logicalHost = NIL;
        if (host != NIL) {
            if (host instanceof AbstractString) {
                logicalHost = LogicalPathname.canonicalizeStringComponent((AbstractString) host);
            }
            if (LOGICAL_PATHNAME_TRANSLATIONS.get(logicalHost) == null) {
                // Not a defined logical pathname host -- A UNC path
                //warning(new LispError(host.printObject() + " is not defined as a logical pathname host."));
                p = Pathname.create();
                p.setHost(host);
            } else { 
                p = LogicalPathname.create();
                p.setHost(logicalHost);
            }
            p.setDevice(Keyword.UNSPECIFIC);
        } else {
            p = Pathname.create();
        }
        if (device != NIL) {
            if (p instanceof LogicalPathname) {
                // "The device component of a logical pathname is always :UNSPECIFIC."
                if (device != Keyword.UNSPECIFIC) {
                    error(new LispError("The device component of a logical pathname must be :UNSPECIFIC."));
                }
            } else {
              p.setDevice(device);
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
                String directory = pathname.asEntryPath();
                Debug.assertTrue(directory != null);  // We should only be listing directories

                if (pathname.getDevice().cdr() instanceof Cons) {
                    return error(new FileError("Unimplemented directory listing of JAR within JAR.", pathname));
                }

                if (directory.length() == 0) {
                    directory = "/*";
                } else {
                    if (directory.endsWith("/")) {
                        directory = "/" + directory + "*";
                    } else {
                        directory = "/" + directory + "/*";
                    }
                }
                SimpleString wildcard = new SimpleString(directory);
                SimpleString wildcardDirectory = new SimpleString(directory + "/");

                ZipFile jar = ZipCache.get((Pathname)pathname.getDevice().car());
                LispObject matches;
                for (Enumeration<? extends ZipEntry> entries = jar.entries(); 
                     entries.hasMoreElements();) {
                    ZipEntry entry = entries.nextElement();
                    String entryName = "/" + entry.getName();

                    if (entryName.endsWith("/")) {
                        matches = Symbol.PATHNAME_MATCH_P
                            .execute(new SimpleString(entryName), wildcardDirectory);
                    } else {
                        matches = Symbol.PATHNAME_MATCH_P.
                            execute(new SimpleString(entryName), wildcard);
                    }
                    if (!matches.equals(NIL)) {
                        String namestring = new String(pathname.getNamestring());
                        namestring = namestring.substring(0, namestring.lastIndexOf("!/") + 2)
                                 + entry.getName();
                        Pathname p = Pathname.create(namestring);
                        result = new Cons(p, result);
                    }
                }
                return result;
            }

            if (pathname.isURL()) {
                return error(new LispError("Unimplemented.")); // XXX
            }

            String s = pathname.getNamestring();
            if (s != null) {
                File f = new File(s);
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
                            Pathname p;
                            String path;
                            if (resolveSymlinks == NIL) {
                              path = file.getAbsolutePath();
                            } else {
                              path = file.getCanonicalPath();
                            }
                            URI pathURI = (new File(path)).toURI();
                            p = Pathname.create(pathURI);
                            result = new Cons(p, result);
                        }
                    } catch (IOException e) {
                        return error(new FileError("Unable to list directory " 
                                                   + pathname.princToString() + ".",
                                                   pathname));
                    } catch (SecurityException e) {
                        Debug.trace(e);
                    } catch (NullPointerException e) {
                        Debug.trace(e);
                    }
                }
            }
            return result;
        }
    }

    @DocString(name="match-wild-jar-pathname",
               args="wild-jar-pathname",
               returns="pathnames",
    doc="Returns the pathnames matching WILD-JAR-PATHNAME which is both wild and a jar-pathname.")
    static final Primitive MATCH_WILD_JAR_PATHNAME = new pf_match_wild_jar_pathname();
    private static class pf_match_wild_jar_pathname extends Primitive {
        pf_match_wild_jar_pathname() {
            super("match-wild-jar-pathname", PACKAGE_SYS, false, "wild-jar-pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname pathname = coerceToPathname(arg);
            if (pathname instanceof LogicalPathname) {
                pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
            }
            if (!pathname.isJar()) {
                return new FileError("Not a jar pathname.", pathname);
            }
            if (!pathname.isWild()) {
                return new FileError("Not a wild pathname.", pathname);
            }
            Pathname jarPathname = Pathname.create(pathname);
            jarPathname.setDirectory(NIL);
            jarPathname.setName(NIL);
            jarPathname.setType(NIL);
            jarPathname.invalidateNamestring();
            LispObject jarTruename = truename(jarPathname, false); 
            
            // We can't match anything in a non-existent jar 
            if (jarTruename == NIL) {
                return NIL;
            }
            
            LispObject result = NIL;
            String wild = "/" + pathname.asEntryPath();
            final SimpleString wildcard = new SimpleString(wild);

            if (pathname.getDevice().cdr() instanceof Cons) {
                ZipFile outerJar = ZipCache.get((Pathname)pathname.getDevice().car());
                String entryPath = ((Pathname)pathname.getDevice().cdr().car()).getNamestring(); //???
                if (entryPath.startsWith("/")) {
                    entryPath = entryPath.substring(1);
                }
                ZipEntry entry = outerJar.getEntry(entryPath);
                InputStream inputStream = null;
                try {
                    inputStream = outerJar.getInputStream(entry);
                } catch (IOException e) {
                    return new FileError("Failed to read zip input stream inside zip.",
                                         pathname);
                }
                ZipInputStream zipInputStream
                    = new ZipInputStream(inputStream);

                try {
                    while ((entry = zipInputStream.getNextEntry()) != null) {
                        String entryName = "/" + entry.getName();
                        LispObject matches = Symbol.PATHNAME_MATCH_P
                            .execute(new SimpleString(entryName), wildcard);
                    
                        if (!matches.equals(NIL)) {
                            String namestring = new String(pathname.getNamestring());
                            namestring = namestring.substring(0, namestring.lastIndexOf("!/") + 2)
                                + entry.getName();
                            Pathname p = Pathname.create(namestring);
                            result = new Cons(p, result);
                        }
                    }
                } catch (IOException e) {
                    return new FileError("Failed to seek through zip inputstream inside zip.",
                                         pathname);
                }
            } else {
                ZipFile jar = ZipCache.get((Pathname)pathname.getDevice().car());
                for (Enumeration<? extends ZipEntry> entries = jar.entries(); 
                     entries.hasMoreElements();) 
                    {
                        ZipEntry entry = entries.nextElement();
                        String entryName = "/" + entry.getName();
                        LispObject matches = Symbol.PATHNAME_MATCH_P
                            .execute(new SimpleString(entryName), wildcard);

                        if (!matches.equals(NIL)) {
                            String namestring = new String(pathname.getNamestring());
                            namestring = namestring.substring(0, namestring.lastIndexOf("!/") + 2)
                                + entry.getName();
                            Pathname p = Pathname.create(namestring);
                            result = new Cons(p, result);
                        }
                    }
            }
            return result;
        }
    }

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
            Pathname p = coerceToPathname(arg);
            return p.isJar() ? T : NIL;
        }
    }

    public boolean isJar() {
        return (getDevice() instanceof Cons);
    }

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
            Pathname p = coerceToPathname(arg);
            return p.isURL() ? T : NIL;
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
            super("merge-pathnames", "pathname &optional default-pathname default-version");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname pathname = coerceToPathname(arg);
            Pathname defaultPathname =
                coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue());
            LispObject defaultVersion = Keyword.NEWEST;
            return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            Pathname pathname = coerceToPathname(first);
            Pathname defaultPathname =
                coerceToPathname(second);
            LispObject defaultVersion = Keyword.NEWEST;
            return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third) {
            Pathname pathname = coerceToPathname(first);
            Pathname defaultPathname =
                coerceToPathname(second);
            LispObject defaultVersion = third;
            return mergePathnames(pathname, defaultPathname, defaultVersion);
        }
    }

    public static final Pathname mergePathnames(Pathname pathname, Pathname defaultPathname) {
        return mergePathnames(pathname, defaultPathname, Keyword.NEWEST);
    }
    
    public static final Pathname mergePathnames(final Pathname pathname,
                                                final Pathname defaultPathname,
                                                final LispObject defaultVersion)
    {
        Pathname result;
        Pathname p = Pathname.create(pathname);
        Pathname d;

        if (pathname instanceof LogicalPathname) {
            result = LogicalPathname.create();
            d = Pathname.create(defaultPathname);
        } else {
            result = Pathname.create();
            if (defaultPathname instanceof LogicalPathname) {
                d = LogicalPathname.translateLogicalPathname((LogicalPathname) defaultPathname);
            } else {
                d = Pathname.create(defaultPathname);
            }
        }
        if (pathname.getHost() != NIL) {
          result.setHost(p.getHost());
        } else {
          result.setHost(d.getHost());
        }

        if (pathname.getDevice() != NIL) { // XXX if device represent JARs we want to merge
          result.setDevice(p.getDevice());
        } else {
            if (!p.isURL()) {
                // If the defaults contain a JAR-PATHNAME, and the
                // pathname to be be merged does not have a specified
                // DEVICE, a specified HOST, and doesn't contain a
                // relative DIRECTORY, then on non-MSDOG, set its
                // device to :UNSPECIFIC.
                if (pathname.getHost() == NIL
                    && pathname.getDevice() == NIL
                    && d.isJar()
                    && !Utilities.isPlatformWindows) {
                    if (pathname.getDirectory() != NIL
                        && pathname.getDirectory().car() == Keyword.ABSOLUTE) {
                      result.setDevice(Keyword.UNSPECIFIC);
                    } else {
                      result.setDevice(d.getDevice());
                    }
                } else {
                  result.setDevice(d.getDevice());
                }
            }
        }

        if (pathname.isJar()) {
            Cons jars = (Cons)result.getDevice();
            LispObject jar = jars.car;
            if (jar instanceof Pathname) {
                Pathname defaults = Pathname.create(d);
                if (defaults.isJar()) {
                  defaults.setDevice(NIL);
                }
                Pathname o = mergePathnames((Pathname)jar, defaults);
                if (o.getDirectory() instanceof Cons
                    && ((Cons)o.getDirectory()).length() == 1) { // i.e. (:ABSOLUTE) or (:RELATIVE)
                  o.setDirectory(NIL);
                }
                ((Cons)result.device).car = o;
            }
            result.setDirectory(p.getDirectory());
        } else {
          result.setDirectory(mergeDirectories(p.getDirectory(), d.getDirectory()));
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
        result.invalidateNamestring();
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


    public static final LispObject truename(Pathname pathname) {
        return truename(pathname, false);
    }

    public static final LispObject truename(LispObject arg) {
        return truename(arg, false);
    }

    public static final LispObject truename(LispObject arg, boolean errorIfDoesNotExist) {
        final Pathname pathname = coerceToPathname(arg);
        return truename(pathname, errorIfDoesNotExist);
    }

    /** @return The canonical TRUENAME as a Pathname if the pathname
     * exists, otherwise returns NIL or possibly a subtype of
     * LispError if there are logical problems with the input.
     */
    public static final LispObject truename(Pathname pathname,
                                            boolean errorIfDoesNotExist) 
    {
        if (pathname == null || pathname.equals(NIL)) {  
           return doTruenameExit(pathname, errorIfDoesNotExist); 
        }
        if (pathname instanceof LogicalPathname) {
            pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
        }
        if (pathname.isWild()) {
            return error(new FileError("Bad place for a wild pathname.",
                                       pathname));
        }
        if (!(pathname.isJar() || pathname.isURL())) {
            Pathname result 
                = mergePathnames(pathname,
                                 coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
                                 NIL);
            final File file = result.getFile();
            if (file.exists()) {
                if (file.isDirectory()) {
                    result = Pathname.getDirectoryPathname(file);
                } else {
                    try {
                        result = Pathname.create(file.getCanonicalPath());
                    } catch (IOException e) {
                        return error(new FileError(e.getMessage(), pathname));
                    }
                }
                if (Utilities.isPlatformUnix) {
                  result.setDevice(Keyword.UNSPECIFIC);
                }
                return result;
            }
        } else if (pathname.isURL()) {
            if (pathname.getInputStream() != null) {
              // If there is no type, query or fragment, we check to
              // see if there is URL available "underneath".
              if (pathname.getName() != NIL 
                  && pathname.getType() == NIL
                  && Symbol.GETF.execute(pathname.getHost(), QUERY, NIL) == NIL
                  && Symbol.GETF.execute(pathname.getHost(), FRAGMENT, NIL) == NIL) {
                Pathname p = Pathname.create(pathname.getNamestring() + "/");
                if (p.getInputStream() != null) {
                  return p;
                }
              }
              return pathname;
            }
        } else
        jarfile: {
            // Possibly canonicalize jar file directory
            Cons jars = (Cons) pathname.getDevice();
            LispObject o = jars.car();
        if (!(o instanceof Pathname)) {
           return doTruenameExit(pathname, errorIfDoesNotExist);
        }
            if (o instanceof Pathname 
                && !(((Pathname)o).isURL())
                // XXX Silently fail to call truename() if the default
                // pathname defaults exist within a jar, as that will
                // (probably) not succeed.  The better solution would
                // probably be to parametize the value of
                // *DEFAULT-PATHNAME-DEFAULTS* on invocations of
                // truename().
                && !coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()).isJar()) 
                {
                LispObject truename = Pathname.truename((Pathname)o, errorIfDoesNotExist);
                if (truename != null && truename != NIL
                    && truename instanceof Pathname) {
                    Pathname truePathname = (Pathname)truename;
                    // A jar that is a directory makes no sense, so exit
                    if (truePathname.getNamestring().endsWith("/")) {
                        break jarfile;
                    }
                    jars.car = truePathname;
                } else {
                    break jarfile;
                }
            }

            // Check for existence of a JAR file and/or JarEntry
            //
            // Cases:
            // 1.  JAR
            // 2.  JAR in JAR
            // 3.  JAR with Entry
            // 4.  JAR in JAR with Entry

            ZipFile jarFile = ZipCache.get((Pathname)jars.car());
            String entryPath = pathname.asEntryPath();
            if (jarFile != null) {
                if (jars.cdr() instanceof Cons) {
                  Pathname inner = (Pathname) jars.cdr().car();
                  InputStream inputStream = Utilities.getInputStream(jarFile, inner);
                  if (inputStream != null) {
                      if (entryPath.length() == 0) {
                          return pathname; // Case 2
                      } else {
                          ZipInputStream zipInputStream
                              = new ZipInputStream(inputStream);
                          ZipEntry entry = Utilities.getEntry(zipInputStream,
                                                              entryPath,
                                                              false);
                          if (entry != null) {
                              // XXX this could possibly be a directory?
                              return pathname; // Case 4
                         }
                      }
                  }
                } else {
                    if (entryPath.length() == 0) {
                        return pathname; // Case 1
                    } else {
                        ZipEntry entry = jarFile.getEntry(entryPath);
                        if (entry != null) {
                            // ensure this isn't a directory
                            if (entry.isDirectory()) {
                                break jarfile;
                            }
                            try {
                                InputStream input = jarFile.getInputStream(entry);
                                if (input != null) {
                                    return pathname; // Case 3
                                }
                            } catch (IOException e) {
                                break jarfile;
                            }
                        }
                    }
                }
            }
        }
        error:
      return doTruenameExit(pathname, errorIfDoesNotExist);
    }
    
    static private LispObject doTruenameExit(Pathname pathname, boolean errorIfDoesNotExist) {
        if (errorIfDoesNotExist) {
            StringBuilder sb = new StringBuilder("The file ");
            sb.append(pathname.princToString());
            sb.append(" does not exist.");
            return error(new FileError(sb.toString(), pathname));
        }
        return NIL;
    }


    protected static URL makeURL(Pathname pathname) {
        URL result = null;
        try {
            if (pathname.isURL()) {
                result = new URL(pathname.getNamestring());
            } else {
                // XXX Properly encode Windows drive letters and UNC paths
                // XXX ensure that we have cannonical path?
                result = new URL("file://" + pathname.getNamestring());
            }
        } catch (MalformedURLException e) {
            Debug.trace("Could not form URL from " + pathname);
        }
        return result;
    }

  public static final Primitive GET_INPUT_STREAM = new pf_get_input_stream();
  @DocString(name="get-input-stream",
             args="pathname",
             doc="Returns a java.io.InputStream for resource denoted by PATHNAME.")
  private static final class pf_get_input_stream extends Primitive {
    pf_get_input_stream() {
      super("ensure-input-stream", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject pathname) {
      Pathname p = (Pathname) coerceToPathname(pathname);
      return new JavaObject(p.getInputStream());
    }
  };


    public InputStream getInputStream() {
        InputStream result = null;
        if (isJar()) {
            String entryPath = asEntryPath();
            // XXX We only return the bytes of an entry in a JAR
            Debug.assertTrue(entryPath != null);
            ZipFile jarFile = ZipCache.get((Pathname)getDevice().car());
            Debug.assertTrue(jarFile != null);
            // Is this a JAR within a JAR?
            if (getDevice().cdr() instanceof Cons) {
                Pathname inner = (Pathname) getDevice().cdr().car();
                InputStream input = Utilities.getInputStream(jarFile, inner);
                ZipInputStream zipInputStream = new ZipInputStream(input);
                result =  Utilities.getEntryAsInputStream(zipInputStream, entryPath);
            } else {
                ZipEntry entry = jarFile.getEntry(entryPath);
                if (entry == null) {
                    Debug.trace("Failed to get InputStream for "    
                                + "'" + getNamestring() + "'");
                    // XXX should this be fatal?
                    Debug.assertTrue(false);
                }
                try {
                    result = jarFile.getInputStream(entry);
                } catch (IOException e) {
                    Debug.warn("Failed to get InputStream from "
                                + "'" + getNamestring() + "'"
                                + ": " + e);
                }
            }
        } else if (isURL()) {
            URL url = this.toURL();
            try { 
                result = url.openStream();
            } catch (IOException e) {
                Debug.warn("Failed to get InputStream from "
                            + "'" + getNamestring() + "'"
                            + ": " + e);
            }
        } else {
            File file = getFile();
            try { 
                result = new FileInputStream(file);
            } catch (IOException e) {
                Debug.warn("Failed to get InputStream from "
                            + "'" + getNamestring() + "'"
                            + ": " + e);
            }
        }
        return result;
    }

    /** @return Time in milliseconds since the UNIX epoch at which the
     * resource was last modified, or 0 if the time is unknown.
     */
    public long getLastModified() {
        if (!(isJar() || isURL())) {
            File f = getFile();
            return f.lastModified();
        }

        if (isJar()) {
            // JAR cases
            // 0.  JAR from URL 
            // 1.  JAR
            // 2.  JAR in JAR
            // 3.  Entry in JAR
            // 4.  Entry in JAR in JAR
            String entryPath = asEntryPath();
            Cons d = (Cons)getDevice();
            if (d.cdr().equals(NIL)) {
                if (entryPath.length() == 0) {
                    LispObject o = d.car();
                        // 0. JAR from URL
                        // 1. JAR
                    return ((Pathname)o).getLastModified();
                } else {
                    // 3. Entry in JAR
                    final ZipEntry entry 
                        = ZipCache.get((Pathname)getDevice().car()).getEntry(entryPath);
                    if (entry == null) {
                        return 0;
                    }
                    final long time = entry.getTime();
                    if (time == -1) {
                        return 0;
                    }
                    return time;
                }
            } else {
                ZipFile outerJar = ZipCache.get((Pathname)d.car());
                if (entryPath.length() == 0) {
                    // 4.  JAR in JAR
                    String jarPath = ((Pathname)d.cdr()).asEntryPath();
                    final ZipEntry entry = outerJar.getEntry(jarPath);
                    final long time = entry.getTime();
                    if (time == -1) {
                        return 0;
                    }
                    return time;
                } else {
                    // 5. Entry in JAR in JAR
                    String innerJarPath = ((Pathname)d.cdr()).asEntryPath();
                    ZipEntry entry = outerJar.getEntry(entryPath);
                    ZipInputStream innerJarInputStream
                        = Utilities.getZipInputStream(outerJar, innerJarPath);
                    ZipEntry innerEntry = Utilities.getEntry(innerJarInputStream,
                                                             entryPath);
                    long time = innerEntry.getTime();
                    if (time == -1) {
                        return 0;
                    }
                    return time;
                }
            }
        }
        if (isURL()) {
            return getURLConnection().getLastModified();
        }
        return 0;
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
            Pathname defaultedPathname =
                mergePathnames(pathname,
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
            Pathname oldTruename = (Pathname) truename(oldPathname, true);
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
                
            Pathname defaultedNewName = mergePathnames(newName, oldTruename, NIL);

            File source = oldTruename.getFile();
            File destination = null;
            if (defaultedNewName instanceof LogicalPathname) {
                destination = LogicalPathname.translateLogicalPathname((LogicalPathname)defaultedNewName)
                    .getFile();
            } else {
                destination = defaultedNewName.getFile();
            }
            // By default, MSDOG doesn't allow one to remove files that are open.
            if (Utilities.isPlatformWindows) {
              if (destination.isFile()) {
                ZipCache.remove(destination);
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
            super("file-namestring", "pathname");
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
    
    public URL toURL() {
        try {
            if (isURL()) {
                return new URL(getNamestring());
            } else {
                return toFile().toURI().toURL();
            }
        } catch (MalformedURLException e) {
            error(new LispError(getNamestring() + " is not a valid URL"));
            return null; // not reached
        }
    }

    public File toFile() {
        if(!isURL()) {
            return new File(getNamestring());
        } else {
            throw new RuntimeException(this + " does not represent a file");
        }
    }

    static {
        LispObject obj = Symbol.DEFAULT_PATHNAME_DEFAULTS.getSymbolValue();
        Symbol.DEFAULT_PATHNAME_DEFAULTS.setSymbolValue(coerceToPathname(obj));
    }

    
    @DocString(name="uri-decode",
               args="string",
               returns="string",
    doc="Decode STRING percent escape sequences in the manner of URI encodings.")
    private static final Primitive URI_DECODE = new pf_uri_decode();
    private static final class pf_uri_decode extends Primitive {
        pf_uri_decode() {
            super("uri-decode", PACKAGE_EXT, true);
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (!(arg instanceof AbstractString)) {
                return type_error(arg, Symbol.STRING);
            }
            String result = uriDecode(((AbstractString)arg).toString());
            return new SimpleString(result);
        }
    };

    static String uriDecode(String s) {
        try {
            URI uri = new URI("file://foo?" + s);
            return uri.getQuery();
        } catch (URISyntaxException e) {}
        return null;  // Error
    }
    
    @DocString(name="uri-encode",
               args="string",
               returns="string",
    doc="Encode percent escape sequences in the manner of URI encodings.")
    private static final Primitive URI_ENCODE = new pf_uri_encode();
    private static final class pf_uri_encode extends Primitive {
        pf_uri_encode() {
            super("uri-encode", PACKAGE_EXT, true);
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (!(arg instanceof AbstractString)) {
                return type_error(arg, Symbol.STRING);
            }
            String result = uriEncode(((AbstractString)arg).toString());
            return new SimpleString(result);
        }
    };

    static String uriEncode(String s) {
        // The constructor we use here only allows absolute paths, so
        // we manipulate the input and output correspondingly.
        String u;
        if (!s.startsWith("/")) {
            u = "/" + s;
        } else {
            u = new String(s);
        }
        try {
            URI uri = new URI("file", "", u, "");
            String result = uri.getRawPath();
            if (!s.startsWith("/")) {
                return result.substring(1);
            } 
            return result;
        } catch (URISyntaxException e) {
            Debug.assertTrue(false);
        }
        return null; // Error
    }


    File getFile() {
        String namestring = getNamestring(); // XXX UNC pathnames currently have no namestring
        if (namestring != null) {
            return new File(namestring);
        }
        error(new FileError("Pathname has no namestring: " + princToString(),
                        this));
        // Not reached.
        return null;
    }
    public static Pathname getDirectoryPathname(File file) {
        try {
            String namestring = file.getCanonicalPath();
            if (namestring != null && namestring.length() > 0) {
                if (namestring.charAt(namestring.length() - 1) != File.separatorChar) {
                    namestring = namestring.concat(File.separator);
                }
            }
            return Pathname.create(namestring);
        } catch (IOException e) {
            error(new LispError(e.getMessage()));
            // Not reached.
            return null;
        }
    }

  // BEGIN REMOVE ME use standard Factory pattern create() method instead
  public static Pathname make(String s) {
    Pathname p = Pathname.create();
    p.init(s);
    return p;
  }

  public static Pathname makeFrom(Pathname p) {
    return Pathname.create(p);
  }

  public static Pathname makeFrom(Pathname p, String s) {
    p.init(s);
    return p;
  }
  // END REMOVE ME
}

