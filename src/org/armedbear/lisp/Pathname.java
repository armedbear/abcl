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
import java.net.URLDecoder;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

public class Pathname extends LispObject {

    protected LispObject host = NIL;
    protected LispObject device = NIL;
    protected LispObject directory = NIL;
    protected LispObject name = NIL;
    // A string, NIL, :WILD or :UNSPECIFIC.
    protected LispObject type = NIL;
    // A positive integer, or NIL, :WILD, :UNSPECIFIC, or :NEWEST.
    protected LispObject version = NIL;

    private String namestring;

    /** The protocol for changing any instance field (i.e. 'host', 'type', etc.)
     *  is to call this method after changing the field to recompute the namestring.
     *  We could do this with setter/getters, but that choose not to in order to avoid the
     *  performance indirection penalty.
     * 
     *  Although, given the number of bugs that crop up when this
     *  protocol is not adhered to, maybe we should consider it.
     */
    public void invalidateNamestring() {
        namestring = null;
    }
    
    // ### %invalidate-namestring
    private static final Primitive _INVALIDATE_NAMESTRING = new pf_invalidate_namestring();
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

    protected Pathname() {}

    /** Copy constructor which shares no structure with the original. */
    protected Pathname(Pathname p) {
        if (p.host != NIL) {
            if (p.host instanceof SimpleString) {
                host = new SimpleString(((SimpleString)p.host).getStringValue());
            } else  if (p.host instanceof Symbol) {
                host = p.host;
            } else if (p.host instanceof Cons) {
                host = new Cons((Cons)p.host);
            } else {
                Debug.assertTrue(false);
            }
        }
        if (p.device != NIL) {
            if (p.device instanceof SimpleString) {
                device = new SimpleString(((SimpleString)p.device).getStringValue());
            } else if (p.device instanceof Cons) {
                Cons jars = (Cons)p.device;
                device = new Cons(NIL, NIL);
                LispObject first = jars.car();
                if (first instanceof Pathname) {
                    ((Cons)device).car = new Pathname((Pathname)first);
                } else {
                    Debug.assertTrue(false);
                }
                if (!jars.cdr().equals(NIL)) {
                    if (jars.cdr() instanceof Cons) {
                        ((Cons)device).cdr = new Cons(new Pathname((Pathname)jars.cdr().car()), NIL);
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
                name = new SimpleString(((SimpleString)p.name).getStringValue());
            } else if (p.name instanceof Symbol) {
                name = p.name;
            } else {
                Debug.assertTrue(false);
            }
        } 
        if (p.type != NIL) {
            if (p.type instanceof SimpleString) {
                type = new SimpleString(((SimpleString)p.type).getStringValue());
            } else if (p.type instanceof Symbol) {
                type = p.type;
            } else {
                Debug.assertTrue(false);
            }
        }
    }

    public Pathname(String s) {
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

    public Pathname(URL url) {
        if ("file".equals(url.getProtocol())) {
            String s;
            try {
                s = URLDecoder.decode(url.getPath(), "UTF-8");
            } catch (java.io.UnsupportedEncodingException uee) {
                // Can't happen: every Java is supposed to support
                // at least UTF-8 encoding
                Debug.assertTrue(false);
                s = null;
            }
            if (s != null) {
		if (Utilities.isPlatformWindows) {
		    //  Workaround for Java's idea of URLs
		    //  new (URL"file:///c:/a/b").getPath() --> "/c:/a/b"
                    //  whereas we need "c" to be the DEVICE.
		    if (s.length() > 2 
			&& s.charAt(0) == '/'
			&& s.charAt(2) == ':') {
			s = s.substring(1);
		    }
		}
                init(s);
                return;
            }
        } else {
            init(url.toString());
            return;
        }
        error(new LispError("Failed to construct Pathname from URL: "
                            + "'" + url.toString() + "'"));
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
            directory = new Cons(Keyword.RELATIVE);
            return;
        }
        if (s.equals("..") || s.equals("../")) {
            directory = list(Keyword.RELATIVE, Keyword.UP);
            return;
        }
        if (Utilities.isPlatformWindows) {
            if (s.startsWith("\\\\")) {
                //UNC path support
                // match \\<server>\<share>\[directories-and-files]

                int shareIndex = s.indexOf('\\', 2);
                int dirIndex = s.indexOf('\\', shareIndex + 1);

                if (shareIndex == -1 || dirIndex == -1) {
                    error(new LispError("Unsupported UNC path format: \"" + s + '"'));
                }

                host = new SimpleString(s.substring(2, shareIndex));
                device = new SimpleString(s.substring(shareIndex + 1, dirIndex));

                Pathname p = new Pathname(s.substring(dirIndex));
                directory = p.directory;
                name = p.name;
                type = p.type;
                version = p.version;
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
                Pathname p = new Pathname(s);
                jars = jars.push(p.device.car());
            }
            if (jar.startsWith("jar:file:")) {
                String jarString 
                    = jar.substring("jar:".length(),
                                    jar.length() - jarSeparator.length());
                // Use URL constructor to normalize Windows' use of device
                URL url = null;
                try {
                    url = new URL(jarString);
                } catch (MalformedURLException e) {
                    error(new LispError("Failed to parse '" + jarString + "'"
                            + " as URL:"
                            + e.getMessage()));
                }
                Pathname jarPathname = new Pathname(url);
                jars = jars.push(jarPathname);
            } else {
                URL url = null;
                try {
                    url = new URL(jar.substring("jar:".length(), jar.length() - 2));
                    Pathname p = new Pathname(url);
                    jars = jars.push(p);
                } catch (MalformedURLException e) {
                    error(new LispError("Failed to parse URL "
                                        + "'" + url + "'"
                                        + e.getMessage()));
                }
            }
            jars = jars.nreverse();
            device = jars;
            invalidateNamestring();
            return;
        }

        // An entry in a JAR file
        final int separatorIndex = s.lastIndexOf(jarSeparator);
        if (separatorIndex > 0 && s.startsWith("jar:")) {
            final String jarURL = s.substring(0, separatorIndex + jarSeparator.length());
            Pathname d = new Pathname(jarURL);
            if (device instanceof Cons) {
                LispObject[] jars = d.copyToArray();
                //  XXX Is this ever reached?  If so, need to append lists
                Debug.assertTrue(false);
            } else {
                device = d.device;
            }
            s = "/" + s.substring(separatorIndex + jarSeparator.length());
            Pathname p = new Pathname(s);
            directory = p.directory;
            name = p.name;
            type = p.type;
            version = p.version;
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
                Pathname p = new Pathname(url.getFile());
                this.host = p.host;
                this.device = p.device;
                this.directory = p.directory;
                this.name = p.name;
                this.type = p.type;
                this.version = p.version;
                return;
            }
            Debug.assertTrue(scheme != null);
            URI uri = null;
            try { 
                uri = url.toURI().normalize();
            } catch (URISyntaxException e) {
                error(new LispError("Could form URI from "
                                    + "'" + url + "'"
                                    + " because: " + e));
            }
            String authority = uri.getAuthority();
            Debug.assertTrue(authority != null);

            host = NIL;
            host = host.push(SCHEME);
            host = host.push(new SimpleString(scheme));
            host = host.push(AUTHORITY);
            host = host.push(new SimpleString(authority));

            device = NIL;
            
            // URI encode necessary characters
            String path = uri.getRawPath();
            if (path == null) {
                path = "";
            } 
            String query = uri.getRawQuery();
            if (query != null) {
                host = host.push(QUERY);
                host = host.push(new SimpleString(query));
            }
            String fragment = uri.getRawFragment();
            if (fragment != null) {
                host = host.push(FRAGMENT);
                host = host.push(new SimpleString(fragment));
            }
            Pathname p = new Pathname(path != null ? path : ""); 

            directory = p.directory;
            name = p.name;
            type = p.type;
            
            host = host.nreverse();
            invalidateNamestring();
            return;
        }

        if (Utilities.isPlatformWindows) {
            if (!s.contains(jarSeparator)) {
                s = s.replace("/", "\\");
            } else {
              StringBuilder result = new StringBuilder();
              for (int i = 0; i < s.length(); i++) {
                  char c = s.charAt(i);
                  if ( c != '/') {
                      result.append(c);
                  } else {
                      if (i != 0 && s.charAt(i-1) != '!') {
                          result.append("\\");
                      } else {
                          result.append(c);
                      }
                  }
              }
              s = result.toString();
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
                device = new SimpleString(s.charAt(0));
                s = s.substring(2);
            }
        }
        String d = null;
        // Find last file separator char.
        if (Utilities.isPlatformWindows) {
            for (int i = s.length(); i-- > 0;) {
                char c = s.charAt(i);
                if (c == '/' || c == '\\') {
                    d = s.substring(0, i + 1);
                    s = s.substring(i + 1);
                    break;
                }
            }
        } else {
            for (int i = s.length(); i-- > 0;) {
                if (s.charAt(i) == '/') {
                    d = s.substring(0, i + 1);
                    s = s.substring(i + 1);
                    break;
                }
            }
        }
        if (d != null) {
            if (s.equals("..")) {
                d = d.concat(s);
                s = "";
            }
            directory = parseDirectory(d);
        }
        if (s.startsWith(".") 
            // No TYPE can be parsed
            && (s.indexOf(".", 1) == -1 
                || s.substring(s.length() -1).equals("."))) {
            name = new SimpleString(s);
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
                name = Keyword.WILD;
            } else {
                name = new SimpleString(n);
            }
        }
        if (t != null) {
            if (t.equals("*")) {
                type = Keyword.WILD;
            } else {
                type = new SimpleString(t);
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
        parts = parts.push(new Cons("HOST", host));
        parts = parts.push(new Cons("DEVICE", device));
        parts = parts.push(new Cons("DIRECTORY", directory));
        parts = parts.push(new Cons("NAME", name));
        parts = parts.push(new Cons("TYPE", type));
        parts = parts.push(new Cons("VERSION", version));
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

    public final LispObject getDevice() {
        return device;
    }

    public String getNamestring() {
        if (namestring != null) {
            return namestring;
        }
        if (name == NIL && type != NIL) {
            Debug.assertTrue(namestring == null);
            return null;
        }
        if (directory instanceof AbstractString) {
            Debug.assertTrue(false);
        }
        StringBuilder sb = new StringBuilder();
        // "If a pathname is converted to a namestring, the symbols NIL and
        // :UNSPECIFIC cause the field to be treated as if it were empty. That
        // is, both NIL and :UNSPECIFIC cause the component not to appear in
        // the namestring." 19.2.2.2.3.1
        if (host != NIL) {
            Debug.assertTrue(host instanceof AbstractString 
                             || isURL());
            if (isURL()) {
                LispObject scheme = Symbol.GETF.execute(host, SCHEME, NIL);
                LispObject authority = Symbol.GETF.execute(host, AUTHORITY, NIL);
                Debug.assertTrue(scheme != NIL);
                sb.append(scheme.getStringValue());
                sb.append(":");
                if (authority != NIL) {
                    sb.append("//");
                    sb.append(authority.getStringValue());
                }
            } else {
                if (!(this instanceof LogicalPathname)) {
                    sb.append("\\\\"); //UNC file support; if there's a host, it's a UNC path.
                }
                sb.append(host.getStringValue());
                if (this instanceof LogicalPathname) {
                    sb.append(':');
                } else {
                    sb.append(File.separatorChar);
                }
            }
        }
        if (device == NIL) {
        } else if (device == Keyword.UNSPECIFIC) {
        } else if (isJar()) {
            LispObject[] jars = ((Cons) device).copyToArray();
            StringBuilder prefix = new StringBuilder();
            for (int i = 0; i < jars.length; i++) {
                prefix.append("jar:");
                if (!((Pathname)jars[i]).isURL() && i == 0) {
                    sb.append("file:");
                }
                sb.append(((Pathname) jars[i]).getNamestring());
                sb.append("!/");
            }
            sb = prefix.append(sb);
        } else if (device instanceof AbstractString) {
            sb.append(device.getStringValue());
            if (this instanceof LogicalPathname
              || host == NIL) {
                sb.append(':'); // non-UNC paths
            }
        } else {
            Debug.assertTrue(false);
        }
        String directoryNamestring = getDirectoryNamestring();
        if (isJar()) {
            if (directoryNamestring.startsWith("/")) {
                sb.append(directoryNamestring.substring(1));
            } else {
                sb.append(directoryNamestring);
            }
        } else {
            sb.append(directoryNamestring);
        }
        if (name instanceof AbstractString) {
            String n = name.getStringValue();
            if (n.indexOf(File.separatorChar) >= 0) {
                Debug.assertTrue(namestring == null);
                return null;
            }
            sb.append(n);
        } else if (name == Keyword.WILD) {
            sb.append('*');
        }
        if (type != NIL && type != Keyword.UNSPECIFIC) {
            sb.append('.');
            if (type instanceof AbstractString) {
                String t = type.getStringValue();
		// Allow Windows shortcuts to include TYPE
		if (!(t.endsWith(".lnk") && Utilities.isPlatformWindows)) {
		    if (t.indexOf('.') >= 0) {
			Debug.assertTrue(namestring == null);
			return null;
		    }
		}
                sb.append(t);
            } else if (type == Keyword.WILD) {
                sb.append('*');
            } else {
                Debug.assertTrue(false);
            }
        }
        
        if (isURL()) {
            LispObject o = Symbol.GETF.execute(host, QUERY, NIL);
            if (o != NIL) {
                sb.append("?");
                sb.append(o.getStringValue());
            }
            o = Symbol.GETF.execute(host, FRAGMENT, NIL);
            if (o != NIL) {
                sb.append("#");
                sb.append(o.getStringValue());
            }
        }
            
        if (this instanceof LogicalPathname) {
            if (version.integerp()) {
                sb.append('.');
                int base = Fixnum.getValue(Symbol.PRINT_BASE.symbolValue());
                if (version instanceof Fixnum) {
                    sb.append(Integer.toString(((Fixnum) version).value, base).toUpperCase());
                } else if (version instanceof Bignum) {
                    sb.append(((Bignum) version).value.toString(base).toUpperCase());
                }
            } else if (version == Keyword.WILD) {
                sb.append(".*");
            } else if (version == Keyword.NEWEST) {
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
        if (directory != NIL) {
            final char separatorChar;
            if (isJar() || isURL()) {
                separatorChar = '/'; 
            } else {
                separatorChar = File.separatorChar;
            }
            LispObject temp = directory;
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
                  + part.writeToString() + ".",
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
                } else {
                    error(new FileError("Unsupported directory component " + part.writeToString() + ".",
                      this));
                }
                sb.append(separatorChar);
                temp = temp.cdr();
            }
        }
        return sb.toString();
    }

    /** @return The representation of this pathname suitable for referencing an entry in a Zip/JAR file */
    protected String asEntryPath() {
        Pathname p = new Pathname();
        p.directory = directory;
        p.name = name;
        p.type = type;
        p.invalidateNamestring();
        String path = p.getNamestring();
        StringBuilder result = new StringBuilder();
        if (Utilities.isPlatformWindows) {
	    for (int i = 0; i < path.length(); i++) {
		char c = path.charAt(i);
		if (c == '\\') {
		    result.append('/');
		} else {
		    result.append(c);
		}
	    }
        } else  {
            result.append(path);
        }
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
        return ((host.sxhash()
          ^ device.sxhash()
          ^ directory.sxhash()
          ^ name.sxhash()
          ^ type.sxhash()) & 0x7fffffff);
    }

    @Override
    public String writeToString() {
        final LispThread thread = LispThread.currentThread();
        boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
        boolean printEscape = (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
        boolean useNamestring;
        String s = null;
        s = getNamestring();
        if (s != null) {
            useNamestring = true;
            if (printReadably) {
                // We have a namestring. Check for pathname components that
                // can't be read from the namestring.
                if ((host != NIL && !isURL())
                    || version != NIL) 
                {
                    useNamestring = false;
                } else if (name instanceof AbstractString) {
                    String n = name.getStringValue();
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
        } else {
            sb.append("#P(");
            if (host != NIL) {
                sb.append(":HOST ");
                sb.append(host.writeToString());
                sb.append(' ');
            }
            if (device != NIL) {
                sb.append(":DEVICE ");
                sb.append(device.writeToString());
                sb.append(' ');
            }
            if (directory != NIL) {
                sb.append(":DIRECTORY ");
                sb.append(directory.writeToString());
                sb.append(" ");
            }
            if (name != NIL) {
                sb.append(":NAME ");
                sb.append(name.writeToString());
                sb.append(' ');
            }
            if (type != NIL) {
                sb.append(":TYPE ");
                sb.append(type.writeToString());
                sb.append(' ');
            }
            if (version != NIL) {
                sb.append(":VERSION ");
                sb.append(version.writeToString());
                sb.append(' ');
            }
            if (sb.charAt(sb.length() - 1) == ' ') {
                sb.setLength(sb.length() - 1);
            }
            sb.append(')');
        }
        return sb.toString();
    }
    // A logical host is represented as the string that names it.
    // (defvar *logical-pathname-translations* (make-hash-table :test 'equal))
    public static EqualHashTable LOGICAL_PATHNAME_TRANSLATIONS =
      new EqualHashTable(64, NIL, NIL);
    private static final Symbol _LOGICAL_PATHNAME_TRANSLATIONS_ =
      exportSpecial("*LOGICAL-PATHNAME-TRANSLATIONS*", PACKAGE_SYS,
      LOGICAL_PATHNAME_TRANSLATIONS);

    public static Pathname parseNamestring(String s) {
        return new Pathname(s);
    }

    public static boolean isValidURL(String s) {
        try {
            URL url = new URL(s);
        } catch (MalformedURLException e) {
            return false;
        }
        return true;
    }

    public static URL toURL(Pathname p) {
        URL url = null;
        if (!(p.host instanceof Cons)) {
            Debug.assertTrue(false); // XXX
        }
        try {
            url = new URL(p.getNamestring());
        } catch (MalformedURLException e) {
            Debug.assertTrue(false); // XXX
        }
        return url;
    }

    URLConnection getURLConnection() {
        Debug.assertTrue(isURL());
        URL url = Pathname.toURL(this);
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
            String h = getHostString(s);
            if (h != null && LOGICAL_PATHNAME_TRANSLATIONS.get(new SimpleString(h)) != null) {
                // A defined logical pathname host.
                return new LogicalPathname(h, s.substring(s.indexOf(':') + 1));
            }
        }
        return new Pathname(s);
    }

    // XXX was @return Pathname
    public static LogicalPathname parseNamestring(AbstractString namestring,
                                                  AbstractString host) 
    {
        String s = namestring.getStringValue();

        // Look for a logical pathname host in the namestring.        
        String h = getHostString(s);
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
            return new LogicalPathname(host.getStringValue(), s);
        }
        error(new LispError(host.writeToString() + " is not defined as a logical pathname host."));
        // Not reached.
        return null;
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

    static final void checkCaseArgument(LispObject arg) {
        if (arg != Keyword.COMMON && arg != Keyword.LOCAL) {
            type_error(arg, list(Symbol.MEMBER, Keyword.COMMON,
              Keyword.LOCAL));
        }
    }
    // ### %pathname-host
    private static final Primitive _PATHNAME_HOST = new pf_pathname_host();
    private static class pf_pathname_host extends Primitive {
        pf_pathname_host() {
            super("%pathname-host", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second);
            return coerceToPathname(first).host;
        }
    }
    // ### %pathname-device
    private static final Primitive _PATHNAME_DEVICE = new pf_pathname_device(); 
    private static class pf_pathname_device extends Primitive {
        pf_pathname_device() {
            super("%pathname-device", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second);
            return coerceToPathname(first).device;
        }
    }
    // ### %pathname-directory
    private static final Primitive _PATHNAME_DIRECTORY = new pf_pathname_directory();
    private static class pf_pathname_directory extends Primitive {
        pf_pathname_directory() {
            super("%pathname-directory", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second);
            return coerceToPathname(first).directory;
        }
    }
    // ### %pathname-name
    private static final Primitive _PATHNAME_NAME = new pf_pathname_name();
    private static class  pf_pathname_name extends Primitive {
        pf_pathname_name() {
            super ("%pathname-name", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second);
            return coerceToPathname(first).name;
        }
    }
    // ### %pathname-type
    private static final Primitive _PATHNAME_TYPE = new pf_pathname_type();
    private static class pf_pathname_type extends Primitive {
        pf_pathname_type() {
            super("%pathname-type", PACKAGE_SYS, false);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            checkCaseArgument(second);
            return coerceToPathname(first).type;
        }
    }
    // ### pathname-version
    private static final Primitive PATHNAME_VERSION = new pf_pathname_version();
    private static class pf_pathname_version extends Primitive {
        pf_pathname_version() {
            super("pathname-version", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPathname(arg).version;
        }
    }
    // ### namestring
    // namestring pathname => namestring
    private static final Primitive NAMESTRING = new pf_namestring();
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
                                      + pathname.writeToString()));
            }
            return new SimpleString(namestring);
        }
    }
    // ### directory-namestring
    // directory-namestring pathname => namestring
    private static final Primitive DIRECTORY_NAMESTRING = new pf_directory_namestring();
    private static class pf_directory_namestring extends Primitive {
        pf_directory_namestring() {
            super("directory-namestring", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return new SimpleString(coerceToPathname(arg).getDirectoryNamestring());
        }
    }
    // ### pathname pathspec => pathname
    private static final Primitive PATHNAME = new pf_pathname();
    private static class pf_pathname extends Primitive {
        pf_pathname() {
            super("pathname", "pathspec");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPathname(arg);
        }
    }
    // ### %parse-namestring string host default-pathname => pathname, position
    private static final Primitive _PARSE_NAMESTRING = new pf_parse_namestring();
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
                    second = ((LogicalPathname) third).host;
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
    // ### make-pathname
    private static final Primitive MAKE_PATHNAME = new pf_make_pathname();
    private static class pf_make_pathname extends Primitive {
        pf_make_pathname() {
            super("make-pathname",
                  "&key host device directory name type version defaults case");
        }
        @Override
        public LispObject execute(LispObject[] args) {
            return _makePathname(args);
        }
    }

    // Used by the #p reader.
    public static final Pathname makePathname(LispObject args) {
        return _makePathname(args.copyToArray());
    }

    public static final Pathname makePathname(File file) {
        String namestring = null;
        try {
            namestring = file.getCanonicalPath();
        } catch (IOException e) {
            Debug.trace("Failed to make a Pathname from "
              + "'" + file + "'");
            return null;
        }
        return new Pathname(namestring);
    }

    static final Pathname _makePathname(LispObject[] args) {
        if (args.length % 2 != 0) {
            error(new ProgramError("Odd number of keyword arguments."));
        }
        LispObject host = NIL;
        LispObject device = NIL;
        LispObject directory = NIL;
        LispObject name = NIL;
        LispObject type = NIL;
        LispObject version = NIL;
        Pathname defaults = null;
        boolean deviceSupplied = false;
        boolean nameSupplied = false;
        boolean typeSupplied = false;
        for (int i = 0; i < args.length; i += 2) {
            LispObject key = args[i];
            LispObject value = args[i + 1];
            if (key == Keyword.HOST) {
                host = value;
            } else if (key == Keyword.DEVICE) {
                device = value;
                deviceSupplied = true;
            } else if (key == Keyword.DIRECTORY) {
                if (value instanceof AbstractString) {
                    directory = list(Keyword.ABSOLUTE, value);
                } else if (value == Keyword.WILD) {
                    directory = list(Keyword.ABSOLUTE, Keyword.WILD);
                } else {
                    directory = value;
                }
            } else if (key == Keyword.NAME) {
                name = value;
                nameSupplied = true;
            } else if (key == Keyword.TYPE) {
                type = value;
                typeSupplied = true;
            } else if (key == Keyword.VERSION) {
                version = value;
            } else if (key == Keyword.DEFAULTS) {
                defaults = coerceToPathname(value);
            } else if (key == Keyword.CASE) {
                // Ignored.
            }
        }
        if (defaults != null) {
            if (host == NIL) {
                host = defaults.host;
            }
            if (directory == NIL) {
                directory = defaults.directory;
            }
            if (!deviceSupplied) {
                device = defaults.device;
            }
            if (!nameSupplied) {
                name = defaults.name;
            }
            if (!typeSupplied) {
                type = defaults.type;
            }
        }
        final Pathname p;
        final boolean logical;
        if (host != NIL) {
            if (host instanceof AbstractString) {
                host = LogicalPathname.canonicalizeStringComponent((AbstractString) host);
            }
            if (LOGICAL_PATHNAME_TRANSLATIONS.get(host) == null) {
                // Not a defined logical pathname host.
                error(new LispError(host.writeToString() + " is not defined as a logical pathname host."));
            }
            p = new LogicalPathname();
            logical = true;
            p.host = host;
            p.device = Keyword.UNSPECIFIC;
        } else {
            p = new Pathname();
            logical = false;
        }
        if (device != NIL) {
            if (logical) {
                // "The device component of a logical pathname is always :UNSPECIFIC."
                if (device != Keyword.UNSPECIFIC) {
                    error(new LispError("The device component of a logical pathname must be :UNSPECIFIC."));
                }
            } else {
                p.device = device;
            }
        }
        if (directory != NIL) {
            if (logical) {
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
                    p.directory = d.nreverse();
                } else if (directory == Keyword.WILD || directory == Keyword.WILD_INFERIORS) {
                    p.directory = directory;
                } else {
                    error(new LispError("Invalid directory component for logical pathname: " + directory.writeToString()));
                }
            } else {
                p.directory = directory;
            }
        }
        if (name != NIL) {
            if (logical && name instanceof AbstractString) {
                p.name = LogicalPathname.canonicalizeStringComponent((AbstractString) name);
            } else if (name instanceof AbstractString) {
                p.name = validateStringComponent((AbstractString) name);
            } else {
                p.name = name;
            }
        }
        if (type != NIL) {
            if (logical && type instanceof AbstractString) {
                p.type = LogicalPathname.canonicalizeStringComponent((AbstractString) type);
            } else {
                p.type = type;
            }
        }
        p.version = version;
        return p;
    }

    private static final AbstractString validateStringComponent(AbstractString s) {
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
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
        LispObject temp = directory;
        while (temp != NIL) {
            LispObject first = temp.car();
            temp = temp.cdr();
            if (first == Keyword.ABSOLUTE || first == Keyword.WILD_INFERIORS) {
                LispObject second = temp.car();
                if (second == Keyword.UP || second == Keyword.BACK) {
                    if (signalError) {
                        StringBuilder sb = new StringBuilder();
                        sb.append(first.writeToString());
                        sb.append(" may not be followed immediately by ");
                        sb.append(second.writeToString());
                        sb.append('.');
                        error(new FileError(sb.toString(), this));
                    }
                    return false;
                }
            }
        }
        return true;
    }
    // ### pathnamep
    private static final Primitive PATHNAMEP = new pf_pathnamep();
    private static class pf_pathnamep extends Primitive  {
        pf_pathnamep() {
            super("pathnamep", "object");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Pathname ? T : NIL;
        }
    }
    // ### logical-pathname-p
    private static final Primitive LOGICAL_PATHNAME_P = new pf_logical_pathname_p();
    private static class pf_logical_pathname_p extends Primitive {
        pf_logical_pathname_p() {
            super("logical-pathname-p", PACKAGE_SYS, true, "object");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof LogicalPathname ? T : NIL;
        }
    }
    // ### user-homedir-pathname &optional host => pathname
    private static final Primitive USER_HOMEDIR_PATHNAME = new pf_user_homedir_pathname();
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
                return new Pathname(s);
            }
            case 1:
                return NIL; 
            default:
                return error(new WrongNumberOfArgumentsException(this));
            }
        }
    }

    // ### list-directory directory
    private static final Primitive LIST_DIRECTORY = new pf_list_directory();
    private static class pf_list_directory extends Primitive {
        pf_list_directory() {
            super("list-directory", PACKAGE_SYS, true, "directory");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname pathname = coerceToPathname(arg);
            if (pathname instanceof LogicalPathname) {
                pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
            }

            LispObject result = NIL;
            if (pathname.isJar()) {
                String directory = pathname.asEntryPath();
                Debug.assertTrue(directory != null);  // We should only be listing directories

                if (pathname.device.cdr() instanceof Cons) {
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

                ZipFile jar = ZipCache.get((Pathname)pathname.device.car());
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
                        Pathname p = new Pathname(namestring);
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
                        for (int i = files.length; i-- > 0;) {
                            File file = files[i];
                            Pathname p;
                            if (file.isDirectory()) {
                                p = Utilities.getDirectoryPathname(file);
                            } else {
                                p = new Pathname(file.getCanonicalPath());
                            }
                            result = new Cons(p, result);
                        }
                    } catch (IOException e) {
                        return error(new FileError("Unable to list directory " + pathname.writeToString() + ".",
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

    // ### match-wild-jar-pathname wild-jar-pathname
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
            Pathname jarPathname = new Pathname(pathname);
            jarPathname.directory = NIL;
            jarPathname.name = NIL;
            jarPathname.type = NIL;
            jarPathname.invalidateNamestring();
            LispObject jarTruename = truename(jarPathname, false); 
            
            // We can't match anything in a non-existent jar 
            if (jarTruename == NIL) {
                return NIL;
            }
            
            LispObject result = NIL;
            String wild = "/" + pathname.asEntryPath();

            if (pathname.device.cdr() instanceof Cons) {
                return error(new FileError("Unimplemented directory listing of JAR within JAR.", pathname));
            }
            
            final SimpleString wildcard = new SimpleString(wild);

            ZipFile jar = ZipCache.get((Pathname)pathname.device.car());

            for (Enumeration<? extends ZipEntry> entries = jar.entries(); entries.hasMoreElements();) {
                ZipEntry entry = entries.nextElement();
                String entryName = "/" + entry.getName();
                
                LispObject matches = Symbol.PATHNAME_MATCH_P
                    .execute(new SimpleString(entryName), wildcard);

                if (!matches.equals(NIL)) {
                    String namestring = new String(pathname.getNamestring());
                    namestring = namestring.substring(0, namestring.lastIndexOf("!/") + 2)
                        + entry.getName();
                    Pathname p = new Pathname(namestring);
                    result = new Cons(p, result);
                }
            }
            return result;
        }
    }

    public boolean isAbsolute()  {
        if (!directory.equals(NIL) || !(directory == null)) {
            if (directory instanceof Cons) {
                if (((Cons)directory).car().equals(Keyword.ABSOLUTE)) {
                    return true;
                }
            }
        }
        return false;
    }

    // ### PATHNAME-JAR-P 
    private static final Primitive PATHNAME_JAR_P = new pf_pathname_jar_p();
    private static class pf_pathname_jar_p extends Primitive {
        pf_pathname_jar_p() {
            super("pathname-jar-p", PACKAGE_EXT, true, "pathname",
                  "Predicate for whether PATHNAME references a JAR.");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname p = coerceToPathname(arg);
            return p.isJar() ? T : NIL;
        }
    }

    public boolean isJar() {
        return (device instanceof Cons);
    }

    // ### PATHNAME-URL-P 
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
        return (host instanceof Cons);
    }

    public boolean isWild() {
        if (host == Keyword.WILD || host == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (device == Keyword.WILD || device == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (directory instanceof Cons) {
            if (memq(Keyword.WILD, directory)) {
                return true;
            }
            if (memq(Keyword.WILD_INFERIORS, directory)) {
                return true;
            }
            Cons d = (Cons) directory;
            while (true) {
                if (d.car() instanceof AbstractString) {
                    String s = d.car().writeToString();
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
        if (name == Keyword.WILD || name == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (name instanceof AbstractString) {
            if (name.writeToString().contains("*")) {
                return true;
            }
        }
        if (type == Keyword.WILD || type == Keyword.WILD_INFERIORS) {
            return true;
        }
        if (type instanceof AbstractString) {
            if (type.writeToString().contains("*")) {
                return true;
            }
        }
        if (version == Keyword.WILD || version == Keyword.WILD_INFERIORS) {
            return true;
        }
        return false;
    }
    // ### %wild-pathname-p
    private static final Primitive _WILD_PATHNAME_P = new pf_wild_pathname_p();
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
                if (pathname.directory instanceof Cons) {
                    if (memq(Keyword.WILD, pathname.directory)) {
                        return T;
                    }
                    if (memq(Keyword.WILD_INFERIORS, pathname.directory)) {
                        return T;
                    }
                }
                return NIL;
            }
            LispObject value;
            if (second == Keyword.HOST) {
                value = pathname.host;
            } else if (second == Keyword.DEVICE) {
                value = pathname.device;
            } else if (second == Keyword.NAME) {
                value = pathname.name;
            } else if (second == Keyword.TYPE) {
                value = pathname.type;
            } else if (second == Keyword.VERSION) {
                value = pathname.version;
            } else {
                return error(new ProgramError("Unrecognized keyword "
                                              + second.writeToString() + "."));
            }
            if (value == Keyword.WILD || value == Keyword.WILD_INFERIORS) {
                return T;
            } else {
                return NIL;
            }
        }
    }

    // ### merge-pathnames pathname &optional default-pathname default-version"
    private static final Primitive MERGE_PATHNAMES = new pf_merge_pathnames();
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
        Pathname p = new Pathname(pathname);
        Pathname d;

        if (pathname instanceof LogicalPathname) {
            result = new LogicalPathname();
            d = new Pathname(defaultPathname);
        } else {
            result = new Pathname();
            if (defaultPathname instanceof LogicalPathname) {
                d = LogicalPathname.translateLogicalPathname((LogicalPathname) defaultPathname);
            } else {
                d = new Pathname(defaultPathname);
            }
        }
        if (pathname.host != NIL) {
            result.host = p.host;
        } else {
            result.host = d.host;
        }

        if (pathname.device != NIL) { // XXX if device represent JARs we want to merge
            result.device = p.device;
        } else {
            if (!p.isURL()) {
                result.device = d.device;
            }
        }

        if (pathname.isJar()) {
            Cons jars = (Cons)result.device;
            LispObject jar = jars.car;
            if (jar instanceof Pathname) {
                Pathname defaults = new Pathname(d);
                if (defaults.isJar()) {
                    defaults.device = NIL;
                }
                Pathname o = mergePathnames((Pathname)jar, defaults);
                if (o.directory instanceof Cons
                    && ((Cons)o.directory).length() == 1) { // i.e. (:ABSOLUTE) or (:RELATIVE)
                    o.directory = NIL;
                }
                ((Cons)result.device).car = o;
            }
            result.directory = p.directory;
        } else {
            result.directory = mergeDirectories(p.directory, d.directory);
        }

        if (pathname.name != NIL) {
            result.name = p.name;
        } else {
            result.name = d.name;
        }
        if (pathname.type != NIL) {
            result.type = p.type;
        } else {
            result.type = d.type;
        }
        if (pathname.version != NIL) {
            result.version = pathname.version;
        } else if (pathname.name instanceof AbstractString) {
            result.version = defaultVersion;
        } else if (defaultPathname.version != NIL) {
            result.version = defaultPathname.version;
        } else {
            result.version = defaultVersion;
        }
        if (pathname instanceof LogicalPathname) {
            // When we're returning a logical
            result.device = Keyword.UNSPECIFIC;
            if (result.directory.listp()) {
                LispObject original = result.directory;
                LispObject canonical = NIL;
                while (original != NIL) {
                    LispObject component = original.car();
                    if (component instanceof AbstractString) {
                        component = LogicalPathname.canonicalizeStringComponent((AbstractString) component);
                    }
                    canonical = canonical.push(component);
                    original = original.cdr();
                }
                result.directory = canonical.nreverse();
            }
            if (result.name instanceof AbstractString) {
                result.name = LogicalPathname.canonicalizeStringComponent((AbstractString) result.name);
            }
            if (result.type instanceof AbstractString) {
                result.type = LogicalPathname.canonicalizeStringComponent((AbstractString) result.type);
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
        if (pathname instanceof LogicalPathname) {
            pathname = LogicalPathname.translateLogicalPathname((LogicalPathname) pathname);
        }
        if (pathname.isWild()) {
            return error(new FileError("Bad place for a wild pathname.",
                                       pathname));
        }
        if (!(pathname.isJar() || pathname.isURL())) {
            pathname
                = mergePathnames(pathname,
                                 coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
                                 NIL);
            final String namestring = pathname.getNamestring();
            if (namestring == null) {
                return error(new FileError("Pathname has no namestring: " 
                                           + pathname.writeToString(),
                                           pathname));
            }
            
            final File file = new File(namestring);
            if (file.isDirectory()) {
                return Utilities.getDirectoryPathname(file);
            }
            if (file.exists()) {
                try {
                    return new Pathname(file.getCanonicalPath());
                } catch (IOException e) {
                    return error(new FileError(e.getMessage(), pathname));
                }
            }
        } else if (pathname.isURL()) {
            if (pathname.getInputStream() != null) {
              // If there is no type, query or fragment, we check to
              // see if there is URL available "underneath".
              if (pathname.name != NIL 
                  && pathname.type == NIL
                  && Symbol.GETF.execute(pathname.host, QUERY, NIL) == NIL
                  && Symbol.GETF.execute(pathname.host, FRAGMENT, NIL) == NIL) {
                Pathname p = new Pathname(pathname.getNamestring() + "/");
                if (p.getInputStream() != null) {
                  return p;
                }
              }
              return pathname;
            }
        } else
        jarfile: {
            // Possibly canonicalize jar file directory
            Cons jars = (Cons) pathname.device;
            LispObject o = jars.car();
            if (o instanceof Pathname && ! (((Pathname)o).isURL())) {
                LispObject truename = Pathname.truename((Pathname)o, errorIfDoesNotExist);
                if (truename != null
                    && truename instanceof Pathname) {
                    jars.car = (Pathname)truename;
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
        if (errorIfDoesNotExist) {
            StringBuilder sb = new StringBuilder("The file ");
            sb.append(pathname.writeToString());
            sb.append(" does not exist.");
            return error(new FileError(sb.toString(), pathname));
        }
        return NIL;
    }


    /** Make a JarURL from a Pathname that references a file */
    private static URL makeJarURL(Pathname p) {
        String jarURL = "jar:file:" + p.getNamestring() + "!/";
        URL result = null;
        try {
            result = new URL(jarURL);
        } catch (MalformedURLException ex) {
            // XXX
            Debug.trace("Could not form URL from pathname "
              + "'" + jarURL + "'"
              + " because " + ex);
        }
        return result;
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

    public InputStream getInputStream() {
        InputStream result = null;
        if (isJar()) {
            String entryPath = asEntryPath();
            // XXX We only return the bytes of an entry in a JAR
            Debug.assertTrue(entryPath != null);
            ZipFile jarFile = ZipCache.get((Pathname)device.car());
            Debug.assertTrue(jarFile != null);
            // Is this a JAR within a JAR?
            if (device.cdr() instanceof Cons) {
                Pathname inner = (Pathname) device.cdr().car();
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
            URL url = toURL(this);
            try { 
                result = url.openStream();
            } catch (IOException e) {
                Debug.warn("Failed to get InputStream from "
                            + "'" + getNamestring() + "'"
                            + ": " + e);
            }
        } else {
            File file = Utilities.getFile(this);
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
            File f = Utilities.getFile(this);
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
            Cons d = (Cons)device;
            if (d.cdr().equals(NIL)) {
                if (entryPath.length() == 0) {
                    LispObject o = d.car();
                        // 0. JAR from URL
                        // 1. JAR
                    return ((Pathname)o).getLastModified();
                } else {
                    // 3. Entry in JAR
                    final ZipEntry entry 
                        = ZipCache.get((Pathname)device.car()).getEntry(entryPath);
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

    // ### mkdir pathname
    private static final Primitive MKDIR = new pf_mkdir();
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
                    
            File file = Utilities.getFile(defaultedPathname);
            return file.mkdir() ? T : NIL;
        }
    }

    // ### rename-file filespec new-name => defaulted-new-name, old-truename, new-truename
    private static final Primitive RENAME_FILE = new pf_rename_file();
    private static class pf_rename_file extends Primitive {
        pf_rename_file() {
            super("rename-file", "filespec new-name");
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            final Pathname original = (Pathname) truename(first, true);
            final String originalNamestring = original.getNamestring();
            Pathname newName = coerceToPathname(second);
            if (newName.isWild()) {
                error(new FileError("Bad place for a wild pathname.", newName));
            }
            if (original.isJar()) {
                error(new FileError("Bad place for a jar pathname.", original));
            }
            if (newName.isJar()) {
                error(new FileError("Bad place for a jar pathname.", newName));
            }
            if (original.isURL()) {
                error(new FileError("Bad place for a URL pathname.", original));
            }
            if (newName.isURL()) {
                error(new FileError("Bad place for a jar pathname.", newName));
            }
                
            newName = mergePathnames(newName, original, NIL);
            final String newNamestring;
            if (newName instanceof LogicalPathname) {
                newNamestring = LogicalPathname.translateLogicalPathname((LogicalPathname) newName).getNamestring();
            } else {
                newNamestring = newName.getNamestring();
            }
            if (originalNamestring != null && newNamestring != null) {
                final File source = new File(originalNamestring);
                final File destination = new File(newNamestring);
                if (Utilities.isPlatformWindows) {
                    if (destination.isFile()) {
			ZipCache.remove(destination);
                        destination.delete();
                    }
                }
                if (source.renameTo(destination)) { // Success!
                        return LispThread.currentThread().setValues(newName, original,
                                                                    truename(newName, true));
                }
            }
            return error(new FileError("Unable to rename "
                                       + original.writeToString()
                                       + " to " + newName.writeToString()
                                       + "."));
        }
    }

    // ### file-namestring pathname => namestring
    private static final Primitive FILE_NAMESTRING = new pf_file_namestring();
    private static class pf_file_namestring extends Primitive {
        pf_file_namestring() {
            super("file-namestring", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            Pathname p = coerceToPathname(arg);
            StringBuilder sb = new StringBuilder();
            if (p.name instanceof AbstractString) {
                sb.append(p.name.getStringValue());
            } else if (p.name == Keyword.WILD) {
                sb.append('*');
            } else {
                return NIL;
            }
            if (p.type instanceof AbstractString) {
                sb.append('.');
                sb.append(p.type.getStringValue());
            } else if (p.type == Keyword.WILD) {
                sb.append(".*");
            }
            return new SimpleString(sb);
        }
    }

    // ### host-namestring pathname => namestring
    private static final Primitive HOST_NAMESTRING = new pf_host_namestring();
    private static class pf_host_namestring extends Primitive {
        pf_host_namestring() {
            super("host-namestring", "pathname");
        }
        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPathname(arg).host; // XXX URL-PATHNAME
        }
    }
    
    public String toString() {
        return getNamestring();
    }

    public URL toURL() throws MalformedURLException {
	if(isURL()) {
	    return new URL(getNamestring());
	} else {
	    return toFile().toURL();
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

}

