/* 
 * URLPathname.java
 *
 * Copyright (C) 2020 @easye
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
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URI;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.text.MessageFormat;

public class URLPathname
  extends Pathname
{
  static public final Symbol SCHEME = internKeyword("SCHEME");
  static public final Symbol AUTHORITY = internKeyword("AUTHORITY");
  static public final Symbol QUERY = internKeyword("QUERY");
  static public final Symbol FRAGMENT = internKeyword("FRAGMENT");

  protected URLPathname() {}

  public static URLPathname create() {
    return new URLPathname();
  }

  public static URLPathname create(Pathname p) {
    if (p instanceof URLPathname) {
      URLPathname result = new URLPathname();
      result.copyFrom(p);
      return result;
    }
    return (URLPathname)createFromFile((Pathname)p);
  }

  public static URLPathname create(URL url) {
    return URLPathname.create(url.toString());
  }

  public static URLPathname create(URI uri) {
    return URLPathname.create(uri.toString());
  }

  static public final LispObject FILE = new SimpleString("file");
  public static URLPathname createFromFile(Pathname p) {
    URLPathname result = new URLPathname();
    result.copyFrom(p);
    LispObject scheme = NIL;
    scheme = scheme.push(FILE).push(SCHEME);
    result.setHost(scheme);
    return result;
  }

  public static URLPathname create(String s) {
    if (!isValidURL(s)) {
      parse_error("Cannot form a PATHNAME-URL from " + s);
    }
    if (s.startsWith(JarPathname.JAR_URI_PREFIX)) {
      return JarPathname.create(s);
    }

    URLPathname result = new URLPathname();
    URL url = null;
    try {
      url = new URL(s);
    } catch (MalformedURLException e) {
      parse_error("Malformed URL in namestring '" + s + "': " + e.toString());
      return (URLPathname) UNREACHED;
    }
    String scheme = url.getProtocol();
    if (scheme.equals("file")) {
      URI uri = null;
      try {
        uri = new URI(s);
      } catch (URISyntaxException ex) {
        parse_error("Improper URI syntax for "
                    + "'" + url.toString() + "'"
                    + ": " + ex.toString());
        return (URLPathname)UNREACHED;
      }
            
      String uriPath = uri.getPath();
      if (null == uriPath) {
        // Under Windows, deal with pathnames containing
        // devices expressed as "file:z:/foo/path"
        uriPath = uri.getSchemeSpecificPart();
        if (uriPath == null || uriPath.equals("")) {
          parse_error("The namestring URI has no path: " + uri);
          return (URLPathname)UNREACHED;
        }
      }
      final File file = new File(uriPath);
      String path = file.getPath();
      if (uri.toString().endsWith("/") && !path.endsWith("/")) {
        path += "/";
      }
      final Pathname p = (Pathname)Pathname.create(path);
      LispObject host = NIL.push(FILE).push(SCHEME);
      result
        .setHost(host)
        .setDevice(p.getDevice())
        .setDirectory(p.getDirectory())
        .setName(p.getName())
        .setType(p.getType())
        .setVersion(p.getVersion());
      return result;  
    }
    Debug.assertTrue(scheme != null);
    URI uri = null;
    try { 
      uri = url.toURI().normalize();
    } catch (URISyntaxException e) {
      parse_error("Couldn't form URI from "
                  + "'" + url + "'"
                  + " because: " + e);
      return (URLPathname)UNREACHED;
    }
    String authority = uri.getAuthority();
    if (authority == null) {
      authority = url.getAuthority();
    }

    LispObject host = NIL;
    host = host.push(SCHEME).push(new SimpleString(scheme));
    if (authority != null) {
      host = host.push(AUTHORITY).push(new SimpleString(authority));
    }
    String query = uri.getRawQuery();
    if (query != null) {
      host = host.push(QUERY).push(new SimpleString(query));
    }
    String fragment = uri.getRawFragment();
    if (fragment != null) {
      host = host.push(FRAGMENT).push(new SimpleString(fragment));
    }
    host = host.nreverse();
    result.setHost(host);

    // URI encode necessary characters
    String path = uri.getRawPath();
    if (path == null) {
      path = "";
    } 

    Pathname p = (Pathname)Pathname.create(path != null ? path : ""); 
    result
      .setDirectory(p.getDirectory())
      .setName(p.getName())
      .setType(p.getType());

    return result;
  }

  public URI toURI() {
    String uriString = getNamestringAsURL();
    try {
      URI uri = new URI(uriString);
      return uri;
    } catch (URISyntaxException eo) {
      return null;
    }
  }

  public URL toURL() {
    URI uri = toURI();
    try {
      if (uri != null) {
        return uri.toURL();
      }
    } catch (MalformedURLException e) { 
    }
    return null;
  }
  
  public File getFile() { 
    if (!hasExplicitFile(this)) {
      return null; // TODO signal that this is not possible?
    }
    URI uri = toURI();
    if (uri == null) {
      return null;
    }
    File result = new File(uri);
    return result;
  }

  static public boolean isFile(Pathname p) {
    LispObject scheme = Symbol.GETF.execute(p.getHost(), SCHEME, NIL);
    if (scheme.equals(NIL)
        || hasExplicitFile(p)) {
      return true;
    }
    return false;
  }
  
  static public boolean hasExplicitFile(Pathname p) {
    if (!p.getHost().listp()) {
        return false;
    }
    LispObject scheme = Symbol.GETF.execute(p.getHost(), SCHEME, NIL);
    return scheme.equalp(FILE);
  }

  public String getNamestring() {
    StringBuilder sb = new StringBuilder();
    return getNamestring(sb);
  }
  
  public String getNamestring(StringBuilder sb) {
    LispObject scheme = Symbol.GETF.execute(getHost(), SCHEME, NIL);
    LispObject authority = Symbol.GETF.execute(getHost(), AUTHORITY, NIL);

    // A scheme of NIL is implicitly "file:", for which we don't emit
    // as part of the usual namestring.  getNamestringAsURI() should
    // emit the 'file:' string
    boolean percentEncode = true;
    if (scheme.equals(NIL)) {
      percentEncode = false;
    } else {
      sb.append(scheme.getStringValue());
      sb.append(":");
      if (authority != NIL) {
        sb.append("//");
        sb.append(authority.getStringValue());
      } else if (scheme.equalp(FILE)) {
        sb.append("//");
      }
    }
    // <https://docs.microsoft.com/en-us/archive/blogs/ie/file-uris-in-windows>
    if (Utilities.isPlatformWindows
        && getDevice() instanceof SimpleString) {
      sb.append("/")
        .append(getDevice().getStringValue())
        .append(":");
    }
    String directoryNamestring = getDirectoryNamestring();
    if (percentEncode) {
      directoryNamestring = uriEncode(directoryNamestring);
    }
    sb.append(directoryNamestring);

    // Use the output of Pathname
    Pathname p = new Pathname();
    p.copyFrom(this)
      .setHost(NIL)
      .setDevice(NIL)
      .setDirectory(NIL);
    String nameTypeVersion = p.getNamestring();
    if (percentEncode) {
      nameTypeVersion = uriEncode(nameTypeVersion);
    }      
    sb.append(nameTypeVersion);

    LispObject o = Symbol.GETF.execute(getHost(), QUERY, NIL);
    if (o != NIL) {
      sb.append("?")
        .append(uriEncode(o.getStringValue()));
    }
    o = Symbol.GETF.execute(getHost(), FRAGMENT, NIL);
    if (o != NIL) {
      sb.append("#")
        .append(uriEncode(o.getStringValue()));
    }

    return sb.toString();
  }

  // We need our "own" rules for outputting a URL
  // 1.  For DOS drive letters
  // 2.  For relative "file" schemas (??)
  public String getNamestringAsURL() {
    LispObject schemeProperty = Symbol.GETF.execute(getHost(), SCHEME, NIL);
    LispObject authorityProperty = Symbol.GETF.execute(getHost(), AUTHORITY, NIL);
    LispObject queryProperty = Symbol.GETF.execute(getHost(), QUERY, NIL);
    LispObject fragmentProperty = Symbol.GETF.execute(getHost(), FRAGMENT, NIL);

    String scheme;
    String authority = null;
    if (!schemeProperty.equals(NIL)) {
      scheme = schemeProperty.getStringValue();
      if (!authorityProperty.equals(NIL)) {
        authority =  authorityProperty.getStringValue();
      }
    } else {
      scheme = "file";
    }

    String directory = getDirectoryNamestring();
    String file = "";
    LispObject fileNamestring = Symbol.FILE_NAMESTRING.execute(this);
    if (!fileNamestring.equals(NIL)) {
      file = fileNamestring.getStringValue();
    }
    String path = "";

    if (!directory.equals("")) {
      if (Utilities.isPlatformWindows
          && getDevice() instanceof SimpleString) {
        path = getDevice().getStringValue() + ":" + directory + file;
      } else {
        path = directory + file;
      }
    } else {
      path = file;
    }

    path = uriEncode(path);

    String query = null;
    if (!queryProperty.equals(NIL)) {
      query = queryProperty.getStringValue();
    }

    String fragment = null;
    if (!fragmentProperty.equals(NIL)) {
      fragment = fragmentProperty.getStringValue();
    }

    StringBuffer result = new StringBuffer(scheme);
    result.append(":");
    result.append("//");
    if (authority != null) {
      result.append(authority);
    }
    if (!path.startsWith("/")) {
      result.append("/");
    }
    result.append(path);

    if (query != null) {
      result.append("?").append(query);
    }

    if (fragment != null) {
      result.append("#").append(fragment);
    }
    return result.toString();
  }

  public LispObject typeOf() {
    return Symbol.URL_PATHNAME;
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.URL_PATHNAME;
  }

  public static LispObject truename(Pathname p, boolean errorIfDoesNotExist) {
    URLPathname pathnameURL = (URLPathname)URLPathname.createFromFile(p);
    return URLPathname.truename(pathnameURL, errorIfDoesNotExist);
  }

  public static LispObject truename(URLPathname p, boolean errorIfDoesNotExist) {
    if (p.getHost().equals(NIL)
        || hasExplicitFile(p)) {
      LispObject fileTruename = Pathname.truename(p, errorIfDoesNotExist);
      if (fileTruename.equals(NIL)) {
        return NIL;
      }
      if (!(fileTruename instanceof URLPathname)) {
        URLPathname urlTruename = URLPathname.createFromFile((Pathname)fileTruename);
        return urlTruename;
      }
      return fileTruename;
    }
        
    if (p.getInputStream() != null) {
      // If there is no type, query or fragment, we check to
      // see if there is URL available "underneath".
      if (p.getName() != NIL 
          && p.getType() == NIL
          && Symbol.GETF.execute(p.getHost(), URLPathname.QUERY, NIL) == NIL
          && Symbol.GETF.execute(p.getHost(), URLPathname.FRAGMENT, NIL) == NIL) {
        if (p.getInputStream() != null) {
          return p;
        }
      }
      return p;
    }
    return Pathname.doTruenameExit(p, errorIfDoesNotExist);
  }
  
  public InputStream getInputStream() {
    InputStream result = null;

    if (URLPathname.isFile(this)) {
      Pathname p = new Pathname();
      p.copyFrom(this)
        .setHost(NIL);
      return p.getInputStream();
    }

    if (URLPathname.isFile(this)) {
      Pathname p = new Pathname();
      p.copyFrom(this)
        .setHost(NIL);
      return p.getInputStream();
    }

    URL url = this.toURL();
    try { 
      result = url.openStream();
    } catch (IOException e) {
      Debug.warn("Failed to get InputStream from "
                 + "'" + getNamestring() + "'"
                 + ": " + e);
    }
    return result;
  }

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

  public long getLastModified() {
    return getURLConnection().getLastModified();
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
}
