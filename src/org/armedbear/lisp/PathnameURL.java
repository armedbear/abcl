/* 
 * PathnameURL.java
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
import java.net.URI;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.text.MessageFormat;

public class PathnameURL
  extends Pathname
{
  static final Symbol SCHEME = internKeyword("SCHEME");
  static final Symbol AUTHORITY = internKeyword("AUTHORITY");
  static final Symbol QUERY = internKeyword("QUERY");
  static final Symbol FRAGMENT = internKeyword("FRAGMENT");

  protected PathnameURL() {}

  public static Pathname create() {
    return new PathnameURL();
  }

  public static PathnameURL create(PathnameURL p) {
    return (PathnameURL) PathnameURL.create(p.getNamestring());
  }

  public static LispObject create(URL url) {
    return PathnameURL.create(url.toString());
  }

  public static LispObject create(URI uri) {
    return PathnameURL.create(uri.toString());
  }

  public static LispObject create(String s) {
    if (!isValidURL(s)) {
      error(new SimpleError("Cannot form a PATHNAME-URL from " + s));
    }
    if (s.startsWith("jar:")) {
      return PathnameJar.create(s);
    }

    PathnameURL result = new PathnameURL();
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
      final Pathname p = (Pathname)Pathname.create(path);
      result.setHost(p.getHost());
      result.setDevice(p.getDevice());
      result.setDirectory(p.getDirectory());
      result.setName(p.getName());
      result.setType(p.getType());
      result.setVersion(p.getVersion());
      return result;  
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
    result.setHost(host.nreverse());

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
    if (!scheme.equals(NIL)) {
      sb.append(scheme.getStringValue());
      sb.append(":");
      if (authority != NIL) {
        sb.append("//");
        sb.append(authority.getStringValue());
      }
    }
    String directoryNamestring = getDirectoryNamestring();
    sb.append(directoryNamestring);

    // Use the output of Pathname
    Pathname p = new Pathname();
    p.copyFrom(this);
    Pathname.ncoerce(this, p);
    p.setHost(NIL);
    p.setDevice(NIL);
    p.setDirectory(NIL);
    String nameTypeVersion = p.getNamestring();
    sb.append(nameTypeVersion);

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

    //namestring = sb.toString();
    return sb.toString();
  }

  public String getNamestringAsURI() {
    LispObject schemeProperty = Symbol.GETF.execute(getHost(), SCHEME, NIL);
    LispObject authorityProperty = Symbol.GETF.execute(getHost(), AUTHORITY, NIL);
    LispObject queryProperty = Symbol.GETF.execute(getHost(), QUERY, NIL);
    LispObject fragmentProperty = Symbol.GETF.execute(getHost(), FRAGMENT, NIL);

    String scheme;
    String authority;
    if (!schemeProperty.equals(NIL)) {
      scheme = schemeProperty.getStringValue();
      authority =  authorityProperty.getStringValue();
    } else {
      scheme = "file";
      authority = "";
    }

    String directory = getDirectoryNamestring();
    String file = Symbol.FILE_NAMESTRING.execute(this).getStringValue();
    String path = "";

    if (!directory.equals("")) {
      path = directory + file;
    } else {
      path = file;
    }

    String query = null;
    if (!queryProperty.equals(NIL)) {
      query = queryProperty.getStringValue();
    }

    String fragment = null;
    if (!fragmentProperty.equals(NIL)) {
      fragment = fragmentProperty.getStringValue();
    }

    try {
      URI uri = new URI(scheme, authority, path, query, fragment);
      return uri.toString();
    } catch (URISyntaxException e) {
      simple_error("Failed to construct a URI: ~a", this);
      return (String)UNREACHED;
    }
  }

  public LispObject typeOf() {
    return Symbol.URL_PATHNAME;
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.URL_PATHNAME;
  }

  public static LispObject truename(PathnameURL p, boolean errorIfDoesNotExist) {
    if (p.getHost().equals(NIL)
        || Symbol.GETF.execute(p.getHost(), PathnameURL.SCHEME, NIL).equals("file")) {
      return Pathname.truename((Pathname)p, errorIfDoesNotExist);
    }
        
    if (p.getInputStream() != null) {
      // If there is no type, query or fragment, we check to
      // see if there is URL available "underneath".
      if (p.getName() != NIL 
          && p.getType() == NIL
          && Symbol.GETF.execute(p.getHost(), PathnameURL.QUERY, NIL) == NIL
          && Symbol.GETF.execute(p.getHost(), PathnameURL.FRAGMENT, NIL) == NIL) {
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

  public long getLastModified() {
    return getURLConnection().getLastModified();
  }
}
                                 


// From Pathname.init()
        // A URL 
        // if (isValidURL(s)) {
        //     URL url = null;
        //     try {
        //         url = new URL(s);
        //     } catch (MalformedURLException e) {
        //         Debug.assertTrue(false);
        //     }
        //     String scheme = url.getProtocol();
        //     if (scheme.equals("file")) {
        //         URI uri = null;
        //         try {
        //             uri = new URI(s);
        //         } catch (URISyntaxException ex) {
        //             error(new SimpleError("Improper URI syntax for "
        //                             + "'" + url.toString() + "'"
        //                             + ": " + ex.toString()));
        //         }
            
        //         String uriPath = uri.getPath();
        //         if (null == uriPath) {
        //                           // Under Windows, deal with pathnames containing
        //                           // devices expressed as "file:z:/foo/path"
        //                           uriPath = uri.getSchemeSpecificPart();
        //                           if (uriPath == null || uriPath.equals("")) {
        //             error(new LispError("The URI has no path: " + uri));
        //           }
        //         }
        //         final File file = new File(uriPath);
        //         String path = file.getPath();
        //         if (uri.toString().endsWith("/") && !path.endsWith("/")) {
        //           path += "/";
        //         }
        //         final Pathname p = (Pathname)Pathname.create(path);
        //         this.setHost(p.getHost());
        //         this.setDevice(p.getDevice());
        //         this.setDirectory(p.getDirectory());
        //         this.setName(p.getName());
        //         this.setType(p.getType());
        //         this.setVersion(p.getVersion());
        //         return;
        //     }
        //     Debug.assertTrue(scheme != null);
        //     URI uri = null;
        //     try { 
        //         uri = url.toURI().normalize();
        //     } catch (URISyntaxException e) {
        //         error(new LispError("Couldn't form URI from "
        //                             + "'" + url + "'"
        //                             + " because: " + e));
        //     }
        //     String authority = uri.getAuthority();
        // if (authority == null) {
        //   authority = url.getAuthority();
        //   if (authority == null) {
        //     Debug.trace(MessageFormat.format("{0} has a null authority.", url));
        //   }
        // }
