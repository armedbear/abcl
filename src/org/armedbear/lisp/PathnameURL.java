/* 
 * PathnameJar.java
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
import java.net.URL;
import java.net.URI;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.text.MessageFormat;

public class PathnameURL extends Pathname {
  static final Symbol SCHEME = internKeyword("SCHEME");
  static final Symbol AUTHORITY = internKeyword("AUTHORITY");
  static final Symbol QUERY = internKeyword("QUERY");
  static final Symbol FRAGMENT = internKeyword("FRAGMENT");

  private PathnameURL() {}

  public static PathnameURL create(String s) {
    // A URL

    if (!isValidURL(s)) {
      error(new SimpleError("Cannot form a PATHNAME-URL from " + s));
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
      final Pathname p = Pathname.create(path);
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
      if (authority == null) {
        Debug.trace(MessageFormat.format("{0} has a null authority.", url));
      }
    }

    LispObject host = NIL;
    host.push(SCHEME);
    host.push(new SimpleString(scheme));
    result.setHost(host);

    if (authority != null) {
      result.setHost(result.getHost().push(AUTHORITY));
      result.setHost(result.getHost().push(new SimpleString(authority)));
    }

    result.setDevice(NIL);
            
    // URI encode necessary characters
    String path = uri.getRawPath();
    if (path == null) {
      path = "";
    } 
    String query = uri.getRawQuery();
    if (query != null) {
      LispObject component = result.getHost();
      component.push(QUERY).push(new SimpleString(query));
      result.setHost(component);
    }
    String fragment = uri.getRawFragment();
    if (fragment != null) {
      LispObject component = result.getHost();
      component
        .push(FRAGMENT)
        .push(new SimpleString(fragment));
      result.setHost(component);
    }
    Pathname p = Pathname.create(path != null ? path : ""); 

    result.setDirectory(p.getDirectory());
    result.setName(p.getName());
    result.setType(p.getType());
            
    result.getHost().nreverse();
    result.invalidateNamestring();
    return result;
  }

  protected PathnameURL(URL url) {
    create(url.toString());
  }
  protected PathnameURL(URI uri) {
    create(uri.toString());
  }

  public String getNamestring() {
    StringBuilder sb = new StringBuilder();
    return getNamestring(sb);
  }
  
  public String getNamestring(StringBuilder sb) {
    LispObject scheme = Symbol.GETF.execute(getHost(), SCHEME, NIL);
    LispObject authority = Symbol.GETF.execute(getHost(), AUTHORITY, NIL);
    Debug.assertTrue(scheme != NIL);
    sb.append(scheme.getStringValue());
    sb.append(":");
    if (authority != NIL) {
      sb.append("//");
      sb.append(authority.getStringValue());
    }
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
    String directoryNamestring = getDirectoryNamestring();
    sb.append(directoryNamestring);

    if (getName() instanceof AbstractString) {
      String n = getName().getStringValue();
      if (n.indexOf('/') >= 0) {
        Debug.assertTrue(namestring == null);
        return null;
      }
      sb.append(n);
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
        } else {
          sb.append(t);
        }
      }
    }
    namestring = sb.toString();
    return namestring;
  }

  public LispObject typeOf() {
    return Symbol.URL_PATHNAME;
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.URL_PATHNAME;
  } 
}
                                 
