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

public class PathnameJar extends PathnameURL {
  protected PathnameJar() {}

  static final private String jarSeparator = "!/";

  /** Enumerate the individual namestrings of the enclosing jars for a namestring */
  static LispObject enumerateJarURIs(String s) {
    LispObject result = NIL;

    // ???  enumerate the contained jars

    // Are namestrings not enough,
    // and this should return the actual chain of DEVICE references?
    result = result.push(new SimpleString(s));
    return result;
  }

  static public LispObject create(String s) {
    if (!s.startsWith("jar:")) {
      error(new SimpleError("Cannot create a PATHNAME-JAR from namestring: " + s));
    }
    
    PathnameJar result = new PathnameJar();

    // A JAR file
    if (s.endsWith(jarSeparator)) {
      LispObject jars = PathnameJar.enumerateJarURIs(s); 
      int i = s.lastIndexOf(jarSeparator, s.length() - jarSeparator.length() - 1);
      String jar = null;
      if (i == -1) {
        jar = s;
      } else {
        // There can be no more than two jar references and the
        // inner one must be a file reference within the outer.
        jar = "jar:file:" + s.substring(i + jarSeparator.length());
        s = s.substring("jar:".length(), i + jarSeparator.length());
        Pathname p = (Pathname)Pathname.create(s);
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
            jarPathname = (Pathname)Pathname.create(uri.getSchemeSpecificPart());
          } else {
            jarPathname = (Pathname)Pathname.create((new File(path)).getPath());
          }
        } else {
          jarPathname = (Pathname)Pathname.create("");
        }
        jars = jars.push(jarPathname);
      } else {
        URL url = null;
        try {
          url = new URL(jar.substring("jar:".length(), jar.length() - 2));
          PathnameURL p = (PathnameURL)PathnameURL.create(url);
          jars = jars.push(p);
        } catch (MalformedURLException e) {
          error(new LispError("Failed to parse URL "
                              + "'" + url + "'"
                              + e.getMessage()));
        }
      }
      jars = jars.nreverse();
      result.setDevice(jars);
      return result;
    }

    // An entry in a JAR file
    final int separatorIndex = s.lastIndexOf(jarSeparator);
    if (separatorIndex == -1) {
      return error(new SimpleError("Couldn't construct a pathname to an entry in a jar from: " + s));
    }

    final String jarURL = s.substring(0, separatorIndex + jarSeparator.length());
    URL url = null;
    try {
      url = new URL(jarURL);
    } catch (MalformedURLException ex) {
      error(new LispError("Failed to parse URL "
                          + "'" + jarURL + "'"
                          + ex.getMessage()));
    }
    PathnameURL d = (PathnameURL)PathnameURL.create(url);
    if (result.getDevice() instanceof Cons) {
      LispObject[] jars = d.copyToArray();
      //  XXX Is this ever reached?  If so, need to append lists
      Debug.assertTrue(false);
    } else {
      result.setDevice(d.getDevice());
    }
    s = "/" + s.substring(separatorIndex + jarSeparator.length());
    PathnameURL p = (PathnameURL)PathnameURL.create("file:" + s); // Use URI escaping rules
    result.setDirectory(p.getDirectory());
    result.setName(p.getName());
    result.setType(p.getType());
    result.setVersion(p.getVersion());
    return result;
  }

  public String getNamestring() {
    StringBuffer sb = new StringBuffer();
    LispObject enclosingJars = getDevice().reverse();

    while (enclosingJars.cdr() != NIL) {
      PathnameURL jarPathname = (PathnameURL)enclosingJars.car();
      sb.append("jar:");
      String jarNamestring = jarPathname.getNamestringAsURI();
      sb.append(jarNamestring);
      sb.append(jarSeparator);

      enclosingJars = enclosingJars.cdr();
    }

    Pathname withoutDevice = new Pathname(this); // not gonna work on Windows
    withoutDevice.setDevice(NIL);
    String withoutDeviceNamestring = withoutDevice.getNamestring(); // need to URI encode?
    sb.append(withoutDeviceNamestring);

    return sb.toString();
  }
}

                                 
