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
import java.util.ArrayList;
import java.util.List;

public class PathnameJar extends PathnameURL {
  protected PathnameJar() {}

  static final private String JAR_URI_SUFFIX = "!/";
  static final private String JAR_URI_PREFIX = "jar:";

  /** Enumerate the individual namestrings of the enclosing jars for */
  public static List<String> enumerateJarURIs(String s) {
    ArrayList<String> result = new ArrayList<String>();

    // ???  enumerate the contained jars
    int i = s.lastIndexOf(JAR_URI_PREFIX);
    if (i == -1) {
      error(new SimpleError("Failed to find any occurence of jars: " + s));
      return null; // not reached
    }
    i += JAR_URI_PREFIX.length(); // advance index to end of "jar:jar:jar:..."
    if ((i % JAR_URI_PREFIX.length()) != 0) {
      error(new SimpleError("Failed to parse prefixed jar: " + s));
      return null;
    }
    int occurrences = i / 4; 
    String withoutPrefixes = s.substring(i);

    String parts[] = withoutPrefixes.split(JAR_URI_SUFFIX);

    for (int j = 0; j < occurrences - 1; j++) {
      String ns = parts[j] + JAR_URI_SUFFIX;
      result.add(ns);
    }

    // Last namestring may have a path 
    String ns = parts[occurrences - 1] + JAR_URI_SUFFIX;
    if (parts.length == (occurrences + 1)) {
      ns = ns + parts[occurrences];
    }
    result.add(ns);

    return result;
  }

  // FIXME: badly named
  static public PathnameURL createJarPathname(String ns) {
    PathnameURL result = null;
    URL url = null;
    URI uri = null;
    if (ns.length() > 0) {
      try {
        url = new URL("file:" + ns);
        uri = url.toURI();
      } catch (MalformedURLException e1) {
        error(new SimpleError("Failed to create URI from " + "'" + ns + "'" + ": " + e1.getMessage()));
      } catch (URISyntaxException e2) {
        error(new SimpleError("Failed to create URI from " + "'" + ns + "'" + ": " + e2.getMessage()));
      }
      String path = uri.getPath();
      if (path == null) {
        // We allow "jar:file:baz.jar!/" to construct a relative
        // path for jar files, so MERGE-PATHNAMES means something.
        result = (PathnameURL)PathnameURL.create(uri.getSchemeSpecificPart());
      } else {
        result = (PathnameURL)PathnameURL.create((new File(path)).getPath());
      }
    } else {
      result = (PathnameURL)PathnameURL.create("");
    }
    return result;
  }


  static public LispObject create(String s) {
    if (!s.startsWith(JAR_URI_PREFIX)) {
      error(new SimpleError("Cannot create a PATHNAME-JAR from namestring: " + s));
    }
    
    // FIXME: should be a PathnameJar
    PathnameURL result = new PathnameURL();

    List<String> jarNamestrings = PathnameJar.enumerateJarURIs(s);
    LispObject enclosingJars = NIL;

    for (int i = 0; i < (jarNamestrings.size() - 1); i++) {
      String ns = jarNamestrings.get(i);
      URL url = null;
      URI uri = null;
      PathnameURL pURL = null;

      if (Pathname.isValidURL(ns)) {
        try {
          url = new URL(ns);
          pURL = (PathnameURL)PathnameURL.create(url);
        } catch (MalformedURLException e) {
          error(new LispError("Failed to parse URL " + "'" + url + "'" + e.getMessage()));
        }
      } else {
        pURL = createJarPathname(ns);
      }

      // FIXME all but the first need to of type PATHNAME-JAR
      enclosingJars = enclosingJars.push(pURL);
    }

    // A JAR file
    // if (s.endsWith(jarSeparator)) {
    //   List<String> jarNamestrings = PathnameJar.enumerateJarURIs(s);
    //   LispObject jars = NIL;
    //   int i = s.lastIndexOf(jarSeparator, s.length() - jarSeparator.length() - 1);
    //   String jar = null;
    //   if (i == -1) {
    //     jar = s;
    //   } else {
    //     // There can be no more than two jar references and the
    //     // inner one must be a file reference within the outer.
    //     jar = "jar:file:" + s.substring(i + jarSeparator.length());
    //     s = s.substring("jar:".length(), i + jarSeparator.length());
    //     Pathname p = (Pathname)Pathname.create(s);
    //     jars = jars.push(p.getDevice().car());
    //   }
    //   if (jar.startsWith("jar:file:")) {
    //     String file
    //       = jar.substring("jar:file:".length(),
    //                       jar.length() - jarSeparator.length());
    //     Pathname jarPathname;
    //     if (file.length() > 0) {
    //       URL url = null;
    //       URI uri = null;
    //       try {
    //         url = new URL("file:" + file);
    //         uri = url.toURI();
    //       } catch (MalformedURLException e1) {
    //         error(new SimpleError("Failed to create URI from "
    //                               + "'" + file + "'"
    //                               + ": " + e1.getMessage()));
    //       } catch (URISyntaxException e2) {
    //         error(new SimpleError("Failed to create URI from "
    //                               + "'" + file + "'"
    //                               + ": " + e2.getMessage()));
    //       }
    //       String path = uri.getPath();
    //       if (path == null) {
    //         // We allow "jar:file:baz.jar!/" to construct a relative
    //         // path for jar files, so MERGE-PATHNAMES means something.
    //         jarPathname = (Pathname)Pathname.create(uri.getSchemeSpecificPart());
    //       } else {
    //         jarPathname = (Pathname)Pathname.create((new File(path)).getPath());
    //       }
    //     } else {
    //       jarPathname = (Pathname)Pathname.create("");
    //     }
    //     jars = jars.push(jarPathname);
    //   } else {
    //     URL url = null;
    //     try {
    //       url = new URL(jar.substring("jar:".length(), jar.length() - 2));
    //       PathnameURL p = (PathnameURL)PathnameURL.create(url);
    //       jars = jars.push(p);
    //     } catch (MalformedURLException e) {
    //       error(new LispError("Failed to parse URL "
    //                           + "'" + url + "'"
    //                           + e.getMessage()));
    //     }
    //   }
    //   jars = jars.nreverse();
    //   result.setDevice(jars);
    //   return result;
    // }

    // An entry in a JAR file
    // final int separatorIndex = s.lastIndexOf(jarSeparator);
    // if (separatorIndex == -1) {
    //   return error(new SimpleError("Couldn't construct a pathname to an entry in a jar from: " + s));
    // }

    // final String jarURL = s.substring(0, separatorIndex + jarSeparator.length());
    // URL url = null;
    // try {
    //   url = new URL(jarURL);
    // } catch (MalformedURLException ex) {
    //   error(new LispError("Failed to parse URL "
    //                       + "'" + jarURL + "'"
    //                       + ex.getMessage()));
    // }
                     
    //    PathnameURL d = (PathnameURL)PathnameURL.create(url);

    String ns = jarNamestrings.get(jarNamestrings.size() - 1);
    int i = ns.indexOf(JAR_URI_SUFFIX);

    String jarNamestring = ns.substring(i + 1);
    if (Pathname.isValidURL(jarNamestring)) {
      result = (PathnameURL)PathnameURL.create(jarNamestring);
    } else {
      result = (PathnameURL)PathnameURL.create("file:" + jarNamestring);
    }

    result.setDevice(enclosingJars.reverse());

    if (i > ns.length() - JAR_URI_SUFFIX.length()) {
      String pathNameAndType = ns.substring(i + JAR_URI_SUFFIX.length());
      PathnameURL p = (PathnameURL)PathnameURL.create("file:" + pathNameAndType); 
      result.setDirectory(p.getDirectory());
      result.setName(p.getName());
      result.setType(p.getType());
      result.setVersion(p.getVersion());
    }
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
      sb.append(JAR_URI_SUFFIX);

      enclosingJars = enclosingJars.cdr();
    }

    Pathname withoutDevice = new Pathname(this); // not gonna work on Windows
    withoutDevice.setDevice(NIL);
    String withoutDeviceNamestring = withoutDevice.getNamestring(); // need to URI encode?
    sb.append(withoutDeviceNamestring);

    return sb.toString();
  }
}

                                 
