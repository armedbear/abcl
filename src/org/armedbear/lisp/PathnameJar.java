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

import java.io.IOException;
import java.io.File;
import java.net.URL;
import java.net.URI;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipFile;

public class PathnameJar
  extends PathnameURL
{
  protected PathnameJar() {}

  public static PathnameJar create(PathnameJar p) {
    return (PathnameJar)PathnameJar.create(p.getNamestring());
  }

  static final public String JAR_URI_SUFFIX = "!/";
  static final public String JAR_URI_PREFIX = "jar:";

  /** 
   *  Enumerate the components of a jar namestring 
   */
  static List<String> enumerate(String s) {
    ArrayList<String> result = new ArrayList<String>();

    int i = s.lastIndexOf(JAR_URI_PREFIX);
    if (i == -1) {
      parse_error("Failed to find any occurence of 'jar:' prefixes:" + s);
      return null; // not reached
    }
    i += JAR_URI_PREFIX.length(); // advance index to end of "jar:jar:jar:..."
    if ((i % JAR_URI_PREFIX.length()) != 0) {
      parse_error("Failed to parse 'jar:' prefixes:" + s);
      return null;
    }
    int prefixCount = i / 4; 
    String withoutPrefixes = s.substring(i);

    String parts[] = withoutPrefixes.split(JAR_URI_SUFFIX);

    // Do we have as many prefixes as suffixes?
    String notEndingInSuffix = withoutPrefixes + "nonce"; 
    String suffixParts[] = notEndingInSuffix.split(JAR_URI_SUFFIX);
    int suffixCount = suffixParts.length - 1;
    if (suffixCount != prefixCount) {
      parse_error("Mismatched 'jar:' prefix and '/!' suffixes in jar: " + s);
      return null;
    }

    if (parts.length == 1) {
      if (!s.endsWith(JAR_URI_SUFFIX)) {
        error(new SimpleError("No trailing jar uri suffix: " + s));
        return null;
      }
      if (!isValidURL(parts[0])) {
        error(new SimpleError("Not a valid URI: " + parts[0]));
        return null;
      }

      result.add(parts[0]);
      return result;
    }

    // the first value is not a jar Pathname, 
    result.add(parts[0]);

    // the references to jars, all suffixed with JAR_URI_SUFFIX
    for (int j = 1; j < prefixCount; j++) {
      String ns = parts[j] + JAR_URI_SUFFIX;
      result.add(ns);
    }

    // possibly return the path inside the last jar
    if (parts.length == (prefixCount + 1)) {
      result.add("/" + parts[parts.length - 1]);
    }

    return result;
  }

  // FIXME: badly named
  static PathnameJar createJarPathname(String ns) {
    PathnameJar result = new PathnameJar();
    if (ns.length() == 0) { // ??? what calls in this state?
      return result;
    }

    PathnameURL p;
    
    URL url = null;
    URI uri = null;

    String nsURL = ns;
    if (!Pathname.isValidURL(nsURL)) {
      nsURL = "file:" + nsURL;
    }
    
    try {
      url = new URL(nsURL);
      uri = url.toURI();
    } catch (MalformedURLException e1) {
      parse_error("Failed to create URI from " + "'" + nsURL + "'" + ": " + e1.getMessage());
      return null;
    } catch (URISyntaxException e2) {
      parse_error("Failed to create URI from " + "'" + nsURL + "'" + ": " + e2.getMessage());
      return null;
    }

    String path = uri.getPath();
    String pathAsURI;
    if (path == null) {
      // We allow "jar:file:baz.jar!/" to construct a relative
      // path for jar files, so MERGE-PATHNAMES means something.
      pathAsURI = "file:" + uri.getSchemeSpecificPart();
    } else {
      // FIXME:  Windows drive letters???
      String pathFromFile = (new File(path)).getPath();
      pathAsURI = "file:" + pathFromFile;
    }
        
    p = (PathnameURL)PathnameURL.create(pathAsURI);
    Pathname.ncoerce(p, result);
    
    return result;
  }

  static public LispObject create(String s) {
    if (!s.startsWith(JAR_URI_PREFIX)) {
      return parse_error("Cannot create a PATHNAME-JAR from namestring: " + s);
    }
    
    List<String> contents = PathnameJar.enumerate(s);

    if (contents == null) {
      return parse_error("Couldn't parse PATHNAME-JAR from namestring: " + s);
    }
    PathnameJar result = new PathnameJar();

    LispObject rootPathname = Pathname.create(contents.get(0));

    LispObject jars = NIL;
    jars = jars.push(rootPathname);
    
    if (contents.size() == 1) {
      result.setDevice(jars);
      return result;
    }

    for (int i = 1; i < contents.size(); i++) {
      String ns = contents.get(i);
      if (ns.endsWith(JAR_URI_SUFFIX)) {
        String nsWithoutSuffix = ns.substring(0, ns.length() - JAR_URI_SUFFIX.length());
        Pathname pathname = (Pathname)Pathname.create(nsWithoutSuffix);
        Pathname jar = new Pathname();
        Pathname.ncoerce(pathname, jar);
        jars = jars.push(jar);
      } else { 
        Pathname p = (Pathname)Pathname.create(contents.get(i));
        Pathname.ncoerce(p, result);
      }
    }
    jars = jars.nreverse();
    result.setDevice(jars);
    return result;
  }

  public String getNamestring() {
    StringBuffer sb = new StringBuffer();

    LispObject jars = getDevice();

    if (jars.equals(NIL)) { // DESCRIBE ends up here somehow
      // attempt to return some sort of representation
      return super.getNamestring();
    }

    for (int i = 0; i < jars.length() - 1; i++) {
      sb.append(JAR_URI_PREFIX);
    }

    LispObject root = jars.car();

    if (root instanceof PathnameURL) {
      String ns = ((PathnameURL)root).getNamestringAsURI();
      sb.append(JAR_URI_PREFIX)
        .append(ns)
        .append(JAR_URI_SUFFIX);
    } else if (root instanceof Pathname) { // For transitional compatibility?
      String ns = ((Pathname)root).getNamestring();
      sb.append(JAR_URI_PREFIX)
        .append("file:")
        .append(ns)
        .append(JAR_URI_SUFFIX);
    } else {
      simple_error("Unable to generate namestring for jar with root pathname ~a", root); 
    }

    if (jars.length() > 1) {
      LispObject innerJars = jars.cdr();
      while (innerJars.car() != NIL) {
        Pathname jar = (Pathname)innerJars.car();
        Pathname p = new Pathname();
        Pathname.ncoerce(jar, p);
        p.setDevice(NIL);
        String ns = p.getNamestring();
        sb.append(ns)
          .append(JAR_URI_SUFFIX);
        innerJars = innerJars.cdr();
      }
    }

    if (getDirectory() != NIL
        || getName() != NIL
        || getType() != NIL) {
      
      Pathname withoutDevice = new Pathname(); 
      Pathname.ncoerce(this, withoutDevice);
      withoutDevice.setDevice(NIL);

      String withoutDeviceNamestring = withoutDevice.getNamestring(); // need to URI encode?
      if (withoutDeviceNamestring.startsWith("/")) {
        sb.append(withoutDeviceNamestring.substring(1));
      } else {
        sb.append(withoutDeviceNamestring);
      }
    }
    
    return sb.toString();
  }


  public static final LispObject truename(PathnameJar pathname,
                                          boolean errorIfDoesNotExist) {
    // PathnameJar result 
    //   = (PathnameJar) mergePathnames(pathname,
    //                                  coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
    //                                  NIL);

    PathnameJar result = pathname;
    
    LispObject enclosingJars = NIL;
    LispObject current = pathname.getDevice();

    while (current instanceof PathnameJar){
      enclosingJars = enclosingJars.push(current);
      current = ((PathnameJar)current).getDevice();
    }

    LispObject outerJar = Pathname.truename(current, errorIfDoesNotExist);

    String outerJarNamestring = ((Pathname)outerJar).getNamestring();
    try {
      ZipFile jarFile = new ZipFile(outerJarNamestring);
      while (enclosingJars.car() != NIL) {
        LispObject jar = enclosingJars.car();
        String ns = ((Pathname)jar).getNamestring();
        ZipFile f = new ZipFile(ns);
        enclosingJars = enclosingJars.cdr();
      }
    } catch (IOException e) {
      return Pathname.doTruenameExit(result, errorIfDoesNotExist);
    }

    return result;
  }

  public LispObject classOf() {
    return BuiltInClass.JAR_PATHNAME;
  }

  @Override
  public LispObject typeOf() {
    return Symbol.JAR_PATHNAME;
  }

        //   jarfile: {
        //     // Possibly canonicalize jar file directory
        //     LispObject o = pathname.getDevice();
        //     if (!(o instanceof Pathname)) {
        //       return doTruenameExit(pathname, errorIfDoesNotExist);
        //     }
        //     if (o instanceof Pathname 
        //         && !(((Pathname)o).isURL())
        //         // XXX Silently fail to call truename() if the default
        //         // pathname defaults exist within a jar, as that will
        //         // (probably) not succeed.  The better solution would
        //         // probably be to parametize the value of
        //         // *DEFAULT-PATHNAME-DEFAULTS* on invocations of
        //         // truename().
        //         && !coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()).isJar()) 
        //       {
        //         LispObject truename = Pathname.truename((Pathname)o, errorIfDoesNotExist);
        //         if (truename != null && truename != NIL
        //             && truename instanceof Pathname) {
        //             Pathname truePathname = (Pathname)truename;
        //             // A jar that is a directory makes no sense, so exit
        //             if (truePathname.getNamestring().endsWith("/")) {
        //                 break jarfile;
        //             }
        //             jars.car = truePathname;
        //         } else {
        //             break jarfile;
        //         }
        //       }

        //     // Check for existence of a JAR file and/or JarEntry
        //     //
        //     // Cases:
        //     // 1.  JAR
        //     // 2.  JAR in JAR
        //     // 3.  JAR with Entry
        //     // 4.  JAR in JAR with Entry

        //     ZipFile jarFile = ZipCache.get((Pathname)jars.car());
        //     String entryPath = pathname.asEntryPath();
        //     if (jarFile != null) {
        //         if (jars.cdr() instanceof Cons) {
        //           Pathname inner = (Pathname) jars.cdr().car();
        //           InputStream inputStream = Utilities.getInputStream(jarFile, inner);
        //           if (inputStream != null) {
        //               if (entryPath.length() == 0) {
        //                   return pathname; // Case 2
        //               } else {
        //                   ZipInputStream zipInputStream
        //                       = new ZipInputStream(inputStream);
        //                   ZipEntry entry = Utilities.getEntry(zipInputStream,
        //                                                       entryPath,
        //                                                       false);
        //                   if (entry != null) {
        //                       // XXX this could possibly be a directory?
        //                       return pathname; // Case 4
        //                  }
        //               }
        //           }
        //         } else {
        //             if (entryPath.length() == 0) {
        //                 return pathname; // Case 1
        //             } else {
        //                 ZipEntry entry = jarFile.getEntry(entryPath);
        //                 if (entry != null) {
        //                     // ensure this isn't a directory
        //                     if (entry.isDirectory()) {
        //                         break jarfile;
        //                     }
        //                     try {
        //                         InputStream input = jarFile.getInputStream(entry);
        //                         if (input != null) {
        //                             return pathname; // Case 3
        //                         }
        //                     } catch (IOException e) {
        //                         break jarfile;
        //                     }
        //                 }
        //             }
        //         }
        //     }
        // }

}

                                 
