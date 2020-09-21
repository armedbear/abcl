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

import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.net.URL;
import java.net.URI;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class PathnameJar
  extends PathnameURL
{
  static final public String JAR_URI_SUFFIX = "!/";
  static final public String JAR_URI_PREFIX = "jar:";

  protected PathnameJar() {}

  public static Pathname create() {
    return new PathnameJar();
  }

  public static PathnameJar create(PathnameJar p) {
    return (PathnameJar)PathnameJar.create(p.getNamestring());
  }

  public static LispObject createFromPathname(Pathname p) {
    if (p instanceof PathnameURL) {
      return PathnameJar.create(JAR_URI_PREFIX + ((PathnameURL)p).getNamestringAsURI() + JAR_URI_SUFFIX);
    } else if (p instanceof Pathname) {
      // FIXME: not going to work with namestrings with characters that need URI escaping
      return PathnameJar.create(JAR_URI_PREFIX + "file://" + p.getNamestring() + JAR_URI_SUFFIX);
    } else {
      return p;
    }
  }

  static public LispObject createFromFile(String s) {
    return PathnameJar.create("jar:file:" + s + JAR_URI_SUFFIX);
  }

  static public LispObject createEntryFromFile(String jar, String entry) {
    return PathnameJar.create("jar:file:" + jar + JAR_URI_SUFFIX + entry);
  }

  static public LispObject createEntryFromJar(PathnameJar jar, String entry) {
    if (jar.isArchiveEntry()) {
      return simple_error("Failed to create the entry ~a in ~a", entry, jar);
    }
    return PathnameJar.create(jar.getNamestring() + entry);
  }
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

    // The root, non-PathnameJar location of this reference
    // For files, possibly either a relative or absolute directory
    result.add(parts[0]);

    // The references to the pathnames of archives located within the
    // root jar.
    // These will be relative directory paths suffixed with JAR_URI_SUFFIX
    for (int j = 1; j < prefixCount; j++) {
      String ns = parts[j] + JAR_URI_SUFFIX;
      result.add(ns);
    }

    // possibly return the path inside the last jar as an absolute path
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
    result.validateComponents();
    return result;
  }

  public LispObject validateComponents() {
    if (!(getDevice() instanceof Cons)) {
      return type_error("Invalid DEVICE for JAR-PATHNAME", getDevice(), Symbol.CONS);
    }

    LispObject jars = getDevice();
    while (!jars.car().equals(NIL)) {
      LispObject jar = jars.car();
      if (!((jar instanceof Pathname)
            || (jar instanceof PathnameURL))) {
        return type_error("The value in DEVICE component of a JAR-PATHNAME is not of expected type",
                          jar,
                          list(Symbol.OR,
                               Symbol.PATHNAME, Symbol.URL_PATHNAME));
      }
      jars = jars.cdr();
    }

    return T;
  }
    

  public String getNamestring() {
    StringBuffer sb = new StringBuffer();

    LispObject jars = getDevice();

    if (jars.equals(NIL) || jars.equals(Keyword.UNSPECIFIC)) { 
        System.out.println("Pathname transitional problem: JAR-PATHNAME has bad PATHNAME-DEVICE");
        return null;
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

  LispObject getRootJar() {
    LispObject jars = getJars();
    if (!(jars instanceof Cons)) {
      System.out.println("Transitional pathname error: PATHNAME-DEVICE is not a Cons cell");
      return NIL;
    }
      
    return jars.car();
  }

  LispObject getJars() {
    return getDevice();
  }

  public static final LispObject truename(PathnameJar pathname,
                                          boolean errorIfDoesNotExist) {
    PathnameJar result = pathname;

    LispObject jars = pathname.getJars();
    Pathname rootJar = (Pathname) pathname.getRootJar();
    LispObject enclosingJars = jars.cdr();

    if (!rootJar.isLocalFile()) {
      // FIXME implement me
      simple_error("Unimplemented TRUENAME for non-file root jar.");
    } 

    PathnameJar p = new PathnameJar();
    p.copyFrom(pathname);
    p.setDevice(new Cons(rootJar, enclosingJars));

    if (!p.isArchiveEntry()) {
      ZipCache.Archive archive = ZipCache.getArchive(p);
      if (archive == null) {
        if (errorIfDoesNotExist) {
          return simple_error("Accessible TRUENAME can't be determined for: ~a", pathname); //time !?
        } else {
          return NIL;
        }
      }
      return p;
    }

    ZipEntry entry = ZipCache.getZipEntry(p);
    if (entry == null) {
      if (errorIfDoesNotExist) {
        return simple_error("Accessible TRUENAME can't be determined for: ~a", pathname); //time !?
      } else {
        return NIL;
      }
    }
    return p;
  }

  public boolean isLocalFile() {
    Pathname p = (Pathname) getRootJar();
    if (p.isLocalFile()) {
      return true;
    }
    return super.isLocalFile();
  }

  public boolean isArchiveEntry() {
    return !(getDirectory().equals(NIL)
             && getName().equals(NIL)
             && getType().equals(NIL));
  }

  public PathnameJar getArchive() {
    if (!isArchiveEntry()) {
      return (PathnameJar)simple_error("Pathname already represents an archive.");
    }
    PathnameJar archive = new PathnameJar();
    archive.copyFrom(this);
    archive
      .setDirectory(NIL)
      .setName(NIL)
      .setType(NIL);
    return archive;
  }

  public LispObject classOf() {
    return BuiltInClass.JAR_PATHNAME;
  }

  @Override
  public LispObject typeOf() {
    return Symbol.JAR_PATHNAME;
  }

  public InputStream getInputStream() {
    String entryPath = asEntryPath();
    // XXX We only return the bytes of an entry in a JAR
    if (!isArchiveEntry()) {
      simple_error("Can only get input stream for an entry in a JAR-PATHNAME.", this);
    }
    InputStream result = ZipCache.getEntryAsInputStream(this);
    if (result == null) {
      error(new FileError("Failed to get InputStream", this));
    }
    return result;
  }

  /** List the contents of the directory */
  static public LispObject listDirectory(PathnameJar pathname) {
    String directory = pathname.asEntryPath();
    // We should only be listing directories
    if (pathname.getDirectory() == NIL) {
      return simple_error("Not a directory in a jar ~a", pathname);
    }

    // if (pathname.getDevice().cdr() instanceof Cons) {
    //   return error(new FileError("Unimplemented directory listing of JAR within JAR.", pathname));
    // }
    
    if (directory.length() == 0) {
      directory = "/*";
    } else {
      if (directory.endsWith("/")) {
        directory = "/" + directory + "*";
      } else {
        directory = "/" + directory + "/*";
      }
    }

    Pathname wildcard = (Pathname)Pathname.create(directory);
    //    String wildcard = new SimpleString(directory);
    // directories in a a jar aren't marked with a suffixed slash
    //    SimpleString wildcardDirectory = new SimpleString(directory + "/");

    LispObject result = NIL;
    //LispObject matches;
    
    Iterator<Map.Entry<PathnameJar,ZipEntry>> iterator = ZipCache.getEntriesIterator(pathname);
    while (iterator.hasNext()) {
      Map.Entry<PathnameJar,ZipEntry> e = iterator.next();
      PathnameJar entry = e.getKey();
      // PathnameJar jarPath = (PathnameJar)PathnameJar.create(e.getKey());
      // ZipEntry entry = e.getValue();
      
      // if (entryName.endsWith("/")) {
      //   matches = Symbol.PATHNAME_MATCH_P
      //     .execute(new SimpleString(entryName), wildcardDirectory);
      // } else {
      //   matches = Symbol.PATHNAME_MATCH_P.
      //     execute(new SimpleString(entryName), wildcard);
      // }
      if (!Symbol.PATHNAME_MATCH_P.execute(entry, wildcard).equals(NIL)) {
        result = result.push(entry);
      }
    }
    return result.nreverse();
  }

  @DocString(name="match-wild-jar-pathname",
             args="wild-jar-pathname",
             returns="pathnames",
  doc="Returns the pathnames matching WILD-JAR-PATHNAME which must be both wild and a JAR-PATHNAME")
  static final Primitive MATCH_WILD_JAR_PATHNAME = new pf_match_wild_jar_pathname();

  private static class pf_match_wild_jar_pathname extends Primitive {
    pf_match_wild_jar_pathname() {
      super(Symbol.MATCH_WILD_JAR_PATHNAME, "wild-jar-pathname");
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
      if (!((PathnameJar)pathname).getJars().cdr().equals(NIL)) {
        return simple_error("Unimplemented match on contents of inner jars"); // FIXME
      }

      PathnameJar jarPathname = new PathnameJar();
      jarPathname.copyFrom(pathname);
      jarPathname
        .setDirectory(NIL)
        .setName(NIL)
        .setType(NIL);
      PathnameJar wildcard = (PathnameJar)Symbol.TRUENAME.execute(jarPathname);
      Iterator<Map.Entry<PathnameJar,ZipEntry>> iterator
        = ZipCache.getEntriesIterator(wildcard);
      wildcard
        .setDirectory(pathname.getDirectory())
        .setName(pathname.getName())
        .setType(pathname.getType());
            
      LispObject result = NIL;
      while (iterator.hasNext()) {
        Map.Entry<PathnameJar,ZipEntry> e = iterator.next();
        PathnameJar entry = e.getKey();
        LispObject matches
          = Symbol.PATHNAME_MATCH_P.execute(entry, wildcard);
          
        if (!matches.equals(NIL)) {
          result = new Cons(entry, result);
        }
      }

      // FIXME implement recursive jars
      // if (pathname.getDevice().cdr() instanceof Cons) {
      //   ZipFile outerJar = ZipCache.get((Pathname)pathname.getDevice().car());
      //   String entryPath = ((Pathname)pathname.getDevice().cdr().car()).getNamestring(); //???
      //   if (entryPath.startsWith("/")) {
      //     entryPath = entryPath.substring(1);
      //   }
      //   ZipEntry entry = outerJar.getEntry(entryPath);
      //   InputStream inputStream = null;
      //   try {
      //     inputStream = outerJar.getInputStream(entry);
      //   } catch (IOException e) {
      //     return new FileError("Failed to read zip input stream inside zip.",
      //                          pathname);
      //   }
      //   ZipInputStream zipInputStream
      //     = new ZipInputStream(inputStream);

      //   try {
      //     while ((entry = zipInputStream.getNextEntry()) != null) {
      //       String entryName = "/" + entry.getName();
      //       LispObject matches = Symbol.PATHNAME_MATCH_P
      //         .execute(new SimpleString(entryName), wildcard);
            
      //       if (!matches.equals(NIL)) {
      //         String namestring = new String(pathname.getNamestring());
      //         namestring = namestring.substring(0, namestring.lastIndexOf("!/") + 2)
      //           + entry.getName();
      //         Pathname p = (Pathname)Pathname.create(namestring);
      //         result = new Cons(p, result);
      //       }
      //     }
      //   } catch (IOException e) {
      //     return new FileError("Failed to seek through zip inputstream inside zip.",
      //                          pathname);
      //   }
      // } else {
      // }
      return result;
    }
  }

  public long getLastModified() {
    if (!isArchiveEntry()) {
      ZipCache.Archive archive = ZipCache.getArchive(this);
      if (archive != null) {
        return archive.lastModified;
      }
    } else {
      ZipEntry entry = ZipCache.getZipEntry(this);
      if (entry != null) {
        return entry.getTime();
      }
    }
    return -1; // shouldn't be reached
  }
}
      

    // String rootJarNamestring = ((Pathname)rootJar).getNamestring();
    // try {
    //   ZipFile jarFile = new ZipFile(rootJarNamestring);
    //   ZipFileInputStream inputStream = null;
    //   while (enclosingJars.car() != NIL) {
    //     LispObject jar = enclosingJars.car();
    //     String ns = ((Pathname)jar).getNamestring();
    //     ZipEntry entry = currentZip.getEntry(ns);
    //     if (entry == null) {
    //       return simple_error("Failed to find entry ~a in ~a", ns, pathname);
    //     }
    //     InputStream i = entry.getInputStream();
    //     ZipInputStream zi = new ZipInputStream(i);
    //     enclosingJars = enclosingJars.cdr();
    //   }
    // } catch (IOException e) {
    //   return Pathname.doTruenameExit(result, errorIfDoesNotExist);
    // }





  // truename
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




// Pathname.init()
          // A JAR file


            // LispObject jars = NIL;
            // int i = s.lastIndexOf(jarSeparator, s.length() - jarSeparator.length() - 1);
            // String jar = null;
            // if (i == -1) {
            //     jar = s;
            // } else {
            //     // There can be no more than two jar references and the
            //     // inner one must be a file reference within the outer.
            //     jar = "jar:file:" + s.substring(i + jarSeparator.length());
            //     s = s.substring("jar:".length(), i + jarSeparator.length());
            //     Pathname p = (Pathname) Pathname.create(s);
            //     jars = jars.push(p.getDevice().car());
            // }
            // if (jar.startsWith("jar:file:")) { // PathnameJar should handle this part…
            //     String file
            //         = jar.substring("jar:file:".length(),
            //                         jar.length() - jarSeparator.length());
            //     Pathname jarPathname;
            //     if (file.length() > 0) {
            //         URL url = null;
            //         URI uri = null;
            //         try {
            //             url = new URL("file:" + file);
            //             uri = url.toURI();
            //         } catch (MalformedURLException e1) {
            //             error(new SimpleError("Failed to create URI from "
            //                                 + "'" + file + "'"
            //                                 + ": " + e1.getMessage()));
            //         } catch (URISyntaxException e2) {
            //             error(new SimpleError("Failed to create URI from "
            //                                 + "'" + file + "'"
            //                                 + ": " + e2.getMessage()));
            //         }
            //         String path = uri.getPath();
            //         if (path == null) {
            //             // We allow "jar:file:baz.jar!/" to construct a relative
            //             // path for jar files, so MERGE-PATHNAMES means something.
            //           jarPathname = (Pathname)Pathname.create(uri.getSchemeSpecificPart());
            //         } else {
            //           jarPathname = (Pathname)Pathname.create((new File(path)).getPath());
            //         }
            //     } else {
            //       jarPathname = (Pathname)Pathname.create("");
            //     }
            //     jars = jars.push(jarPathname);
            // } else {
            //     URL url = null;
            //     try {
            //         url = new URL(jar.substring("jar:".length(), jar.length() - 2));
            //         Pathname p = (Pathname)Pathname.create(url);
            //         jars = jars.push(p);
            //     } catch (MalformedURLException e) {
            //         error(new LispError("Failed to parse URL "
            //                             + "'" + url + "'"
            //                             + e.getMessage()));
            //     }
            // }
            // jars = jars.nreverse();
            // setDevice(jars);
            // invalidateNamestring();
            // return;


        // // An entry in a JAR file
        // final int separatorIndex = s.lastIndexOf(jarSeparator);
        // if (separatorIndex > 0 && s.startsWith("jar:")) {
        //   return PathnameJar.create(s);
        // }
        //     final String jarURL = s.substring(0, separatorIndex + jarSeparator.length());
        //     URL url = null;
        //     try {
        //         url = new URL(jarURL);
        //     } catch (MalformedURLException ex) {
        //         error(new LispError("Failed to parse URL "
        //                             + "'" + jarURL + "'"
        //                             + ex.getMessage()));
        //     }
        //     Pathname d = (Pathname)Pathname.create(url);
        //     if (getDevice() instanceof Cons) {
        //         LispObject[] jars = d.copyToArray();
        //         //  XXX Is this ever reached?  If so, need to append lists
        //         Debug.assertTrue(false);
        //     } else {
        //         setDevice(d.getDevice());
        //     }
        //     s = "/" + s.substring(separatorIndex + jarSeparator.length());
        //     Pathname p = (Pathname)Pathname.create("file:" + s); // Use URI escaping rules
        //     setDirectory(p.getDirectory());
        //     setName(p.getName());
        //     setType(p.getType());
        //     setVersion(p.getVersion());
        //     return;
        // }




