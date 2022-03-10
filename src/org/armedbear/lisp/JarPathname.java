/* 
 * JarPathname.java
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

public class JarPathname
  extends URLPathname
{
  static final public String JAR_URI_SUFFIX = "!/";
  static final public String JAR_URI_PREFIX = "jar:";

  protected JarPathname() {}

  public static JarPathname create() {
    return new JarPathname();
  }

  public static JarPathname create(JarPathname p) {
    JarPathname result = new JarPathname();
    result.copyFrom(p);
    return result;
  }

  public static JarPathname createFromPathname(Pathname p) {
    JarPathname result = new JarPathname();
    URLPathname rootDevice = new URLPathname();

    if (p instanceof URLPathname) {
      rootDevice.copyFrom(p);
    } else if (p instanceof Pathname) {
      rootDevice = URLPathname.create(p);
    } else {
      simple_error("Argument is already a JAR-PATHNAME: ~a", p);
    }

    result.setDevice(new Cons(rootDevice, NIL));

    return result;
  }

  /** Transform a reference to a nested Jar to an entry */
  public static JarPathname archiveAsEntry(JarPathname p) {
    JarPathname result = new JarPathname();
    result = (JarPathname)result.copyFrom(p);

    LispObject jars = result.getJars();
    jars = jars.nreverse();
    Pathname entry = (Pathname)jars.car();
    jars = jars.cdr().nreverse();

    result
      .setDevice(jars)
      .setDirectory(entry.getDirectory())
      .setName(entry.getName())
      .setType(entry.getType());
    
    return result;
  }
    

  /** Transform an entry in a jar to a reference as a jar */
  public static JarPathname createFromEntry(JarPathname p) {
    JarPathname result = new JarPathname();
    result
      .copyFrom(p)
      .setDirectory(NIL)
      .setName(NIL)
      .setType(NIL)
      .setVersion(Keyword.NEWEST);
    Pathname entryPath = p.getEntryPath();
    LispObject device = result.getDevice();
    device = device.reverse().push(entryPath).reverse();
    result.setDevice(device);
    return result;
  }

  @DocString(name="as-jar-pathname-archive",
             args="pathname",
             returns="jar-pathname",
             doc="Returns PATHNAME as a reference to a JAR-PATHNAME archive"
             + "\n"
             + "If PATHNAME names an ordinary file, the resulting JAR-PATHNAME addresses the"
             + "file as an archive.  If PATHNAME names an entry in an archive, the resulting"
             + "JAR-PATHNAME addresses that entry as a zip archive within that archive.")
  private static final Primitive AS_JAR_PATHNAME_ARCHIVE = new pf_as_jar_pathname_archive();
  private static class pf_as_jar_pathname_archive extends Primitive {
    pf_as_jar_pathname_archive() {
      super("as-jar-pathname-archive", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg) {
      if (arg instanceof AbstractString) {
        arg = coerceToPathname(arg);
      }
      if (arg instanceof JarPathname) {
        return createFromEntry((JarPathname)arg);
      } if (arg instanceof Pathname) {
        return createFromPathname((Pathname)arg);
      }
      type_error(arg,
                 list(Symbol.OR,
                      Symbol.PATHNAME, Symbol.URL_PATHNAME,
                      Symbol.JAR_PATHNAME));
      return (LispObject)UNREACHED;
    }
  };
    
  static public JarPathname createFromFile(String s) {
    JarPathname result
      = JarPathname.create(JAR_URI_PREFIX + "file:" + s + JAR_URI_SUFFIX);
    result.setVersion(Keyword.NEWEST);
    return result;
  }

  static public JarPathname createEntryFromFile(String jar, String entry) {
    JarPathname result
      = JarPathname.create(JAR_URI_PREFIX + "file:" + jar + JAR_URI_SUFFIX + entry);
    result.setVersion(Keyword.NEWEST);
    return result;
  }

  static public JarPathname createEntryFromJar(JarPathname jar, String entry) {
    if (jar.isArchiveEntry()) {
      simple_error("Failed to create the entry ~a in ~a", entry, jar);
      return (JarPathname)UNREACHED;
    }
    JarPathname result = new JarPathname();
    result.copyFrom(jar);
    String path = new String(entry);
    if (!path.startsWith("/")) {
      path = "/" + path;
    }
    Pathname p = Pathname.create(path);
    result
      .setDirectory(p.getDirectory())
      .setName(p.getName())
      .setType(p.getType())
      .setVersion(Keyword.NEWEST);
    
    return result;
  }
  /** 
   *  Enumerate the components of a jar namestring 
   */
  static List<String> enumerate(String s) {
    ArrayList<String> result = new ArrayList<String>();

    int i = s.lastIndexOf(JAR_URI_PREFIX);
    if (i == -1) {
      parse_error("Failed to find any occurence of '" + JAR_URI_PREFIX + "' prefixes:" + s);
      return null; // not reached
    }
    i += JAR_URI_PREFIX.length(); // advance index to end of "jar:jar:jar:..."
    if ((i % JAR_URI_PREFIX.length()) != 0) {
      parse_error("Failed to parse 'jar:' prefixes:" + s);
      return null;
    }
    int prefixCount = i / JAR_URI_PREFIX.length(); 
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

    // The root, non-JarPathname location of this reference
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

  static public JarPathname create(String s) {
    if (!s.startsWith(JAR_URI_PREFIX)) {
      parse_error("Cannot create a PATHNAME-JAR from namestring: " + s);
      return (JarPathname)UNREACHED;
    }
    
    List<String> contents = JarPathname.enumerate(s);

    if (contents == null) {
      parse_error("Couldn't parse PATHNAME-JAR from namestring: " + s);
      return (JarPathname)UNREACHED;
    }
    
    JarPathname result = new JarPathname();

    // Normalize the root jar to be a URL
    URLPathname rootPathname;
    String rootNamestring = contents.get(0);
    if (!isValidURL(rootNamestring)) {
      Pathname root = Pathname.create(rootNamestring);
      rootPathname = URLPathname.createFromFile(root);
    } else {
      rootPathname = URLPathname.create(rootNamestring);
    }

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
        jar.copyFrom(pathname);
        jars = jars.push(jar);
      } else { 
        Pathname p = (Pathname)Pathname.create(contents.get(i));
        result.copyFrom(p);
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

    LispObject rootJar = getRootJar();
    if (!(rootJar instanceof URLPathname)) {
        return type_error("The first element in the DEVICE component of a JAR-PATHNAME is not of expected type",
                          rootJar,
                          Symbol.URL_PATHNAME);
    }

    jars = jars.cdr();
    
    while (!jars.car().equals(NIL)) {
      LispObject jar = jars.car();
      if (!((jar instanceof Pathname)
            || (jar instanceof URLPathname))) {
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

    LispObject jars = getJars();

    if (jars.equals(NIL) || jars.equals(Keyword.UNSPECIFIC)) { 
      // type_error("JAR-PATHNAME has bad DEVICE",
      //            jars,
      //            list(Symbol.NOT,
      //                 list(Symbol.OR,
      //                      list(Symbol.EQL, NIL),
      //                      list(Symbol.EQL, Keyword.UNSPECIFIC))));
      return null;
    }

    for (int i = 0; i < jars.length() - 1; i++) {
      sb.append(JAR_URI_PREFIX);
    }

    LispObject root = getRootJar();

    if (root instanceof URLPathname) {
      String ns = ((URLPathname)root).getNamestringAsURL();
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

    LispObject innerJars = jars.cdr();
    while (innerJars.car() != NIL) {
      Pathname jar = (Pathname)innerJars.car();
      Pathname p = new Pathname();
      p.copyFrom(jar)
        .setDevice(NIL);
      String ns = p.getNamestring();
      sb.append(ns)
        .append(JAR_URI_SUFFIX);
      innerJars = innerJars.cdr();
    }

    if (getDirectory() != NIL
        || getName() != NIL
        || getType() != NIL) {
      
      Pathname withoutDevice = new Pathname();
      withoutDevice
        .copyFrom(this)
        .setDevice(NIL);

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
      type_error("JAR-PATHNAME device is not a cons",
                 jars, Symbol.CONS);
      return (LispObject)UNREACHED;
    }
      
    return jars.car();
  }

  String getRootJarAsURLString() {
    return
      JarPathname.JAR_URI_PREFIX
      + ((URLPathname)getRootJar()).getNamestring()
      + JarPathname.JAR_URI_SUFFIX;
  }


  LispObject getJars() {
    return getDevice();
  }

  public static LispObject truename(Pathname pathname,
                                    boolean errorIfDoesNotExist) {
    if (!(pathname instanceof JarPathname)) {
      return URLPathname.truename(pathname, errorIfDoesNotExist);
    }
    JarPathname p = new JarPathname();
    p.copyFrom(pathname);

    // Run truename resolution on the path of local jar archives
    if (p.isLocalFile()) {
      Pathname rootJar;
      if (URLPathname.hasExplicitFile((Pathname)p.getRootJar())) {
        rootJar = new URLPathname();
      } else {
        rootJar = new Pathname();
      }
      rootJar.copyFrom((Pathname)p.getRootJar());

      // Ensure that we don't return a JarPathname if the current
      // default is one when we resolve its TRUENAME.  Under Windows,
      // the device will get filled in with the DOS drive letter if
      // applicable.
      if (rootJar.getDevice().equals(NIL)
          && !Utilities.isPlatformWindows) {
        rootJar.setDevice(Keyword.UNSPECIFIC);
      }
      LispObject rootJarTruename = Pathname.truename(rootJar, errorIfDoesNotExist);
      if (rootJarTruename.equals(NIL)) {
        return Pathname.doTruenameExit(rootJar, errorIfDoesNotExist);
      }
      LispObject otherJars = p.getJars().cdr();
      URLPathname newRootJar;
      if (rootJarTruename instanceof Pathname) {
        newRootJar = URLPathname.createFromFile((Pathname)rootJarTruename);
      } else {
        newRootJar = (URLPathname) rootJarTruename;
      }

      p.setDevice(new Cons(newRootJar, otherJars));
    }

    if (!p.isArchiveEntry()) {
      ZipCache.Archive archive = ZipCache.getArchive(p);
      if (archive == null) {
        return Pathname.doTruenameExit(pathname, errorIfDoesNotExist);
      }
      return p;
    }

    ZipEntry entry = ZipCache.getZipEntry(p);
    if (entry == null) {
      return Pathname.doTruenameExit(pathname, errorIfDoesNotExist);
    }
    return p;
  }

  public boolean isLocalFile() {
    Pathname p = (Pathname) getRootJar();
    if (p != null) {
      return p.isLocalFile();
    }
    return false;
  }

  public boolean isArchiveEntry() {
    return !(getDirectory().equals(NIL)
             && getName().equals(NIL)
             && getType().equals(NIL));
  }

  public JarPathname getArchive() {
    if (!isArchiveEntry()) {
      return (JarPathname)simple_error("Pathname already represents an archive.");
    }
    JarPathname archive = new JarPathname();
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

  /** List the contents of a directory within a JAR archive */
  static public LispObject listDirectory(JarPathname pathname) {
    String directory = pathname.asEntryPath();
    // We should only be listing directories
    if (pathname.getDirectory() == NIL) {
      return simple_error("Not a directory in a jar ~a", pathname);
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

    Pathname wildcard = (Pathname)Pathname.create(directory);

    LispObject result = NIL;
    
    Iterator<Map.Entry<JarPathname,ZipEntry>> iterator = ZipCache.getEntriesIterator(pathname);
    while (iterator.hasNext()) {
      Map.Entry<JarPathname,ZipEntry> e = iterator.next();
      JarPathname entry = e.getKey();
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

      JarPathname jarPathname = new JarPathname();
      jarPathname
        .copyFrom(pathname)
        .setDirectory(NIL)
        .setName(NIL)
        .setType(NIL);
      JarPathname wildcard = (JarPathname)Symbol.TRUENAME.execute(jarPathname);
      Iterator<Map.Entry<JarPathname,ZipEntry>> iterator
        = ZipCache.getEntriesIterator(wildcard);
      wildcard
        .setDirectory(pathname.getDirectory())
        .setName(pathname.getName())
        .setType(pathname.getType());
            
      LispObject result = NIL;
      while (iterator.hasNext()) {
        Map.Entry<JarPathname,ZipEntry> e = iterator.next();
        JarPathname entry = e.getKey();
        LispObject matches
          = Symbol.PATHNAME_MATCH_P.execute(entry, wildcard);
          
        if (!matches.equals(NIL)) {
          result = new Cons(entry, result);
        }
      }

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
    return 0;
  }

  static JarPathname joinEntry(JarPathname root, Pathname entry) {
    JarPathname result = new JarPathname();
    result
      .copyFrom(root)
      .setDirectory(entry.getDirectory())
      .setName(entry.getName())
      .setType(entry.getType()); // ??? VERSION
    return result;
  }
}
