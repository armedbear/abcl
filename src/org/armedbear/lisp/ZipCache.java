/*
 * ZipCache.java
 *
 * Copyright (C) 2010, 2014 Mark Evenson
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import org.armedbear.lisp.util.HttpHead;
import static org.armedbear.lisp.Lisp.*;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * A cache for all zip/jar file access by JarPathname that uses the last
 * modified time of the cached resource.
 *
 * If you run into problems with caching, use
 * (SYS::DISABLE-ZIP-CACHE).  Once disabled, the caching cannot be
 * re-enabled.
 *
 */ 
public class ZipCache {
  public static final boolean checkZipFile(Pathname name) {
    InputStream input = name.getInputStream();
    try {
      byte[] bytes = new byte[4];
      int bytesRead = input.read(bytes);
      return bytesRead == 4 && bytes[0] == 80 && bytes[1] == 75 && bytes[2] == 3 && bytes[3] == 4;
    } catch (Throwable t) {
      // any error probably means 'no'
      return false;
    } finally {
      if (input != null) {
        try {
          input.close();
        } catch (IOException e) {
        } // ignore exceptions
      }
    }
  }
  static InputStream getInputStream(ZipFile jarFile, String entryPath) {
    ZipEntry entry = jarFile.getEntry(entryPath);
    if (entry == null) {
      Debug.trace("Failed to find entry " + "'" + entryPath + "'" + " in " + "'" + jarFile.getName() + "'");
      return null;
    }
    InputStream result = null;
    try {
      result = jarFile.getInputStream(entry);
    } catch (IOException e) {
      Debug.trace("Failed to open InputStream for " + "'" + entryPath + "'" + " in " + "'" + jarFile.getName() + "'");
      return null;
    }
    return result;
  }
  public static ZipInputStream getZipInputStream(ZipFile zipfile, String entryName) {
    return ZipCache.getZipInputStream(zipfile, entryName, false);
  }
  public static ZipInputStream getZipInputStream(ZipFile zipfile, String entryName, boolean errorOnFailure) {
    ZipEntry zipEntry = zipfile.getEntry(entryName);
    ZipInputStream stream = null;
    try {
      stream = new ZipInputStream(zipfile.getInputStream(zipEntry));
    } catch (IOException e) {
      if (errorOnFailure) {
        simple_error("Failed to open '" + entryName + "' in zipfile '" + zipfile + "': " + e.getMessage());
      }
      return null;
    }
    return stream;
  }
  public static ByteArrayOutputStream readEntry(ZipInputStream stream) {
    ByteArrayOutputStream result = new ByteArrayOutputStream();
    int count;
    byte[] buf = new byte[1024]; // What's a decent buffer size?
    try {
      while ((count = stream.read(buf, 0, buf.length)) != -1) {
        result.write(buf, 0, count);
      }
    } catch (IOException e) {
      Debug.trace("Failed to read entry from " + stream + ": " + e);
      return null;
    }
    return result;
  }
  public static ZipEntry getEntry(ZipInputStream zipInputStream, String entryName) {
    return ZipCache.getEntry(zipInputStream, entryName, false);
  }
  public static ZipEntry getEntry(ZipInputStream zipInputStream, String entryName, boolean errorOnFailure) {
    ZipEntry entry = null;
    do {
      try {
        entry = zipInputStream.getNextEntry();
      } catch (IOException e) {
        if (errorOnFailure) {
          Lisp.error(new FileError("Failed to seek for " + "'" + entryName + "'" + " in " + zipInputStream.toString()));
        }
        return null;
      }
    } while (entry != null && !entry.getName().equals(entryName));
    if (entry != null) {
      return entry;
    }
    if (errorOnFailure) {
      Lisp.error(new FileError("Failed to find " + "'" + entryName + "'" + " in " + zipInputStream.toString()));
    }
    return null;
  }

  public static InputStream getEntryAsInputStream(ZipInputStream zipInputStream, String entryName) {
    ZipEntry entry = getEntry(zipInputStream, entryName);
    ByteArrayOutputStream bytes = readEntry(zipInputStream);
    return new ByteArrayInputStream(bytes.toByteArray());
  }

  public static InputStream getEntryAsInputStream(PathnameJar archiveEntry) {
    InputStream result = null;
    if (archiveEntry.getDevice().length() > 1) {
      simple_error("Unimplemented retrieval of InputStream from a nested jar reference");
      return (InputStream)UNREACHED;
      // Pathname inner = (Pathname) getDevice().cdr().car();
      // InputStream input = ZipCache.getInputStream(jarFile, inner);
      // ZipInputStream zipInputStream = new ZipInputStream(input);
      // result =  ZipCache.getEntryAsInputStream(zipInputStream, entryPath);
    } else {
      Archive archive = ZipCache.getArchive(archiveEntry);
      // ZipFile zipFile = archive.file;
      // ZipEntry entry = archive.getEntry(archiveEntry);
      
      result = archive.getEntryAsInputStream(archiveEntry);
      if (result == null) {
        simple_error("Failed to get InputStream for ~a", archiveEntry);
      }
    }
    return result;
  }

  // To make this thread safe, we should return a proxy for ZipFile
  // that keeps track of the number of outstanding references handed
  // out, not allowing ZipFile.close() to succeed until that count
  // has been reduced to 1 or the finalizer is executing.
  // Unfortunately the relatively simple strategy of extending
  // ZipFile via a CachedZipFile does not work because there is not
  // a null arg constructor for ZipFile.
  static HashMap<PathnameJar, Archive> cache = new HashMap<PathnameJar, Archive>();

  abstract static public class Archive {
    PathnameJar root;
    LinkedHashMap<PathnameJar, ZipEntry> entries
      = new LinkedHashMap<PathnameJar, ZipEntry>();
    long lastModified;

    abstract InputStream getEntryAsInputStream(PathnameJar entry);
    abstract ZipEntry getEntry(PathnameJar entry);
    abstract void populateAllEntries();
    abstract void close();
  }

  static public class ArchiveStream
    extends Archive
  {
    InputStream source;

    public InputStream getEntryAsInputStream(PathnameJar entry) {
      return null;
    }

    public ZipEntry getEntry(PathnameJar entry) {
      return null;
    }

    void populateAllEntries() {}

    void close () {
      if (source != null) {
        try {
          source.close();
        } catch (IOException ex) {
          {}
        }
      }
    }
  }

  static public class ArchiveURL
    extends ArchiveFile
  {
    JarURLConnection connection;
    void close() {
      super.close();
      // TODO: do we need to clean up from the connection?
    }
  }

  static public class ArchiveFile
    extends Archive
  { 
    ZipFile file;

    ZipFile get() { return file;}

    public ZipEntry getEntry(PathnameJar entryPathname) {
      ZipEntry result = entries.get(entryPathname);
      if (result != null) {
        return result;
      }
      String entryPath = entryPathname.asEntryPath();
      result = file.getEntry(entryPath);

      if (result == null) {
        return null;
      }

      // ZipFile.getEntry() will return directories when asked for
      // files.
      if (result.isDirectory()
          && (!entryPathname.getName().equals(NIL)
              || !entryPathname.getType().equals(NIL))) {
        return null;
      }
      
      entries.put(entryPathname, result);
      return result;
    }

    void populateAllEntries() {
      ZipFile f = file;
      if (f.size() == entries.size()) {
        return;
      }

      Enumeration<? extends ZipEntry> e = f.entries();
      while (e.hasMoreElements()) {
        ZipEntry entry = e.nextElement();
        String name = entry.getName();
        PathnameJar entryPathname
          = (PathnameJar)PathnameJar.createEntryFromJar(root, name);
        entries.put(entryPathname, entry);
      }
    }

    InputStream getEntryAsInputStream(PathnameJar entry) {
      InputStream result = null;
      ZipEntry zipEntry = getEntry(entry);

      try { 
        result = file.getInputStream(zipEntry);
      } catch (IOException e) {}

      return result;
    }
    void close() {
      if (file != null) {
        try {
          file.close();
        } catch (IOException e) {}
          
      }
    }
  }
  

  static boolean cacheEnabled = true;
  private final static Primitive DISABLE_ZIP_CACHE = new disable_zip_cache();
  final static class disable_zip_cache extends Primitive {
    disable_zip_cache() {
      super("disable-zip-cache", PACKAGE_SYS, true, "",
            "Disable all caching of ABCL FASLs and ZIPs.");
    }
    @Override
    public LispObject execute() {
      ZipCache.disable();
      return T;
    }
  }
  static public synchronized void disable() {
    cacheEnabled = false;
    cache.clear();  
  }

  synchronized public static LinkedHashMap<PathnameJar,ZipEntry> getEntries(PathnameJar jar) {
    Archive archive = getArchive(jar);
    archive.populateAllEntries(); // Very expensive for jars with large number of entries
    return archive.entries;
  }

  synchronized public static Iterator<Map.Entry<PathnameJar,ZipEntry>> getEntriesIterator(PathnameJar jar) {
    LinkedHashMap<PathnameJar,ZipEntry> entries = getEntries(jar);
    Set<Map.Entry<PathnameJar,ZipEntry>> set = entries.entrySet();
    return set.iterator();
  }


  static ZipEntry getZipEntry(PathnameJar archiveEntry) {
    PathnameJar archiveJar = archiveEntry.getArchive();
    Archive zip = getArchive(archiveJar);
    ZipEntry entry = zip.getEntry(archiveEntry);
    return entry;
  }

  synchronized public static Archive getArchive(PathnameJar jar) {
    Archive cached = cache.get(jar);
    if (cached != null) {
      return cached;
    }

    Pathname rootJar = (Pathname) jar.getRootJar();
    LispObject innerJars = jar.getJars().cdr();

    if (!rootJar.isLocalFile()) {
      return getArchiveURL(jar);
    }
    
    if (innerJars.equals(NIL)) {
      return getArchiveFile(jar);
    } else {
      PathnameJar rootPathname = (PathnameJar)PathnameJar.createFromPathname(rootJar);

      Archive current = ZipCache.getArchive(rootPathname);
      
      while (innerJars.car() != NIL) {
      // TODO Finish me!
      // Pathname next = (Pathname)innerJars.car();
      // String entryPath = next.asEntryPath();
      // InputStream source;
      // if (current instanceof ArchiveFile) {
      //   ArchiveFile zipFile = ((ArchiveFile)current).get();
      //   ZipEntry entry = zipFile.getEntry(entryPath);
      //   source = zipFile.getInputStream(entry);
      //   ArchiveStream stream = new ArchiveStream();
      //   stream.source = source;
      // }
      }
      // FIXME
      simple_error("Currently unimplemented recursive archive: ~a" , jar);
      return (Archive)UNREACHED;
    }
  }

  public static Archive getArchiveURL(PathnameJar jar) {
    Pathname rootJar = (Pathname) jar.getRootJar();
    String rootJarURLString = jar.getRootJarAsURLString();
    URL rootJarURL = null;
    try {
      rootJarURL = new URL(rootJarURLString);
      JarURLConnection jarConnection
        = (JarURLConnection) rootJarURL.openConnection();

      ArchiveURL result = new ArchiveURL();
      result.root = jar;
      result.connection = jarConnection;
      result.file = (ZipFile)jarConnection.getJarFile();
      result.lastModified = 0; // FIXME
      cache.put(jar, result);
      return result;
    } catch (MalformedURLException e) {
      simple_error("Failed to form root URL for ~a: ~a", jar, rootJarURLString);
       return (Archive)UNREACHED;      
    } catch (IOException e) {
      simple_error("Failed to fetch ~a: ~a", jar, e);
      return (Archive)UNREACHED;      
    }
  }

  static public Archive getArchiveFile(PathnameJar jar) {
    try {
      ArchiveFile result = new ArchiveFile();
      File f = ((Pathname)jar.getRootJar()).getFile();
      result.root = jar;
      result.file = new ZipFile(f);
      result.lastModified = f.lastModified();
      cache.put(jar, result);
      return result;
    } catch (ZipException e) {
      error(new FileError("Failed to open local zip archive"
                          + " because " + e, jar));
                          
      return (Archive)UNREACHED;
    } catch (IOException e) {
      error(new FileError("Failed to open local zip archive"
                          + " because " + e, jar));
      return (Archive)UNREACHED;
    }
  }

  // Archive archive  = cache.getArchive(jar);

  //   // Check that the cache entry still accesses a valid ZipFile
  //   if (entry != null) {
  //     // Simplest way to call private ZipFile.ensureOpen()
  //     try {
  //       int size = archive.file.size(); 
  //     } catch (IllegalStateException e) {
  //       cache.remove(jar);
  //       entry = null;
  //     }
  //   }

  //   if (entry != null) {
  //     if (url.getProtocol().equals("file")) {
  //       File f = new File(url.getPath());
  //       long current = f.lastModified();
  //       if (current > entry.lastModified) {
  //         try {
  //           entry.file = new ZipFile(f);
  //           entry.lastModified = current;
  //         } catch (IOException e) {
  //           Debug.trace(e.toString()); // XXX
  //         }
  //       }
  //     } else if (url.getProtocol().equals("http")) {
  //       checkRemoteLastModified();
  //     } else {
  //       entry = fetchURL(url, false);
  //       zipCache.put(url, entry);
  //     }
  //   } else {
  //     if (url.getProtocol().equals("file")) {
  //       entry = new Entry();
  //       String path = url.getPath();
        
  //       if (Utilities.isPlatformWindows) {
  //         String authority = url.getAuthority();
  //         if (authority != null) {
  //           path = authority + path;
  //         }
  //       }
  //       File f = new File(path);
  //       entry.lastModified = f.lastModified();
  //       try {
  //         entry.file = new ZipFile(f);
  //       } catch (ZipException e) {
  //         error(new FileError("Failed to get cached ZipFile"
  //                             + " because " + e,
  //                             Pathname.makePathname(f)));
  //       } catch (IOException e) {
  //         error(new FileError("Failed to get cached ZipFile"
  //                             + " because " + e,
  //                             Pathname.makePathname(f)));
  //       }
  //     } else {
  //       entry = fetchURL(url, true);
  //     }
  //     zipCache.put(url, entry);
  //   }
  //   return entry.file;
  // }


  static void checkRemoteLastModified(ArchiveURL archive) {
    // Unfortunately, the Apple JDK under OS X doesn't do
    // HTTP HEAD requests, instead refetching the entire
    // resource, and I assume this is the case in all
    // Sun-derived JVMs.  So, we use a custom HEAD
    // implementation only looking for Last-Modified
    // headers, which if we don't find, we give up and
    // refetch the resource.

    String dateString = null;

    String url = archive.root.getRootJarAsURLString();
    
    try {
      dateString = HttpHead.get(url, "Last-Modified");
    } catch (IOException ex) {
      Debug.trace(ex);
    }
    Date date = null;
    ParsePosition pos = new ParsePosition(0);

    final SimpleDateFormat ASCTIME
      = new SimpleDateFormat("EEE MMM d HH:mm:ss yyyy", Locale.US);
    final SimpleDateFormat RFC_1036
      = new SimpleDateFormat("EEEE, dd-MMM-yy HH:mm:ss zzz", Locale.US);
    final SimpleDateFormat RFC_1123
      = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.US);

    if (dateString != null) {
      date = RFC_1123.parse(dateString, pos);
      if (date == null) {
        date = RFC_1036.parse(dateString, pos);
        if (date == null) {
          date = ASCTIME.parse(dateString, pos);
        }
      }
    }

    // Replace older item in cache
    if (date == null || date.getTime() > archive.lastModified) {
      PathnameJar root = archive.root;
      Archive entry = getArchiveURL(root);
      cache.put(root, entry);
    }
    if (date == null) {
      if (dateString == null) {
        Debug.trace("Failed to retrieve request header: "
                    + url.toString());
      } else {
        Debug.trace("Failed to parse Last-Modified date: " +
                    dateString);
      }
    }
  }

  // FIXME recode once local fileaccesses are working
  static private ZipFile getRemote(PathnameJar url) {
    ZipFile result = null;
    URL jarURL = null;
    try {
      jarURL = new URL("jar:" + url.getRootJar() + "!/");
    } catch (MalformedURLException e) {
      error(new LispError("Failed to form a jar: URL from "
                          + "'" + url + "'" 
                          + " because " + e));
    }
    URLConnection connection = null;
    try {
      connection = jarURL.openConnection();
    } catch (IOException e) {
      error(new LispError("Failed to open "
                          + "'" + jarURL + "'"
                          + " with exception " 
                          + e));
    }
    if (!(connection instanceof JarURLConnection)) {
      error(new LispError("Could not get a URLConnection from " 
                          + "'" + jarURL + "'"));
    }
    JarURLConnection jarURLConnection = (JarURLConnection) connection;
    //    jarURLConnection.setUseCaches(useCaches);
    try {
      result = jarURLConnection.getJarFile();
    } catch (IOException e) {
      error(new LispError("Failed to fetch URL "
                          + "'" + jarURLConnection + "'"
                          + " because " + e));
    }
    if (cacheEnabled) {
      ArchiveURL archive = new ArchiveURL();
      archive.file = result;
      archive.lastModified = jarURLConnection.getLastModified();
      archive.root = url;
      cache.put(url, archive);
    }
    return result;
  }

  // ## remove-zip-cache-entry pathname => boolean
  private static final Primitive REMOVE_ZIP_CACHE_ENTRY = new remove_zip_cache_entry();
  private static class remove_zip_cache_entry extends Primitive { 
    remove_zip_cache_entry() {
      super("remove-zip-cache-entry", PACKAGE_SYS, true, "pathname");
    }
    @Override
    public LispObject execute(LispObject arg) {
      Pathname p = coerceToPathname(arg);
      if (!(p instanceof PathnameJar)) {
        type_error(arg, Symbol.JAR_PATHNAME);
      }
      boolean result = ZipCache.remove((PathnameJar)p);
      return result ? T : NIL;
    }
  }
      
  synchronized public static boolean remove(PathnameJar p) {
    Archive archive = cache.get(p.getNamestring());
    if (archive != null) {
      archive.close();
      cache.remove(p);
      return true;
    }
    return false;
  }

  static public long getLastModified(PathnameJar p) {
    return 0; // FIXME
    // JAR cases
    // 0.  JAR from URL 
    // 1.  JAR
    // 2.  JAR in JAR
    // 3.  Entry in JAR
    // 4.  Entry in JAR in JAR
    // String entryPath = p.asEntryPath();

    // LispObject jars = p.getDevice();
    // if (jars.cdr().equals(NIL)) {
    //   if (entryPath.length() == 0) {
    //     LispObject o = jars.car();
    //     // 0. JAR from URL
    //     // 1. JAR
    //     return ((Pathname)o).getLastModified();
    //   } else {
    //     // 3. Entry in JAR
    //     final ZipEntry entry 
    //       = ZipCache.get((Pathname)getDevice().car()).getEntry(entryPath);
    //     if (entry == null) {
    //       return 0;
    //     }
    //     final long time = entry.getTime();
    //     if (time == -1) {
    //       return 0;
    //     }
    //     return time;
    //   }
    // } else {
    //   ZipFile outerJar = ZipCache.get((Pathname)d.car());
    //   if (entryPath.length() == 0) {
    //     // 4.  JAR in JAR
    //     String jarPath = ((Pathname)d.cdr()).asEntryPath();
    //     final ZipEntry entry = outerJar.getEntry(jarPath);
    //     final long time = entry.getTime();
    //     if (time == -1) {
    //       return 0;
    //     }
    //     return time;
    //   } else {
    //     // 5. Entry in JAR in JAR
    //     String innerJarPath = ((Pathname)d.cdr()).asEntryPath();
    //     ZipEntry entry = outerJar.getEntry(entryPath);
    //     ZipInputStream innerJarInputStream
    //       = ZipCache.getZipInputStream(outerJar, innerJarPath);
    //     ZipEntry innerEntry = ZipCache.getEntry(innerJarInputStream,
    //                                             entryPath);
    //     long time = innerEntry.getTime();
    //     if (time == -1) {
    //       return 0;
    //     }
    //     return time;
    //   }
    // }
  }

}
