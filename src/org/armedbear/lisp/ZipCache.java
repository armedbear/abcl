/*
 * ZipCache.java
 *
 * Copyright (C) 2010 Mark Evenson
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
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * A cache for all zip/jar file accesses by URL that uses the last
 * modified time of the cached resource.
 */ 
public class ZipCache {
  static class Entry {
    long lastModified;
    ZipFile file;
  }
    
  static HashMap<URL, Entry> zipCache = new HashMap<URL, Entry>();

  public static ZipFile get(LispObject arg) {
      return get(Pathname.makeURL(arg));
  }
  
  public static ZipFile get(URL url) {
      Entry entry = zipCache.get(url);
      if (entry != null) {
          if (url.getProtocol().equals("file")) {
              File f = new File(url.getPath());
                long current = f.lastModified();
                if (current > entry.lastModified) {
                    try {
                    entry.file.close(); 
                    entry.file = new ZipFile(f);
                    entry.lastModified = current;
                    } catch (IOException e) {
                        Debug.trace(e.toString()); // XXX
                    }
                }
            } else {
              // Unfortunately, the Apple JDK under OS X doesn't do
              // HTTP HEAD requests, instead refetching the entire
              // resource, so the following code is a waste.  I assume
              // this is the case in all Sun-dervied JVMs. We'll have
              // to implement a custom HTTP lastModified checker.

              // URLConnection connection;
              // try {
              //     connection = url.openConnection();
              // } catch (IOException ex) {
              //     Debug.trace("Failed to open "
              //                 + "'" + url + "'");
              //     return null;
              // }
              // long current = connection.getLastModified();
              // if (current > entry.lastModified) {
              //     try {
              //         entry.file.close();
              //     } catch (IOException ex) {}
              //     entry = fetchURL(url, false);
              // }
            }
        } else {
           if (url.getProtocol().equals("file")) {
                entry = new Entry();
                File f = new File(url.getPath());
                entry.lastModified = f.lastModified();
                try {
                    entry.file = new ZipFile(f);
                } catch (ZipException e) {
                    Debug.trace(e); // XXX
                    return null;
                } catch (IOException e) {
                    Debug.trace(e); // XXX
                    return null;
                }
            } else {
                entry = fetchURL(url, true);
            }
            zipCache.put(url, entry);
        }
        return entry.file;
  }
      
      static private Entry fetchURL(URL url, boolean cached) {
          Entry result = new Entry();
          URL jarURL = null;
          try {
              jarURL = new URL("jar:" + url + "!/");
          } catch (MalformedURLException e) {
              Debug.trace(e);
              Debug.assertTrue(false); // XXX
          }
          URLConnection connection;
          try {
              connection = jarURL.openConnection();
          } catch (IOException ex) {
              Debug.trace("Failed to open "
                          + "'" + jarURL + "'");
              return null;
          }
          if (!(connection instanceof JarURLConnection)) {
              // XXX
              Debug.trace("Could not get a URLConnection from " + jarURL);
              return null;
          }
          JarURLConnection jarURLConnection = (JarURLConnection) connection;
          jarURLConnection.setUseCaches(cached);
          try {
              result.file = jarURLConnection.getJarFile();
          } catch (IOException e) {
              Debug.trace(e);
              Debug.assertTrue(false); // XXX
          }
          result.lastModified = jarURLConnection.getLastModified();
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
      URL url = Pathname.makeURL(p);
      boolean result = ZipCache.remove(url);
      return result ? T : NIL;
    }
  }
      

  public static boolean remove(URL url) {
    Entry entry = zipCache.get(url);
    if (entry != null) {
      try {
        entry.file.close();
      } catch (IOException e) {}
      zipCache.remove(entry);
      return true;
    }
    return false;
  }
  }