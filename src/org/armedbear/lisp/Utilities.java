/*
 * Utilities.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

public final class Utilities
{
    public static final boolean isPlatformUnix;
    public static final boolean isPlatformWindows;

    static {
        String osName = System.getProperty("os.name");
        isPlatformUnix = osName.startsWith("Linux") ||
            osName.startsWith("Mac OS X") || osName.startsWith("Darwin") ||
            osName.startsWith("Solaris") ||
            osName.startsWith("SunOS") || osName.startsWith("AIX") ||
            osName.startsWith("FreeBSD") || osName.startsWith("OpenBSD") ||
            osName.startsWith("NetBSD");
        isPlatformWindows = osName.startsWith("Windows");
    }

    public static boolean isFilenameAbsolute(String filename)
    {
        final int length = filename.length();
        if (length > 0) {
            char c0 = filename.charAt(0);
            if (c0 == '\\' || c0 == '/')
                return true;
            if (length > 2) {
                if (isPlatformWindows) {
                    // Check for drive letter.
                    char c1 = filename.charAt(1);
                    if (c1 == ':') {
                        if (c0 >= 'a' && c0 <= 'z')
                            return true;
                        if (c0 >= 'A' && c0 <= 'Z')
                            return true;
                    }
                } else {
                    // Unix.
                    if (filename.equals("~") || filename.startsWith("~/"))
                        return true;
                }
            }
        }
        return false;
    }

    public static ZipInputStream getZipInputStream(ZipFile zipfile,
                                                   String entryName) {
        return Utilities.getZipInputStream(zipfile, entryName, false);
    }

  public static ZipInputStream getZipInputStream(ZipFile zipfile,
                                                 String entryName,
                                                 boolean errorOnFailure) {
    ZipEntry zipEntry = zipfile.getEntry(entryName);
    ZipInputStream stream = null;
    try {
      stream = new ZipInputStream(zipfile.getInputStream(zipEntry));
    } catch (IOException e) {
      if (errorOnFailure) {
          Lisp.error(new FileError("Failed to open '" + entryName + "' in zipfile '"
                                   + zipfile + "': " + e.getMessage()));
      }
      return null;
    }
    return stream;
  }

  public static InputStream getEntryAsInputStream(ZipInputStream zipInputStream,
                                                  String entryName)
    {
        ZipEntry entry = getEntry(zipInputStream, entryName);
        ByteArrayOutputStream bytes = readEntry(zipInputStream);
        return new ByteArrayInputStream(bytes.toByteArray());

    }

    public static ByteArrayOutputStream readEntry(ZipInputStream stream) {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        int count;
        byte buf[] = new byte[1024];
        try {
            while ((count = stream.read(buf, 0, buf.length)) != -1) {
                result.write(buf, 0, count);
            }
        } catch (java.io.IOException e) {
            Debug.trace("Failed to read entry from " 
                        + stream
                        + ": " + e);
            return null;
        }
        return result;
    }

    public static ZipEntry getEntry(ZipInputStream zipInputStream, String entryName) {
        return Utilities.getEntry(zipInputStream, entryName, false);
    }

  public static ZipEntry getEntry(ZipInputStream zipInputStream,
                                  String entryName,
                                  boolean errorOnFailure)
  {
    ZipEntry entry = null;
    do {
      try {
        entry = zipInputStream.getNextEntry();
      } catch (IOException e) {
        if (errorOnFailure) {
          Lisp.error(new FileError("Failed to seek for "
            + "'" + entryName + "'"
            + " in " + zipInputStream.toString()));
        }
        return null;
      }
    } while (entry != null && !entry.getName().equals(entryName));
    if (entry != null) {
      return entry;
    }
    if (errorOnFailure) {
      Lisp.error(new FileError("Failed to find "
        + "'" + entryName + "'"
        + " in " + zipInputStream.toString()));
    }
    return null;

  }
    
    public static final boolean checkZipFile(Pathname name) {
        InputStream input = name.getInputStream();
        try {
            byte[] bytes = new byte[4];
            int bytesRead = input.read(bytes);
            return (bytesRead == 4
                    && bytes[0] == 0x50
                    && bytes[1] == 0x4b
                    && bytes[2] == 0x03
                    && bytes[3] == 0x04);
        } catch (Throwable t) { // any error probably means 'no'
            return false;
        } finally {
            if (input != null) {
                try {
                    input.close();
                }
                catch (IOException e) {} // ignore exceptions
            }
        }
    }

    static InputStream getInputStream(ZipFile jarFile, Pathname inner) {
        String entryPath = inner.asEntryPath();
        ZipEntry entry = jarFile.getEntry(entryPath);
        if (entry == null) {
            Debug.trace("Failed to find entry "
                    + "'" + entryPath + "'"
                    + " in " 
                    + "'" + jarFile.getName() + "'");
            return null;
        }
        InputStream result = null;
        try {
            result = jarFile.getInputStream(entry);
        } catch (IOException e) {
            Debug.trace("Failed to open InputStream for "
              + "'" + entryPath + "'"
              + " in "
              + "'" + jarFile.getName() + "'");
            return null;
        }
        return result;
    }

    static String escapeFormat(String s) {
        return s.replace("~", "~~");
    }
}
