/*
 * unzip.java
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

@DocString(name="unzip",
           args="pathname &optional directory => unzipped_pathnames",
           doc="Unpack zip archive at PATHNAME returning a list of extracted pathnames.\nIf the optional DIRECTORY is specified, root the abstraction in that directory, otherwise use the current value of *DEFAULT-PATHNAME-DEFAULTS.")
public final class unzip 
  extends Primitive
{
    public unzip() {
        super("unzip", PACKAGE_SYS, true, 
              "pathname &optional directory => unzipped_pathnames");
    }
  
    @Override
    public LispObject execute(LispObject first) {
        Pathname zipFile = coerceToPathname(first);
        Pathname directory = coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue());
        return unzipToDirectory(zipFile, directory);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second) {
        Pathname zipFile = coerceToPathname(first);
        Pathname directory = coerceToPathname(second);
        directory.name = NIL;
        directory.type = NIL;
        directory.invalidateNamestring();
        return unzipToDirectory(zipFile, directory);
    }
  
    private LispObject unzipToDirectory(Pathname zipPath, Pathname dirPath) {
        if (!zipPath.isAbsolute()) {
            zipPath = Pathname.mergePathnames(zipPath,
                                              coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()));
        }
        LispObject o = Pathname.truename(zipPath, false);
        if (!(o instanceof Pathname)) {
            return error(new FileError("No file found: " + zipPath, zipPath));
        }
        String zip = ((Pathname)o).getNamestring();
        if (zip == null) {
            return error(new FileError("Pathname has no namestring: " + zip, zipPath));
        }
        String dir = dirPath.getNamestring();
        if (dir == null) {
            return error(new FileError("Could not parse diretory: " + dirPath, dirPath));
        }
        LispObject result = NIL;
        try {
            ZipFile zipfile = new ZipFile(zip);
            
            byte[] buffer = new byte[4096];
            for (Enumeration<? extends ZipEntry> entries =  zipfile.entries();entries.hasMoreElements();) {
                ZipEntry entry = entries.nextElement();
                String name = entry.getName();
                String filename = dir + name;
                File file = new File(filename);
                if (entry.isDirectory()) {
                    file.mkdirs();
                    continue;
                }
                FileOutputStream out = new FileOutputStream(file);
                InputStream in = zipfile.getInputStream(entry);
                int n;
                while ((n = in.read(buffer)) > 0) {
                    out.write(buffer, 0, n);
                }
                out.close();
                in.close();
                result = result.push(new Pathname(filename));
            }
        } catch (IOException e) {
            return error(new FileError("Failed to unzip " 
                                       + "'" + zipPath + "'"
                                       + " into " + "'" + dirPath + "'"
                                       + ": " + e, zipPath)); 
        }
        return result;
    }

    private static final Primitive unzip = new unzip();
}
