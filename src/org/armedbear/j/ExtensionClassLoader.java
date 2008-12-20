/*
 * ExtensionClassLoader.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
 */

package org.armedbear.j;

import java.io.DataInputStream;

public final class ExtensionClassLoader extends ClassLoader
{
    public Class loadClass(String s, boolean resolve) throws ClassNotFoundException
    {
        try {
            File file = null;
            String classname = null;
            Class c = null;
            if (s.endsWith(".class")) {
                // String passed in is a file name, not a class name.
                // By default, extension classes are in ~/.j
                file = File.getInstance(Directories.getEditorDirectory(), s);
            } else {
                // Must be class name.
                classname = s;
            }
            if (classname != null) {
                c = findLoadedClass(classname);
                if (c == null) {
                    try {
                        c = findSystemClass(classname);
                    }
                    catch (Exception e) {}
                }
                if (c == null) {
                    // We did not find it.  Look for a .class file in ~/.j
                    Debug.assertTrue(file == null);
                    file = File.getInstance(Directories.getEditorDirectory(),
                                            classname.concat(".class"));
                }
            }
            if (c == null) {
                Log.debug("looking for extension class in file " + file.getCanonicalPath());
                if (file.isFile()) {
                    long length = file.length();
                    if (length < Integer.MAX_VALUE) {
                        byte[] classbytes = new byte[(int)length];
                        DataInputStream in = new DataInputStream(file.getInputStream());
                        in.readFully(classbytes);
                        in.close();
                        c = defineClass(classname, classbytes, 0, (int) length);
                    }
                } else
                    Log.debug("not found " + file.getCanonicalPath());
            }
            if (c != null && resolve)
                resolveClass(c);
            return c;
        }
        catch (Exception e) {
            throw new ClassNotFoundException(e.toString());
        }
    }
}
