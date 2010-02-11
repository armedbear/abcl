/*
 * delete_file.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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
import java.io.IOException;

public final class delete_file extends Primitive
{
    private delete_file()
    {
        super("delete-file", "filespec");
    }

    // ### delete-file filespec => t
    @Override
    public LispObject execute(LispObject arg)
    {
        // Don't follow symlinks! We want to delete the symlink itself, not
        // the linked-to file.
        Pathname pathname = coerceToPathname(arg);
        if (arg instanceof Stream)
            ((Stream)arg)._close();
        if (pathname instanceof LogicalPathname)
            pathname = LogicalPathname.translateLogicalPathname((LogicalPathname)pathname);
        if (pathname.isWild())
            return error(new FileError("Bad place for a wild pathname.",
                                        pathname));
        final Pathname defaultedPathname =
            Pathname.mergePathnames(pathname,
                                    coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
                                    NIL);
        final String namestring = defaultedPathname.getNamestring();
        if (namestring == null)
            return error(new FileError("Pathname has no namestring: " + defaultedPathname.writeToString(),
                                        defaultedPathname));
        final File file = new File(namestring);
	ZipCache.remove(file);
        if (file.exists()) {
            // File exists.
            for (int i = 0; i < 5; i++) {
                if (file.delete())
                    return T;
                System.gc();
                Thread.yield();
            }
            Pathname truename = new Pathname(file.getAbsolutePath());
            StringBuilder sb = new StringBuilder("Unable to delete ");
            sb.append(file.isDirectory() ? "directory " : "file ");
            sb.append(truename.writeToString());
            sb.append('.');
            return error(new FileError(sb.toString(), truename));
        } else {
            // File does not exist.
            return T;
        }
    }

    private static final Primitive DELETE_FILE = new delete_file();
}
