/*
 * probe_file.java
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

public final class probe_file
{
    // ### probe-file
    // probe-file pathspec => truename
    private static final Primitive PROBE_FILE =
        new Primitive("probe-file", "pathspec")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return Pathname.truename(arg, false);
        }
    };

    // ### truename
    // truename filespec => truename
    private static final Primitive TRUENAME =
        new Primitive("truename", "filespec")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return Pathname.truename(arg, true);
        }
    };

    // ### probe-directory
    // probe-directory pathspec => truename
    private static final Primitive PROBE_DIRECTORY =
        new Primitive("probe-directory", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            Pathname pathname = coerceToPathname(arg);
            if (pathname.isWild())
                error(new FileError("Bad place for a wild pathname.", pathname));
            File file = Utilities.getFile(pathname);
            return file.isDirectory() ? Utilities.getDirectoryPathname(file) : NIL;
        }
    };

    // ### file-directory-p
    // file-directory-p pathspec => generalized-boolean
    private static final Primitive FILE_DIRECTORY_P =
        new Primitive("file-directory-p", PACKAGE_EXT, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            Pathname pathname = coerceToPathname(arg);
            if (pathname.isWild())
                error(new FileError("Bad place for a wild pathname.", pathname));
            File file = Utilities.getFile(pathname);
            return file.isDirectory() ? T : NIL;
        }
    };
}
