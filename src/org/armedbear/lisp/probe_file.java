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
    public static final Primitive PROBE_FILE 
      = new pf_probe_file();
    @DocString(name="probe-file",
               args="pathspec",
               returns="truename")
    private static final class pf_probe_file extends Primitive {
        pf_probe_file() {
            super("probe-file", "pathspec");
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            return Pathname.truename(arg, false);
        }
    };

    public static final Primitive TRUENAME 
        = new pf_truename();
    @DocString(name="truename",
               args="pathspec",
               returns="pathname")
    private static class pf_truename extends Primitive {
        pf_truename() {
            super("truename", "filespec");
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            return Pathname.truename(arg, true);
        }
    };

    public static final Primitive PROBE_DIRECTORY 
        = new pf_probe_directory();
    @DocString(name="probe-directory",
               args="pathspec",
               returns="truename")
    private static final class pf_probe_directory extends Primitive {
        pf_probe_directory() {
            super("probe-directory", PACKAGE_EXT, true);
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            Pathname pathname = coerceToPathname(arg);
            if (pathname.isWild())
                error(new FileError("Bad place for a wild pathname.", pathname));
            Pathname defaultedPathname = (Pathname)Pathname.MERGE_PATHNAMES.execute(pathname);
            File file = defaultedPathname.getFile();
            return file.isDirectory() ? Pathname.getDirectoryPathname(file) : NIL;
        }
    };

    public static final Primitive FILE_DIRECTORY_P 
        = new pf_file_directory_p();
    @DocString(name="file-directory-p",
               args="pathspec",
               returns="generalized-boolean")
    private static final class pf_file_directory_p extends Primitive {
        pf_file_directory_p() {
            super("file-directory-p", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject arg)  // XXX Should this merge with defaults?
        {
            Pathname pathname = coerceToPathname(arg);
            if (pathname.isWild())
                error(new FileError("Bad place for a wild pathname.", pathname));
            File file = pathname.getFile();
            return file.isDirectory() ? T : NIL;
        }
    };
}
