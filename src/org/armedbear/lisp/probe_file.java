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
            super(Symbol.PROBE_FILE, "pathspec");
        }
        @Override
        public LispObject execute(LispObject arg)
        {
          if (arg == null || arg.equals(NIL)) {
            return NIL;
          }
          Pathname p = coerceToPathname(arg);
          if (p.isWild()) {
            return error(new FileError("Cannot find the TRUENAME for a wild pathname.",
                                       p));
          }
          // TODO: refactor Pathname{,Jar,URL}.truename() to be non-static?
          if (p instanceof JarPathname) {
            return JarPathname.truename(p, false);
          } else if (p instanceof URLPathname) {
            return URLPathname.truename((URLPathname)p, false);
          } else {
            return Pathname.truename(p, false);
          }
        }
    };

    public static final Primitive TRUENAME 
        = new pf_truename();
    @DocString(name="truename",
               args="pathspec",
               returns="pathname")
    private static class pf_truename extends Primitive {
        pf_truename() {
            super(Symbol.TRUENAME, "filespec");
        }
        @Override
        public LispObject execute(LispObject arg)
        {
          Pathname p = coerceToPathname(arg);
          if (p.isWild()) {
            return error(new FileError("Cannot find the TRUENAME for a wild pathname.",
                                       p));
          }

          // TODO: refactor Pathname{,Jar,URL}.truename() to be non-static?
          if (p instanceof JarPathname) {
            return JarPathname.truename(p, true);
          } else if (p instanceof URLPathname) {
            return URLPathname.truename((URLPathname)p, true);
          } else {
            return Pathname.truename(p, true);
          }
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
            if (pathname.isWild()) {
                error(new FileError("Cannot probe a wild pathname as a directory.", pathname));
            }
            Pathname defaultedPathname = (Pathname)Pathname.MERGE_PATHNAMES.execute(pathname);
            if (defaultedPathname instanceof JarPathname) {
              if (defaultedPathname.getName().equals(NIL)
                  && defaultedPathname.getType().equals(NIL)) {
                return Symbol.PROBE_FILE.execute(defaultedPathname);
              }
              SimpleString lastDirectory = (SimpleString)Symbol.FILE_NAMESTRING.execute(defaultedPathname);
              LispObject appendedDirectory 
                = defaultedPathname.getDirectory().reverse().push(lastDirectory).reverse();
              defaultedPathname.setDirectory(appendedDirectory);
              return Symbol.PROBE_FILE.execute(defaultedPathname);
            }
            
            File file = defaultedPathname.getFile();
            if (file == null || !file.isDirectory()) {
              return NIL;
            }

            if (defaultedPathname.getName().equals(NIL)
                && defaultedPathname.getType().equals(NIL)) {
              return Symbol.PROBE_FILE.execute(defaultedPathname);
            }
            SimpleString lastDirectory = (SimpleString)Symbol.FILE_NAMESTRING.execute(defaultedPathname);
            LispObject appendedDirectory 
              = defaultedPathname.getDirectory().reverse().push(lastDirectory).reverse();
            defaultedPathname.setDirectory(appendedDirectory);
            return Symbol.PROBE_FILE.execute(defaultedPathname);
        }
    };

    public static final Primitive FILE_DIRECTORY_P 
        = new pf_file_directory_p();
    @DocString(name="file-directory-p",
               args="pathspec &key (wild-error-p t)",
               returns="generalized-boolean")
    private static final class pf_file_directory_p extends Primitive {
        pf_file_directory_p() {
            super("file-directory-p", PACKAGE_EXT, true);
        }

        private LispObject isDirectory(Pathname p) {
          LispObject result = PROBE_DIRECTORY.execute(p);
          return result.equals(NIL) ? NIL : T;
        }

        @Override
        public LispObject execute(LispObject arg)  // XXX Should this merge with defaults?
        {
            Pathname pathname = coerceToPathname(arg);
            if (pathname.isWild()) {
                error(new FileError("Fundamentally unable to determine whether a wild pathname is a directory.",
                                    pathname));
            }
            return isDirectory(pathname);
        }

        @Override
        public LispObject execute(LispObject arg, LispObject wildErrorPKeyword, LispObject wildErrorP)
        {
            if (!(wildErrorPKeyword.equals(Keyword.WILD_ERROR_P))) {
                type_error(wildErrorPKeyword, Keyword.WILD_ERROR_P);
            }
            Pathname pathname = coerceToPathname(arg);
            if (wildErrorP != NIL) {
                if (pathname.isWild()) {
                    error(new FileError("Fundamentally to determine whether a wild pathname is a directory.",
                                        pathname));
                }
            }
            return isDirectory(pathname);
        }
    };
}
