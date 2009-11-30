/*
 * PackageFunctions.java
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

public final class PackageFunctions
{
    // ### packagep
    // packagep object => generalized-boolean
    private static final Primitive PACKAGEP = new Primitive("packagep", "object")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return arg instanceof Package ? T : NIL;
        }
    };

    // ### package-name
    // package-name package => nicknames
    private static final Primitive PACKAGE_NAME =
        new Primitive("package-name", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).NAME();
        }
    };

    // ### package-nicknames
    // package-nicknames package => nicknames
    private static final Primitive PACKAGE_NICKNAMES =
        new Primitive("package-nicknames", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).packageNicknames();
        }
    };

    // ### package-use-list
    // package-use-list package => use-list
    private static final Primitive PACKAGE_USE_LIST =
        new Primitive("package-use-list", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).getUseList();
        }
    };

    // ### package-used-by-list
    // package-used-by-list package => used-by-list
    private static final Primitive PACKAGE_USED_BY_LIST =
        new Primitive("package-used-by-list", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).getUsedByList();
        }
    };

    // ### %import
    // %import symbols &optional package => t
    private static final Primitive _IMPORT =
        new Primitive("%import", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length == 0 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject symbols = args[0];
            Package pkg =
                args.length == 2 ? coerceToPackage(args[1]) : getCurrentPackage();
            if (symbols.listp()) {
                while (symbols != NIL) {
                    pkg.importSymbol(checkSymbol(symbols.car()));
                    symbols = symbols.cdr();
                }
            } else
                pkg.importSymbol(checkSymbol(symbols));
            return T;
        }
    };

    // ### unexport
    // unexport symbols &optional package => t
    private static final Primitive UNEXPORT =
        new Primitive("unexport", "symbols &optional package")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length == 0 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject symbols = args[0];
            Package pkg =
                args.length == 2 ? coerceToPackage(args[1]) : getCurrentPackage();
            if (symbols.listp()) {
                while (symbols != NIL) {
                    pkg.unexport(checkSymbol(symbols.car()));
                    symbols = symbols.cdr();
                }
            } else
                pkg.unexport(checkSymbol(symbols));
            return T;
        }
    };

    // ### shadow
    // shadow symbol-names &optional package => t
    private static final Primitive SHADOW =
        new Primitive("shadow", "symbol-names &optional package")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length == 0 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject symbols = args[0];
            Package pkg =
                args.length == 2 ? coerceToPackage(args[1]) : getCurrentPackage();
            if (symbols.listp()) {
                while (symbols != NIL) {
                    pkg.shadow(javaString(symbols.car()));
                    symbols = symbols.cdr();
                }
            } else
                pkg.shadow(javaString(symbols));
            return T;
        }
    };

    // ### shadowing-import
    // shadowing-import symbols &optional package => t
    private static final Primitive SHADOWING_IMPORT =
        new Primitive("shadowing-import", "symbols &optional package")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length == 0 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject symbols = args[0];
            Package pkg =
                args.length == 2 ? coerceToPackage(args[1]) : getCurrentPackage();
            if (symbols.listp()) {
                while (symbols != NIL) {
                    pkg.shadowingImport(checkSymbol(symbols.car()));
                    symbols = symbols.cdr();
                }
            } else
                pkg.shadowingImport(checkSymbol(symbols));
            return T;
        }
    };

    // ### package-shadowing-symbols
    // package-shadowing-symbols package => used-by-list
    private static final Primitive PACKAGE_SHADOWING_SYMBOLS =
        new Primitive("package-shadowing-symbols", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).getShadowingSymbols();
        }
    };

    // ### delete-package
    private static final Primitive DELETE_PACKAGE =
        new Primitive("delete-package", "package")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return coerceToPackage(arg).delete() ? T : NIL;
        }
    };

    // ### unuse-package
    // unuse-package packages-to-unuse &optional package => t
    private static final Primitive USE_PACKAGE =
        new Primitive("unuse-package", "packages-to-unuse &optional package")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 1 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            Package pkg;
            if (args.length == 2)
                pkg = coerceToPackage(args[1]);
            else
                pkg = getCurrentPackage();
            if (args[0] instanceof Cons) {
                LispObject list = args[0];
                while (list != NIL) {
                    pkg.unusePackage(coerceToPackage(list.car()));
                    list = list.cdr();
                }
            } else
                pkg.unusePackage(coerceToPackage(args[0]));
            return T;
        }
    };

    // ### rename-package
    // rename-package package new-name &optional new-nicknames => package-object
    private static final Primitive RENAME_PACKAGE =
        new Primitive("rename-package", "package new-name &optional new-nicknames")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 2 || args.length > 3)
                return error(new WrongNumberOfArgumentsException(this));
            Package pkg = coerceToPackage(args[0]);
            String newName = javaString(args[1]);
            LispObject nicknames = args.length == 3 ? checkList(args[2]) : NIL;
            pkg.rename(newName, nicknames);
            return pkg;
        }
    };

    private static final Primitive LIST_ALL_PACKAGES =
        new Primitive("list-all-packages", "")
    {
        @Override
        public LispObject execute()
        {
            return Packages.listAllPackages();
        }
    };

    // ### %defpackage name nicknames size shadows shadowing-imports use
    // imports interns exports doc-string => package
    private static final Primitive _DEFPACKAGE =
        new Primitive("%defpackage", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length != 10)
                return error(new WrongNumberOfArgumentsException(this));
            final String packageName = args[0].getStringValue();
            LispObject nicknames = checkList(args[1]);
            // FIXME size is ignored
            // LispObject size = args[2];
            LispObject shadows = checkList(args[3]);
            LispObject shadowingImports = checkList(args[4]);
            LispObject use = checkList(args[5]);
            LispObject imports = checkList(args[6]);
            LispObject interns = checkList(args[7]);
            LispObject exports = checkList(args[8]);
            // FIXME docString is ignored
            // LispObject docString = args[9];
            Package pkg = Packages.findPackage(packageName);
            if (pkg != null)
                return pkg;
            if (nicknames != NIL) {
                LispObject list = nicknames;
                while (list != NIL) {
                    String nick = javaString(list.car());
                    if (Packages.findPackage(nick) != null) {
                        return error(new PackageError("A package named " + nick +
                                                       " already exists."));
                    }
                    list = list.cdr();
                }
            }
            pkg = Packages.createPackage(packageName);
            while (nicknames != NIL) {
                LispObject string = nicknames.car().STRING();
                pkg.addNickname(string.getStringValue());
                nicknames = nicknames.cdr();
            }
            while (shadows != NIL) {
                String symbolName = shadows.car().getStringValue();
                pkg.shadow(symbolName);
                shadows = shadows.cdr();
            }
            while (shadowingImports != NIL) {
                LispObject si = shadowingImports.car();
                Package otherPkg = coerceToPackage(si.car());
                LispObject symbolNames = si.cdr();
                while (symbolNames != NIL) {
                    String symbolName = symbolNames.car().getStringValue();
                    Symbol sym = otherPkg.findAccessibleSymbol(symbolName);
                    if (sym != null)
                        pkg.shadowingImport(sym);
                    else
                        return error(new LispError(symbolName +
                                                    " not found in package " +
                                                    otherPkg.getName() + "."));
                    symbolNames = symbolNames.cdr();
                }
                shadowingImports = shadowingImports.cdr();
            }
            while (use != NIL) {
                LispObject obj = use.car();
                if (obj instanceof Package)
                    pkg.usePackage((Package)obj);
                else {
                    LispObject string = obj.STRING();
                    Package p = Packages.findPackage(string.getStringValue());
                    if (p == null)
                        return error(new LispError(obj.writeToString() +
                                                    " is not the name of a package."));
                    pkg.usePackage(p);
                }
                use = use.cdr();
            }
            while (imports != NIL) {
                LispObject si = imports.car();
                Package otherPkg = coerceToPackage(si.car());
                LispObject symbolNames = si.cdr();
                while (symbolNames != NIL) {
                    String symbolName = symbolNames.car().getStringValue();
                    Symbol sym = otherPkg.findAccessibleSymbol(symbolName);
                    if (sym != null)
                        pkg.importSymbol(sym);
                    else
                        return error(new LispError(symbolName +
                                                    " not found in package " +
                                                    otherPkg.getName() + "."));
                    symbolNames = symbolNames.cdr();
                }
                imports = imports.cdr();
            }
            while (interns != NIL) {
                String symbolName = interns.car().getStringValue();
                pkg.intern(symbolName);
                interns = interns.cdr();
            }
            while (exports != NIL) {
                String symbolName = exports.car().getStringValue();
                pkg.export(pkg.intern(symbolName));
                exports = exports.cdr();
            }
            return pkg;
        }
    };
}
