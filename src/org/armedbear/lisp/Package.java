/*
 * Package.java
 *
 * Copyright (C) 2002-2007 Peter Graves <peter@armedbear.org>
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

public final class Package extends LispObject implements java.io.Serializable
{
    private String name;
    private transient SimpleString lispName;

    private transient LispObject propertyList;

    /** Symbols internal to the package. */
    private transient final ConcurrentHashMap<String, Symbol> internalSymbols
            = new ConcurrentHashMap<String, Symbol>(16);
    /** Symbols exported from the package.
     *
     * Those symbols in this collection are not contained in the internalSymbols
     */
    private transient final ConcurrentHashMap<String, Symbol> externalSymbols
            = new ConcurrentHashMap<String, Symbol>(16);

    private transient HashMap<String,Symbol> shadowingSymbols;
    private transient ArrayList<String> nicknames;
    private transient LispObject useList = null;
    private transient ArrayList<Package> usedByList = null;

    // Anonymous package.
    public Package()
    {
    }

    public Package(String name)
    {
        this.name = name;
        lispName = new SimpleString(name);
    }

    public Package(String name, int size)
    {
        this.name = name;
        lispName = new SimpleString(name);
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.PACKAGE;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.PACKAGE;
    }

    @Override
    public LispObject getDescription()
    {
        if (name != null) {
            StringBuilder sb = new StringBuilder("The ");
            sb.append(name);
            sb.append(" package");
            return new SimpleString(sb);
        }
        return new SimpleString("PACKAGE");
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.PACKAGE)
            return T;
        if (type == BuiltInClass.PACKAGE)
            return T;
        return super.typep(type);
    }

    public final String getName()
    {
        return name;
    }

    public final LispObject NAME()
    {
        return lispName != null ? lispName : NIL;
    }

    @Override
    public final LispObject getPropertyList()
    {
        if (propertyList == null)
            propertyList = NIL;
        return propertyList;
    }

    @Override
    public final void setPropertyList(LispObject obj)
    {
        if (obj == null)
            throw new NullPointerException();
        propertyList = obj;
    }

    public final List getNicknames()
    {
        return nicknames;
    }

    private void makeSymbolsUninterned(ConcurrentHashMap symbolMap) {
        Symbol sym;
        for (Iterator<Symbol> it = symbolMap.values().iterator();
                it.hasNext();) {
            sym = it.next();
            if (sym.getPackage() == this) {
                sym.setPackage(NIL);
            }
        }
        symbolMap.clear();
    }

    public final synchronized boolean delete()
    {
        if (name != null) {
            if(useList instanceof Cons) {
                LispObject usedPackages = useList;
                while (usedPackages != NIL) {
                    Package pkg = (Package) usedPackages.car();
                    unusePackage(pkg);
                    usedPackages = usedPackages.cdr();
                }
            }

            Packages.deletePackage(this);

            makeSymbolsUninterned(internalSymbols);
            makeSymbolsUninterned(externalSymbols); // also clears externalSymbols

            name = null;
            lispName = null;
            nicknames = null;
            
            return true;
        }
        return false;
    }

    public final synchronized void rename(String newName, LispObject newNicks)

    {
        ArrayList<String> arrayList = null;
        while (newNicks != NIL) {
            if (arrayList == null)
                arrayList = new ArrayList<String>();
            arrayList.add(javaString(newNicks.car()));
            newNicks = newNicks.cdr();
        }
        // Remove old name and nicknames from Packages map.
        Packages.deletePackage(this);
        // Now change the names...
        name = newName;
        lispName = new SimpleString(newName);
        nicknames = arrayList;
        // And add the package back.
        Packages.addPackage(this);
    }

    public Symbol findInternalSymbol(SimpleString name)
    {
        return internalSymbols.get(name.toString());
    }

    public Symbol findExternalSymbol(SimpleString name)
    {
        return externalSymbols.get(name.toString());
    }

    public Symbol findExternalSymbol(SimpleString name, int hash)
    {
        return externalSymbols.get(name.toString());
    }

    // Returns null if symbol is not accessible in this package.
    public Symbol findAccessibleSymbol(String name)

    {
        return findAccessibleSymbol(new SimpleString(name));
    }

    // Returns null if symbol is not accessible in this package.
    public Symbol findAccessibleSymbol(SimpleString name)

    {
        // Look in external and internal symbols of this package.
        Symbol symbol = externalSymbols.get(name.toString());
        if (symbol != null)
            return symbol;
        symbol = internalSymbols.get(name.toString());
        if (symbol != null)
            return symbol;
        // Look in external symbols of used packages.
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                symbol = pkg.findExternalSymbol(name);
                if (symbol != null)
                    return symbol;
                usedPackages = usedPackages.cdr();
            }
        }
        // Not found.
        return null;
    }

    public LispObject findSymbol(String name)

    {
        final SimpleString s = new SimpleString(name);
        final LispThread thread = LispThread.currentThread();
        // Look in external and internal symbols of this package.
        Symbol symbol = externalSymbols.get(name);
        if (symbol != null)
            return thread.setValues(symbol, Keyword.EXTERNAL);
        symbol = internalSymbols.get(name);
        if (symbol != null)
            return thread.setValues(symbol, Keyword.INTERNAL);
        // Look in external symbols of used packages.
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                symbol = pkg.findExternalSymbol(s);
                if (symbol != null)
                    return thread.setValues(symbol, Keyword.INHERITED);
                usedPackages = usedPackages.cdr();
            }
        }
        // Not found.
        return thread.setValues(NIL, NIL);
    }

    // Helper function to add NIL to PACKAGE_CL.
    public void addSymbol(Symbol symbol)
    {
        Debug.assertTrue(symbol.getPackage() == this);
        Debug.assertTrue(symbol.getName().equals("NIL"));
        externalSymbols.put(symbol.name.toString(), symbol);
    }

    private Symbol addSymbol(String name)
    {
        Symbol symbol = new Symbol(name, this);
        if (this == PACKAGE_KEYWORD) {
            symbol.initializeConstant(symbol);
            externalSymbols.put(name.toString(), symbol);
        } else
            internalSymbols.put(name.toString(), symbol);
        
        return symbol;
    }

    private Symbol addSymbol(SimpleString name)
    {
        return addSymbol(name.toString());
    }

    public Symbol addInternalSymbol(String symbolName)
    {
        final Symbol symbol = new Symbol(symbolName, this);
        internalSymbols.put(symbolName, symbol);
        return symbol;
    }

    public Symbol addExternalSymbol(String symbolName)
    {
        final Symbol symbol = new Symbol(symbolName, this);
        externalSymbols.put(symbolName, symbol);
        return symbol;
    }

    public synchronized Symbol intern(SimpleString symbolName)
    {
        return intern(symbolName.toString());
    }

    public synchronized Symbol intern(String symbolName)
    {
        // Look in external and internal symbols of this package.
        Symbol symbol = externalSymbols.get(symbolName);
        if (symbol != null)
            return symbol;
        symbol = internalSymbols.get(symbolName);
        if (symbol != null)
            return symbol;
        // Look in external symbols of used packages.
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                symbol = pkg.externalSymbols.get(symbolName);
                if (symbol != null)
                    return symbol;
                usedPackages = usedPackages.cdr();
            }
        }
        // Not found.
        return addSymbol(symbolName);
    }

    public synchronized Symbol intern(final SimpleString s,
                                      final LispThread thread)
    {
        // Look in external and internal symbols of this package.
        Symbol symbol = externalSymbols.get(s.toString());
        if (symbol != null)
            return (Symbol) thread.setValues(symbol, Keyword.EXTERNAL);
        symbol = internalSymbols.get(s.toString());
        if (symbol != null)
            return (Symbol) thread.setValues(symbol, Keyword.INTERNAL);
        // Look in external symbols of used packages.
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                symbol = pkg.findExternalSymbol(s);
                if (symbol != null)
                    return (Symbol) thread.setValues(symbol, Keyword.INHERITED);
                usedPackages = usedPackages.cdr();
            }
        }
        // Not found.
        return (Symbol) thread.setValues(addSymbol(s), NIL);
    }

    public synchronized Symbol internAndExport(String symbolName)

    {
        final SimpleString s = new SimpleString(symbolName);
        // Look in external and internal symbols of this package.
        Symbol symbol = externalSymbols.get(s.toString());
        if (symbol != null)
            return symbol;
        symbol = internalSymbols.get(s.toString());
        if (symbol != null) {
            export(symbol);
            return symbol;
        }
        if (useList instanceof Cons) {
            // Look in external symbols of used packages.
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                symbol = pkg.findExternalSymbol(s);
                if (symbol != null) {
                    export(symbol);
                    return symbol;
                }
                usedPackages = usedPackages.cdr();
            }
        }
        // Not found.
        symbol = new Symbol(s, this);
        if (this == PACKAGE_KEYWORD)
            symbol.initializeConstant(symbol);
        externalSymbols.put(s.toString(), symbol);
        return symbol;
    }

    public synchronized LispObject unintern(final Symbol symbol)

    {
        final String symbolName = symbol.getName();
        final boolean shadow;
        if (shadowingSymbols != null && shadowingSymbols.get(symbolName) == symbol)
            shadow = true;
        else
            shadow = false;
        if (shadow) {
            // Check for conflicts that might be exposed in used package list
            // if we remove the shadowing symbol.
            Symbol sym = null;
            if (useList instanceof Cons) {
                LispObject usedPackages = useList;
                while (usedPackages != NIL) {
                    Package pkg = (Package) usedPackages.car();
                    Symbol s = pkg.findExternalSymbol(symbol.name);
                    if (s != null) {
                        if (sym == null)
                            sym = s;
                        else if (sym != s) {
                            StringBuilder sb =
                                new StringBuilder("Uninterning the symbol ");
                            sb.append(symbol.getQualifiedName());
                            sb.append(" causes a name conflict between ");
                            sb.append(sym.getQualifiedName());
                            sb.append(" and ");
                            sb.append(s.getQualifiedName());
                            return error(new PackageError(sb.toString()));
                        }
                    }
                    usedPackages = usedPackages.cdr();
                }
            }
        }
        // Reaching here, it's OK to remove the symbol.
        if (internalSymbols.get(symbol.name.toString()) == symbol)
            internalSymbols.remove(symbol.name.toString());
        else if (externalSymbols.get(symbol.name.toString()) == symbol)
            externalSymbols.remove(symbol.name.toString());
        else
            // Not found.
            return NIL;
        if (shadow) {
            Debug.assertTrue(shadowingSymbols != null);
            shadowingSymbols.remove(symbolName);
        }
        if (symbol.getPackage() == this)
            symbol.setPackage(NIL);
        return T;
    }

    public synchronized void importSymbol(Symbol symbol)
    {
        if (symbol.getPackage() == this)
            return; // Nothing to do.
        Symbol sym = findAccessibleSymbol(symbol.name);
        if (sym != null && sym != symbol) {
            StringBuilder sb = new StringBuilder("The symbol ");
            sb.append(sym.getQualifiedName());
            sb.append(" is already accessible in package ");
            sb.append(name);
            sb.append('.');
            error(new PackageError(sb.toString()));
        }
        internalSymbols.put(symbol.name.toString(), symbol);
        if (symbol.getPackage() == NIL)
            symbol.setPackage(this);
    }

    public synchronized void export(final Symbol symbol)
    {
        final String symbolName = symbol.getName();
        boolean added = false;
        if (symbol.getPackage() != this) {
            Symbol sym = findAccessibleSymbol(symbol.name);
            if (sym != symbol) {
                StringBuilder sb = new StringBuilder("The symbol ");
                sb.append(symbol.getQualifiedName());
                sb.append(" is not accessible in package ");
                sb.append(name);
                sb.append('.');
                error(new PackageError(sb.toString()));
                return;
            }
            internalSymbols.put(symbol.name.toString(), symbol);
            added = true;
        }
        if (added || internalSymbols.get(symbol.name.toString()) == symbol) {
            if (usedByList != null) {
                for (Iterator it = usedByList.iterator(); it.hasNext();) {
                    Package pkg = (Package) it.next();
                    Symbol sym = pkg.findAccessibleSymbol(symbol.name);
                    if (sym != null && sym != symbol) {
                        if (pkg.shadowingSymbols != null &&
                            pkg.shadowingSymbols.get(symbolName) == sym) {
                            // OK.
                        } else {
                            StringBuilder sb = new StringBuilder("The symbol ");
                            sb.append(sym.getQualifiedName());
                            sb.append(" is already accessible in package ");
                            sb.append(pkg.getName());
                            sb.append('.');
                            error(new PackageError(sb.toString()));
                            return;
                        }
                    }
                }
            }
            // No conflicts.
            internalSymbols.remove(symbol.name.toString());
            externalSymbols.put(symbol.name.toString(), symbol);
            return;
        }
        if (externalSymbols.get(symbol.name.toString()) == symbol)
            // Symbol is already exported; there's nothing to do.
            return;
        StringBuilder sb = new StringBuilder("The symbol ");
        sb.append(symbol.getQualifiedName());
        sb.append(" is not accessible in package ");
        sb.append(name);
        sb.append('.');
        error(new PackageError(sb.toString()));
    }

    public synchronized void unexport(final Symbol symbol)

    {
        if (symbol.getPackage() == this) {
            if (externalSymbols.get(symbol.name.toString()) == symbol) {
                externalSymbols.remove(symbol.name.toString());
                internalSymbols.put(symbol.name.toString(), symbol);
            }
        } else {
            // Signal an error if symbol is not accessible.
            if (useList instanceof Cons) {
                LispObject usedPackages = useList;
                while (usedPackages != NIL) {
                    Package pkg = (Package) usedPackages.car();
                    if (pkg.findExternalSymbol(symbol.name) == symbol)
                        return; // OK.
                    usedPackages = usedPackages.cdr();
                }
            }
            StringBuilder sb = new StringBuilder("The symbol ");
            sb.append(symbol.getQualifiedName());
            sb.append(" is not accessible in package ");
            sb.append(name);
            error(new PackageError(sb.toString()));
        }
    }

    public synchronized void shadow(final String symbolName)

    {
        if (shadowingSymbols == null)
            shadowingSymbols = new HashMap<String,Symbol>();
        final SimpleString s = new SimpleString(symbolName);
        Symbol symbol = externalSymbols.get(s.toString());
        if (symbol != null) {
            shadowingSymbols.put(symbolName, symbol);
            return;
        }
        symbol = internalSymbols.get(s.toString());
        if (symbol != null) {
            shadowingSymbols.put(symbolName, symbol);
            return;
        }
        if (shadowingSymbols.get(symbolName) != null)
            return;
        symbol = new Symbol(s, this);
        internalSymbols.put(s.toString(), symbol);
        shadowingSymbols.put(symbolName, symbol);
    }

    public synchronized void shadowingImport(Symbol symbol)
    {
        LispObject where = NIL;
        final String symbolName = symbol.getName();
        Symbol sym = externalSymbols.get(symbol.name.toString());
        if (sym != null) {
            where = Keyword.EXTERNAL;
        } else {
            sym = internalSymbols.get(symbol.name.toString());
            if (sym != null) {
                where = Keyword.INTERNAL;
            } else {
                // Look in external symbols of used packages.
                if (useList instanceof Cons) {
                    LispObject usedPackages = useList;
                    while (usedPackages != NIL) {
                        Package pkg = (Package) usedPackages.car();
                        sym = pkg.findExternalSymbol(symbol.name);
                        if (sym != null) {
                            where = Keyword.INHERITED;
                            break;
                        }
                        usedPackages = usedPackages.cdr();
                    }
                }
            }
        }
        if (sym != null) {
            if (where == Keyword.INTERNAL || where == Keyword.EXTERNAL) {
                if (sym != symbol) {
                    if (shadowingSymbols != null)
                        shadowingSymbols.remove(symbolName);
                    unintern(sym);
                } else if (where == Keyword.INTERNAL) {
                    // Assert rgument is already correctly a shadowing import
                    Debug.assertTrue(shadowingSymbols != null);
                    Debug.assertTrue(shadowingSymbols.get(symbolName) != null);
                    return;
                }
            }
        }
        internalSymbols.put(symbol.name.toString(), symbol);
        if (shadowingSymbols == null)
            shadowingSymbols = new HashMap<String,Symbol>();
        Debug.assertTrue(shadowingSymbols.get(symbolName) == null);
        shadowingSymbols.put(symbolName, symbol);
    }

    // "USE-PACKAGE causes PACKAGE to inherit all the external symbols of
    // PACKAGES-TO-USE. The inherited symbols become accessible as internal
    // symbols of PACKAGE."
    public void usePackage(Package pkg)
    {
        if (useList == null)
            useList = NIL;
        if (!memq(pkg, useList)) {
            // "USE-PACKAGE checks for name conflicts between the newly
            // imported symbols and those already accessible in package."
            Collection symbols = pkg.getExternalSymbols();
            for (Iterator<Symbol> i = symbols.iterator(); i.hasNext();) {
                Symbol symbol = i.next();
                Symbol existing = findAccessibleSymbol(symbol.name);
                if (existing != null && existing != symbol) {
                    if (shadowingSymbols == null ||
                        shadowingSymbols.get(symbol.getName()) == null)
                    {
                        error(new PackageError("A symbol named " + symbol.getName() +
                                                " is already accessible in package " +
                                                name + "."));
                        return;
                    }
                }
            }
            useList = useList.push(pkg);
            // Add this package to the used-by list of pkg.
            if (pkg.usedByList != null)
                Debug.assertTrue(!pkg.usedByList.contains(this));
            if (pkg.usedByList == null)
                pkg.usedByList = new ArrayList<Package>();
            pkg.usedByList.add(this);
        }
    }

    public void unusePackage(Package pkg)
    {
        if (useList instanceof Cons) {
            if (memq(pkg, useList)) {
                // FIXME Modify the original list instead of copying it!
                LispObject newList = NIL;
                while (useList != NIL) {
                    if (useList.car() != pkg)
                        newList = newList.push(useList.car());
                    useList = useList.cdr();
                }
                useList = newList.nreverse();
                Debug.assertTrue(!memq(pkg, useList));
                Debug.assertTrue(pkg.usedByList != null);
                Debug.assertTrue(pkg.usedByList.contains(this));
                pkg.usedByList.remove(this);
            }
        }
    }

    public final void addNickname(String s)
    {
        // This call will signal an error if there's a naming conflict.
        Packages.addNickname(this, s);

        if (nicknames != null) {
            if (nicknames.contains(s))
                return; // Nothing to do.
        } else
            nicknames = new ArrayList<String>();

        nicknames.add(s);
    }

    public String getNickname()
    {
        if (nicknames != null && nicknames.size() > 0)
            return (String) nicknames.get(0);
        return null;
    }

    public LispObject packageNicknames()
    {
        LispObject list = NIL;
        if (nicknames != null) {
            for (int i = nicknames.size(); i-- > 0;) {
                String nickname = (String) nicknames.get(i);
                list = new Cons(new SimpleString(nickname), list);
            }
        }
        return list;
    }

    public LispObject getUseList()
    {
        if (useList == null)
            useList = NIL;
        return useList;
    }

    public boolean uses(LispObject pkg)
    {
        return (useList instanceof Cons) && memq(pkg, useList);
    }

    public LispObject getUsedByList()
    {
        LispObject list = NIL;
        if (usedByList != null) {
            for (Iterator it = usedByList.iterator(); it.hasNext();) {
                Package pkg = (Package) it.next();
                list = new Cons(pkg, list);
            }
        }
        return list;
    }

    public LispObject getShadowingSymbols()
    {
        LispObject list = NIL;
        if (shadowingSymbols != null) {
            for (Iterator it = shadowingSymbols.values().iterator(); it.hasNext();) {
                Symbol symbol = (Symbol) it.next();
                list = new Cons(symbol, list);
            }
        }
        return list;
    }

    public synchronized Collection getExternalSymbols()
    {
        return externalSymbols.values();
    }

    public synchronized List<Symbol> getAccessibleSymbols()
    {
        ArrayList<Symbol> list = new ArrayList<Symbol>();
        list.addAll(internalSymbols.values());
        list.addAll(externalSymbols.values());
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                list.addAll(pkg.externalSymbols.values());

                usedPackages = usedPackages.cdr();
            }
        }
        return list;
    }

    public synchronized LispObject PACKAGE_INTERNAL_SYMBOLS()
    {
        LispObject list = NIL;
        Collection symbols = internalSymbols.values();
        for (Iterator<Symbol> i = symbols.iterator(); i.hasNext();)
            list = new Cons(i.next(), list);
        return list;
    }

    public synchronized LispObject PACKAGE_EXTERNAL_SYMBOLS()
    {
        LispObject list = NIL;
        Collection symbols = externalSymbols.values();
        for (Iterator<Symbol> i = symbols.iterator(); i.hasNext();)
            list = new Cons(i.next(), list);
        return list;
    }

    public synchronized LispObject PACKAGE_INHERITED_SYMBOLS()
    {
        LispObject list = NIL;
        if (useList instanceof Cons) {
            LispObject usedPackages = useList;
            while (usedPackages != NIL) {
                Package pkg = (Package) usedPackages.car();
                Collection externals = pkg.getExternalSymbols();
                for (Iterator<Symbol> i = externals.iterator(); i.hasNext();) {
                    Symbol symbol = i.next();
                    if (shadowingSymbols != null && shadowingSymbols.get(symbol.getName()) != null)
                        continue;
                    if (externalSymbols.get(symbol.name.toString()) == symbol)
                        continue;
                    list = new Cons(symbol, list);
                }
                usedPackages = usedPackages.cdr();
            }
        }
        return list;
    }

    public synchronized LispObject getSymbols()
    {
        LispObject list = NIL;
        Collection internals = internalSymbols.values();
        for (Iterator<Symbol> i = internals.iterator(); i.hasNext();)
            list = new Cons(i.next(), list);
        Collection externals = externalSymbols.values();
        for (Iterator<Symbol> i = externals.iterator(); i.hasNext();)
            list = new Cons(i.next(), list);
        return list;
    }

    public synchronized Symbol[] symbols()
    {
        Collection internals = internalSymbols.values();
        Collection externals = externalSymbols.values();
        Symbol[] array = new Symbol[internals.size() + externals.size()];
        int i = 0;
        for (Iterator it = internals.iterator(); it.hasNext();) {
            Symbol symbol = (Symbol) it.next();
            array[i++] = symbol;
        }
        for (Iterator it = externals.iterator(); it.hasNext();) {
            Symbol symbol = (Symbol) it.next();
            array[i++] = symbol;
        }
        return array;
    }

    @Override
    public String writeToString()
    {
        if (_PRINT_FASL_.symbolValue() != NIL && name != null) {
            StringBuilder sb = new StringBuilder("#.(FIND-PACKAGE \"");
            sb.append(name);
            sb.append("\")");
            return sb.toString();
        } else
            return toString();
    }
    
    @Override
    public String toString() {
         if (name != null) {
            StringBuilder sb = new StringBuilder("#<PACKAGE \"");
            sb.append(name);
            sb.append("\">");
            return sb.toString();
        } else
            return unreadableString("PACKAGE");
    }

    public Object readResolve() throws java.io.ObjectStreamException {
	Package pkg = Packages.findPackage(name);
	if(pkg != null) {
	    return pkg;
	} else {
	    return error(new PackageError(name + " is not the name of a package."));
	}
    }
}
