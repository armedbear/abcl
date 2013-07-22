/*
 * Packages.java
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public final class Packages
{
  private static final ArrayList<Package> packages = new ArrayList<Package>();
  private static final HashMap<String,Package> map = new HashMap<String,Package>();

  public static final synchronized Package createPackage(String name)
  {
    return createPackage(name, 0);
  }

  public static final synchronized Package createPackage(String name, int size)
  {
    Package pkg = (Package) map.get(name);
    if (pkg == null)
      {
        pkg = size != 0 ? new Package(name, size) : new Package(name);
        packages.add(pkg);
        map.put(name, pkg);
      }
    else
      Debug.trace("package " + name + " already exists");
    return pkg;
  }

  public static final synchronized void addPackage(Package pkg)

  {
    final String name = pkg.getName();
    if (map.get(name) != null)
      {
        error(new LispError("A package named " + name + " already exists."));
        return;
      }
    packages.add(pkg);
    map.put(name, pkg);
    List nicknames = pkg.getNicknames();
    if (nicknames != null)
      {
        for (Iterator it = nicknames.iterator(); it.hasNext();)
          {
            String nickname = (String) it.next();
            addNickname(pkg, nickname);
          }
      }
  }

  /** 
      Returns the current package of the current LispThread.
      
      Intended to be used from Java code manipulating an Interpreter
      instance.
  */
  public static final synchronized Package findPackage(String name) {
    return getCurrentPackage().findPackage(name);
  }
    
  // Finds package named `name'.  Returns null if package doesn't exist.
  // Called by Package.findPackage after checking package-local package
  // nicknames.
  static final synchronized Package findPackageGlobally(String name)
  {
    return (Package) map.get(name);
  }

  public static final synchronized Package makePackage(String name)

  {
    if (map.get(name) != null)
      {
        error(new LispError("A package named " + name + " already exists."));
        // Not reached.
        return null;
      }
    Package pkg = new Package(name);
    packages.add(pkg);
    map.put(name, pkg);
    return pkg;
  }

  public static final synchronized void addNickname(Package pkg, String nickname)

  {
    Object obj = map.get(nickname);
    if (obj != null && obj != pkg)
      {
        error(new PackageError("A package named " + nickname + " already exists."));
        return;
      }
    map.put(nickname, pkg);
  }

  // Removes name and nicknames from map, removes pkg from packages.
  public static final synchronized boolean deletePackage(Package pkg)
  {
    String name = pkg.getName();
    if (name != null)
      {
        map.remove(name);
        List nicknames = pkg.getNicknames();
        if (nicknames != null)
          {
            for (Iterator it = nicknames.iterator(); it.hasNext();)
              {
                String nickname = (String) it.next();
                map.remove(nickname);
              }
          }
        packages.remove(pkg);
        return true;
      }
    return false;
  }

  public static final synchronized LispObject listAllPackages()
  {
    LispObject result = NIL;
    for (Package pkg : packages) {
      result = new Cons(pkg, result);
    }
    return result;
  }

  public static final synchronized Package[] getAllPackages()
  {
    Package[] array = new Package[packages.size()];
    packages.toArray(array);
    return array;
  }

  public static final synchronized LispObject getPackagesNicknamingPackage(Package thePackage)
  {
    LispObject result = NIL;
    for (Package pkg : packages) {
      for (Package nicknamedPackage : pkg.getLocallyNicknamedPackages()) {
        if (thePackage.equals(nicknamedPackage)) {
          result = new Cons(pkg, result);
        }
      }
    }
    return result;
  }
}
