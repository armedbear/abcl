/*
 * LispClass.java
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

import java.util.concurrent.ConcurrentHashMap;
import static org.armedbear.lisp.Lisp.*;

public abstract class LispClass extends StandardObject
{
  private static final ConcurrentHashMap<Symbol, LispObject> map
          = new ConcurrentHashMap<Symbol, LispObject>();

  public static <T extends LispClass> T addClass(Symbol symbol, T c)
  {
    map.put(symbol, c);
    return c;
  }

  public static LispObject addClass(Symbol symbol, LispObject c)
  {
    map.put(symbol, c);
    return c;
  }

  public static void removeClass(Symbol symbol)
  {
    map.remove(symbol);
  }

  public static LispClass findClass(Symbol symbol)
  {
    return (LispClass)map.get(symbol);
  }

  public static LispObject findClass(LispObject name, boolean errorp)

  {
    final Symbol symbol = checkSymbol(name);
    final LispObject c;
    c = map.get(symbol);
    if (c != null)
      return c;
    if (errorp)
      {
        StringBuilder sb =
          new StringBuilder("There is no class named ");
        sb.append(name.princToString());
        sb.append('.');
        return error(new LispError(sb.toString()));
      }
    return NIL;
  }

  private final int sxhash;

  private LispObject name;
  private LispObject propertyList;
  private Layout classLayout;
  private LispObject directSuperclasses = NIL;
  private LispObject directSubclasses = NIL;
  private LispObject classPrecedenceList = NIL;
  private LispObject directMethods = NIL;
  private LispObject documentation = NIL;
  private boolean finalized;

  protected LispClass(Layout layout)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    sxhash = hashCode() & 0x7fffffff;
  }

  protected LispClass(Symbol symbol)
  {
    this(null, symbol);
  }

  protected LispClass(Layout layout, Symbol symbol)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    setName(symbol);
    sxhash = hashCode() & 0x7fffffff;
  }

  protected LispClass(Layout layout,
                      Symbol symbol, LispObject directSuperclasses)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    sxhash = hashCode() & 0x7fffffff;
    setName(symbol);
    setDirectSuperclasses(directSuperclasses);
  }

  @Override
  public LispObject getParts()
  {
    LispObject result = NIL;
    result = result.push(new Cons("NAME", name != null ? name : NIL));
    result = result.push(new Cons("LAYOUT",
                                  getClassLayout() != null
                                  ? getClassLayout() : NIL));
    result = result.push(new Cons("DIRECT-SUPERCLASSES",
                                  getDirectSuperclasses()));
    result = result.push(new Cons("DIRECT-SUBCLASSES", getDirectSubclasses()));
    result = result.push(new Cons("CLASS-PRECEDENCE-LIST", getCPL()));
    result = result.push(new Cons("DIRECT-METHODS", getDirectMethods()));
    result = result.push(new Cons("DOCUMENTATION", getDocumentation()));
    return result.nreverse();
  }

  @Override
  public final int sxhash()
  {
    return sxhash;
  }

  public LispObject getName()
  {
    return name;
  }

  public void setName(LispObject name)
  {
    this.name = name;
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

  public Layout getClassLayout()
  {
    return classLayout;
  }

  public void setClassLayout(LispObject layout)
  {
    classLayout = layout == NIL ? null : (Layout)layout;
  }

  public final int getLayoutLength()
  {
    if (layout == null)
      return 0;
    return layout.getLength();
  }

  public LispObject getDirectSuperclasses()
  {
    return directSuperclasses;
  }

  public void setDirectSuperclasses(LispObject directSuperclasses)
  {
    this.directSuperclasses = directSuperclasses;
  }

  public boolean isFinalized()
  {
    return finalized;
  }

  public void setFinalized(boolean b)
  {
    finalized = b;
  }

  // When there's only one direct superclass...
  public final void setDirectSuperclass(LispObject superclass)
  {
    setDirectSuperclasses(new Cons(superclass));
  }

  public LispObject getDirectSubclasses()
  {
    return directSubclasses;
  }

  public void setDirectSubclasses(LispObject directSubclasses)
  {
    this.directSubclasses = directSubclasses;
  }

  public LispObject getCPL()
  {
    return classPrecedenceList;
  }

  public void setCPL(LispObject... cpl)
  {
    LispObject obj1 = cpl[0];
    if (obj1 instanceof Cons && cpl.length == 1)
      classPrecedenceList = obj1;
    else
      {
        Debug.assertTrue(obj1 == this);
        LispObject l = NIL;
        for (int i = cpl.length; i-- > 0;)
            l = new Cons(cpl[i], l);
        classPrecedenceList = l;
      }
  }

  public LispObject getDirectMethods()
  {
    return directMethods;
  }

  public void setDirectMethods(LispObject methods)
  {
    directMethods = methods;
  }

  public LispObject getDocumentation()
  {
    return documentation;
  }

  public void setDocumentation(LispObject doc)
  {
    documentation = doc;
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.CLASS;
  }

  @Override
  public LispObject classOf()
  {
    return StandardClass.CLASS;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.CLASS)
      return T;
    if (type == StandardClass.CLASS)
      return T;
    return super.typep(type);
  }

  public boolean subclassp(LispObject obj)
  {
      return subclassp(this, obj);
  }

  public static boolean subclassp(LispObject cls, LispObject obj)
  {
    LispObject cpl;

    if (cls instanceof LispClass)
      cpl = ((LispClass)cls).getCPL();
    else
      cpl = Symbol.CLASS_PRECEDENCE_LIST.execute(cls);

    while (cpl != NIL)
      {
        if (cpl.car() == obj)
          return true;
        cpl = ((Cons)cpl).cdr;
      }
    return false;
  }

  // ### find-class symbol &optional errorp environment => class
  private static final Primitive FIND_CLASS =
    new Primitive(Symbol.FIND_CLASS, "symbol &optional errorp environment")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return findClass(arg, true);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        return findClass(first, second != NIL);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)

      {
        // FIXME Use environment!
        return findClass(first, second != NIL);
      }
    };

  // ### %set-find-class
  private static final Primitive _SET_FIND_CLASS =
    new Primitive("%set-find-class", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        final Symbol name = checkSymbol(first);
        if (second == NIL)
          {
            removeClass(name);
            return second;
          }
        addClass(name, second);
        return second;
      }
    };

  // ### subclassp
  private static final Primitive SUBCLASSP =
    new Primitive(Symbol.SUBCLASSP, "class")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        return LispClass.subclassp(first, second) ? T : NIL;
      }
    };
}
