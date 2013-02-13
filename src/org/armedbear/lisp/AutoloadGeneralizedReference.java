/*
 * AutoloadGeneralizedReference.java
 *
 * Copyright (C) 2014 Mark Evenson
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

public final class AutoloadGeneralizedReference extends Autoload
{
  Symbol indicator;  
  private AutoloadGeneralizedReference(Symbol symbol, Symbol indicator, String filename) {
    super(symbol, filename, null);
    this.indicator = indicator; 
  }

  @Override
  public void load()
  {
    Load.loadSystemFile(getFileName(), true);
  }

  static final Symbol SETF_EXPANDER  = PACKAGE_SYS.intern("SETF-EXPANDER");
  public static final Primitive AUTOLOAD_SETF_EXPANDER = new pf_autoload_setf_expander();
  @DocString(
    name="autoload-setf-expander",
    args="symbol-or-symbols filename",
    doc="Setup the autoload for SYMBOL-OR-SYMBOLS on the setf-expander from FILENAME."
  )
  private static final class pf_autoload_setf_expander extends Primitive {
    pf_autoload_setf_expander() {    
      super("autoload-setf-expander", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second) {
      final String filename = second.getStringValue();
      return installAutoloadGeneralizedReference(first, SETF_EXPANDER, filename);
    }
  };

  static final Symbol SETF_FUNCTION  = PACKAGE_SYS.intern("SETF-FUNCTION");
  public static final Primitive AUTOLOAD_SETF_FUNCTION = new pf_autoload_setf_function();
  @DocString(
    name="autoload-setf-function", 
    args="symbol-or-symbols filename",
    doc="Setup the autoload for SYMBOL-OR-SYMBOLS on the setf-function from FILENAME."
  )
  private static final class pf_autoload_setf_function extends Primitive {
    pf_autoload_setf_function() {    
      super("autoload-setf-function", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second) {
      final String filename = second.getStringValue();
      return installAutoloadGeneralizedReference(first, SETF_FUNCTION, filename);
    }
  };

  public static final Primitive AUTOLOAD_REF_P = new pf_autoload_ref_p();
  @DocString(
    name="autoload-ref-p",
    args="symbol",
    doc="Boolean predicate for whether SYMBOL has generalized reference functions which need to be resolved."
  )
  private static final class pf_autoload_ref_p extends Primitive {
    pf_autoload_ref_p() {
      super("autoload-ref-p", PACKAGE_EXT, true, "symbol");
    }
    @Override
    public LispObject execute(LispObject arg) {
      LispObject list = checkSymbol(arg).getPropertyList();
      while (list != NIL) {
        if (list.car() instanceof AutoloadGeneralizedReference) {
          return T;
        }

        list = list.cdr();
      }
      return NIL;
    }
  };
        

  private static final LispObject installAutoloadGeneralizedReference(LispObject first, 
                                                                      Symbol indicator, 
                                                                      String filename) 
  {
    if (first instanceof Symbol) {
      Symbol symbol = checkSymbol(first);
      install(symbol, indicator, filename);
      return T;
    }
    if (first instanceof Cons) {
      for (LispObject list = first; list != NIL; list = list.cdr()) {
        Symbol symbol = checkSymbol(list.car());
        install(symbol, indicator, filename);
      }
      return T;
    }
    return error(new TypeError(first));
  }

  private static LispObject install(Symbol symbol, Symbol indicator, String filename) {
    if (get(symbol, indicator) == NIL) {
      return Primitives.PUT.execute(symbol, indicator,
                                    new AutoloadGeneralizedReference(symbol, indicator, filename));
    } else {
      return NIL;
    }
    
  }
  @Override
  public LispObject execute()
  {
    load();
    return get(symbol, indicator, null).execute();
  }

  @Override
  public LispObject execute(LispObject arg)
  {
    load();
    return get(symbol, indicator, null).execute(arg);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)

  {
    load();
    return get(symbol, indicator, null).execute(first, second);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)

  {
    load();
    return get(symbol, indicator, null).execute(first, second, third);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)

  {
    load();
    return get(symbol, indicator, null).execute(first, second, third, fourth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)

  {
    load();
    return get(symbol, indicator, null).execute(first, second, third, fourth, fifth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)

  {
    load();
    return get(symbol, indicator, null).execute(first, second, third, fourth, fifth, sixth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)

  {
    load();
    return symbol.execute(first, second, third, fourth, fifth, sixth,
                          seventh);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)

  {
    load();
    return get(symbol, indicator, null).execute(first, second, third, fourth, fifth, sixth,
                                                seventh, eighth);
  }

  @Override
  public LispObject execute(LispObject[] args)
  {
    load();
    return get(symbol, indicator, null).execute(args);
  }

  

}
