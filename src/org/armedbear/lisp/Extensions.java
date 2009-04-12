/*
 * Extensions.java
 *
 * Copyright (C) 2002-2007 Peter Graves
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

import java.io.File;
import java.io.IOException;

public final class Extensions extends Lisp
{
  // ### *ed-functions*
  public static final Symbol _ED_FUNCTIONS_ =
    exportSpecial("*ED-FUNCTIONS*", PACKAGE_EXT,
                  list(intern("DEFAULT-ED-FUNCTION", PACKAGE_SYS)));

  // ### truly-the value-type form => result*
  private static final SpecialOperator TRULY_THE =
    new SpecialOperator("truly-the", PACKAGE_EXT, true, "type value")
    {
      @Override
      public LispObject execute(LispObject args, Environment env)
        throws ConditionThrowable
      {
        if (args.length() != 2)
          return error(new WrongNumberOfArgumentsException(this));
        return eval(args.cadr(), env, LispThread.currentThread());
      }
    };

  // ### neq
  private static final Primitive NEQ =
    new Primitive(Symbol.NEQ, "obj1 obj2")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        return first != second ? T : NIL;
      }
    };

  // ### memq item list => tail
  private static final Primitive MEMQ =
    new Primitive(Symbol.MEMQ, "item list")
    {
      @Override
      public LispObject execute(LispObject item, LispObject list)
        throws ConditionThrowable
      {
        while (list instanceof Cons)
          {
            if (item == ((Cons)list).car)
              return list;
            list = ((Cons)list).cdr;
          }
        if (list != NIL)
          type_error(list, Symbol.LIST);
        return NIL;
      }
    };

  // ### memql item list => tail
  private static final Primitive MEMQL =
    new Primitive(Symbol.MEMQL, "item list")
    {
      @Override
      public LispObject execute(LispObject item, LispObject list)
        throws ConditionThrowable
      {
        while (list instanceof Cons)
          {
            if (item.eql(((Cons)list).car))
              return list;
            list = ((Cons)list).cdr;
          }
        if (list != NIL)
          type_error(list, Symbol.LIST);
        return NIL;
      }
    };

  // ### adjoin-eql item list => new-list
  private static final Primitive ADJOIN_EQL =
    new Primitive(Symbol.ADJOIN_EQL, "item list")
    {
      @Override
      public LispObject execute(LispObject item, LispObject list)
        throws ConditionThrowable
      {
        return memql(item, list) ? list : new Cons(item, list);
      }
    };

  // ### special-variable-p
  private static final Primitive SPECIAL_VARIABLE_P =
    new Primitive("special-variable-p", PACKAGE_EXT, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.isSpecialVariable() ? T : NIL;
      }
    };

  // ### source
  private static final Primitive SOURCE =
    new Primitive("source", PACKAGE_EXT, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return get(arg, Symbol._SOURCE, NIL);
      }
    };

  // ### source-file-position
  private static final Primitive SOURCE_FILE_POSITION =
    new Primitive("source-file-position", PACKAGE_EXT, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        LispObject obj = get(arg, Symbol._SOURCE, NIL);
        if (obj instanceof Cons)
          return obj.cdr();
        return NIL;
      }
    };

  // ### source-pathname
  public static final Primitive SOURCE_PATHNAME =
    new Primitive("source-pathname", PACKAGE_EXT, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        LispObject obj = get(arg, Symbol._SOURCE, NIL);
        if (obj instanceof Cons)
          return obj.car();
        return obj;
      }
    };

  // ### exit
  private static final Primitive EXIT =
    new Primitive("exit", PACKAGE_EXT, true, "&key status")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        exit(0);
        return LispThread.currentThread().nothing();
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        int status = 0;
        if (first == Keyword.STATUS)
          {
            if (second instanceof Fixnum)
              status = ((Fixnum)second).value;
          }
        exit(status);
        return LispThread.currentThread().nothing();
      }
    };

  // ### quit
  private static final Primitive QUIT =
    new Primitive("quit", PACKAGE_EXT, true, "&key status")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        exit(0);
        return LispThread.currentThread().nothing();
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        int status = 0;
        if (first == Keyword.STATUS)
          {
            if (second instanceof Fixnum)
              status = ((Fixnum)second).value;
          }
        exit(status);
        return LispThread.currentThread().nothing();
      }
    };

  // ### dump-java-stack
  private static final Primitive DUMP_JAVA_STACK =
    new Primitive("dump-java-stack", PACKAGE_EXT, true)
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        Thread.dumpStack();
        return LispThread.currentThread().nothing();
      }
    };

  // ### make-temp-file => namestring
  private static final Primitive MAKE_TEMP_FILE =
    new Primitive("make-temp-file", PACKAGE_EXT, true, "")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        try
          {
            File file = File.createTempFile("abcl", null, null);
            if (file != null)
              return new Pathname(file.getPath());
          }
        catch (IOException e)
          {
            Debug.trace(e);
          }
        return NIL;
      }
    };

  // ### interrupt-lisp
  private static final Primitive INTERRUPT_LISP =
    new Primitive("interrupt-lisp", PACKAGE_EXT, true, "")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        setInterrupted(true);
        return T;
      }
    };

  // ### getenv
  private static final Primitive GETENV =
      new Primitive("getenv", PACKAGE_EXT, true)
  {
    @Override
    public LispObject execute(LispObject arg) throws ConditionThrowable
    {
      AbstractString string;
      if (arg instanceof AbstractString) {
        string = (AbstractString) arg;
      } else
        return type_error(arg, Symbol.STRING);
      String result = System.getenv(string.getStringValue());
      if (result != null)
        return new SimpleString(result);
      else
        return NIL;
    }
  };
}
