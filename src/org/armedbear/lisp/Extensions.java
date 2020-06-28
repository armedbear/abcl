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

import static org.armedbear.lisp.Lisp.*;

import java.io.File;
import java.io.IOException;
import java.util.*;

public final class Extensions
{
  // ### *ed-functions*
  public static final Symbol _ED_FUNCTIONS_ =
    exportSpecial("*ED-FUNCTIONS*", PACKAGE_EXT,
                  list(intern("DEFAULT-ED-FUNCTION", PACKAGE_SYS)));

  // ### truly-the value-type form => result*
  private static final SpecialOperator TRULY_THE = new truly_the();
  private static class truly_the extends SpecialOperator {
    truly_the() {
      super("truly-the", PACKAGE_EXT, true, "type value");
    }
    @Override
    public LispObject execute(LispObject args, Environment env)
    {
      if (args.length() != 2)
        return error(new WrongNumberOfArgumentsException(this, 2));
      return eval(args.cadr(), env, LispThread.currentThread());
    }
  }

  // ### neq
  private static final Primitive NEQ = new neq();
  private static class neq extends Primitive 
  {
    neq() 
    {
      super(Symbol.NEQ, "obj1 obj2");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
        return first != second ? T : NIL;
    }
  }

  // ### memq item list => tail
  private static final Primitive MEMQ = new memq();
  private static class memq extends Primitive 
  {
    memq() 
    {
      super(Symbol.MEMQ, "item list");
    }
    @Override
    public LispObject execute(LispObject item, LispObject list)
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
  }

  // ### memql item list => tail
  private static final Primitive MEMQL = new memql();
  private static class memql extends Primitive
  {
    memql() {
      super(Symbol.MEMQL, "item list");
    }
    @Override
    public LispObject execute(LispObject item, LispObject list)
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
  }

  // ### adjoin-eql item list => new-list
  private static final Primitive ADJOIN_EQL = new adjoin_eql();
  private static class adjoin_eql extends Primitive {
    adjoin_eql() {
      super(Symbol.ADJOIN_EQL, "item list");
    }
    @Override
    public LispObject execute(LispObject item, LispObject list)
    {
      return memql(item, list) ? list : new Cons(item, list);
    }
  }

  // ### special-variable-p
  private static final Primitive SPECIAL_VARIABLE_P = new special_variable_p();
  private static class special_variable_p extends Primitive {
    special_variable_p() {
      super("special-variable-p", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return arg.isSpecialVariable() ? T : NIL;
    }
  }

  // ### source symbol 
  private static final Primitive SOURCE = new source();
  private static class source extends Primitive {
    source() {
      super("source", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return get(arg, Symbol._SOURCE, NIL);
    }
  }

  // ### source-file-position symbol
  private static final Primitive SOURCE_FILE_POSITION = new source_file_position();
  private static class source_file_position extends Primitive {
    source_file_position() {
      super("source-file-position", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      LispObject obj = get(arg, Symbol._SOURCE, NIL);
      if (obj instanceof Cons)
        return obj.cdr();
      return NIL;
    }
  }

  // XXX rename to something else as it doesn't always refer to a pathname.
  public static final Primitive SOURCE_PATHNAME = new pf_source_pathname();
  @DocString(
    name="source-pathname",
    args="symbol",
    doc="Returns either the pathname corresponding to the file from which this symbol was compiled,"
    + "or the keyword :TOP-LEVEL."
  )
  private static class pf_source_pathname extends Primitive {
    pf_source_pathname() {
      super("source-pathname", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      LispObject obj = get(arg, Symbol._SOURCE, NIL);
      if (obj instanceof Cons)
        return obj.car();
      return obj;
    }
  }

  // ### exit
  private static final Primitive EXIT = new exit();
  private static class exit extends Primitive {
    exit() {
      super("exit", PACKAGE_EXT, true, "&key status");
    }
    @Override
    public LispObject execute()
    {
      throw new ProcessingTerminated();
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
      
    {
      int status = 0;
      if (first == Keyword.STATUS)
        {
          if (second instanceof Fixnum)
            status = ((Fixnum)second).value;
        }
      throw new ProcessingTerminated(status);
    }
  }

  // ### quit
  private static final Primitive QUIT = new quit();
  private static class quit extends Primitive {
    quit() {
      super("quit", PACKAGE_EXT, true, "&key status");
    }
    @Override
    public LispObject execute()
    {
      ((Stream)Symbol.STANDARD_OUTPUT.getSymbolValue())._finishOutput();
      ((Stream)Symbol.ERROR_OUTPUT.getSymbolValue())._finishOutput();
      throw new ProcessingTerminated();
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      int status = 0;
      if (first == Keyword.STATUS)
        {
          if (second instanceof Fixnum)
            status = ((Fixnum)second).value;
        }
      throw new ProcessingTerminated(status);
    }
  }

  // ### dump-java-stack
  private static final Primitive DUMP_JAVA_STACK = new dump_java_stack();
  private static class dump_java_stack extends Primitive {
    dump_java_stack() {
      super("dump-java-stack", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute()
    {
      Thread.dumpStack();
      return LispThread.currentThread().nothing();
    }
  }

  public static final Primitive MAKE_TEMP_FILE = new make_temp_file();
  @DocString(name="make-temp-file",
             doc="Create and return the pathname of a previously non-existent file.",
             args="&key prefix suffix")
  private static class make_temp_file extends Primitive { 
    make_temp_file() {
      super("make-temp-file", PACKAGE_EXT, true, "&key prefix suffix");
    }
    @Override
    public LispObject execute(LispObject ... args)
    {
      String prefix = "abcl";
      String suffix = null; 
      if ( args.length % 2 != 0) {
        error(new WrongNumberOfArgumentsException("Expecting an even number of arguments including keywords."));
      }

      for (int i = 0; i < args.length; i++ ) {
        if (args[i].SYMBOLP() != NIL) {
          if (args[i].equals(Keyword.PREFIX)) {
            String specifiedPrefix = args[i + 1].getStringValue();
            if (specifiedPrefix != null) {
              if (specifiedPrefix.equals(NIL.getStringValue())) {
                error (new TypeError("Cannot create temporary file with NIL prefix."));
              }
              prefix = specifiedPrefix;
              i += 1;
            }
          } else if (args[i].equals(Keyword.SUFFIX)) {
            String specifiedSuffix = args[i + 1].getStringValue();
            if (specifiedSuffix != null) {
              if (specifiedSuffix.equals(NIL.getStringValue())) {
                suffix =null;
              } else {
                suffix = specifiedSuffix;
              }
              i += 1;
            }
          }
        } else {
          error(new TypeError("Expected matching keyword argument.", args[i], Keyword.PREFIX.classOf()));
        }
      }
      return createTempFile(prefix, suffix);
    }

    @Override
    public LispObject execute() {
      return createTempFile("abcl", null);
    }

    private LispObject createTempFile(String prefix, String suffix) {
      try {
        File file = File.createTempFile(prefix, suffix, null);
        if (file != null)
          return Pathname.create(file.getPath());
      } catch (IllegalArgumentException e) {
        // "Failed to create temporary file due to argument problems."
        error(new JavaException(e));
      } catch (SecurityException e) {
        //"Failed to create problem due to problems with JVM SecurityManager."
        error(new JavaException(e));
      } catch (IOException e) {
        // "Failed to create temporary file."
        error(new JavaException(e));
      }
      return NIL;
    }
  }

  public static final Primitive MAKE_TEMP_DIRECTORY = new make_temp_directory();
  @DocString(name="make-temp-directory",
             doc="Create and return the pathname of a previously non-existent directory.")
  private static class make_temp_directory extends Primitive { 
    make_temp_directory() {
      super("make-temp-directory", PACKAGE_EXT, true, "");
    }
    @Override
    public LispObject execute()
    {
      try {
        File dir = File.createTempFile("abcl", null);
        dir.delete();
        if (dir.mkdirs()) {
          return Pathname.create(dir + "/");
        }
      } catch (Throwable t) {
        Debug.trace(t);
      }
      return NIL;
    }
  }

  // ### interrupt-lisp
  private static final Primitive INTERRUPT_LISP = new interrupt_lisp();
  private static class interrupt_lisp extends Primitive {
    interrupt_lisp() {
      super("interrupt-lisp", PACKAGE_EXT, true, "");
    }
    @Override
    public LispObject execute()
    {
      setInterrupted(true);
      return T;
    }
  }

  // ### getenv variable => string
  private static final Primitive GETENV = new getenv();
  private static class getenv extends Primitive 
  {
    getenv() 
    {
      super("getenv", PACKAGE_EXT, true, "variable",
             "Return the value of the environment VARIABLE if it exists, otherwise return NIL.");
    }
    @Override
    public LispObject execute(LispObject arg)
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
  }

  // ### getenv-all variable => string
  private static final Primitive GETENV_ALL = new getenv_all();
  private static class getenv_all extends Primitive 
  {
    getenv_all() 
    {
      super("getenv-all", PACKAGE_EXT, true, "variable",
             "Returns all environment variables as an alist containing (name . value)");
    }
    @Override
    public LispObject execute()
    {
      Cons result = new Cons(NIL);
      Map<String, String> env = System.getenv();
      for (Map.Entry<String, String> entry : env.entrySet()) {
          Cons entryPair = new Cons(new SimpleString(entry.getKey()), 
                                    new SimpleString(entry.getValue()));
          result = new Cons(entryPair, result);
      }
      return result;
    }
  }
}
