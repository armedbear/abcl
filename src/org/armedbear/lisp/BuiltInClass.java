/*
 * BuiltInClass.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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

public class BuiltInClass extends LispClass
{
  private BuiltInClass(Symbol symbol)
  {
    super(symbol);
  }

  public LispObject typeOf()
  {
    return Symbol.BUILT_IN_CLASS;
  }

  public LispObject classOf()
  {
    return StandardClass.BUILT_IN_CLASS;
  }

  public LispObject typep(LispObject type) throws ConditionThrowable
  {
    if (type == Symbol.BUILT_IN_CLASS)
      return T;
    if (type == StandardClass.BUILT_IN_CLASS)
      return T;
    return super.typep(type);
  }

  public LispObject getDescription() throws ConditionThrowable
  {
    return new SimpleString(writeToString());
  }

  public String writeToString() throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer("#<BUILT-IN-CLASS ");
    sb.append(symbol.writeToString());
    sb.append('>');
    return sb.toString();
  }

  private static BuiltInClass addClass(Symbol symbol)
  {
    BuiltInClass c = new BuiltInClass(symbol);
    addClass(symbol, c);
    return c;
  }

  public static final BuiltInClass CLASS_T              = addClass(T);

  public static final BuiltInClass ARRAY                = addClass(Symbol.ARRAY);
  public static final BuiltInClass BIGNUM               = addClass(Symbol.BIGNUM);
  public static final BuiltInClass BASE_STRING          = addClass(Symbol.BASE_STRING);
  public static final BuiltInClass BIT_VECTOR           = addClass(Symbol.BIT_VECTOR);
  public static final BuiltInClass BROADCAST_STREAM     = addClass(Symbol.BROADCAST_STREAM);
  public static final BuiltInClass CASE_FROB_STREAM     = addClass(Symbol.CASE_FROB_STREAM);
  public static final BuiltInClass CHARACTER            = addClass(Symbol.CHARACTER);
  public static final BuiltInClass COMPLEX              = addClass(Symbol.COMPLEX);
  public static final BuiltInClass CONCATENATED_STREAM  = addClass(Symbol.CONCATENATED_STREAM);
  public static final BuiltInClass CONS                 = addClass(Symbol.CONS);
  public static final BuiltInClass DOUBLE_FLOAT         = addClass(Symbol.DOUBLE_FLOAT);
  public static final BuiltInClass ECHO_STREAM          = addClass(Symbol.ECHO_STREAM);
  public static final BuiltInClass ENVIRONMENT          = addClass(Symbol.ENVIRONMENT);
  public static final BuiltInClass FILE_STREAM          = addClass(Symbol.FILE_STREAM);
  public static final BuiltInClass FIXNUM               = addClass(Symbol.FIXNUM);
  public static final BuiltInClass FLOAT                = addClass(Symbol.FLOAT);
  public static final BuiltInClass FUNCTION             = addClass(Symbol.FUNCTION);
  public static final BuiltInClass HASH_TABLE           = addClass(Symbol.HASH_TABLE);
  public static final BuiltInClass INTEGER              = addClass(Symbol.INTEGER);
  public static final BuiltInClass JAVA_OBJECT          = addClass(Symbol.JAVA_OBJECT);
  public static final BuiltInClass LIST                 = addClass(Symbol.LIST);
  public static final BuiltInClass LOGICAL_PATHNAME     = addClass(Symbol.LOGICAL_PATHNAME);
  public static final BuiltInClass MAILBOX              = addClass(Symbol.MAILBOX);
  public static final BuiltInClass METHOD_COMBINATION   = addClass(Symbol.METHOD_COMBINATION);
  public static final BuiltInClass MUTEX                = addClass(Symbol.MUTEX);
  public static final BuiltInClass NIL_VECTOR           = addClass(Symbol.NIL_VECTOR);
  public static final BuiltInClass NULL                 = addClass(Symbol.NULL);
  public static final BuiltInClass NUMBER               = addClass(Symbol.NUMBER);
  public static final BuiltInClass PACKAGE              = addClass(Symbol.PACKAGE);
  public static final BuiltInClass PATHNAME             = addClass(Symbol.PATHNAME);
  public static final BuiltInClass RANDOM_STATE         = addClass(Symbol.RANDOM_STATE);
  public static final BuiltInClass RATIO                = addClass(Symbol.RATIO);
  public static final BuiltInClass RATIONAL             = addClass(Symbol.RATIONAL);
  public static final BuiltInClass READTABLE            = addClass(Symbol.READTABLE);
  public static final BuiltInClass REAL                 = addClass(Symbol.REAL);
  public static final BuiltInClass RESTART              = addClass(Symbol.RESTART);
  public static final BuiltInClass SEQUENCE             = addClass(Symbol.SEQUENCE);
  public static final BuiltInClass SIMPLE_ARRAY         = addClass(Symbol.SIMPLE_ARRAY);
  public static final BuiltInClass SIMPLE_BASE_STRING   = addClass(Symbol.SIMPLE_BASE_STRING);
  public static final BuiltInClass SIMPLE_BIT_VECTOR    = addClass(Symbol.SIMPLE_BIT_VECTOR);
  public static final BuiltInClass SIMPLE_STRING        = addClass(Symbol.SIMPLE_STRING);
  public static final BuiltInClass SIMPLE_VECTOR        = addClass(Symbol.SIMPLE_VECTOR);
  public static final BuiltInClass SINGLE_FLOAT         = addClass(Symbol.SINGLE_FLOAT);
  public static final BuiltInClass SLIME_INPUT_STREAM   = addClass(Symbol.SLIME_INPUT_STREAM);
  public static final BuiltInClass SLIME_OUTPUT_STREAM  = addClass(Symbol.SLIME_OUTPUT_STREAM);
  public static final BuiltInClass SOCKET_STREAM        = addClass(Symbol.SOCKET_STREAM);
  public static final BuiltInClass STREAM               = addClass(Symbol.STREAM);
  public static final BuiltInClass STRING               = addClass(Symbol.STRING);
  public static final BuiltInClass STRING_INPUT_STREAM  = addClass(Symbol.STRING_INPUT_STREAM);
  public static final BuiltInClass STRING_OUTPUT_STREAM = addClass(Symbol.STRING_OUTPUT_STREAM);
  public static final BuiltInClass STRING_STREAM        = addClass(Symbol.STRING_STREAM);
  public static final BuiltInClass SYMBOL               = addClass(Symbol.SYMBOL);
  public static final BuiltInClass SYNONYM_STREAM       = addClass(Symbol.SYNONYM_STREAM);
  public static final BuiltInClass THREAD               = addClass(Symbol.THREAD);
  public static final BuiltInClass TWO_WAY_STREAM       = addClass(Symbol.TWO_WAY_STREAM);
  public static final BuiltInClass VECTOR               = addClass(Symbol.VECTOR);

  public static final StructureClass STRUCTURE_OBJECT =
    new StructureClass(Symbol.STRUCTURE_OBJECT, list1(CLASS_T));
  static
  {
    addClass(Symbol.STRUCTURE_OBJECT, STRUCTURE_OBJECT);
  }

  static
  {
    ARRAY.setDirectSuperclass(CLASS_T);
    ARRAY.setCPL(ARRAY, CLASS_T);
    BASE_STRING.setDirectSuperclass(STRING);
    BASE_STRING.setCPL(BASE_STRING, STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
    BIGNUM.setDirectSuperclass(INTEGER);
    BIGNUM.setCPL(BIGNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
    BIT_VECTOR.setDirectSuperclass(VECTOR);
    BIT_VECTOR.setCPL(BIT_VECTOR, VECTOR, ARRAY, SEQUENCE, CLASS_T);
    BROADCAST_STREAM.setDirectSuperclass(STREAM);
    BROADCAST_STREAM.setCPL(BROADCAST_STREAM, STREAM, CLASS_T);
    CASE_FROB_STREAM.setDirectSuperclass(STREAM);
    CASE_FROB_STREAM.setCPL(CASE_FROB_STREAM, STREAM, CLASS_T);
    CHARACTER.setDirectSuperclass(CLASS_T);
    CHARACTER.setCPL(CHARACTER, CLASS_T);
    CLASS_T.setCPL(CLASS_T);
    COMPLEX.setDirectSuperclass(NUMBER);
    COMPLEX.setCPL(COMPLEX, NUMBER, CLASS_T);
    CONCATENATED_STREAM.setDirectSuperclass(STREAM);
    CONCATENATED_STREAM.setCPL(CONCATENATED_STREAM, STREAM, CLASS_T);
    CONS.setDirectSuperclass(LIST);
    CONS.setCPL(CONS, LIST, SEQUENCE, CLASS_T);
    DOUBLE_FLOAT.setDirectSuperclass(FLOAT);
    DOUBLE_FLOAT.setCPL(DOUBLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
    ECHO_STREAM.setDirectSuperclass(STREAM);
    ECHO_STREAM.setCPL(ECHO_STREAM, STREAM, CLASS_T);
    ENVIRONMENT.setDirectSuperclass(CLASS_T);
    ENVIRONMENT.setCPL(ENVIRONMENT, CLASS_T);
    FIXNUM.setDirectSuperclass(INTEGER);
    FIXNUM.setCPL(FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
    FILE_STREAM.setDirectSuperclass(STREAM);
    FILE_STREAM.setCPL(FILE_STREAM, STREAM, CLASS_T);
    FLOAT.setDirectSuperclass(REAL);
    FLOAT.setCPL(FLOAT, REAL, NUMBER, CLASS_T);
    FUNCTION.setDirectSuperclass(CLASS_T);
    FUNCTION.setCPL(FUNCTION, CLASS_T);
    HASH_TABLE.setDirectSuperclass(CLASS_T);
    HASH_TABLE.setCPL(HASH_TABLE, CLASS_T);
    INTEGER.setDirectSuperclass(RATIONAL);
    INTEGER.setCPL(INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
    JAVA_OBJECT.setDirectSuperclass(CLASS_T);
    JAVA_OBJECT.setCPL(JAVA_OBJECT, CLASS_T);
    LIST.setDirectSuperclass(SEQUENCE);
    LIST.setCPL(LIST, SEQUENCE, CLASS_T);
    LOGICAL_PATHNAME.setDirectSuperclass(PATHNAME);
    LOGICAL_PATHNAME.setCPL(LOGICAL_PATHNAME, PATHNAME, CLASS_T);
    MAILBOX.setDirectSuperclass(CLASS_T);
    MAILBOX.setCPL(MAILBOX, CLASS_T);
    METHOD_COMBINATION.setDirectSuperclass(CLASS_T);
    METHOD_COMBINATION.setCPL(METHOD_COMBINATION, CLASS_T);
    MUTEX.setDirectSuperclass(CLASS_T);
    MUTEX.setCPL(MUTEX, CLASS_T);
    NIL_VECTOR.setDirectSuperclass(STRING);
    NIL_VECTOR.setCPL(NIL_VECTOR, STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
    NULL.setDirectSuperclass(LIST);
    NULL.setCPL(NULL, SYMBOL, LIST, SEQUENCE, CLASS_T);
    NUMBER.setDirectSuperclass(CLASS_T);
    NUMBER.setCPL(NUMBER, CLASS_T);
    PACKAGE.setDirectSuperclass(CLASS_T);
    PACKAGE.setCPL(PACKAGE, CLASS_T);
    PATHNAME.setDirectSuperclass(CLASS_T);
    PATHNAME.setCPL(PATHNAME, CLASS_T);
    RANDOM_STATE.setDirectSuperclass(CLASS_T);
    RANDOM_STATE.setCPL(RANDOM_STATE, CLASS_T);
    RATIO.setDirectSuperclass(RATIONAL);
    RATIO.setCPL(RATIO, RATIONAL, REAL, NUMBER, CLASS_T);
    RATIONAL.setDirectSuperclass(REAL);
    RATIONAL.setCPL(RATIONAL, REAL, NUMBER, CLASS_T);
    READTABLE.setDirectSuperclass(CLASS_T);
    READTABLE.setCPL(READTABLE, CLASS_T);
    REAL.setDirectSuperclass(NUMBER);
    REAL.setCPL(REAL, NUMBER, CLASS_T);
    RESTART.setDirectSuperclass(CLASS_T);
    RESTART.setCPL(RESTART, CLASS_T);
    SEQUENCE.setDirectSuperclass(CLASS_T);
    SEQUENCE.setCPL(SEQUENCE, CLASS_T);
    SIMPLE_ARRAY.setDirectSuperclass(ARRAY);
    SIMPLE_ARRAY.setCPL(SIMPLE_ARRAY, ARRAY, CLASS_T);
    SIMPLE_BASE_STRING.setDirectSuperclasses(list2(BASE_STRING, SIMPLE_STRING));
    SIMPLE_BASE_STRING.setCPL(SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING,
                              STRING, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
                              CLASS_T);
    SIMPLE_BIT_VECTOR.setDirectSuperclasses(list2(BIT_VECTOR, SIMPLE_ARRAY));
    SIMPLE_BIT_VECTOR.setCPL(SIMPLE_BIT_VECTOR, BIT_VECTOR, VECTOR,
                             SIMPLE_ARRAY, ARRAY, SEQUENCE, CLASS_T);
    SIMPLE_STRING.setDirectSuperclasses(list2(STRING, SIMPLE_ARRAY));
    SIMPLE_STRING.setCPL(SIMPLE_STRING, STRING, VECTOR, SIMPLE_ARRAY, ARRAY,
                         SEQUENCE, CLASS_T);
    SIMPLE_VECTOR.setDirectSuperclasses(list2(VECTOR, SIMPLE_ARRAY));
    SIMPLE_VECTOR.setCPL(SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
                         CLASS_T);
    SINGLE_FLOAT.setDirectSuperclass(FLOAT);
    SINGLE_FLOAT.setCPL(SINGLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
    SLIME_INPUT_STREAM.setDirectSuperclass(STRING_STREAM);
    SLIME_INPUT_STREAM.setCPL(SLIME_INPUT_STREAM, STRING_STREAM, STREAM,
                              CLASS_T);
    SLIME_OUTPUT_STREAM.setDirectSuperclass(STRING_STREAM);
    SLIME_OUTPUT_STREAM.setCPL(SLIME_OUTPUT_STREAM, STRING_STREAM, STREAM,
                               CLASS_T);
    SOCKET_STREAM.setDirectSuperclass(TWO_WAY_STREAM);
    SOCKET_STREAM.setCPL(SOCKET_STREAM, TWO_WAY_STREAM, STREAM, CLASS_T);
    STREAM.setDirectSuperclass(CLASS_T);
    STREAM.setCPL(STREAM, CLASS_T);
    STRING.setDirectSuperclass(VECTOR);
    STRING.setCPL(STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
    STRING_INPUT_STREAM.setDirectSuperclass(STRING_STREAM);
    STRING_INPUT_STREAM.setCPL(STRING_INPUT_STREAM, STRING_STREAM, STREAM,
                               CLASS_T);
    STRING_OUTPUT_STREAM.setDirectSuperclass(STRING_STREAM);
    STRING_OUTPUT_STREAM.setCPL(STRING_OUTPUT_STREAM, STRING_STREAM, STREAM,
                                CLASS_T);
    STRING_STREAM.setDirectSuperclass(STREAM);
    STRING_STREAM.setCPL(STRING_STREAM, STREAM, CLASS_T);
    STRUCTURE_OBJECT.setCPL(STRUCTURE_OBJECT, CLASS_T);
    SYMBOL.setDirectSuperclass(CLASS_T);
    SYMBOL.setCPL(SYMBOL, CLASS_T);
    SYNONYM_STREAM.setDirectSuperclass(STREAM);
    SYNONYM_STREAM.setCPL(SYNONYM_STREAM, STREAM, CLASS_T);
    THREAD.setDirectSuperclass(CLASS_T);
    THREAD.setCPL(THREAD, CLASS_T);
    TWO_WAY_STREAM.setDirectSuperclass(STREAM);
    TWO_WAY_STREAM.setCPL(TWO_WAY_STREAM, STREAM, CLASS_T);
    VECTOR.setDirectSuperclasses(list2(ARRAY, SEQUENCE));
    VECTOR.setCPL(VECTOR, ARRAY, SEQUENCE, CLASS_T);
  }

  static
  {
    try
      {
        StandardClass.initializeStandardClasses();
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
  }
}
