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

import static org.armedbear.lisp.Lisp.*;

public class BuiltInClass extends LispClass
{
  private BuiltInClass(Symbol symbol)
  {
    super(symbol);
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.BUILT_IN_CLASS;
  }

  @Override
  public LispObject classOf()
  {
    return StandardClass.BUILT_IN_CLASS;
  }

  @Override
  public boolean isFinalized()
  {
    return true;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.BUILT_IN_CLASS)
      return T;
    if (type == StandardClass.BUILT_IN_CLASS)
      return T;
    return super.typep(type);
  }

  @Override
  public LispObject getDescription()
  {
    return new SimpleString(writeToString());
  }

  @Override
  public String writeToString()
  {
    StringBuilder sb = new StringBuilder("#<BUILT-IN-CLASS ");
    sb.append(getName().writeToString());
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
  public static final BuiltInClass CHARACTER            = addClass(Symbol.CHARACTER);
  public static final BuiltInClass COMPLEX              = addClass(Symbol.COMPLEX);
  public static final BuiltInClass CONS                 = addClass(Symbol.CONS);
  public static final BuiltInClass DOUBLE_FLOAT         = addClass(Symbol.DOUBLE_FLOAT);
  public static final BuiltInClass ENVIRONMENT          = addClass(Symbol.ENVIRONMENT);
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
  public static final BuiltInClass JAR_PATHNAME         = addClass(Symbol.JAR_PATHNAME);
  public static final BuiltInClass URL_PATHNAME         = addClass(Symbol.URL_PATHNAME);
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
  public static final BuiltInClass STRING               = addClass(Symbol.STRING);
  public static final BuiltInClass SYMBOL               = addClass(Symbol.SYMBOL);
  public static final BuiltInClass THREAD               = addClass(Symbol.THREAD);
  public static final BuiltInClass VECTOR               = addClass(Symbol.VECTOR);
  public static final BuiltInClass STACK_FRAME          = addClass(Symbol.STACK_FRAME);
  public static final BuiltInClass LISP_STACK_FRAME     = addClass(Symbol.LISP_STACK_FRAME);
  public static final BuiltInClass JAVA_STACK_FRAME     = addClass(Symbol.JAVA_STACK_FRAME);


  public static final StructureClass STRUCTURE_OBJECT =
    (StructureClass)addClass(Symbol.STRUCTURE_OBJECT,
             new StructureClass(Symbol.STRUCTURE_OBJECT, list(CLASS_T)));

    /* All the stream classes below are being defined as structure classes
       but won't be available as such until further action is taken:
       the 'defstruct' internal administration is missing.

       For STREAM and SYSTEM-STREAM, that bit is added in boot.lisp */

  public static final LispClass STREAM =
    addClass(Symbol.STREAM,
             new StructureClass(Symbol.STREAM, list(STRUCTURE_OBJECT)));
  public static final LispClass SYSTEM_STREAM =
    addClass(Symbol.SYSTEM_STREAM,
             new StructureClass(Symbol.SYSTEM_STREAM, list(STREAM)));
  public static final LispClass TWO_WAY_STREAM =
    addClass(Symbol.TWO_WAY_STREAM,
             new StructureClass(Symbol.TWO_WAY_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass BROADCAST_STREAM =
    addClass(Symbol.BROADCAST_STREAM,
             new StructureClass(Symbol.BROADCAST_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass ECHO_STREAM =
    addClass(Symbol.ECHO_STREAM,
             new StructureClass(Symbol.ECHO_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass CASE_FROB_STREAM =
    addClass(Symbol.CASE_FROB_STREAM,
             new StructureClass(Symbol.CASE_FROB_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass STRING_STREAM =
    addClass(Symbol.STRING_STREAM,
             new StructureClass(Symbol.STRING_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass STRING_INPUT_STREAM =
    addClass(Symbol.STRING_INPUT_STREAM,
             new StructureClass(Symbol.STRING_INPUT_STREAM, list(STRING_STREAM)));
  public static final LispClass STRING_OUTPUT_STREAM =
    addClass(Symbol.STRING_OUTPUT_STREAM,
             new StructureClass(Symbol.STRING_OUTPUT_STREAM, list(STRING_STREAM)));
  public static final LispClass SYNONYM_STREAM =
    addClass(Symbol.SYNONYM_STREAM,
             new StructureClass(Symbol.SYNONYM_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass FILE_STREAM =
    addClass(Symbol.FILE_STREAM,
             new StructureClass(Symbol.FILE_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass JAR_STREAM =
    addClass(Symbol.JAR_STREAM,
             new StructureClass(Symbol.JAR_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass URL_STREAM =
    addClass(Symbol.URL_STREAM,
             new StructureClass(Symbol.URL_STREAM, list(SYSTEM_STREAM)));
  public static final LispClass CONCATENATED_STREAM =
    addClass(Symbol.CONCATENATED_STREAM,
             new StructureClass(Symbol.CONCATENATED_STREAM, list(SYSTEM_STREAM)));



    // Implementation defined streams
  public static final LispClass SOCKET_STREAM =
    addClass(Symbol.SOCKET_STREAM,
             new StructureClass(Symbol.SOCKET_STREAM, list(TWO_WAY_STREAM)));
  public static final LispClass SLIME_INPUT_STREAM =
    addClass(Symbol.SLIME_INPUT_STREAM,
             new StructureClass(Symbol.SLIME_INPUT_STREAM, list(STRING_STREAM)));
  public static final LispClass SLIME_OUTPUT_STREAM =
    addClass(Symbol.SLIME_OUTPUT_STREAM,
             new StructureClass(Symbol.SLIME_OUTPUT_STREAM, list(STRING_STREAM)));



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
    BROADCAST_STREAM.setCPL(BROADCAST_STREAM, SYSTEM_STREAM, STREAM,
                            STRUCTURE_OBJECT, CLASS_T);
    CASE_FROB_STREAM.setCPL(CASE_FROB_STREAM, SYSTEM_STREAM, STREAM,
                            STRUCTURE_OBJECT, CLASS_T);
    CHARACTER.setDirectSuperclass(CLASS_T);
    CHARACTER.setCPL(CHARACTER, CLASS_T);
    CLASS_T.setCPL(CLASS_T);
    COMPLEX.setDirectSuperclass(NUMBER);
    COMPLEX.setCPL(COMPLEX, NUMBER, CLASS_T);
    CONCATENATED_STREAM.setCPL(CONCATENATED_STREAM, SYSTEM_STREAM, STREAM,
                               STRUCTURE_OBJECT, CLASS_T);
    CONS.setDirectSuperclass(LIST);
    CONS.setCPL(CONS, LIST, SEQUENCE, CLASS_T);
    DOUBLE_FLOAT.setDirectSuperclass(FLOAT);
    DOUBLE_FLOAT.setCPL(DOUBLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
    ECHO_STREAM.setCPL(ECHO_STREAM, SYSTEM_STREAM, STREAM,
                       STRUCTURE_OBJECT, CLASS_T);
    ENVIRONMENT.setDirectSuperclass(CLASS_T);
    ENVIRONMENT.setCPL(ENVIRONMENT, CLASS_T);
    FIXNUM.setDirectSuperclass(INTEGER);
    FIXNUM.setCPL(FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
    FILE_STREAM.setCPL(FILE_STREAM, SYSTEM_STREAM, STREAM,
                       STRUCTURE_OBJECT, CLASS_T);
    JAR_STREAM.setCPL(JAR_STREAM, SYSTEM_STREAM, STREAM,
                      STRUCTURE_OBJECT, CLASS_T);
    URL_STREAM.setCPL(URL_STREAM, SYSTEM_STREAM, STREAM,
                      STRUCTURE_OBJECT, CLASS_T);
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
    JAR_PATHNAME.setDirectSuperclass(PATHNAME);
    JAR_PATHNAME.setCPL(JAR_PATHNAME, PATHNAME, CLASS_T);
    URL_PATHNAME.setDirectSuperclass(PATHNAME);
    URL_PATHNAME.setCPL(URL_PATHNAME, PATHNAME, CLASS_T);
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
    SIMPLE_BASE_STRING.setDirectSuperclasses(list(BASE_STRING, SIMPLE_STRING));
    SIMPLE_BASE_STRING.setCPL(SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING,
                              STRING, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
                              CLASS_T);
    SIMPLE_BIT_VECTOR.setDirectSuperclasses(list(BIT_VECTOR, SIMPLE_ARRAY));
    SIMPLE_BIT_VECTOR.setCPL(SIMPLE_BIT_VECTOR, BIT_VECTOR, VECTOR,
                             SIMPLE_ARRAY, ARRAY, SEQUENCE, CLASS_T);
    SIMPLE_STRING.setDirectSuperclasses(list(BASE_STRING, STRING, SIMPLE_ARRAY));
    SIMPLE_STRING.setCPL(SIMPLE_STRING, BASE_STRING, STRING, VECTOR,
                         SIMPLE_ARRAY, ARRAY, SEQUENCE, CLASS_T);
    SIMPLE_VECTOR.setDirectSuperclasses(list(VECTOR, SIMPLE_ARRAY));
    SIMPLE_VECTOR.setCPL(SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
                         CLASS_T);
    SINGLE_FLOAT.setDirectSuperclass(FLOAT);
    SINGLE_FLOAT.setCPL(SINGLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
    SLIME_INPUT_STREAM.setCPL(SLIME_INPUT_STREAM, STRING_STREAM, SYSTEM_STREAM,
                              STREAM, STRUCTURE_OBJECT, CLASS_T);
    SLIME_OUTPUT_STREAM.setCPL(SLIME_OUTPUT_STREAM, STRING_STREAM, SYSTEM_STREAM,
                               STREAM, STRUCTURE_OBJECT, CLASS_T);
    SOCKET_STREAM.setCPL(SOCKET_STREAM, TWO_WAY_STREAM, SYSTEM_STREAM, STREAM,
                         STRUCTURE_OBJECT, CLASS_T);
    STREAM.setCPL(STREAM, STRUCTURE_OBJECT, CLASS_T);
    STRING.setDirectSuperclass(VECTOR);
    STRING.setCPL(STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
    STRING_INPUT_STREAM.setCPL(STRING_INPUT_STREAM, STRING_STREAM,
                               SYSTEM_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
    STRING_OUTPUT_STREAM.setCPL(STRING_OUTPUT_STREAM, STRING_STREAM,
                                SYSTEM_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
    STRING_STREAM.setCPL(STRING_STREAM, SYSTEM_STREAM, STREAM,
                         STRUCTURE_OBJECT, CLASS_T);
    STRUCTURE_OBJECT.setCPL(STRUCTURE_OBJECT, CLASS_T);
    SYMBOL.setDirectSuperclass(CLASS_T);
    SYMBOL.setCPL(SYMBOL, CLASS_T);
    SYNONYM_STREAM.setCPL(SYNONYM_STREAM, SYSTEM_STREAM,
                          STREAM, STRUCTURE_OBJECT, CLASS_T);
    SYSTEM_STREAM.setCPL(SYSTEM_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
    THREAD.setDirectSuperclass(CLASS_T);
    THREAD.setCPL(THREAD, CLASS_T);
    TWO_WAY_STREAM.setCPL(TWO_WAY_STREAM, SYSTEM_STREAM, STREAM,
                          STRUCTURE_OBJECT, CLASS_T);
    VECTOR.setDirectSuperclasses(list(ARRAY, SEQUENCE));
    VECTOR.setCPL(VECTOR, ARRAY, SEQUENCE, CLASS_T);
    STACK_FRAME.setDirectSuperclasses(CLASS_T);
    STACK_FRAME.setCPL(STACK_FRAME, CLASS_T);
    LISP_STACK_FRAME.setDirectSuperclasses(STACK_FRAME);
    LISP_STACK_FRAME.setCPL(LISP_STACK_FRAME, STACK_FRAME, CLASS_T);
    JAVA_STACK_FRAME.setDirectSuperclasses(STACK_FRAME);
    JAVA_STACK_FRAME.setCPL(JAVA_STACK_FRAME, STACK_FRAME, CLASS_T);
  }

  static
  {
    StandardClass.initializeStandardClasses();
  }
}
