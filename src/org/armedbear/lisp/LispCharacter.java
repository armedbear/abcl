/*
 * LispCharacter.java
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

public final class LispCharacter extends LispObject
{
  public static final LispCharacter[] constants = new LispCharacter[CHAR_MAX];

  static
  {
    for (int i = constants.length; i-- > 0;)
      constants[i] = new LispCharacter((char)i);
  }

  public final char value;

  public static LispCharacter getInstance(char c)
  {
    try
      {
        return constants[c];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        return new LispCharacter(c);
      }
  }

  // This needs to be public for the compiler.
  public LispCharacter(char c)
  {
    this.value = c;
  }

  @Override
  public LispObject typeOf()
  {
    if (isStandardChar())
      return Symbol.STANDARD_CHAR;
    return Symbol.CHARACTER;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.CHARACTER;
  }

  @Override
  public LispObject getDescription()
  {
    FastStringBuffer sb = new FastStringBuffer("character #\\");
    sb.append(value);
    sb.append(" char-code #x");
    sb.append(Integer.toHexString(value));
    return new SimpleString(sb);
  }

  @Override
  public LispObject typep(LispObject type) throws ConditionThrowable
  {
    if (type == Symbol.CHARACTER)
      return T;
    if (type == BuiltInClass.CHARACTER)
      return T;
    if (type == Symbol.BASE_CHAR)
      return T;
    if (type == Symbol.STANDARD_CHAR)
      return isStandardChar() ? T : NIL;
    return super.typep(type);
  }

  @Override
  public LispObject CHARACTERP()
  {
    return T;
  }

  @Override
  public boolean characterp()
  {
    return true;
  }

  @Override
  public LispObject STRING()
  {
    return new SimpleString(value);
  }

  private boolean isStandardChar()
  {
    if (value >= ' ' && value < 127)
      return true;
    if (value == '\n')
      return true;
    return false;
  }

  @Override
  public boolean eql(char c)
  {
    return value == c;
  }

  @Override
  public boolean eql(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof LispCharacter)
      {
        if (value == ((LispCharacter)obj).value)
          return true;
      }
    return false;
  }

  @Override
  public boolean equal(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof LispCharacter)
      {
        if (value == ((LispCharacter)obj).value)
          return true;
      }
    return false;
  }

  @Override
  public boolean equalp(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof LispCharacter)
      {
        if (value == ((LispCharacter)obj).value)
          return true;
        return LispCharacter.toLowerCase(value) == LispCharacter.toLowerCase(((LispCharacter)obj).value);
      }
    return false;
  }

  public static char getValue(LispObject obj) throws ConditionThrowable
  {
    try
      {
        return ((LispCharacter)obj).value;
      }
    catch (ClassCastException e)
      {
        type_error(obj, Symbol.CHARACTER);
        // Not reached.
        return 0;
      }
  }

  public final char getValue()
  {
    return value;
  }

  @Override
  public Object javaInstance()
  {
    return Character.valueOf(value);
  }

  @Override
  public Object javaInstance(Class c)
  {
    return javaInstance();
  }

  @Override
  public int sxhash()
  {
    return value;
  }

  @Override
  public int psxhash()
  {
    return Character.toUpperCase(value);
  }

  @Override
  public final String writeToString() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
    // "Specifically, if *PRINT-READABLY* is true, printing proceeds as if
    // *PRINT-ESCAPE*, *PRINT-ARRAY*, and *PRINT-GENSYM* were also true,
    // and as if *PRINT-LENGTH*, *PRINT-LEVEL*, and *PRINT-LINES* were
    // false."
    boolean printEscape =
      printReadably || (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
    FastStringBuffer sb = new FastStringBuffer();
    if (printEscape)
      {
        sb.append("#\\");
        switch (value)
          {
          case 0:
            sb.append("Null");
            break;
          case 7:
            sb.append("Bell");
            break;
          case '\b':
            sb.append("Backspace");
            break;
          case '\t':
            sb.append("Tab");
            break;
          case '\n':
            sb.append("Newline");
            break;
          case '\f':
            sb.append("Page");
            break;
          case '\r':
            sb.append("Return");
            break;
          case 127:
            sb.append("Rubout");
            break;
          default:
            sb.append(value);
            break;
          }
      }
    else
      {
        sb.append(value);
      }
    return sb.toString();
  }

  // ### character
  private static final Primitive CHARACTER =
    new Primitive(Symbol.CHARACTER, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof LispCharacter)
          return arg;
        if (arg instanceof AbstractString)
          {
            if (arg.length() == 1)
              return ((AbstractString)arg).AREF(0);
          }
        else if (arg instanceof Symbol)
          {
            String name = ((Symbol)arg).getName();
            if (name.length() == 1)
              return LispCharacter.getInstance(name.charAt(0));
          }
        return type_error(arg, Symbol.CHARACTER_DESIGNATOR);
      }
    };

  // ### whitespacep
  private static final Primitive WHITESPACEP =
    new Primitive("whitespacep", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return Character.isWhitespace(((LispCharacter)arg).value) ? T : NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### char-code
  private static final Primitive CHAR_CODE =
    new Primitive(Symbol.CHAR_CODE, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            int n = ((LispCharacter)arg).value;
            return n < 256 ? Fixnum.constants[n] : new Fixnum(n);
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### char-int
  private static final Primitive CHAR_INT =
    new Primitive(Symbol.CHAR_INT, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            int n = ((LispCharacter)arg).value;
            return n < 256 ? Fixnum.constants[n] : new Fixnum(n);
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### code-char
  private static final Primitive CODE_CHAR =
    new Primitive(Symbol.CODE_CHAR, "code")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            int n = ((Fixnum)arg).value;
            if (n < CHAR_MAX)
              return constants[n];
            else if (n <= Character.MAX_VALUE)
              return new LispCharacter((char)n);
          }
        catch (ClassCastException e)
          {
              // SBCL signals a type-error here: "not of type (UNSIGNED-BYTE 8)"
          }
        return NIL;
      }
    };

  // ### characterp
  private static final Primitive CHARACTERP =
    new Primitive(Symbol.CHARACTERP, "object")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg instanceof LispCharacter ? T : NIL;
      }
    };

  // ### both-case-p
  private static final Primitive BOTH_CASE_P =
    new Primitive(Symbol.BOTH_CASE_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        char c = getValue(arg);
        if (Character.isLowerCase(c) || Character.isUpperCase(c))
          return T;
        return NIL;
      }
    };

  // ### lower-case-p
  private static final Primitive LOWER_CASE_P =
    new Primitive(Symbol.LOWER_CASE_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return Character.isLowerCase(getValue(arg)) ? T : NIL;
      }
    };

  // ### upper-case-p
  private static final Primitive UPPER_CASE_P =
    new Primitive(Symbol.UPPER_CASE_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return Character.isUpperCase(getValue(arg)) ? T : NIL;
      }
    };

  // ### char-downcase
  private static final Primitive CHAR_DOWNCASE =
    new Primitive(Symbol.CHAR_DOWNCASE, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        char c;
        try
          {
            c = ((LispCharacter)arg).value;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
        if (c < 128)
          return constants[LOWER_CASE_CHARS[c]];
        return LispCharacter.getInstance(toLowerCase(c));
      }
    };

  // ### char-upcase
  private static final Primitive CHAR_UPCASE =
    new Primitive(Symbol.CHAR_UPCASE, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final char c;
        try
          {
            c = ((LispCharacter)arg).value;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
        if (c < 128)
          return constants[UPPER_CASE_CHARS[c]];
        return LispCharacter.getInstance(toUpperCase(c));
      }
    };

  // ### digit-char
  private static final Primitive DIGIT_CHAR =
    new Primitive(Symbol.DIGIT_CHAR, "weight &optional radix")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        int weight;
        try
          {
            weight = ((Fixnum)arg).value;
          }
        catch (ClassCastException e)
          {
            if (arg instanceof Bignum)
              return NIL;
            return type_error(arg, Symbol.INTEGER);
          }
        if (weight < 10)
          return constants['0' + weight];
        return NIL;
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        int radix;
        try
          {
            radix = ((Fixnum)second).value;
          }
        catch (ClassCastException e)
          {
            radix = -1;
          }
        if (radix < 2 || radix > 36)
          return type_error(second,
                                 list3(Symbol.INTEGER, Fixnum.TWO,
                                       Fixnum.constants[36]));
        int weight;
        try
          {
            weight = ((Fixnum)first).value;
          }
        catch (ClassCastException e)
          {
            if (first instanceof Bignum)
              return NIL;
            return type_error(first, Symbol.INTEGER);
          }
        if (weight >= radix)
          return NIL;
        if (weight < 10)
          return constants['0' + weight];
        return constants['A' + weight - 10];
      }
    };

  // ### digit-char-p char &optional radix => weight
  private static final Primitive DIGIT_CHAR_P =
    new Primitive(Symbol.DIGIT_CHAR_P, "char &optional radix")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            int n = Character.digit(((LispCharacter)arg).value, 10);
            return n < 0 ? NIL : Fixnum.constants[n];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        char c;
        try
          {
            c = ((LispCharacter)first).value;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.CHARACTER);
          }
        try
          {
            int radix = ((Fixnum)second).value;
            if (radix >= 2 && radix <= 36)
              {
                int n = Character.digit(c, radix);
                return n < 0 ? NIL : Fixnum.constants[n];
              }
          }
        catch (ClassCastException e) {}
        return type_error(second,
                               list3(Symbol.INTEGER, Fixnum.TWO,
                                     Fixnum.constants[36]));
      }
    };

  // ### standard-char-p
  private static final Primitive STANDARD_CHAR_P =
    new Primitive(Symbol.STANDARD_CHAR_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((LispCharacter)arg).isStandardChar() ? T : NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### graphic-char-p
  private static final Primitive GRAPHIC_CHAR_P =
    new Primitive(Symbol.GRAPHIC_CHAR_P, "char")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            char c = ((LispCharacter)arg).value;
            if (c >= ' ' && c < 127)
              return T;
            return Character.isISOControl(c) ? NIL : T;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### alpha-char-p
  private static final Primitive ALPHA_CHAR_P =
    new Primitive(Symbol.ALPHA_CHAR_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return Character.isLetter(((LispCharacter)arg).value) ? T : NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  // ### alphanumericp
  private static final Primitive ALPHANUMERICP =
    new Primitive(Symbol.ALPHANUMERICP, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return Character.isLetterOrDigit(((LispCharacter)arg).value) ? T : NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CHARACTER);
          }
      }
    };

  public static final int nameToChar(String s)
  {
    String lower = s.toLowerCase();
    if (lower.equals("null"))
      return 0;
    if (lower.equals("bell"))
      return 7;
    if (lower.equals("backspace"))
      return '\b';
    if (lower.equals("tab"))
      return '\t';
    if (lower.equals("linefeed"))
      return '\n';
    if (lower.equals("newline"))
      return '\n';
    if (lower.equals("page"))
      return '\f';
    if (lower.equals("return"))
      return '\r';
    if (lower.equals("space"))
      return ' ';
    if (lower.equals("rubout"))
      return 127;
    // Unknown.
    return -1;
  }

  // ### name-char
  private static final Primitive NAME_CHAR =
    new Primitive(Symbol.NAME_CHAR, "name")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        String s = arg.STRING().getStringValue();
        int n = nameToChar(s);
        return n >= 0 ? LispCharacter.getInstance((char)n) : NIL;
      }
    };

  public static final String charToName(char c)
  {
    switch (c)
      {
      case 0:
        return "Null";
      case 7:
        return "Bell";
      case '\b':
        return "Backspace";
      case '\t':
        return "Tab";
      case '\n':
        return "Newline";
      case '\f':
        return "Page";
      case '\r':
        return "Return";
      case ' ':
        return "Space";
      case 127:
        return "Rubout";
      }
    return null;
  }

  // ### char-name
  private static final Primitive CHAR_NAME =
    new Primitive(Symbol.CHAR_NAME, "character")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        String name = charToName(LispCharacter.getValue(arg));
        return name != null ? new SimpleString(name) : NIL;
      }
    };

  public static final char toUpperCase(char c)
  {
    if (c < 128)
      return UPPER_CASE_CHARS[c];
    return Character.toUpperCase(c);
  }

  private static final char[] UPPER_CASE_CHARS = new char[128];

  static
  {
    for (int i = UPPER_CASE_CHARS.length; i-- > 0;)
      UPPER_CASE_CHARS[i] = Character.toUpperCase((char)i);
  }

  public static final char toLowerCase(char c)
  {
    if (c < 128)
      return LOWER_CASE_CHARS[c];
    return Character.toLowerCase(c);
  }

  private static final char[] LOWER_CASE_CHARS = new char[128];

  static
  {
    for (int i = LOWER_CASE_CHARS.length; i-- > 0;)
      LOWER_CASE_CHARS[i] = Character.toLowerCase((char)i);
  }
}
