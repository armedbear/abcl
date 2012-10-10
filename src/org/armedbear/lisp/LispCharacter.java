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

import static org.armedbear.lisp.Lisp.*;
import java.util.HashMap;
import java.util.Map;

public final class LispCharacter extends LispObject
{
  public static final LispCharacter[] constants;
  public static final CharHashMap<LispCharacter> lispChars;

  static
  {
    lispChars = new CharHashMap<LispCharacter>(LispCharacter.class,null){
      public LispCharacter get(char c) {
        LispCharacter lc = super.get(c);
        if (lc==null) {
          lc = new LispCharacter(c);
          put(c, lc);
        }
        return lc;
      }
    };
    constants = lispChars.constants;
    for (int i = constants.length; i-- > 0;)
      constants[i] = new LispCharacter((char)i);
  }

  public final char value;
  private String name;
  public static LispCharacter getInstance(char c)
  {
    return lispChars.get(c);
  }

  // This needs to be public for the compiler.
  private LispCharacter(char c)
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
    StringBuilder sb = new StringBuilder("character #\\");
    sb.append(value);
    sb.append(" char-code #x");
    sb.append(Integer.toHexString(value));
    return new SimpleString(sb);
  }

  @Override
  public LispObject typep(LispObject type)
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
  public boolean characterp()
  {
    return true;
  }

  @Override
  public LispObject STRING()
  {
    return new SimpleString(value);
  }

  boolean isStandardChar()
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

  public static char getValue(LispObject obj)
  {
      if (obj instanceof LispCharacter)
        return ((LispCharacter)obj).value;
      type_error(obj, Symbol.CHARACTER);
        // Not reached.
      return 0;
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

  /** See LispObject.getStringValue() */
  @Override
  public String getStringValue()
  {
    return String.valueOf(value);
  }

  @Override
  public final String printObject()
  {
    final LispThread thread = LispThread.currentThread();
    boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
    // "Specifically, if *PRINT-READABLY* is true, printing proceeds as if
    // *PRINT-ESCAPE*, *PRINT-ARRAY*, and *PRINT-GENSYM* were also true,
    // and as if *PRINT-LENGTH*, *PRINT-LEVEL*, and *PRINT-LINES* were
    // false."
    boolean printEscape =
      printReadably || (Symbol.PRINT_ESCAPE.symbolValue(thread) != NIL);
    StringBuilder sb = new StringBuilder();
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
          case 27:
            sb.append("Escape");
            break;
          case 127:
            sb.append("Rubout");
            break;
	  default:
	    if (name!=null)
              sb.append(name);
	    else
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
      public LispObject execute(LispObject arg)
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
      public LispObject execute(LispObject arg)
      {
          return Character.isWhitespace(LispCharacter.getValue(arg)) ? T : NIL;
      }
    };

  // ### char-code
  private static final Primitive CHAR_CODE =
    new Primitive(Symbol.CHAR_CODE, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          int n = LispCharacter.getValue(arg);
          return Fixnum.getInstance(n);
      }
    };

  // ### char-int
  private static final Primitive CHAR_INT =
    new Primitive(Symbol.CHAR_INT, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          int n = LispCharacter.getValue(arg);
          return Fixnum.getInstance(n);
      }
    };

  // ### code-char
  private static final Primitive CODE_CHAR =
    new Primitive(Symbol.CODE_CHAR, "code")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        int n = Fixnum.getValue(arg);
        if (Character.isValidCodePoint(n))
          return LispCharacter.getInstance((char)n);
        return NIL;
      }
    };

  // ### characterp
  private static final Primitive CHARACTERP =
    new Primitive(Symbol.CHARACTERP, "object")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return arg instanceof LispCharacter ? T : NIL;
      }
    };

  // ### both-case-p
  private static final Primitive BOTH_CASE_P =
    new Primitive(Symbol.BOTH_CASE_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
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
      public LispObject execute(LispObject arg)
      {
        return Character.isLowerCase(getValue(arg)) ? T : NIL;
      }
    };

  // ### upper-case-p
  private static final Primitive UPPER_CASE_P =
    new Primitive(Symbol.UPPER_CASE_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return Character.isUpperCase(getValue(arg)) ? T : NIL;
      }
    };

  // ### char-downcase
  private static final Primitive CHAR_DOWNCASE =
    new Primitive(Symbol.CHAR_DOWNCASE, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          final char c = LispCharacter.getValue(arg);
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
      public LispObject execute(LispObject arg)
      {
        final char c;
        c = LispCharacter.getValue(arg);
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
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof Bignum)
              return NIL;

          int weight = Fixnum.getValue(arg);
        if (weight < 10)
          return constants['0'+weight];
        return NIL;
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        int radix;
        if (second instanceof Fixnum)
            radix = ((Fixnum)second).value;
        else
            radix = -1;
        
        if (radix < 2 || radix > 36)
          return type_error(second,
                                 list(Symbol.INTEGER, Fixnum.TWO,
                                       Fixnum.constants[36]));
        if (first instanceof Bignum)
            return NIL;
        int weight = Fixnum.getValue(first);
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
      public LispObject execute(LispObject arg)
      {
          final int n = Character.digit(LispCharacter.getValue(arg), 10);
          return n < 0 ? NIL : Fixnum.getInstance(n);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        char c;
            c = LispCharacter.getValue(first);
        if (second instanceof Fixnum)
          {
            int radix = ((Fixnum)second).value;
            if (radix >= 2 && radix <= 36)
              {
                int n = Character.digit(c, radix);
                return n < 0 ? NIL : Fixnum.constants[n];
              }
          }
        return type_error(second,
                               list(Symbol.INTEGER, Fixnum.TWO,
                                     Fixnum.constants[36]));
      }
    };

  // ### standard-char-p
  private static final Primitive STANDARD_CHAR_P =
    new Primitive(Symbol.STANDARD_CHAR_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkCharacter(arg).isStandardChar() ? T : NIL;
      }
    };

  // ### graphic-char-p
  private static final Primitive GRAPHIC_CHAR_P =
    new Primitive(Symbol.GRAPHIC_CHAR_P, "char")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          char c = LispCharacter.getValue(arg);
          if (c >= ' ' && c < 127)
            return T;
          return Character.isISOControl(c) ? NIL : T;
      }
    };

  // ### alpha-char-p
  private static final Primitive ALPHA_CHAR_P =
    new Primitive(Symbol.ALPHA_CHAR_P, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return Character.isLetter(LispCharacter.getValue(arg)) ? T : NIL;
      }
    };

  // ### alphanumericp
  private static final Primitive ALPHANUMERICP =
    new Primitive(Symbol.ALPHANUMERICP, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return Character.isLetterOrDigit(LispCharacter.getValue(arg)) ? T : NIL;
      }
    };

  public static final int nameToChar(String s)
  {
    String lower = s.toLowerCase();
    LispCharacter lc = namedToChar.get(lower);
    if (lc!=null) return lc.value;
    if (lower.length() == 5
        && lower.startsWith("u")) {
        try {
            final int i = Integer.parseInt(lower.substring(1, 5), 16);
            return i;
        } catch (NumberFormatException e) {};
    }

    if (lower.equals("null"))
      return 0;
    if (lower.equals("nul"))
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
    if (lower.equals("escape"))
        return 27;
    if (lower.equals("space"))
      return ' ';
    if (lower.equals("rubout"))
      return 127;
    if (lower.startsWith("u")) {
      int length = lower.length();
      if (length > 1 && length < 5) {
        try {
          final int i = Integer.parseInt(lower.substring(1), 16);
          return i;
        } catch (NumberFormatException e) {};
        // fall through
      }
    }

    // Unknown.
    return -1;
  }

  // ### name-char
  private static final Primitive NAME_CHAR =
    new Primitive(Symbol.NAME_CHAR, "name")
    {
      @Override
      public LispObject execute(LispObject arg)
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
      case 27:
        return "Escape";
      case ' ':
        return "Space";
      case 127:
        return "Rubout";
      }

    if (c<0 || c>255) return null;
    return lispChars.get(c).name;
  }

  // ### char-name
  private static final Primitive CHAR_NAME =
    new Primitive(Symbol.CHAR_NAME, "character")
    {
      @Override
      public LispObject execute(LispObject arg)
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

  static int maxNamedChar = 0; 
  static Map<String, LispCharacter> namedToChar = new HashMap<String, LispCharacter>(); 
  
  static void setCharNames(int i, String[] string) { 
    int settingChar = i; 
    int index = 0; 
    int stringLen = string.length; 
    while(stringLen-->0) { 
      setCharName(settingChar,string[index]); 
      index++; 
      settingChar++; 
    } 
    if (maxNamedChar<settingChar) maxNamedChar = settingChar;  
  } 
  
  static void setCharName(int settingChar, String string) { 
    LispCharacter c = lispChars.get((char)settingChar); 
    c.name = string; 
    namedToChar.put(string.toLowerCase(), c); 
  } 
   
  static { 
   new CharNameMaker0(); 
  } 
   
  static class CharNameMaker0{ 
    { 
      setCharNames(0,new String[]{"Null", "Soh", "Stx", "Etx", "Eot", "Enq", "Ack", "Bell", "Backspace", "Tab", "Newline", "Vt", "Page", "Return", "So", "Si", "Dle", "Dc1", "Dc2", "Dc3", "Dc4", "Nak", "Syn", "Etb", "Can", "Em", "Sub", "Escape", "Fs", "Gs", "Rs", "Us"}); 
      setCharNames(128,new String[]{"U0080", "U0081", "U0082", "U0083", "U0084", "U0085", "U0086", "U0087", "U0088", "U0089", "U008a", "U008b", "U008c", "U008d", "U008e", "U008f", "U0090", "U0091", "U0092", "U0093", "U0094", "U0095", "U0096", "U0097", "U0098", "U0099", "U009a", "U009b", "U009c", "U009d", "U009e", "U009f"}); 
    } 
  }  

  static final char[] UPPER_CASE_CHARS = new char[128];

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

  static final char[] LOWER_CASE_CHARS = new char[128];

  static
  {
    for (int i = LOWER_CASE_CHARS.length; i-- > 0;)
      LOWER_CASE_CHARS[i] = Character.toLowerCase((char)i);
  }
}
