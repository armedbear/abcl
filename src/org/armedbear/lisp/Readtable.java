/*
 * Readtable.java
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
import java.util.Iterator;

public class Readtable extends LispObject
{
  public static final byte SYNTAX_TYPE_CONSTITUENT           = 0;
  public static final byte SYNTAX_TYPE_WHITESPACE            = 1;
  public static final byte SYNTAX_TYPE_TERMINATING_MACRO     = 2;
  public static final byte SYNTAX_TYPE_NON_TERMINATING_MACRO = 3;
  public static final byte SYNTAX_TYPE_SINGLE_ESCAPE         = 4;
  public static final byte SYNTAX_TYPE_MULTIPLE_ESCAPE       = 5;

  protected final CharHashMap<Byte> syntax = new CharHashMap<Byte>(Byte.class,SYNTAX_TYPE_CONSTITUENT);
  protected final CharHashMap<LispObject> readerMacroFunctions = new CharHashMap<LispObject>(LispObject.class,null);
  protected final CharHashMap<DispatchTable> dispatchTables = new CharHashMap<DispatchTable>(DispatchTable.class,null);

  protected LispObject readtableCase;

  public Readtable()
  {
    initialize();
  }

  protected void initialize()
  {
    Byte[] syntax = this.syntax.constants;
    syntax[9]    = SYNTAX_TYPE_WHITESPACE; // tab
    syntax[10]   = SYNTAX_TYPE_WHITESPACE; // linefeed
    syntax[12]   = SYNTAX_TYPE_WHITESPACE; // form feed
    syntax[13]   = SYNTAX_TYPE_WHITESPACE; // return
    syntax[' ']  = SYNTAX_TYPE_WHITESPACE;

    syntax['"']  = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax['\''] = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax['(']  = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax[')']  = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax[',']  = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax[';']  = SYNTAX_TYPE_TERMINATING_MACRO;
    syntax['`']  = SYNTAX_TYPE_TERMINATING_MACRO;

    syntax['#']  = SYNTAX_TYPE_NON_TERMINATING_MACRO;

    syntax['\\'] = SYNTAX_TYPE_SINGLE_ESCAPE;
    syntax['|']  = SYNTAX_TYPE_MULTIPLE_ESCAPE;

    LispObject[] readerMacroFunctions = this.readerMacroFunctions.constants;
    readerMacroFunctions[';']  = LispReader.READ_COMMENT;
    readerMacroFunctions['"']  = LispReader.READ_STRING;
    readerMacroFunctions['(']  = LispReader.READ_LIST;
    readerMacroFunctions[')']  = LispReader.READ_RIGHT_PAREN;
    readerMacroFunctions['\''] = LispReader.READ_QUOTE;
    readerMacroFunctions['#']  = LispReader.READ_DISPATCH_CHAR;

    // BACKQUOTE-MACRO and COMMA-MACRO are defined in backquote.lisp.
    readerMacroFunctions['`']  = Symbol.BACKQUOTE_MACRO;
    readerMacroFunctions[',']  = Symbol.COMMA_MACRO;

    DispatchTable dt = new DispatchTable();
    LispObject[] dtfunctions = dt.functions.constants;
    dtfunctions['(']  = LispReader.SHARP_LEFT_PAREN;
    dtfunctions['*']  = LispReader.SHARP_STAR;
    dtfunctions['.']  = LispReader.SHARP_DOT;
    dtfunctions[':']  = LispReader.SHARP_COLON;
    dtfunctions['A']  = LispReader.SHARP_A;
    dtfunctions['B']  = LispReader.SHARP_B;
    dtfunctions['C']  = LispReader.SHARP_C;
    dtfunctions['O']  = LispReader.SHARP_O;
    dtfunctions['P']  = LispReader.SHARP_P;
    dtfunctions['R']  = LispReader.SHARP_R;
    dtfunctions['S']  = LispReader.SHARP_S;
    dtfunctions['X']  = LispReader.SHARP_X;
    dtfunctions['\''] = LispReader.SHARP_QUOTE;
    dtfunctions['\\'] = LispReader.SHARP_BACKSLASH;
    dtfunctions['|']  = LispReader.SHARP_VERTICAL_BAR;
    dtfunctions[')']  = LispReader.SHARP_ILLEGAL;
    dtfunctions['<']  = LispReader.SHARP_ILLEGAL;
    dtfunctions[' ']  = LispReader.SHARP_ILLEGAL;
    dtfunctions[8]    = LispReader.SHARP_ILLEGAL; // backspace
    dtfunctions[9]    = LispReader.SHARP_ILLEGAL; // tab
    dtfunctions[10]   = LispReader.SHARP_ILLEGAL; // newline, linefeed
    dtfunctions[12]   = LispReader.SHARP_ILLEGAL; // page
    dtfunctions[13]   = LispReader.SHARP_ILLEGAL; // return

    dispatchTables.constants['#'] = dt;

    readtableCase = Keyword.UPCASE;
  }

  public Readtable(LispObject obj)
  {
    Readtable rt;
    if (obj == NIL)
      rt = checkReadtable(STANDARD_READTABLE.symbolValue());
    else
      rt = checkReadtable(obj);
    synchronized (rt)
      {
        copyReadtable(rt, this);
      }
  }

  // FIXME synchronization
  static void copyReadtable(Readtable from, Readtable to)
  {
    Iterator<Character> charIterator = from.syntax.getCharIterator();
      while (charIterator.hasNext()) {
        char c = charIterator.next();
          Byte dt = from.syntax.get(c);
          if (dt!=null) {
              to.syntax.put(c, dt);          
          } else {
              to.syntax.put(c, null);          
          }      
      }      
      charIterator = from.readerMacroFunctions.getCharIterator();
      while (charIterator.hasNext()) {
        char c = charIterator.next();
          LispObject dt = from.readerMacroFunctions.get(c);
          if (dt!=null) {
              to.readerMacroFunctions.put(c, dt);          
          } else {
              to.readerMacroFunctions.put(c, null);          
          }      
      }
      charIterator = from.dispatchTables.getCharIterator();
      while (charIterator.hasNext()) {
        char c = charIterator.next();
          DispatchTable dt = from.dispatchTables.get(c);
          if (dt!=null) {
              to.dispatchTables.put(c, new DispatchTable(dt));          
          } else {
              to.dispatchTables.put(c, null);          
          }      
      }
      to.readtableCase = from.readtableCase;
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.READTABLE;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.READTABLE;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.READTABLE)
      return T;
    if (type == BuiltInClass.READTABLE)
      return T;
    return super.typep(type);
  }

  @Override
  public String toString()
  {
    return unreadableString("READTABLE");
  }

  public LispObject getReadtableCase()
  {
    return readtableCase;
  }

  public boolean isWhitespace(char c)
  {
    return getSyntaxType(c) == SYNTAX_TYPE_WHITESPACE;
  }

  public byte getSyntaxType(char c)
  {
    return syntax.get(c);
  }

  public boolean isInvalid(char c)
  {
    switch (c)
      {
      case 8:
      case 9:
      case 10:
      case 12:
      case 13:
      case 32:
      case 127:
        return true;
      default:
        return false;
      }
  }

  public void checkInvalid(char c, Stream stream)
  {
    // "... no mechanism is provided for changing the constituent trait of a
    // character." (2.1.4.2)
    if (isInvalid(c))
      {
        String name = LispCharacter.charToName(c);
        StringBuilder sb = new StringBuilder("Invalid character");
        if (name != null)
          {
            sb.append(" #\\");
            sb.append(name);
          }
        error(new ReaderError(sb.toString(), stream));
      }
  }

  public LispObject getReaderMacroFunction(char c)
  {
    return readerMacroFunctions.get(c);
  }

  LispObject getMacroCharacter(char c)
  {
    LispObject function = getReaderMacroFunction(c);
    LispObject non_terminating_p;
    if (function != null)
      {
        if (syntax.get(c) == SYNTAX_TYPE_NON_TERMINATING_MACRO)
          non_terminating_p = T;
        else
          non_terminating_p = NIL;
      }
    else
      {
        function = NIL;
        non_terminating_p = NIL;
      }
    return LispThread.currentThread().setValues(function, non_terminating_p);
  }

  void makeDispatchMacroCharacter(char dispChar, LispObject non_terminating_p)
  {
    byte syntaxType;
    if (non_terminating_p != NIL)
      syntaxType = SYNTAX_TYPE_NON_TERMINATING_MACRO;
    else
      syntaxType = SYNTAX_TYPE_TERMINATING_MACRO;
    // FIXME synchronization
    syntax.put(dispChar,syntaxType);
    readerMacroFunctions.put(dispChar, LispReader.READ_DISPATCH_CHAR);
    dispatchTables.put(dispChar, new DispatchTable());
  }

  public LispObject getDispatchMacroCharacter(char dispChar, char subChar)

  {
    DispatchTable dispatchTable = dispatchTables.get(dispChar);
    if (dispatchTable == null)
      {
        LispCharacter c = LispCharacter.getInstance(dispChar);
        return error(new LispError(c.writeToString() +
                                    " is not a dispatch character."));
      }
    LispObject function =
      dispatchTable.functions.get(LispCharacter.toUpperCase(subChar));
    return (function != null) ? function : NIL;
  }

  public void setDispatchMacroCharacter(char dispChar, char subChar,
                                        LispObject function)

  {
    DispatchTable dispatchTable = dispatchTables.get(dispChar);
    if (dispatchTable == null)
      {
        LispCharacter c = LispCharacter.getInstance(dispChar);
        error(new LispError(c.writeToString() +
                             " is not a dispatch character."));
      }
    dispatchTable.functions.put(LispCharacter.toUpperCase(subChar), function);
  }

  protected static class DispatchTable
  {
	protected final CharHashMap<LispObject> functions;

    public DispatchTable()
    {
      functions = new CharHashMap<LispObject>(LispObject.class,null);
    }

    @SuppressWarnings("unchecked")
    public DispatchTable(DispatchTable dt)
    {
      functions = (CharHashMap<LispObject>) dt.functions.clone();
    }
  }

  // ### readtablep
  private static final Primitive READTABLEP =
    new Primitive("readtablep", "object")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return arg instanceof Readtable ? T : NIL;
      }
    };

  // ### copy-readtable
  private static final Primitive COPY_READTABLE =
    new Primitive("copy-readtable", "&optional from-readtable to-readtable")
    {
      @Override
      public LispObject execute()
      {
        return new Readtable(currentReadtable());
      }

      @Override
      public LispObject execute(LispObject arg)
      {
        return new Readtable(arg);
      }

      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        Readtable from = designator_readtable(first);
        if (second == NIL)
          return new Readtable(from);
        Readtable to = checkReadtable(second);
        copyReadtable(from, to);
        return to;
      }
    };

  // ### get-macro-character char &optional readtable
  // => function, non-terminating-p
  private static final Primitive GET_MACRO_CHARACTER =
    new Primitive("get-macro-character", "char &optional readtable")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        char c = LispCharacter.getValue(arg);
        Readtable rt = currentReadtable();
        return rt.getMacroCharacter(c);
      }

      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        char c = LispCharacter.getValue(first);
        Readtable rt = designator_readtable(second);
        return rt.getMacroCharacter(c);
      }
    };

  // ### set-macro-character char new-function &optional non-terminating-p readtable
  // => t
  private static final Primitive SET_MACRO_CHARACTER =
    new Primitive("set-macro-character",
                  "char new-function &optional non-terminating-p readtable")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        return execute(first, second, NIL, currentReadtable());
      }

      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)

      {
        return execute(first, second, third, currentReadtable());
      }

      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)

      {
        char c = LispCharacter.getValue(first);
        final LispObject designator;
        if (second instanceof Function
            || second instanceof StandardGenericFunction)
          designator = second;
        else if (second instanceof Symbol)
          designator = second;
        else
          return error(new LispError(second.writeToString() +
                                      " does not designate a function."));
        byte syntaxType;
        if (third != NIL)
          syntaxType = SYNTAX_TYPE_NON_TERMINATING_MACRO;
        else
          syntaxType = SYNTAX_TYPE_TERMINATING_MACRO;
        Readtable rt = designator_readtable(fourth);
        // REVIEW synchronization
        rt.syntax.put(c, syntaxType);
        rt.readerMacroFunctions.put(c, designator);
        return T;
      }
    };

  // ### make-dispatch-macro-character char &optional non-terminating-p readtable
  // => t
  private static final Primitive MAKE_DISPATCH_MACRO_CHARACTER =
    new Primitive("make-dispatch-macro-character",
                  "char &optional non-terminating-p readtable")
    {
      @Override
      public LispObject execute(LispObject[] args)
      {
        if (args.length < 1 || args.length > 3)
          return error(new WrongNumberOfArgumentsException(this));
        char dispChar = LispCharacter.getValue(args[0]);
        LispObject non_terminating_p;
        if (args.length > 1)
          non_terminating_p = args[1];
        else
          non_terminating_p = NIL;
        Readtable readtable;
        if (args.length == 3)
          readtable = checkReadtable(args[2]);
        else
          readtable = currentReadtable();
        readtable.makeDispatchMacroCharacter(dispChar, non_terminating_p);
        return T;
      }
    };

  // ### get-dispatch-macro-character disp-char sub-char &optional readtable
  // => function
  private static final Primitive GET_DISPATCH_MACRO_CHARACTER =
    new Primitive("get-dispatch-macro-character",
                  "disp-char sub-char &optional readtable")
    {
      @Override
      public LispObject execute(LispObject[] args)
      {
        if (args.length < 2 || args.length > 3)
          return error(new WrongNumberOfArgumentsException(this));
        char dispChar = LispCharacter.getValue(args[0]);
        char subChar = LispCharacter.getValue(args[1]);
        Readtable readtable;
        if (args.length == 3)
          readtable = designator_readtable(args[2]);
        else
          readtable = currentReadtable();
        return readtable.getDispatchMacroCharacter(dispChar, subChar);
      }
    };

  // ### set-dispatch-macro-character disp-char sub-char new-function &optional readtable
  // => t
  private static final Primitive SET_DISPATCH_MACRO_CHARACTER =
    new Primitive("set-dispatch-macro-character",
                  "disp-char sub-char new-function &optional readtable")
    {
      @Override
      public LispObject execute(LispObject[] args)
      {
        if (args.length < 3 || args.length > 4)
          return error(new WrongNumberOfArgumentsException(this));
        char dispChar = LispCharacter.getValue(args[0]);
        char subChar = LispCharacter.getValue(args[1]);
        LispObject function = coerceToFunction(args[2]);
        Readtable readtable;
        if (args.length == 4)
          readtable = designator_readtable(args[3]);
        else
          readtable = currentReadtable();
        readtable.setDispatchMacroCharacter(dispChar, subChar, function);
        return T;
      }
    };

  // ### set-syntax-from-char to-char from-char &optional to-readtable from-readtable
  // => t
  private static final Primitive SET_SYNTAX_FROM_CHAR =
    new Primitive("set-syntax-from-char",
                  "to-char from-char &optional to-readtable from-readtable")
    {
      @Override
      public LispObject execute(LispObject[] args)
      {
        if (args.length < 2 || args.length > 4)
          return error(new WrongNumberOfArgumentsException(this));
        char toChar = LispCharacter.getValue(args[0]);
        char fromChar = LispCharacter.getValue(args[1]);
        Readtable toReadtable;
        if (args.length > 2)
          toReadtable = checkReadtable(args[2]);
        else
          toReadtable = currentReadtable();
        Readtable fromReadtable;
        if (args.length > 3)
          fromReadtable = designator_readtable(args[3]);
        else
          fromReadtable = checkReadtable(STANDARD_READTABLE.symbolValue());
        // REVIEW synchronization
        toReadtable.syntax.put(toChar, fromReadtable.syntax.get(fromChar));
        toReadtable.readerMacroFunctions.put(toChar,
        		fromReadtable.readerMacroFunctions.get(fromChar));
        // "If the character is a dispatching macro character, its entire
        // dispatch table of reader macro functions is copied."
        DispatchTable found = fromReadtable.dispatchTables.get(fromChar);
        if (found!=null)
        	toReadtable.dispatchTables.put(toChar, new DispatchTable(found));          
        else
            // Don't leave behind dispatch tables when fromChar
            // doesn't have one
        	toReadtable.dispatchTables.put(toChar, null);
        return T;
      }
    };

  // ### readtable-case readtable => mode
  private static final Primitive READTABLE_CASE =
    new Primitive("readtable-case", "readtable")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkReadtable(arg).readtableCase;
      }
    };

  // ### %set-readtable-case readtable new-mode => new-mode
  private static final Primitive _SET_READTABLE_CASE =
    new Primitive("%set-readtable-case", PACKAGE_SYS, false,
                  "readtable new-mode")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
            final Readtable readtable = checkReadtable(first);
            if (second == Keyword.UPCASE || second == Keyword.DOWNCASE ||
                second == Keyword.INVERT || second == Keyword.PRESERVE)
              {
                readtable.readtableCase = second;
                return second;
              }
            return type_error(second, list(Symbol.MEMBER,
                                                 Keyword.INVERT,
                                                 Keyword.PRESERVE,
                                                 Keyword.DOWNCASE,
                                                 Keyword.UPCASE));
      }
    };
}
