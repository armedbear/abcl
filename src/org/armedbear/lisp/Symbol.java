/*
 * Symbol.java
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

public class Symbol extends LispObject implements java.io.Serializable
{
  // Bit flags.
  private static final int FLAG_SPECIAL           = 0x0001;
  private static final int FLAG_CONSTANT          = 0x0002;
  private static final int FLAG_BUILT_IN_FUNCTION = 0x0004;

  public static final Symbol addFunction(String name, LispObject obj)
  {
    Symbol symbol = PACKAGE_CL.internAndExport(name);
    symbol.function = obj;
    return symbol;
  }

  public final SimpleString name;
  private int hash = -1;

  /** To be accessed by LispThread only:
   * used to find the index in the LispThread.specials array
   */
  transient int specialIndex = LispThread.UNASSIGNED_SPECIAL_INDEX;
  private LispObject pkg; // Either a package object or NIL.
  private transient LispObject value;
  private transient LispObject function;
  private transient LispObject propertyList;
  private int flags;

  // Construct an uninterned symbol.
  public Symbol(String s)
  {
    name = new SimpleString(s);
    pkg = NIL;
  }

  public Symbol(SimpleString string)
  {
    name = string;
    pkg = NIL;
  }

  public Symbol(String s, Package pkg)
  {
    name = new SimpleString(s);
    this.pkg = pkg;
  }

  public Symbol(SimpleString string, Package pkg)
  {
    name = string;
    this.pkg = pkg;
  }

  public Symbol(SimpleString string, int hash, Package pkg)
  {
    name = string;
    this.hash = hash;
    this.pkg = pkg;
  }

    @Override
    @SuppressWarnings("FinalizeDeclaration")
    protected void finalize() throws Throwable {
        try {
            if (specialIndex != LispThread.UNASSIGNED_SPECIAL_INDEX)
                LispThread.releaseSpecialIndex(this);
        } finally {
            super.finalize();
        }
    }

  @Override
  public LispObject typeOf()
  {
    if (pkg == PACKAGE_KEYWORD)
      return Symbol.KEYWORD;
    if (this == T)
      return Symbol.BOOLEAN;
    return Symbol.SYMBOL;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.SYMBOL;
  }

  @Override
  public LispObject getDescription()
  {
    final LispThread thread = LispThread.currentThread();
    final SpecialBindingsMark mark = thread.markSpecialBindings();
    thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
    try
      {
        StringBuilder sb = new StringBuilder("The symbol ");
        sb.append(name.writeToString());
        sb.append(" at #x");
        sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
        if (pkg instanceof Package)
          {
            sb.append(", an ");
            Symbol sym = ((Package)pkg).findExternalSymbol(name);
            sb.append(sym == this ? "external" : "internal");
            sb.append(" symbol in the ");
            sb.append(((Package)pkg).getName());
            sb.append(" package");
          }
        return new SimpleString(sb);
      }
    finally
      {
        thread.resetSpecialBindings(mark);
      }
  }

  @Override
  public LispObject getParts()
  {
    LispObject parts = NIL;
    parts = parts.push(new Cons("name", name));
    parts = parts.push(new Cons("package", pkg));
    parts = parts.push(new Cons("value", value));
    parts = parts.push(new Cons("function", function));
    parts = parts.push(new Cons("plist", propertyList));
    parts = parts.push(new Cons("flags", Fixnum.getInstance(flags)));
    parts = parts.push(new Cons("hash", Fixnum.getInstance(hash)));
    return parts.nreverse();
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.SYMBOL)
      return T;
    if (type == BuiltInClass.SYMBOL)
      return T;
    if (type == Symbol.KEYWORD)
      return pkg == PACKAGE_KEYWORD ? T : NIL;
    if (type == Symbol.BOOLEAN)
      return this == T ? T : NIL;
    return super.typep(type);
  }

  @Override
  public boolean constantp()
  {
    return (flags & FLAG_CONSTANT) != 0;
  }

  @Override
  public final LispObject STRING()
  {
    return name;
  }

  public final LispObject getPackage()
  {
    return pkg;
  }

  public final void setPackage(LispObject obj)
  {
    pkg = obj;
  }

  @Override
  public final boolean isSpecialOperator()
  {
    return (function instanceof SpecialOperator);
  }

  @Override
  public final boolean isSpecialVariable()
  {
    return (flags & FLAG_SPECIAL) != 0;
  }

  public final void setSpecial(boolean b)
  {
    if (b)
      flags |= FLAG_SPECIAL;
    else
      flags &= ~FLAG_SPECIAL;
  }

  public final void initializeSpecial(LispObject value)
  {
    flags |= FLAG_SPECIAL;
    this.value = value;
  }

  public final boolean isConstant()
  {
    return (flags & FLAG_CONSTANT) != 0;
  }

  public final void initializeConstant(LispObject value)
  {
    flags |= (FLAG_SPECIAL | FLAG_CONSTANT);
    this.value = value;
  }

  public final boolean isBuiltInFunction()
  {
    return (flags & FLAG_BUILT_IN_FUNCTION) != 0;
  }

  public final void setBuiltInFunction(boolean b)
  {
    if (b)
      flags |= FLAG_BUILT_IN_FUNCTION;
    else
      flags &= ~FLAG_BUILT_IN_FUNCTION;
  }

  public final String getName()
  {
    return name.getStringValue();
  }

  public final String getQualifiedName()
  {
    final String n = name.getStringValue();
    if (pkg == NIL)
      return("#:".concat(n));
    if (pkg == PACKAGE_KEYWORD)
      return ":".concat(n);
    StringBuilder sb = new StringBuilder(((Package)pkg).getName());
    if (((Package)pkg).findExternalSymbol(name) != null)
      sb.append(':');
    else
      sb.append("::");
    sb.append(n);
    return sb.toString();
  }

  @Override
  public String toString() {
      return getQualifiedName();
  }

  /** Gets the value associated with the symbol
   * as set by SYMBOL-VALUE.
   *
   * @return The associated value, or null if unbound.
   *
   * @see Symbol#symbolValue
   */
  @Override
  public LispObject getSymbolValue()
  {
    return value;
  }

  /** Sets the value associated with the symbol
   * as if set by SYMBOL-VALUE.
   *
   * @return The associated value, or null if unbound.
   *
   * @see Symbol#symbolValue
   */
  public final void setSymbolValue(LispObject value)
  {
    if (isConstant())
      // Complement the check already done in SpecialOperators.sf_setq
      error(new ProgramError("Can't change value of constant symbol " + writeToString() + "."));
    this.value = value;
  }

  /** Returns the value associated with this symbol in the current
   * thread context when it is treated as a special variable.
   *
   * A lisp error is thrown if the symbol is unbound.
   *
   * @return The associated value
   *
   * @see LispThread#lookupSpecial
   * @see Symbol#getSymbolValue()
   *
   */
  public final LispObject symbolValue()
  {
    return symbolValue(LispThread.currentThread());
  }

  /** Returns the value associated with this symbol in the specified
   * thread context when it is treated as a special variable.
   *
   * A lisp error is thrown if the symbol is unbound.
   *
   * @return The associated value
   *
   * @see LispThread#lookupSpecial
   * @see Symbol#getSymbolValue()
   *
   */
  public final LispObject symbolValue(LispThread thread)
  {
    LispObject val = thread.lookupSpecial(this);
    if (val != null)
      return val;
    if (value != null)
      return value;
    return error(new UnboundVariable(this));
  }

  /** Returns the value of the symbol in the current thread context;
   * if the symbol has been declared special, the value of the innermost
   * binding is returned. Otherwise, the SYMBOL-VALUE is returned, or
   * null if unbound.
   *
   * @return A lisp object, or null if unbound
   *
   * @see LispThread#lookupSpecial
   * @see Symbol#getSymbolValue()
   *
   */
  public final LispObject symbolValueNoThrow()
  {
    return symbolValueNoThrow(LispThread.currentThread());
  }

  /** Returns the value of the symbol in the current thread context;
   * if the symbol has been declared special, the value of the innermost
   * binding is returned. Otherwise, the SYMBOL-VALUE is returned, or
   * null if unbound.
   *
   * @return A lisp object, or null if unbound
   *
   * @see LispThread#lookupSpecial
   * @see Symbol#getSymbolValue()
   *
   */
  public final LispObject symbolValueNoThrow(LispThread thread)
  {
    if ((flags & FLAG_SPECIAL) != 0)
      {
        LispObject val = thread.lookupSpecial(this);
        if (val != null)
          return val;
      }
    return value;
  }

  @Override
  public LispObject getSymbolFunction()
  {
    return function;
  }

  @Override
  public final LispObject getSymbolFunctionOrDie()
  {
    if (function == null)
      return error(new UndefinedFunction(this));
    if (function instanceof Autoload)
      {
        Autoload autoload = (Autoload) function;
        autoload.load();
      }
    return function;
  }

  @Override
  public final LispObject getSymbolSetfFunction()
  {
    return get(this, Symbol.SETF_FUNCTION, NIL);
  }


  @Override
  public final LispObject getSymbolSetfFunctionOrDie()
  {
    LispObject obj = get(this, Symbol.SETF_FUNCTION, null);
    if (obj == null)
      error(new UndefinedFunction(list(Keyword.NAME,
                                         list(Symbol.SETF,
                                               this))));
    return obj;
  }

  public final void setSymbolFunction(LispObject obj)
  {
    this.function = obj;
  }

  /** See LispObject.getStringValue() */
  @Override
  public String getStringValue()
  {
    return name.getStringValue();
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

  @Override
  public String writeToString()
  {
    final String n = name.getStringValue();
    final LispThread thread = LispThread.currentThread();
    boolean printEscape = (PRINT_ESCAPE.symbolValue(thread) != NIL);
    LispObject printCase = PRINT_CASE.symbolValue(thread);
    final LispObject readtableCase =
      ((Readtable)CURRENT_READTABLE.symbolValue(thread)).getReadtableCase();
    boolean printReadably = (PRINT_READABLY.symbolValue(thread) != NIL);
    if (printReadably)
      {
        if (readtableCase != Keyword.UPCASE ||
            printCase != Keyword.UPCASE)
          {
            StringBuilder sb = new StringBuilder();
            if (pkg == PACKAGE_KEYWORD)
              {
                sb.append(':');
              }
            else if (pkg instanceof Package)
              {
                sb.append(multipleEscape(((Package)pkg).getName()));
                sb.append("::");
              }
            else
              {
                sb.append("#:");
              }
            sb.append(multipleEscape(n));
            return sb.toString();
          }
        else
          printEscape = true;
      }
    if (!printEscape)
      {
        if (pkg == PACKAGE_KEYWORD)
          {
            if (printCase == Keyword.DOWNCASE)
              return n.toLowerCase();
            if (printCase == Keyword.CAPITALIZE)
              return capitalize(n, readtableCase);
            return n;
          }
        // Printer escaping is disabled.
        if (readtableCase == Keyword.UPCASE)
          {
            if (printCase == Keyword.DOWNCASE)
              return n.toLowerCase();
            if (printCase == Keyword.CAPITALIZE)
              return capitalize(n, readtableCase);
            return n;
          }
        else if (readtableCase == Keyword.DOWNCASE)
          {
            // "When the readtable case is :DOWNCASE, uppercase characters
            // are printed in their own case, and lowercase characters are
            // printed in the case specified by *PRINT-CASE*." (22.1.3.3.2)
            if (printCase == Keyword.DOWNCASE)
              return n;
            if (printCase == Keyword.UPCASE)
              return n.toUpperCase();
            if (printCase == Keyword.CAPITALIZE)
              return capitalize(n, readtableCase);
            return n;
          }
        else if (readtableCase == Keyword.PRESERVE)
          {
            return n;
          }
        else // INVERT
          return invert(n);
      }
    // Printer escaping is enabled.
    final boolean escapeSymbolName = needsEscape(n, readtableCase, thread);
    String symbolName = escapeSymbolName ? multipleEscape(n) : n;
    if (!escapeSymbolName)
      {
        if (readtableCase == Keyword.PRESERVE) { }
        else if (readtableCase == Keyword.INVERT)
          symbolName = invert(symbolName);
        else if (printCase == Keyword.DOWNCASE)
          symbolName = symbolName.toLowerCase();
        else if (printCase == Keyword.UPCASE)
          symbolName = symbolName.toUpperCase();
        else if (printCase == Keyword.CAPITALIZE)
          symbolName = capitalize(symbolName, readtableCase);
      }
    if (pkg == NIL)
      {
        if (printReadably || PRINT_GENSYM.symbolValue(thread) != NIL)
          return "#:".concat(symbolName);
        else
          return symbolName;
      }
    if (pkg == PACKAGE_KEYWORD)
      return ":".concat(symbolName);
    // "Package prefixes are printed if necessary." (22.1.3.3.1)
    final Package currentPackage = (Package) _PACKAGE_.symbolValue(thread);
    if (pkg == currentPackage)
      return symbolName;
    if (currentPackage != null && currentPackage.uses(pkg))
      {
        // Check for name conflict in current package.
        if (currentPackage.findExternalSymbol(name) == null)
          if (currentPackage.findInternalSymbol(name) == null)
            if (((Package)pkg).findExternalSymbol(name) != null)
              return symbolName;
      }
    // Has this symbol been imported into the current package?
    if (currentPackage.findExternalSymbol(name) == this)
      return symbolName;
    if (currentPackage.findInternalSymbol(name) == this)
      return symbolName;
    // Package prefix is necessary.
    String packageName = ((Package)pkg).getName();
    final boolean escapePackageName = needsEscape(packageName, readtableCase, thread);
    if (escapePackageName)
      {
        packageName = multipleEscape(packageName);
      }
    else
      {
        if (readtableCase == Keyword.UPCASE)
          {
            if (printCase == Keyword.DOWNCASE)
              packageName = packageName.toLowerCase();
            else if (printCase == Keyword.CAPITALIZE)
              packageName = capitalize(packageName, readtableCase);
          }
        else if (readtableCase == Keyword.DOWNCASE)
          {
            if (printCase == Keyword.UPCASE)
              packageName = packageName.toUpperCase();
            else if (printCase == Keyword.CAPITALIZE)
              packageName = capitalize(packageName, readtableCase);
          }
        else if (readtableCase == Keyword.INVERT)
          {
            packageName = invert(packageName);
          }
      }
    StringBuilder sb = new StringBuilder(packageName);
    if (((Package)pkg).findExternalSymbol(name) != null
        && DOUBLE_COLON_PACKAGE_SEPARATORS.symbolValue(thread) == NIL)
      sb.append(':');
    else
      sb.append("::");
    sb.append(symbolName);
    return sb.toString();
  }

  private static final String invert(String s)
  {
    // "When the readtable case is :INVERT, the case of all alphabetic
    // characters in single case symbol names is inverted. Mixed-case
    // symbol names are printed as is." (22.1.3.3.2)
    final int limit = s.length();
    final int LOWER = 1;
    final int UPPER = 2;
    int state = 0;
    for (int i = 0; i < limit; i++)
      {
        char c = s.charAt(i);
        if (Character.isUpperCase(c))
          {
            if (state == LOWER)
              return s; // Mixed case.
            state = UPPER;
          }
        if (Character.isLowerCase(c))
          {
            if (state == UPPER)
              return s; // Mixed case.
            state = LOWER;
          }
      }
    StringBuilder sb = new StringBuilder(limit);
    for (int i = 0; i < limit; i++)
      {
        char c = s.charAt(i);
        if (Character.isUpperCase(c))
          sb.append(Character.toLowerCase(c));
        else if (Character.isLowerCase(c))
          sb.append(Character.toUpperCase(c));
        else
          sb.append(c);
      }
    return sb.toString();
  }

  private static final boolean needsEscape(String s,
                                           LispObject readtableCase,
                                           LispThread thread)

  {
    boolean escape = false;
    final int length = s.length();
    if (length == 0)
      return true;
    if (s.charAt(0) == '#')
      return true;
    int radix;
    LispObject printBaseBinding = PRINT_BASE.symbolValue(thread); 
    if (printBaseBinding instanceof Fixnum)
      {
        radix = ((Fixnum)printBaseBinding).value;
      }
    else
      {
        error(new TypeError("The value of *PRINT-BASE* is not of type (INTEGER 2 36)."));
        // Not reached.
        return false;
      }
    if (radix < 2 || radix > 36)
      {
        error(new TypeError("The value of *PRINT-BASE* is not of type (INTEGER 2 36)."));
        // Not reached.
        return false;
      }
    boolean seenNonDigit = false;
    for (int i = length; i-- > 0;)
      {
        char c = s.charAt(i);
        if ("(),|\\`'\";:".indexOf(c) >= 0)
          return true;
        if (Character.isWhitespace(c))
          return true;
        if (readtableCase == Keyword.UPCASE)
          {
            if (Character.isLowerCase(c))
              return true;
          }
        else if (readtableCase == Keyword.DOWNCASE)
          {
            if (Character.isUpperCase(c))
              return true;
          }
        if (!escape && !seenNonDigit)
          {
            if (Character.digit(c, radix) < 0)
              seenNonDigit = true;
          }
      }
    if (!seenNonDigit)
      return true;
    if (s.charAt(0) == '.')
      {
        boolean allDots = true;
        for (int i = length; i-- > 1;)
          {
            if (s.charAt(i) != '.')
              {
                allDots = false;
                break;
              }
          }
        if (allDots)
          return true;
      }
    return false;
  }

  private static final String multipleEscape(String s)
  {
    StringBuilder sb = new StringBuilder("|");
    final int limit = s.length();
    for (int i = 0; i < limit; i++)
      {
        char c = s.charAt(i);
        if (c == '|' || c == '\\')
          sb.append('\\');
        sb.append(c);
      }
    sb.append('|');
    return sb.toString();
  }

  private static final String capitalize(String s, LispObject readtableCase)
  {
    if (readtableCase == Keyword.INVERT || readtableCase == Keyword.PRESERVE)
      return s;
    final int limit = s.length();
    StringBuilder sb = new StringBuilder(limit);
    boolean lastCharWasAlphanumeric = false;
    for (int i = 0; i < limit; i++)
      {
        char c = s.charAt(i);
        if (Character.isLowerCase(c))
          {
            if (readtableCase == Keyword.UPCASE)
              sb.append(c);
            else // DOWNCASE
              sb.append(lastCharWasAlphanumeric ? c : LispCharacter.toUpperCase(c));
            lastCharWasAlphanumeric = true;
          }
        else if (Character.isUpperCase(c))
          {
            if (readtableCase == Keyword.UPCASE)
              sb.append(lastCharWasAlphanumeric ? LispCharacter.toLowerCase(c) : c);
            else // DOWNCASE
              sb.append(c);
            lastCharWasAlphanumeric = true;
          }
        else
          {
            sb.append(c);
            lastCharWasAlphanumeric = Character.isDigit(c);
          }
      }
    return sb.toString();
  }

  @Override
  public final int sxhash()
  {
    int h = hash;
    if (h < 0)
      {
        h = name.sxhash();
        hash = h;
      }
    return h;
  }

  @Override
  final public LispObject execute()
  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(NIL);

    return fun.execute();
  }

  @Override
  final public LispObject execute(LispObject arg)
  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(arg));

    return fun.execute(arg);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second));

    return fun.execute(first, second);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third));

    return fun.execute(first, second, third);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third, fourth));

    return fun.execute(first, second, third, fourth);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third, fourth,
                                      fifth));

    return fun.execute(first, second, third, fourth,
                       fifth);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third, fourth,
                                      fifth, sixth));

    return fun.execute(first, second, third, fourth,
                       fifth, sixth);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third, fourth,
                                      fifth, sixth, seventh));

    return fun.execute(first, second, third, fourth,
                       fifth, sixth, seventh);
  }

  @Override
  final public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)

  {
    LispObject fun;
    if ((fun = function) == null)
        return undefinedFunction(list(first, second, third, fourth,
                                      fifth, sixth, seventh, eighth));

    return fun.execute(first, second, third, fourth,
                       fifth, sixth, seventh, eighth);
  }

  @Override
  final public LispObject execute(LispObject[] args)
  {
    LispObject fun;
    if ((fun = function) == null) {
        LispObject list = NIL;
        for (int i = args.length; i-- > 0;)
          list = new Cons(args[i], list);
        return undefinedFunction(list);
    }

    return fun.execute(args);
  }

  private final LispObject undefinedFunction(LispObject args)

  {
    return LispThread.currentThread().execute(Symbol.UNDEFINED_FUNCTION_CALLED,
                                              this, args);
  }

  @Override
  public void incrementCallCount()
  {
    if (function != null)
      function.incrementCallCount();
  }

  @Override
  public void incrementHotCount()
  {
    if (function != null)
      function.incrementHotCount();
  }

    public Object readResolve() throws java.io.ObjectStreamException {
	if(pkg instanceof Package) {
	    Symbol s = ((Package) pkg).intern(name.getStringValue());
	    return s;
	} else {
	    return this;
	}
    }


  // External symbols in CL package.
  public static final Symbol AND_ALLOW_OTHER_KEYS =
    PACKAGE_CL.addExternalSymbol("&ALLOW-OTHER-KEYS");
  public static final Symbol AND_AUX =
    PACKAGE_CL.addExternalSymbol("&AUX");
  public static final Symbol AND_BODY =
    PACKAGE_CL.addExternalSymbol("&BODY");
  public static final Symbol AND_ENVIRONMENT =
    PACKAGE_CL.addExternalSymbol("&ENVIRONMENT");
  public static final Symbol AND_KEY =
    PACKAGE_CL.addExternalSymbol("&KEY");
  public static final Symbol AND_OPTIONAL =
    PACKAGE_CL.addExternalSymbol("&OPTIONAL");
  public static final Symbol AND_REST =
    PACKAGE_CL.addExternalSymbol("&REST");
  public static final Symbol AND_WHOLE =
    PACKAGE_CL.addExternalSymbol("&WHOLE");
  public static final Symbol STAR =
    PACKAGE_CL.addExternalSymbol("*");
  public static final Symbol STAR_STAR =
    PACKAGE_CL.addExternalSymbol("**");
  public static final Symbol STAR_STAR_STAR =
    PACKAGE_CL.addExternalSymbol("***");
  public static final Symbol BREAK_ON_SIGNALS =
    PACKAGE_CL.addExternalSymbol("*BREAK-ON-SIGNALS*");
  public static final Symbol _COMPILE_FILE_PATHNAME_ =
    PACKAGE_CL.addExternalSymbol("*COMPILE-FILE-PATHNAME*");
  public static final Symbol COMPILE_FILE_TRUENAME =
    PACKAGE_CL.addExternalSymbol("*COMPILE-FILE-TRUENAME*");
  public static final Symbol COMPILE_PRINT =
    PACKAGE_CL.addExternalSymbol("*COMPILE-PRINT*");
  public static final Symbol COMPILE_VERBOSE =
    PACKAGE_CL.addExternalSymbol("*COMPILE-VERBOSE*");
  public static final Symbol DEBUG_IO =
    PACKAGE_CL.addExternalSymbol("*DEBUG-IO*");
  public static final Symbol DEBUGGER_HOOK =
    PACKAGE_CL.addExternalSymbol("*DEBUGGER-HOOK*");
  public static final Symbol DEFAULT_PATHNAME_DEFAULTS =
    PACKAGE_CL.addExternalSymbol("*DEFAULT-PATHNAME-DEFAULTS*");
  public static final Symbol ERROR_OUTPUT =
    PACKAGE_CL.addExternalSymbol("*ERROR-OUTPUT*");
  public static final Symbol FEATURES =
    PACKAGE_CL.addExternalSymbol("*FEATURES*");
  public static final Symbol GENSYM_COUNTER =
    PACKAGE_CL.addExternalSymbol("*GENSYM-COUNTER*");
  public static final Symbol LOAD_PATHNAME =
    PACKAGE_CL.addExternalSymbol("*LOAD-PATHNAME*");
  public static final Symbol LOAD_PRINT =
    PACKAGE_CL.addExternalSymbol("*LOAD-PRINT*");
  public static final Symbol LOAD_TRUENAME =
    PACKAGE_CL.addExternalSymbol("*LOAD-TRUENAME*");
  public static final Symbol LOAD_VERBOSE =
    PACKAGE_CL.addExternalSymbol("*LOAD-VERBOSE*");
  public static final Symbol MACROEXPAND_HOOK =
    PACKAGE_CL.addExternalSymbol("*MACROEXPAND-HOOK*");
  public static final Symbol MODULES =
    PACKAGE_CL.addExternalSymbol("*MODULES*");
  public static final Symbol _PACKAGE_ =
    PACKAGE_CL.addExternalSymbol("*PACKAGE*");
  public static final Symbol PRINT_ARRAY =
    PACKAGE_CL.addExternalSymbol("*PRINT-ARRAY*");
  public static final Symbol PRINT_BASE =
    PACKAGE_CL.addExternalSymbol("*PRINT-BASE*");
  public static final Symbol PRINT_CASE =
    PACKAGE_CL.addExternalSymbol("*PRINT-CASE*");
  public static final Symbol PRINT_CIRCLE =
    PACKAGE_CL.addExternalSymbol("*PRINT-CIRCLE*");
  public static final Symbol PRINT_ESCAPE =
    PACKAGE_CL.addExternalSymbol("*PRINT-ESCAPE*");
  public static final Symbol PRINT_GENSYM =
    PACKAGE_CL.addExternalSymbol("*PRINT-GENSYM*");
  public static final Symbol PRINT_LENGTH =
    PACKAGE_CL.addExternalSymbol("*PRINT-LENGTH*");
  public static final Symbol PRINT_LEVEL =
    PACKAGE_CL.addExternalSymbol("*PRINT-LEVEL*");
  public static final Symbol PRINT_LINES =
    PACKAGE_CL.addExternalSymbol("*PRINT-LINES*");
  public static final Symbol PRINT_MISER_WIDTH =
    PACKAGE_CL.addExternalSymbol("*PRINT-MISER-WIDTH*");
  public static final Symbol PRINT_PPRINT_DISPATCH =
    PACKAGE_CL.addExternalSymbol("*PRINT-PPRINT-DISPATCH*");
  public static final Symbol PRINT_PRETTY =
    PACKAGE_CL.addExternalSymbol("*PRINT-PRETTY*");
  public static final Symbol PRINT_RADIX =
    PACKAGE_CL.addExternalSymbol("*PRINT-RADIX*");
  public static final Symbol PRINT_READABLY =
    PACKAGE_CL.addExternalSymbol("*PRINT-READABLY*");
  public static final Symbol PRINT_RIGHT_MARGIN =
    PACKAGE_CL.addExternalSymbol("*PRINT-RIGHT-MARGIN*");
  public static final Symbol QUERY_IO =
    PACKAGE_CL.addExternalSymbol("*QUERY-IO*");
  public static final Symbol _RANDOM_STATE_ =
    PACKAGE_CL.addExternalSymbol("*RANDOM-STATE*");
  public static final Symbol READ_BASE =
    PACKAGE_CL.addExternalSymbol("*READ-BASE*");
  public static final Symbol READ_DEFAULT_FLOAT_FORMAT =
    PACKAGE_CL.addExternalSymbol("*READ-DEFAULT-FLOAT-FORMAT*");
  public static final Symbol READ_EVAL =
    PACKAGE_CL.addExternalSymbol("*READ-EVAL*");
  public static final Symbol READ_SUPPRESS =
    PACKAGE_CL.addExternalSymbol("*READ-SUPPRESS*");
  public static final Symbol CURRENT_READTABLE =
    PACKAGE_CL.addExternalSymbol("*READTABLE*");
  public static final Symbol STANDARD_INPUT =
    PACKAGE_CL.addExternalSymbol("*STANDARD-INPUT*");
  public static final Symbol STANDARD_OUTPUT =
    PACKAGE_CL.addExternalSymbol("*STANDARD-OUTPUT*");
  public static final Symbol TERMINAL_IO =
    PACKAGE_CL.addExternalSymbol("*TERMINAL-IO*");
  public static final Symbol TRACE_OUTPUT =
    PACKAGE_CL.addExternalSymbol("*TRACE-OUTPUT*");
  public static final Symbol PLUS =
    PACKAGE_CL.addExternalSymbol("+");
  public static final Symbol PLUS_PLUS =
    PACKAGE_CL.addExternalSymbol("++");
  public static final Symbol PLUS_PLUS_PLUS =
    PACKAGE_CL.addExternalSymbol("+++");
  public static final Symbol MINUS =
    PACKAGE_CL.addExternalSymbol("-");
  public static final Symbol SLASH =
    PACKAGE_CL.addExternalSymbol("/");
  public static final Symbol SLASH_SLASH =
    PACKAGE_CL.addExternalSymbol("//");
  public static final Symbol SLASH_SLASH_SLASH =
    PACKAGE_CL.addExternalSymbol("///");
  public static final Symbol NOT_EQUALS =
    PACKAGE_CL.addExternalSymbol("/=");
  public static final Symbol ONE_PLUS =
    PACKAGE_CL.addExternalSymbol("1+");
  public static final Symbol ONE_MINUS =
    PACKAGE_CL.addExternalSymbol("1-");
  public static final Symbol LT =
    PACKAGE_CL.addExternalSymbol("<");
  public static final Symbol LE =
    PACKAGE_CL.addExternalSymbol("<=");
  public static final Symbol EQUALS =
    PACKAGE_CL.addExternalSymbol("=");
  public static final Symbol GT =
    PACKAGE_CL.addExternalSymbol(">");
  public static final Symbol GE =
    PACKAGE_CL.addExternalSymbol(">=");
  public static final Symbol ABORT =
    PACKAGE_CL.addExternalSymbol("ABORT");
  public static final Symbol ABS =
    PACKAGE_CL.addExternalSymbol("ABS");
  public static final Symbol ACONS =
    PACKAGE_CL.addExternalSymbol("ACONS");
  public static final Symbol ACOS =
    PACKAGE_CL.addExternalSymbol("ACOS");
  public static final Symbol ACOSH =
    PACKAGE_CL.addExternalSymbol("ACOSH");
  public static final Symbol ADD_METHOD =
    PACKAGE_CL.addExternalSymbol("ADD-METHOD");
  public static final Symbol ADJOIN =
    PACKAGE_CL.addExternalSymbol("ADJOIN");
  public static final Symbol ADJUST_ARRAY =
    PACKAGE_CL.addExternalSymbol("ADJUST-ARRAY");
  public static final Symbol ADJUSTABLE_ARRAY_P =
    PACKAGE_CL.addExternalSymbol("ADJUSTABLE-ARRAY-P");
  public static final Symbol ALLOCATE_INSTANCE =
    PACKAGE_CL.addExternalSymbol("ALLOCATE-INSTANCE");
  public static final Symbol ALPHA_CHAR_P =
    PACKAGE_CL.addExternalSymbol("ALPHA-CHAR-P");
  public static final Symbol ALPHANUMERICP =
    PACKAGE_CL.addExternalSymbol("ALPHANUMERICP");
  public static final Symbol AND =
    PACKAGE_CL.addExternalSymbol("AND");
  public static final Symbol APPEND =
    PACKAGE_CL.addExternalSymbol("APPEND");
  public static final Symbol APPLY =
    PACKAGE_CL.addExternalSymbol("APPLY");
  public static final Symbol APROPOS =
    PACKAGE_CL.addExternalSymbol("APROPOS");
  public static final Symbol APROPOS_LIST =
    PACKAGE_CL.addExternalSymbol("APROPOS-LIST");
  public static final Symbol AREF =
    PACKAGE_CL.addExternalSymbol("AREF");
  public static final Symbol ARITHMETIC_ERROR =
    PACKAGE_CL.addExternalSymbol("ARITHMETIC-ERROR");
  public static final Symbol ARITHMETIC_ERROR_OPERANDS =
    PACKAGE_CL.addExternalSymbol("ARITHMETIC-ERROR-OPERANDS");
  public static final Symbol ARITHMETIC_ERROR_OPERATION =
    PACKAGE_CL.addExternalSymbol("ARITHMETIC-ERROR-OPERATION");
  public static final Symbol ARRAY =
    PACKAGE_CL.addExternalSymbol("ARRAY");
  public static final Symbol ARRAY_DIMENSION =
    PACKAGE_CL.addExternalSymbol("ARRAY-DIMENSION");
  public static final Symbol ARRAY_DIMENSION_LIMIT =
    PACKAGE_CL.addExternalSymbol("ARRAY-DIMENSION-LIMIT");
  public static final Symbol ARRAY_DIMENSIONS =
    PACKAGE_CL.addExternalSymbol("ARRAY-DIMENSIONS");
  public static final Symbol ARRAY_DISPLACEMENT =
    PACKAGE_CL.addExternalSymbol("ARRAY-DISPLACEMENT");
  public static final Symbol ARRAY_ELEMENT_TYPE =
    PACKAGE_CL.addExternalSymbol("ARRAY-ELEMENT-TYPE");
  public static final Symbol ARRAY_HAS_FILL_POINTER_P =
    PACKAGE_CL.addExternalSymbol("ARRAY-HAS-FILL-POINTER-P");
  public static final Symbol ARRAY_IN_BOUNDS_P =
    PACKAGE_CL.addExternalSymbol("ARRAY-IN-BOUNDS-P");
  public static final Symbol ARRAY_RANK =
    PACKAGE_CL.addExternalSymbol("ARRAY-RANK");
  public static final Symbol ARRAY_RANK_LIMIT =
    PACKAGE_CL.addExternalSymbol("ARRAY-RANK-LIMIT");
  public static final Symbol ARRAY_ROW_MAJOR_INDEX =
    PACKAGE_CL.addExternalSymbol("ARRAY-ROW-MAJOR-INDEX");
  public static final Symbol ARRAY_TOTAL_SIZE =
    PACKAGE_CL.addExternalSymbol("ARRAY-TOTAL-SIZE");
  public static final Symbol ARRAY_TOTAL_SIZE_LIMIT =
    PACKAGE_CL.addExternalSymbol("ARRAY-TOTAL-SIZE-LIMIT");
  public static final Symbol ARRAYP =
    PACKAGE_CL.addExternalSymbol("ARRAYP");
  public static final Symbol ASH =
    PACKAGE_CL.addExternalSymbol("ASH");
  public static final Symbol ASIN =
    PACKAGE_CL.addExternalSymbol("ASIN");
  public static final Symbol ASINH =
    PACKAGE_CL.addExternalSymbol("ASINH");
  public static final Symbol ASSERT =
    PACKAGE_CL.addExternalSymbol("ASSERT");
  public static final Symbol ASSOC =
    PACKAGE_CL.addExternalSymbol("ASSOC");
  public static final Symbol ASSOC_IF =
    PACKAGE_CL.addExternalSymbol("ASSOC-IF");
  public static final Symbol ASSOC_IF_NOT =
    PACKAGE_CL.addExternalSymbol("ASSOC-IF-NOT");
  public static final Symbol ATAN =
    PACKAGE_CL.addExternalSymbol("ATAN");
  public static final Symbol ATANH =
    PACKAGE_CL.addExternalSymbol("ATANH");
  public static final Symbol ATOM =
    PACKAGE_CL.addExternalSymbol("ATOM");
  public static final Symbol BASE_CHAR =
    PACKAGE_CL.addExternalSymbol("BASE-CHAR");
  public static final Symbol BASE_STRING =
    PACKAGE_CL.addExternalSymbol("BASE-STRING");
  public static final Symbol BIGNUM =
    PACKAGE_CL.addExternalSymbol("BIGNUM");
  public static final Symbol BIT =
    PACKAGE_CL.addExternalSymbol("BIT");
  public static final Symbol BIT_AND =
    PACKAGE_CL.addExternalSymbol("BIT-AND");
  public static final Symbol BIT_ANDC1 =
    PACKAGE_CL.addExternalSymbol("BIT-ANDC1");
  public static final Symbol BIT_ANDC2 =
    PACKAGE_CL.addExternalSymbol("BIT-ANDC2");
  public static final Symbol BIT_EQV =
    PACKAGE_CL.addExternalSymbol("BIT-EQV");
  public static final Symbol BIT_IOR =
    PACKAGE_CL.addExternalSymbol("BIT-IOR");
  public static final Symbol BIT_NAND =
    PACKAGE_CL.addExternalSymbol("BIT-NAND");
  public static final Symbol BIT_NOR =
    PACKAGE_CL.addExternalSymbol("BIT-NOR");
  public static final Symbol BIT_NOT =
    PACKAGE_CL.addExternalSymbol("BIT-NOT");
  public static final Symbol BIT_ORC1 =
    PACKAGE_CL.addExternalSymbol("BIT-ORC1");
  public static final Symbol BIT_ORC2 =
    PACKAGE_CL.addExternalSymbol("BIT-ORC2");
  public static final Symbol BIT_VECTOR =
    PACKAGE_CL.addExternalSymbol("BIT-VECTOR");
  public static final Symbol BIT_VECTOR_P =
    PACKAGE_CL.addExternalSymbol("BIT-VECTOR-P");
  public static final Symbol BIT_XOR =
    PACKAGE_CL.addExternalSymbol("BIT-XOR");
  public static final Symbol BLOCK =
    PACKAGE_CL.addExternalSymbol("BLOCK");
  public static final Symbol BOOLE =
    PACKAGE_CL.addExternalSymbol("BOOLE");
  public static final Symbol BOOLE_1 =
    PACKAGE_CL.addExternalSymbol("BOOLE-1");
  public static final Symbol BOOLE_2 =
    PACKAGE_CL.addExternalSymbol("BOOLE-2");
  public static final Symbol BOOLE_AND =
    PACKAGE_CL.addExternalSymbol("BOOLE-AND");
  public static final Symbol BOOLE_ANDC1 =
    PACKAGE_CL.addExternalSymbol("BOOLE-ANDC1");
  public static final Symbol BOOLE_ANDC2 =
    PACKAGE_CL.addExternalSymbol("BOOLE-ANDC2");
  public static final Symbol BOOLE_C1 =
    PACKAGE_CL.addExternalSymbol("BOOLE-C1");
  public static final Symbol BOOLE_C2 =
    PACKAGE_CL.addExternalSymbol("BOOLE-C2");
  public static final Symbol BOOLE_CLR =
    PACKAGE_CL.addExternalSymbol("BOOLE-CLR");
  public static final Symbol BOOLE_EQV =
    PACKAGE_CL.addExternalSymbol("BOOLE-EQV");
  public static final Symbol BOOLE_IOR =
    PACKAGE_CL.addExternalSymbol("BOOLE-IOR");
  public static final Symbol BOOLE_NAND =
    PACKAGE_CL.addExternalSymbol("BOOLE-NAND");
  public static final Symbol BOOLE_NOR =
    PACKAGE_CL.addExternalSymbol("BOOLE-NOR");
  public static final Symbol BOOLE_ORC1 =
    PACKAGE_CL.addExternalSymbol("BOOLE-ORC1");
  public static final Symbol BOOLE_ORC2 =
    PACKAGE_CL.addExternalSymbol("BOOLE-ORC2");
  public static final Symbol BOOLE_SET =
    PACKAGE_CL.addExternalSymbol("BOOLE-SET");
  public static final Symbol BOOLE_XOR =
    PACKAGE_CL.addExternalSymbol("BOOLE-XOR");
  public static final Symbol BOOLEAN =
    PACKAGE_CL.addExternalSymbol("BOOLEAN");
  public static final Symbol BOTH_CASE_P =
    PACKAGE_CL.addExternalSymbol("BOTH-CASE-P");
  public static final Symbol BOUNDP =
    PACKAGE_CL.addExternalSymbol("BOUNDP");
  public static final Symbol BREAK =
    PACKAGE_CL.addExternalSymbol("BREAK");
  public static final Symbol BROADCAST_STREAM =
    PACKAGE_CL.addExternalSymbol("BROADCAST-STREAM");
  public static final Symbol BROADCAST_STREAM_STREAMS =
    PACKAGE_CL.addExternalSymbol("BROADCAST-STREAM-STREAMS");
  public static final Symbol BUILT_IN_CLASS =
    PACKAGE_CL.addExternalSymbol("BUILT-IN-CLASS");
  public static final Symbol BUTLAST =
    PACKAGE_CL.addExternalSymbol("BUTLAST");
  public static final Symbol BYTE =
    PACKAGE_CL.addExternalSymbol("BYTE");
  public static final Symbol BYTE_POSITION =
    PACKAGE_CL.addExternalSymbol("BYTE-POSITION");
  public static final Symbol BYTE_SIZE =
    PACKAGE_CL.addExternalSymbol("BYTE-SIZE");
  public static final Symbol CAAAAR =
    PACKAGE_CL.addExternalSymbol("CAAAAR");
  public static final Symbol CAAADR =
    PACKAGE_CL.addExternalSymbol("CAAADR");
  public static final Symbol CAAAR =
    PACKAGE_CL.addExternalSymbol("CAAAR");
  public static final Symbol CAADAR =
    PACKAGE_CL.addExternalSymbol("CAADAR");
  public static final Symbol CAADDR =
    PACKAGE_CL.addExternalSymbol("CAADDR");
  public static final Symbol CAADR =
    PACKAGE_CL.addExternalSymbol("CAADR");
  public static final Symbol CAAR =
    PACKAGE_CL.addExternalSymbol("CAAR");
  public static final Symbol CADAAR =
    PACKAGE_CL.addExternalSymbol("CADAAR");
  public static final Symbol CADADR =
    PACKAGE_CL.addExternalSymbol("CADADR");
  public static final Symbol CADAR =
    PACKAGE_CL.addExternalSymbol("CADAR");
  public static final Symbol CADDAR =
    PACKAGE_CL.addExternalSymbol("CADDAR");
  public static final Symbol CADDDR =
    PACKAGE_CL.addExternalSymbol("CADDDR");
  public static final Symbol CADDR =
    PACKAGE_CL.addExternalSymbol("CADDR");
  public static final Symbol CADR =
    PACKAGE_CL.addExternalSymbol("CADR");
  public static final Symbol CALL_ARGUMENTS_LIMIT =
    PACKAGE_CL.addExternalSymbol("CALL-ARGUMENTS-LIMIT");
  public static final Symbol CALL_METHOD =
    PACKAGE_CL.addExternalSymbol("CALL-METHOD");
  public static final Symbol CALL_NEXT_METHOD =
    PACKAGE_CL.addExternalSymbol("CALL-NEXT-METHOD");
  public static final Symbol CAR =
    PACKAGE_CL.addExternalSymbol("CAR");
  public static final Symbol CASE =
    PACKAGE_CL.addExternalSymbol("CASE");
  public static final Symbol CATCH =
    PACKAGE_CL.addExternalSymbol("CATCH");
  public static final Symbol CCASE =
    PACKAGE_CL.addExternalSymbol("CCASE");
  public static final Symbol CDAAAR =
    PACKAGE_CL.addExternalSymbol("CDAAAR");
  public static final Symbol CDAADR =
    PACKAGE_CL.addExternalSymbol("CDAADR");
  public static final Symbol CDAAR =
    PACKAGE_CL.addExternalSymbol("CDAAR");
  public static final Symbol CDADAR =
    PACKAGE_CL.addExternalSymbol("CDADAR");
  public static final Symbol CDADDR =
    PACKAGE_CL.addExternalSymbol("CDADDR");
  public static final Symbol CDADR =
    PACKAGE_CL.addExternalSymbol("CDADR");
  public static final Symbol CDAR =
    PACKAGE_CL.addExternalSymbol("CDAR");
  public static final Symbol CDDAAR =
    PACKAGE_CL.addExternalSymbol("CDDAAR");
  public static final Symbol CDDADR =
    PACKAGE_CL.addExternalSymbol("CDDADR");
  public static final Symbol CDDAR =
    PACKAGE_CL.addExternalSymbol("CDDAR");
  public static final Symbol CDDDAR =
    PACKAGE_CL.addExternalSymbol("CDDDAR");
  public static final Symbol CDDDDR =
    PACKAGE_CL.addExternalSymbol("CDDDDR");
  public static final Symbol CDDDR =
    PACKAGE_CL.addExternalSymbol("CDDDR");
  public static final Symbol CDDR =
    PACKAGE_CL.addExternalSymbol("CDDR");
  public static final Symbol CDR =
    PACKAGE_CL.addExternalSymbol("CDR");
  public static final Symbol CEILING =
    PACKAGE_CL.addExternalSymbol("CEILING");
  public static final Symbol CELL_ERROR =
    PACKAGE_CL.addExternalSymbol("CELL-ERROR");
  public static final Symbol CELL_ERROR_NAME =
    PACKAGE_CL.addExternalSymbol("CELL-ERROR-NAME");
  public static final Symbol CERROR =
    PACKAGE_CL.addExternalSymbol("CERROR");
  public static final Symbol CHANGE_CLASS =
    PACKAGE_CL.addExternalSymbol("CHANGE-CLASS");
  public static final Symbol CHAR =
    PACKAGE_CL.addExternalSymbol("CHAR");
  public static final Symbol CHAR_CODE =
    PACKAGE_CL.addExternalSymbol("CHAR-CODE");
  public static final Symbol CHAR_CODE_LIMIT =
    PACKAGE_CL.addExternalSymbol("CHAR-CODE-LIMIT");
  public static final Symbol CHAR_DOWNCASE =
    PACKAGE_CL.addExternalSymbol("CHAR-DOWNCASE");
  public static final Symbol CHAR_EQUAL =
    PACKAGE_CL.addExternalSymbol("CHAR-EQUAL");
  public static final Symbol CHAR_GREATERP =
    PACKAGE_CL.addExternalSymbol("CHAR-GREATERP");
  public static final Symbol CHAR_INT =
    PACKAGE_CL.addExternalSymbol("CHAR-INT");
  public static final Symbol CHAR_LESSP =
    PACKAGE_CL.addExternalSymbol("CHAR-LESSP");
  public static final Symbol CHAR_NAME =
    PACKAGE_CL.addExternalSymbol("CHAR-NAME");
  public static final Symbol CHAR_NOT_EQUAL =
    PACKAGE_CL.addExternalSymbol("CHAR-NOT-EQUAL");
  public static final Symbol CHAR_NOT_GREATERP =
    PACKAGE_CL.addExternalSymbol("CHAR-NOT-GREATERP");
  public static final Symbol CHAR_NOT_LESSP =
    PACKAGE_CL.addExternalSymbol("CHAR-NOT-LESSP");
  public static final Symbol CHAR_UPCASE =
    PACKAGE_CL.addExternalSymbol("CHAR-UPCASE");
  public static final Symbol CHAR_NE =
    PACKAGE_CL.addExternalSymbol("CHAR/=");
  public static final Symbol CHAR_LT =
    PACKAGE_CL.addExternalSymbol("CHAR<");
  public static final Symbol CHAR_LE =
    PACKAGE_CL.addExternalSymbol("CHAR<=");
  public static final Symbol CHAR_EQUALS =
    PACKAGE_CL.addExternalSymbol("CHAR=");
  public static final Symbol CHAR_GT =
    PACKAGE_CL.addExternalSymbol("CHAR>");
  public static final Symbol CHAR_GE =
    PACKAGE_CL.addExternalSymbol("CHAR>=");
  public static final Symbol CHARACTER =
    PACKAGE_CL.addExternalSymbol("CHARACTER");
  public static final Symbol CHARACTERP =
    PACKAGE_CL.addExternalSymbol("CHARACTERP");
  public static final Symbol CHECK_TYPE =
    PACKAGE_CL.addExternalSymbol("CHECK-TYPE");
  public static final Symbol CIS =
    PACKAGE_CL.addExternalSymbol("CIS");
  public static final Symbol CLASS =
    PACKAGE_CL.addExternalSymbol("CLASS");
  public static final Symbol CLASS_NAME =
    PACKAGE_CL.addExternalSymbol("CLASS-NAME");
  public static final Symbol CLASS_OF =
    PACKAGE_CL.addExternalSymbol("CLASS-OF");
  public static final Symbol CLEAR_INPUT =
    PACKAGE_CL.addExternalSymbol("CLEAR-INPUT");
  public static final Symbol CLEAR_OUTPUT =
    PACKAGE_CL.addExternalSymbol("CLEAR-OUTPUT");
  public static final Symbol CLOSE =
    PACKAGE_CL.addExternalSymbol("CLOSE");
  public static final Symbol CLRHASH =
    PACKAGE_CL.addExternalSymbol("CLRHASH");
  public static final Symbol CODE_CHAR =
    PACKAGE_CL.addExternalSymbol("CODE-CHAR");
  public static final Symbol COERCE =
    PACKAGE_CL.addExternalSymbol("COERCE");
  public static final Symbol COMPILATION_SPEED =
    PACKAGE_CL.addExternalSymbol("COMPILATION-SPEED");
  public static final Symbol COMPILE =
    PACKAGE_CL.addExternalSymbol("COMPILE");
  public static final Symbol COMPILE_FILE =
    PACKAGE_CL.addExternalSymbol("COMPILE-FILE");
  public static final Symbol COMPILE_FILE_PATHNAME =
    PACKAGE_CL.addExternalSymbol("COMPILE-FILE-PATHNAME");
  public static final Symbol COMPILED_FUNCTION =
    PACKAGE_CL.addExternalSymbol("COMPILED-FUNCTION");
  public static final Symbol COMPILED_FUNCTION_P =
    PACKAGE_CL.addExternalSymbol("COMPILED-FUNCTION-P");
  public static final Symbol COMPILER_MACRO =
    PACKAGE_CL.addExternalSymbol("COMPILER-MACRO");
  public static final Symbol COMPILER_MACRO_FUNCTION =
    PACKAGE_CL.addExternalSymbol("COMPILER-MACRO-FUNCTION");
  public static final Symbol COMPLEMENT =
    PACKAGE_CL.addExternalSymbol("COMPLEMENT");
  public static final Symbol COMPLEX =
    PACKAGE_CL.addExternalSymbol("COMPLEX");
  public static final Symbol COMPLEXP =
    PACKAGE_CL.addExternalSymbol("COMPLEXP");
  public static final Symbol COMPUTE_APPLICABLE_METHODS =
    PACKAGE_CL.addExternalSymbol("COMPUTE-APPLICABLE-METHODS");
  public static final Symbol COMPUTE_RESTARTS =
    PACKAGE_CL.addExternalSymbol("COMPUTE-RESTARTS");
  public static final Symbol CONCATENATE =
    PACKAGE_CL.addExternalSymbol("CONCATENATE");
  public static final Symbol CONCATENATED_STREAM =
    PACKAGE_CL.addExternalSymbol("CONCATENATED-STREAM");
  public static final Symbol CONCATENATED_STREAM_STREAMS =
    PACKAGE_CL.addExternalSymbol("CONCATENATED-STREAM-STREAMS");
  public static final Symbol COND =
    PACKAGE_CL.addExternalSymbol("COND");
  public static final Symbol CONDITION =
    PACKAGE_CL.addExternalSymbol("CONDITION");
  public static final Symbol CONJUGATE =
    PACKAGE_CL.addExternalSymbol("CONJUGATE");
  public static final Symbol CONS =
    PACKAGE_CL.addExternalSymbol("CONS");
  public static final Symbol CONSP =
    PACKAGE_CL.addExternalSymbol("CONSP");
  public static final Symbol CONSTANTLY =
    PACKAGE_CL.addExternalSymbol("CONSTANTLY");
  public static final Symbol CONSTANTP =
    PACKAGE_CL.addExternalSymbol("CONSTANTP");
  public static final Symbol CONTINUE =
    PACKAGE_CL.addExternalSymbol("CONTINUE");
  public static final Symbol CONTROL_ERROR =
    PACKAGE_CL.addExternalSymbol("CONTROL-ERROR");
  public static final Symbol COPY_ALIST =
    PACKAGE_CL.addExternalSymbol("COPY-ALIST");
  public static final Symbol COPY_LIST =
    PACKAGE_CL.addExternalSymbol("COPY-LIST");
  public static final Symbol COPY_PPRINT_DISPATCH =
    PACKAGE_CL.addExternalSymbol("COPY-PPRINT-DISPATCH");
  public static final Symbol COPY_READTABLE =
    PACKAGE_CL.addExternalSymbol("COPY-READTABLE");
  public static final Symbol COPY_SEQ =
    PACKAGE_CL.addExternalSymbol("COPY-SEQ");
  public static final Symbol COPY_STRUCTURE =
    PACKAGE_CL.addExternalSymbol("COPY-STRUCTURE");
  public static final Symbol COPY_SYMBOL =
    PACKAGE_CL.addExternalSymbol("COPY-SYMBOL");
  public static final Symbol COPY_TREE =
    PACKAGE_CL.addExternalSymbol("COPY-TREE");
  public static final Symbol COS =
    PACKAGE_CL.addExternalSymbol("COS");
  public static final Symbol COSH =
    PACKAGE_CL.addExternalSymbol("COSH");
  public static final Symbol COUNT =
    PACKAGE_CL.addExternalSymbol("COUNT");
  public static final Symbol COUNT_IF =
    PACKAGE_CL.addExternalSymbol("COUNT-IF");
  public static final Symbol COUNT_IF_NOT =
    PACKAGE_CL.addExternalSymbol("COUNT-IF-NOT");
  public static final Symbol CTYPECASE =
    PACKAGE_CL.addExternalSymbol("CTYPECASE");
  public static final Symbol DEBUG =
    PACKAGE_CL.addExternalSymbol("DEBUG");
  public static final Symbol DECF =
    PACKAGE_CL.addExternalSymbol("DECF");
  public static final Symbol DECLAIM =
    PACKAGE_CL.addExternalSymbol("DECLAIM");
  public static final Symbol DECLARATION =
    PACKAGE_CL.addExternalSymbol("DECLARATION");
  public static final Symbol DECLARE =
    PACKAGE_CL.addExternalSymbol("DECLARE");
  public static final Symbol DECODE_FLOAT =
    PACKAGE_CL.addExternalSymbol("DECODE-FLOAT");
  public static final Symbol DECODE_UNIVERSAL_TIME =
    PACKAGE_CL.addExternalSymbol("DECODE-UNIVERSAL-TIME");
  public static final Symbol DEFCLASS =
    PACKAGE_CL.addExternalSymbol("DEFCLASS");
  public static final Symbol DEFCONSTANT =
    PACKAGE_CL.addExternalSymbol("DEFCONSTANT");
  public static final Symbol DEFGENERIC =
    PACKAGE_CL.addExternalSymbol("DEFGENERIC");
  public static final Symbol DEFINE_COMPILER_MACRO =
    PACKAGE_CL.addExternalSymbol("DEFINE-COMPILER-MACRO");
  public static final Symbol DEFINE_CONDITION =
    PACKAGE_CL.addExternalSymbol("DEFINE-CONDITION");
  public static final Symbol DEFINE_METHOD_COMBINATION =
    PACKAGE_CL.addExternalSymbol("DEFINE-METHOD-COMBINATION");
  public static final Symbol DEFINE_MODIFY_MACRO =
    PACKAGE_CL.addExternalSymbol("DEFINE-MODIFY-MACRO");
  public static final Symbol DEFINE_SETF_EXPANDER =
    PACKAGE_CL.addExternalSymbol("DEFINE-SETF-EXPANDER");
  public static final Symbol DEFINE_SYMBOL_MACRO =
    PACKAGE_CL.addExternalSymbol("DEFINE-SYMBOL-MACRO");
  public static final Symbol DEFMACRO =
    PACKAGE_CL.addExternalSymbol("DEFMACRO");
  public static final Symbol DEFMETHOD =
    PACKAGE_CL.addExternalSymbol("DEFMETHOD");
  public static final Symbol DEFPACKAGE =
    PACKAGE_CL.addExternalSymbol("DEFPACKAGE");
  public static final Symbol DEFPARAMETER =
    PACKAGE_CL.addExternalSymbol("DEFPARAMETER");
  public static final Symbol DEFSETF =
    PACKAGE_CL.addExternalSymbol("DEFSETF");
  public static final Symbol DEFSTRUCT =
    PACKAGE_CL.addExternalSymbol("DEFSTRUCT");
  public static final Symbol DEFTYPE =
    PACKAGE_CL.addExternalSymbol("DEFTYPE");
  public static final Symbol DEFUN =
    PACKAGE_CL.addExternalSymbol("DEFUN");
  public static final Symbol DEFVAR =
    PACKAGE_CL.addExternalSymbol("DEFVAR");
  public static final Symbol DELETE =
    PACKAGE_CL.addExternalSymbol("DELETE");
  public static final Symbol DELETE_DUPLICATES =
    PACKAGE_CL.addExternalSymbol("DELETE-DUPLICATES");
  public static final Symbol DELETE_FILE =
    PACKAGE_CL.addExternalSymbol("DELETE-FILE");
  public static final Symbol DELETE_IF =
    PACKAGE_CL.addExternalSymbol("DELETE-IF");
  public static final Symbol DELETE_IF_NOT =
    PACKAGE_CL.addExternalSymbol("DELETE-IF-NOT");
  public static final Symbol DELETE_PACKAGE =
    PACKAGE_CL.addExternalSymbol("DELETE-PACKAGE");
  public static final Symbol DENOMINATOR =
    PACKAGE_CL.addExternalSymbol("DENOMINATOR");
  public static final Symbol DEPOSIT_FIELD =
    PACKAGE_CL.addExternalSymbol("DEPOSIT-FIELD");
  public static final Symbol DESCRIBE =
    PACKAGE_CL.addExternalSymbol("DESCRIBE");
  public static final Symbol DESCRIBE_OBJECT =
    PACKAGE_CL.addExternalSymbol("DESCRIBE-OBJECT");
  public static final Symbol DESTRUCTURING_BIND =
    PACKAGE_CL.addExternalSymbol("DESTRUCTURING-BIND");
  public static final Symbol DIGIT_CHAR =
    PACKAGE_CL.addExternalSymbol("DIGIT-CHAR");
  public static final Symbol DIGIT_CHAR_P =
    PACKAGE_CL.addExternalSymbol("DIGIT-CHAR-P");
  public static final Symbol DIRECTORY =
    PACKAGE_CL.addExternalSymbol("DIRECTORY");
  public static final Symbol DIRECTORY_NAMESTRING =
    PACKAGE_CL.addExternalSymbol("DIRECTORY-NAMESTRING");
  public static final Symbol DISASSEMBLE =
    PACKAGE_CL.addExternalSymbol("DISASSEMBLE");
  public static final Symbol DIVISION_BY_ZERO =
    PACKAGE_CL.addExternalSymbol("DIVISION-BY-ZERO");
  public static final Symbol DO =
    PACKAGE_CL.addExternalSymbol("DO");
  public static final Symbol DO_STAR =
    PACKAGE_CL.addExternalSymbol("DO*");
  public static final Symbol DO_ALL_SYMBOLS =
    PACKAGE_CL.addExternalSymbol("DO-ALL-SYMBOLS");
  public static final Symbol DO_EXTERNAL_SYMBOLS =
    PACKAGE_CL.addExternalSymbol("DO-EXTERNAL-SYMBOLS");
  public static final Symbol DO_SYMBOLS =
    PACKAGE_CL.addExternalSymbol("DO-SYMBOLS");
  public static final Symbol DOCUMENTATION =
    PACKAGE_CL.addExternalSymbol("DOCUMENTATION");
  public static final Symbol DOLIST =
    PACKAGE_CL.addExternalSymbol("DOLIST");
  public static final Symbol DOTIMES =
    PACKAGE_CL.addExternalSymbol("DOTIMES");
  public static final Symbol DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("DOUBLE-FLOAT");
  public static final Symbol DOUBLE_FLOAT_EPSILON =
    PACKAGE_CL.addExternalSymbol("DOUBLE-FLOAT-EPSILON");
  public static final Symbol DOUBLE_FLOAT_NEGATIVE_EPSILON =
    PACKAGE_CL.addExternalSymbol("DOUBLE-FLOAT-NEGATIVE-EPSILON");
  public static final Symbol DPB =
    PACKAGE_CL.addExternalSymbol("DPB");
  public static final Symbol DRIBBLE =
    PACKAGE_CL.addExternalSymbol("DRIBBLE");
  public static final Symbol DYNAMIC_EXTENT =
    PACKAGE_CL.addExternalSymbol("DYNAMIC-EXTENT");
  public static final Symbol ECASE =
    PACKAGE_CL.addExternalSymbol("ECASE");
  public static final Symbol ECHO_STREAM =
    PACKAGE_CL.addExternalSymbol("ECHO-STREAM");
  public static final Symbol ECHO_STREAM_INPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("ECHO-STREAM-INPUT-STREAM");
  public static final Symbol ECHO_STREAM_OUTPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("ECHO-STREAM-OUTPUT-STREAM");
  public static final Symbol ED =
    PACKAGE_CL.addExternalSymbol("ED");
  public static final Symbol EIGHTH =
    PACKAGE_CL.addExternalSymbol("EIGHTH");
  public static final Symbol ELT =
    PACKAGE_CL.addExternalSymbol("ELT");
  public static final Symbol ENCODE_UNIVERSAL_TIME =
    PACKAGE_CL.addExternalSymbol("ENCODE-UNIVERSAL-TIME");
  public static final Symbol END_OF_FILE =
    PACKAGE_CL.addExternalSymbol("END-OF-FILE");
  public static final Symbol ENDP =
    PACKAGE_CL.addExternalSymbol("ENDP");
  public static final Symbol ENOUGH_NAMESTRING =
    PACKAGE_CL.addExternalSymbol("ENOUGH-NAMESTRING");
  public static final Symbol ENSURE_DIRECTORIES_EXIST =
    PACKAGE_CL.addExternalSymbol("ENSURE-DIRECTORIES-EXIST");
  public static final Symbol ENSURE_GENERIC_FUNCTION =
    PACKAGE_CL.addExternalSymbol("ENSURE-GENERIC-FUNCTION");
  public static final Symbol EQ =
    PACKAGE_CL.addExternalSymbol("EQ");
  public static final Symbol EQL =
    PACKAGE_CL.addExternalSymbol("EQL");
  public static final Symbol EQUAL =
    PACKAGE_CL.addExternalSymbol("EQUAL");
  public static final Symbol EQUALP =
    PACKAGE_CL.addExternalSymbol("EQUALP");
  public static final Symbol ERROR =
    PACKAGE_CL.addExternalSymbol("ERROR");
  public static final Symbol ETYPECASE =
    PACKAGE_CL.addExternalSymbol("ETYPECASE");
  public static final Symbol EVAL =
    PACKAGE_CL.addExternalSymbol("EVAL");
  public static final Symbol EVAL_WHEN =
    PACKAGE_CL.addExternalSymbol("EVAL-WHEN");
  public static final Symbol EVENP =
    PACKAGE_CL.addExternalSymbol("EVENP");
  public static final Symbol EVERY =
    PACKAGE_CL.addExternalSymbol("EVERY");
  public static final Symbol EXP =
    PACKAGE_CL.addExternalSymbol("EXP");
  public static final Symbol EXPORT =
    PACKAGE_CL.addExternalSymbol("EXPORT");
  public static final Symbol EXPT =
    PACKAGE_CL.addExternalSymbol("EXPT");
  public static final Symbol EXTENDED_CHAR =
    PACKAGE_CL.addExternalSymbol("EXTENDED-CHAR");
  public static final Symbol FBOUNDP =
    PACKAGE_CL.addExternalSymbol("FBOUNDP");
  public static final Symbol FCEILING =
    PACKAGE_CL.addExternalSymbol("FCEILING");
  public static final Symbol FDEFINITION =
    PACKAGE_CL.addExternalSymbol("FDEFINITION");
  public static final Symbol FFLOOR =
    PACKAGE_CL.addExternalSymbol("FFLOOR");
  public static final Symbol FIFTH =
    PACKAGE_CL.addExternalSymbol("FIFTH");
  public static final Symbol FILE_AUTHOR =
    PACKAGE_CL.addExternalSymbol("FILE-AUTHOR");
  public static final Symbol FILE_ERROR =
    PACKAGE_CL.addExternalSymbol("FILE-ERROR");
  public static final Symbol FILE_ERROR_PATHNAME =
    PACKAGE_CL.addExternalSymbol("FILE-ERROR-PATHNAME");
  public static final Symbol FILE_LENGTH =
    PACKAGE_CL.addExternalSymbol("FILE-LENGTH");
  public static final Symbol FILE_NAMESTRING =
    PACKAGE_CL.addExternalSymbol("FILE-NAMESTRING");
  public static final Symbol FILE_POSITION =
    PACKAGE_CL.addExternalSymbol("FILE-POSITION");
  public static final Symbol FILE_STREAM =
    PACKAGE_CL.addExternalSymbol("FILE-STREAM");
  public static final Symbol FILE_STRING_LENGTH =
    PACKAGE_CL.addExternalSymbol("FILE-STRING-LENGTH");
  public static final Symbol FILE_WRITE_DATE =
    PACKAGE_CL.addExternalSymbol("FILE-WRITE-DATE");
  public static final Symbol FILL =
    PACKAGE_CL.addExternalSymbol("FILL");
  public static final Symbol FILL_POINTER =
    PACKAGE_CL.addExternalSymbol("FILL-POINTER");
  public static final Symbol FIND =
    PACKAGE_CL.addExternalSymbol("FIND");
  public static final Symbol FIND_ALL_SYMBOLS =
    PACKAGE_CL.addExternalSymbol("FIND-ALL-SYMBOLS");
  public static final Symbol FIND_CLASS =
    PACKAGE_CL.addExternalSymbol("FIND-CLASS");
  public static final Symbol FIND_IF =
    PACKAGE_CL.addExternalSymbol("FIND-IF");
  public static final Symbol FIND_IF_NOT =
    PACKAGE_CL.addExternalSymbol("FIND-IF-NOT");
  public static final Symbol FIND_METHOD =
    PACKAGE_CL.addExternalSymbol("FIND-METHOD");
  public static final Symbol FIND_PACKAGE =
    PACKAGE_CL.addExternalSymbol("FIND-PACKAGE");
  public static final Symbol FIND_RESTART =
    PACKAGE_CL.addExternalSymbol("FIND-RESTART");
  public static final Symbol FIND_SYMBOL =
    PACKAGE_CL.addExternalSymbol("FIND-SYMBOL");
  public static final Symbol FINISH_OUTPUT =
    PACKAGE_CL.addExternalSymbol("FINISH-OUTPUT");
  public static final Symbol FIRST =
    PACKAGE_CL.addExternalSymbol("FIRST");
  public static final Symbol FIXNUM =
    PACKAGE_CL.addExternalSymbol("FIXNUM");
  public static final Symbol FLET =
    PACKAGE_CL.addExternalSymbol("FLET");
  public static final Symbol FLOAT =
    PACKAGE_CL.addExternalSymbol("FLOAT");
  public static final Symbol FLOAT_DIGITS =
    PACKAGE_CL.addExternalSymbol("FLOAT-DIGITS");
  public static final Symbol FLOAT_PRECISION =
    PACKAGE_CL.addExternalSymbol("FLOAT-PRECISION");
  public static final Symbol FLOAT_RADIX =
    PACKAGE_CL.addExternalSymbol("FLOAT-RADIX");
  public static final Symbol FLOAT_SIGN =
    PACKAGE_CL.addExternalSymbol("FLOAT-SIGN");
  public static final Symbol FLOATING_POINT_INEXACT =
    PACKAGE_CL.addExternalSymbol("FLOATING-POINT-INEXACT");
  public static final Symbol FLOATING_POINT_INVALID_OPERATION =
    PACKAGE_CL.addExternalSymbol("FLOATING-POINT-INVALID-OPERATION");
  public static final Symbol FLOATING_POINT_OVERFLOW =
    PACKAGE_CL.addExternalSymbol("FLOATING-POINT-OVERFLOW");
  public static final Symbol FLOATING_POINT_UNDERFLOW =
    PACKAGE_CL.addExternalSymbol("FLOATING-POINT-UNDERFLOW");
  public static final Symbol FLOATP =
    PACKAGE_CL.addExternalSymbol("FLOATP");
  public static final Symbol FLOOR =
    PACKAGE_CL.addExternalSymbol("FLOOR");
  public static final Symbol FMAKUNBOUND =
    PACKAGE_CL.addExternalSymbol("FMAKUNBOUND");
  public static final Symbol FORCE_OUTPUT =
    PACKAGE_CL.addExternalSymbol("FORCE-OUTPUT");
  public static final Symbol FORMAT =
    PACKAGE_CL.addExternalSymbol("FORMAT");
  public static final Symbol FORMATTER =
    PACKAGE_CL.addExternalSymbol("FORMATTER");
  public static final Symbol FOURTH =
    PACKAGE_CL.addExternalSymbol("FOURTH");
  public static final Symbol FRESH_LINE =
    PACKAGE_CL.addExternalSymbol("FRESH-LINE");
  public static final Symbol FROUND =
    PACKAGE_CL.addExternalSymbol("FROUND");
  public static final Symbol FTRUNCATE =
    PACKAGE_CL.addExternalSymbol("FTRUNCATE");
  public static final Symbol FTYPE =
    PACKAGE_CL.addExternalSymbol("FTYPE");
  public static final Symbol FUNCALL =
    PACKAGE_CL.addExternalSymbol("FUNCALL");
  public static final Symbol FUNCTION =
    PACKAGE_CL.addExternalSymbol("FUNCTION");
  public static final Symbol FUNCTION_KEYWORDS =
    PACKAGE_CL.addExternalSymbol("FUNCTION-KEYWORDS");
  public static final Symbol FUNCTION_LAMBDA_EXPRESSION =
    PACKAGE_CL.addExternalSymbol("FUNCTION-LAMBDA-EXPRESSION");
  public static final Symbol FUNCTIONP =
    PACKAGE_CL.addExternalSymbol("FUNCTIONP");
  public static final Symbol GCD =
    PACKAGE_CL.addExternalSymbol("GCD");
  public static final Symbol GENERIC_FUNCTION =
    PACKAGE_CL.addExternalSymbol("GENERIC-FUNCTION");
  public static final Symbol GENSYM =
    PACKAGE_CL.addExternalSymbol("GENSYM");
  public static final Symbol GENTEMP =
    PACKAGE_CL.addExternalSymbol("GENTEMP");
  public static final Symbol GET =
    PACKAGE_CL.addExternalSymbol("GET");
  public static final Symbol GET_DECODED_TIME =
    PACKAGE_CL.addExternalSymbol("GET-DECODED-TIME");
  public static final Symbol GET_DISPATCH_MACRO_CHARACTER =
    PACKAGE_CL.addExternalSymbol("GET-DISPATCH-MACRO-CHARACTER");
  public static final Symbol GET_INTERNAL_REAL_TIME =
    PACKAGE_CL.addExternalSymbol("GET-INTERNAL-REAL-TIME");
  public static final Symbol GET_INTERNAL_RUN_TIME =
    PACKAGE_CL.addExternalSymbol("GET-INTERNAL-RUN-TIME");
  public static final Symbol GET_MACRO_CHARACTER =
    PACKAGE_CL.addExternalSymbol("GET-MACRO-CHARACTER");
  public static final Symbol GET_OUTPUT_STREAM_STRING =
    PACKAGE_CL.addExternalSymbol("GET-OUTPUT-STREAM-STRING");
  public static final Symbol GET_PROPERTIES =
    PACKAGE_CL.addExternalSymbol("GET-PROPERTIES");
  public static final Symbol GET_SETF_EXPANSION =
    PACKAGE_CL.addExternalSymbol("GET-SETF-EXPANSION");
  public static final Symbol GET_UNIVERSAL_TIME =
    PACKAGE_CL.addExternalSymbol("GET-UNIVERSAL-TIME");
  public static final Symbol GETF =
    PACKAGE_CL.addExternalSymbol("GETF");
  public static final Symbol GETHASH =
    PACKAGE_CL.addExternalSymbol("GETHASH");
  public static final Symbol GO =
    PACKAGE_CL.addExternalSymbol("GO");
  public static final Symbol GRAPHIC_CHAR_P =
    PACKAGE_CL.addExternalSymbol("GRAPHIC-CHAR-P");
  public static final Symbol HANDLER_BIND =
    PACKAGE_CL.addExternalSymbol("HANDLER-BIND");
  public static final Symbol HANDLER_CASE =
    PACKAGE_CL.addExternalSymbol("HANDLER-CASE");
  public static final Symbol HASH_TABLE =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE");
  public static final Symbol HASH_TABLE_COUNT =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-COUNT");
  public static final Symbol HASH_TABLE_P =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-P");
  public static final Symbol HASH_TABLE_REHASH_SIZE =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-REHASH-SIZE");
  public static final Symbol HASH_TABLE_REHASH_THRESHOLD =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-REHASH-THRESHOLD");
  public static final Symbol HASH_TABLE_SIZE =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-SIZE");
  public static final Symbol HASH_TABLE_TEST =
    PACKAGE_CL.addExternalSymbol("HASH-TABLE-TEST");
  public static final Symbol HOST_NAMESTRING =
    PACKAGE_CL.addExternalSymbol("HOST-NAMESTRING");
  public static final Symbol IDENTITY =
    PACKAGE_CL.addExternalSymbol("IDENTITY");
  public static final Symbol IF =
    PACKAGE_CL.addExternalSymbol("IF");
  public static final Symbol IGNORABLE =
    PACKAGE_CL.addExternalSymbol("IGNORABLE");
  public static final Symbol IGNORE =
    PACKAGE_CL.addExternalSymbol("IGNORE");
  public static final Symbol IGNORE_ERRORS =
    PACKAGE_CL.addExternalSymbol("IGNORE-ERRORS");
  public static final Symbol IMAGPART =
    PACKAGE_CL.addExternalSymbol("IMAGPART");
  public static final Symbol IMPORT =
    PACKAGE_CL.addExternalSymbol("IMPORT");
  public static final Symbol IN_PACKAGE =
    PACKAGE_CL.addExternalSymbol("IN-PACKAGE");
  public static final Symbol INCF =
    PACKAGE_CL.addExternalSymbol("INCF");
  public static final Symbol INITIALIZE_INSTANCE =
    PACKAGE_CL.addExternalSymbol("INITIALIZE-INSTANCE");
  public static final Symbol INLINE =
    PACKAGE_CL.addExternalSymbol("INLINE");
  public static final Symbol INPUT_STREAM_P =
    PACKAGE_CL.addExternalSymbol("INPUT-STREAM-P");
  public static final Symbol INSPECT =
    PACKAGE_CL.addExternalSymbol("INSPECT");
  public static final Symbol INTEGER =
    PACKAGE_CL.addExternalSymbol("INTEGER");
  public static final Symbol INTEGER_DECODE_FLOAT =
    PACKAGE_CL.addExternalSymbol("INTEGER-DECODE-FLOAT");
  public static final Symbol INTEGER_LENGTH =
    PACKAGE_CL.addExternalSymbol("INTEGER-LENGTH");
  public static final Symbol INTEGERP =
    PACKAGE_CL.addExternalSymbol("INTEGERP");
  public static final Symbol INTERACTIVE_STREAM_P =
    PACKAGE_CL.addExternalSymbol("INTERACTIVE-STREAM-P");
  public static final Symbol INTERN =
    PACKAGE_CL.addExternalSymbol("INTERN");
  public static final Symbol INTERNAL_TIME_UNITS_PER_SECOND =
    PACKAGE_CL.addExternalSymbol("INTERNAL-TIME-UNITS-PER-SECOND");
  public static final Symbol INTERSECTION =
    PACKAGE_CL.addExternalSymbol("INTERSECTION");
  public static final Symbol INVALID_METHOD_ERROR =
    PACKAGE_CL.addExternalSymbol("INVALID-METHOD-ERROR");
  public static final Symbol INVOKE_DEBUGGER =
    PACKAGE_CL.addExternalSymbol("INVOKE-DEBUGGER");
  public static final Symbol INVOKE_RESTART =
    PACKAGE_CL.addExternalSymbol("INVOKE-RESTART");
  public static final Symbol INVOKE_RESTART_INTERACTIVELY =
    PACKAGE_CL.addExternalSymbol("INVOKE-RESTART-INTERACTIVELY");
  public static final Symbol ISQRT =
    PACKAGE_CL.addExternalSymbol("ISQRT");
  public static final Symbol KEYWORD =
    PACKAGE_CL.addExternalSymbol("KEYWORD");
  public static final Symbol KEYWORDP =
    PACKAGE_CL.addExternalSymbol("KEYWORDP");
  public static final Symbol LABELS =
    PACKAGE_CL.addExternalSymbol("LABELS");
  public static final Symbol LAMBDA =
    PACKAGE_CL.addExternalSymbol("LAMBDA");
  public static final Symbol LAMBDA_LIST_KEYWORDS =
    PACKAGE_CL.addExternalSymbol("LAMBDA-LIST-KEYWORDS");
  public static final Symbol LAMBDA_PARAMETERS_LIMIT =
    PACKAGE_CL.addExternalSymbol("LAMBDA-PARAMETERS-LIMIT");
  public static final Symbol LAST =
    PACKAGE_CL.addExternalSymbol("LAST");
  public static final Symbol LCM =
    PACKAGE_CL.addExternalSymbol("LCM");
  public static final Symbol LDB =
    PACKAGE_CL.addExternalSymbol("LDB");
  public static final Symbol LDB_TEST =
    PACKAGE_CL.addExternalSymbol("LDB-TEST");
  public static final Symbol LDIFF =
    PACKAGE_CL.addExternalSymbol("LDIFF");
  public static final Symbol LEAST_NEGATIVE_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-DOUBLE-FLOAT");
  public static final Symbol LEAST_NEGATIVE_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-LONG-FLOAT");
  public static final Symbol LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT");
  public static final Symbol LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT");
  public static final Symbol LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT");
  public static final Symbol LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT");
  public static final Symbol LEAST_NEGATIVE_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-SHORT-FLOAT");
  public static final Symbol LEAST_NEGATIVE_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-NEGATIVE-SINGLE-FLOAT");
  public static final Symbol LEAST_POSITIVE_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-DOUBLE-FLOAT");
  public static final Symbol LEAST_POSITIVE_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-LONG-FLOAT");
  public static final Symbol LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT");
  public static final Symbol LEAST_POSITIVE_NORMALIZED_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT");
  public static final Symbol LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT");
  public static final Symbol LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT");
  public static final Symbol LEAST_POSITIVE_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-SHORT-FLOAT");
  public static final Symbol LEAST_POSITIVE_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("LEAST-POSITIVE-SINGLE-FLOAT");
  public static final Symbol LENGTH =
    PACKAGE_CL.addExternalSymbol("LENGTH");
  public static final Symbol LET =
    PACKAGE_CL.addExternalSymbol("LET");
  public static final Symbol LET_STAR =
    PACKAGE_CL.addExternalSymbol("LET*");
  public static final Symbol LISP_IMPLEMENTATION_TYPE =
    PACKAGE_CL.addExternalSymbol("LISP-IMPLEMENTATION-TYPE");
  public static final Symbol LISP_IMPLEMENTATION_VERSION =
    PACKAGE_CL.addExternalSymbol("LISP-IMPLEMENTATION-VERSION");
  public static final Symbol LIST =
    PACKAGE_CL.addExternalSymbol("LIST");
  public static final Symbol LIST_STAR =
    PACKAGE_CL.addExternalSymbol("LIST*");
  public static final Symbol LIST_ALL_PACKAGES =
    PACKAGE_CL.addExternalSymbol("LIST-ALL-PACKAGES");
  public static final Symbol LIST_LENGTH =
    PACKAGE_CL.addExternalSymbol("LIST-LENGTH");
  public static final Symbol LISTEN =
    PACKAGE_CL.addExternalSymbol("LISTEN");
  public static final Symbol LISTP =
    PACKAGE_CL.addExternalSymbol("LISTP");
  public static final Symbol LOAD =
    PACKAGE_CL.addExternalSymbol("LOAD");
  public static final Symbol LOAD_LOGICAL_PATHNAME_TRANSLATIONS =
    PACKAGE_CL.addExternalSymbol("LOAD-LOGICAL-PATHNAME-TRANSLATIONS");
  public static final Symbol LOAD_TIME_VALUE =
    PACKAGE_CL.addExternalSymbol("LOAD-TIME-VALUE");
  public static final Symbol LOCALLY =
    PACKAGE_CL.addExternalSymbol("LOCALLY");
  public static final Symbol LOG =
    PACKAGE_CL.addExternalSymbol("LOG");
  public static final Symbol LOGAND =
    PACKAGE_CL.addExternalSymbol("LOGAND");
  public static final Symbol LOGANDC1 =
    PACKAGE_CL.addExternalSymbol("LOGANDC1");
  public static final Symbol LOGANDC2 =
    PACKAGE_CL.addExternalSymbol("LOGANDC2");
  public static final Symbol LOGBITP =
    PACKAGE_CL.addExternalSymbol("LOGBITP");
  public static final Symbol LOGCOUNT =
    PACKAGE_CL.addExternalSymbol("LOGCOUNT");
  public static final Symbol LOGEQV =
    PACKAGE_CL.addExternalSymbol("LOGEQV");
  public static final Symbol LOGICAL_PATHNAME =
    PACKAGE_CL.addExternalSymbol("LOGICAL-PATHNAME");
  public static final Symbol LOGICAL_PATHNAME_TRANSLATIONS =
    PACKAGE_CL.addExternalSymbol("LOGICAL-PATHNAME-TRANSLATIONS");
  public static final Symbol LOGIOR =
    PACKAGE_CL.addExternalSymbol("LOGIOR");
  public static final Symbol LOGNAND =
    PACKAGE_CL.addExternalSymbol("LOGNAND");
  public static final Symbol LOGNOR =
    PACKAGE_CL.addExternalSymbol("LOGNOR");
  public static final Symbol LOGNOT =
    PACKAGE_CL.addExternalSymbol("LOGNOT");
  public static final Symbol LOGORC1 =
    PACKAGE_CL.addExternalSymbol("LOGORC1");
  public static final Symbol LOGORC2 =
    PACKAGE_CL.addExternalSymbol("LOGORC2");
  public static final Symbol LOGTEST =
    PACKAGE_CL.addExternalSymbol("LOGTEST");
  public static final Symbol LOGXOR =
    PACKAGE_CL.addExternalSymbol("LOGXOR");
  public static final Symbol LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("LONG-FLOAT");
  public static final Symbol LONG_FLOAT_EPSILON =
    PACKAGE_CL.addExternalSymbol("LONG-FLOAT-EPSILON");
  public static final Symbol LONG_FLOAT_NEGATIVE_EPSILON =
    PACKAGE_CL.addExternalSymbol("LONG-FLOAT-NEGATIVE-EPSILON");
  public static final Symbol LONG_SITE_NAME =
    PACKAGE_CL.addExternalSymbol("LONG-SITE-NAME");
  public static final Symbol LOOP =
    PACKAGE_CL.addExternalSymbol("LOOP");
  public static final Symbol LOOP_FINISH =
    PACKAGE_CL.addExternalSymbol("LOOP-FINISH");
  public static final Symbol LOWER_CASE_P =
    PACKAGE_CL.addExternalSymbol("LOWER-CASE-P");
  public static final Symbol MACHINE_INSTANCE =
    PACKAGE_CL.addExternalSymbol("MACHINE-INSTANCE");
  public static final Symbol MACHINE_TYPE =
    PACKAGE_CL.addExternalSymbol("MACHINE-TYPE");
  public static final Symbol MACHINE_VERSION =
    PACKAGE_CL.addExternalSymbol("MACHINE-VERSION");
  public static final Symbol MACRO_FUNCTION =
    PACKAGE_CL.addExternalSymbol("MACRO-FUNCTION");
  public static final Symbol MACROEXPAND =
    PACKAGE_CL.addExternalSymbol("MACROEXPAND");
  public static final Symbol MACROEXPAND_1 =
    PACKAGE_CL.addExternalSymbol("MACROEXPAND-1");
  public static final Symbol MACROLET =
    PACKAGE_CL.addExternalSymbol("MACROLET");
  public static final Symbol MAKE_ARRAY =
    PACKAGE_CL.addExternalSymbol("MAKE-ARRAY");
  public static final Symbol MAKE_BROADCAST_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-BROADCAST-STREAM");
  public static final Symbol MAKE_CONCATENATED_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-CONCATENATED-STREAM");
  public static final Symbol MAKE_CONDITION =
    PACKAGE_CL.addExternalSymbol("MAKE-CONDITION");
  public static final Symbol MAKE_DISPATCH_MACRO_CHARACTER =
    PACKAGE_CL.addExternalSymbol("MAKE-DISPATCH-MACRO-CHARACTER");
  public static final Symbol MAKE_ECHO_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-ECHO-STREAM");
  public static final Symbol MAKE_HASH_TABLE =
    PACKAGE_CL.addExternalSymbol("MAKE-HASH-TABLE");
  public static final Symbol MAKE_INSTANCE =
    PACKAGE_CL.addExternalSymbol("MAKE-INSTANCE");
  public static final Symbol MAKE_INSTANCES_OBSOLETE =
    PACKAGE_CL.addExternalSymbol("MAKE-INSTANCES-OBSOLETE");
  public static final Symbol MAKE_LIST =
    PACKAGE_CL.addExternalSymbol("MAKE-LIST");
  public static final Symbol MAKE_LOAD_FORM =
    PACKAGE_CL.addExternalSymbol("MAKE-LOAD-FORM");
  public static final Symbol MAKE_LOAD_FORM_SAVING_SLOTS =
    PACKAGE_CL.addExternalSymbol("MAKE-LOAD-FORM-SAVING-SLOTS");
  public static final Symbol MAKE_METHOD =
    PACKAGE_CL.addExternalSymbol("MAKE-METHOD");
  public static final Symbol MAKE_PACKAGE =
    PACKAGE_CL.addExternalSymbol("MAKE-PACKAGE");
  public static final Symbol MAKE_PATHNAME =
    PACKAGE_CL.addExternalSymbol("MAKE-PATHNAME");
  public static final Symbol MAKE_RANDOM_STATE =
    PACKAGE_CL.addExternalSymbol("MAKE-RANDOM-STATE");
  public static final Symbol MAKE_SEQUENCE =
    PACKAGE_CL.addExternalSymbol("MAKE-SEQUENCE");
  public static final Symbol MAKE_STRING =
    PACKAGE_CL.addExternalSymbol("MAKE-STRING");
  public static final Symbol MAKE_STRING_INPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-STRING-INPUT-STREAM");
  public static final Symbol MAKE_STRING_OUTPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-STRING-OUTPUT-STREAM");
  public static final Symbol MAKE_SYMBOL =
    PACKAGE_CL.addExternalSymbol("MAKE-SYMBOL");
  public static final Symbol MAKE_SYNONYM_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-SYNONYM-STREAM");
  public static final Symbol MAKE_TWO_WAY_STREAM =
    PACKAGE_CL.addExternalSymbol("MAKE-TWO-WAY-STREAM");
  public static final Symbol MAKUNBOUND =
    PACKAGE_CL.addExternalSymbol("MAKUNBOUND");
  public static final Symbol MAP =
    PACKAGE_CL.addExternalSymbol("MAP");
  public static final Symbol MAP_INTO =
    PACKAGE_CL.addExternalSymbol("MAP-INTO");
  public static final Symbol MAPC =
    PACKAGE_CL.addExternalSymbol("MAPC");
  public static final Symbol MAPCAN =
    PACKAGE_CL.addExternalSymbol("MAPCAN");
  public static final Symbol MAPCAR =
    PACKAGE_CL.addExternalSymbol("MAPCAR");
  public static final Symbol MAPCON =
    PACKAGE_CL.addExternalSymbol("MAPCON");
  public static final Symbol MAPHASH =
    PACKAGE_CL.addExternalSymbol("MAPHASH");
  public static final Symbol MAPL =
    PACKAGE_CL.addExternalSymbol("MAPL");
  public static final Symbol MAPLIST =
    PACKAGE_CL.addExternalSymbol("MAPLIST");
  public static final Symbol MASK_FIELD =
    PACKAGE_CL.addExternalSymbol("MASK-FIELD");
  public static final Symbol MAX =
    PACKAGE_CL.addExternalSymbol("MAX");
  public static final Symbol MEMBER =
    PACKAGE_CL.addExternalSymbol("MEMBER");
  public static final Symbol MEMBER_IF =
    PACKAGE_CL.addExternalSymbol("MEMBER-IF");
  public static final Symbol MEMBER_IF_NOT =
    PACKAGE_CL.addExternalSymbol("MEMBER-IF-NOT");
  public static final Symbol MERGE =
    PACKAGE_CL.addExternalSymbol("MERGE");
  public static final Symbol MERGE_PATHNAMES =
    PACKAGE_CL.addExternalSymbol("MERGE-PATHNAMES");
  public static final Symbol METHOD =
    PACKAGE_CL.addExternalSymbol("METHOD");
  public static final Symbol METHOD_COMBINATION =
    PACKAGE_CL.addExternalSymbol("METHOD-COMBINATION");
  public static final Symbol METHOD_COMBINATION_ERROR =
    PACKAGE_CL.addExternalSymbol("METHOD-COMBINATION-ERROR");
  public static final Symbol METHOD_QUALIFIERS =
    PACKAGE_CL.addExternalSymbol("METHOD-QUALIFIERS");
  public static final Symbol MIN =
    PACKAGE_CL.addExternalSymbol("MIN");
  public static final Symbol MINUSP =
    PACKAGE_CL.addExternalSymbol("MINUSP");
  public static final Symbol MISMATCH =
    PACKAGE_CL.addExternalSymbol("MISMATCH");
  public static final Symbol MOD =
    PACKAGE_CL.addExternalSymbol("MOD");
  public static final Symbol MOST_NEGATIVE_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-NEGATIVE-DOUBLE-FLOAT");
  public static final Symbol MOST_NEGATIVE_FIXNUM =
    PACKAGE_CL.addExternalSymbol("MOST-NEGATIVE-FIXNUM");
  public static final Symbol MOST_NEGATIVE_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-NEGATIVE-LONG-FLOAT");
  public static final Symbol MOST_NEGATIVE_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-NEGATIVE-SHORT-FLOAT");
  public static final Symbol MOST_NEGATIVE_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-NEGATIVE-SINGLE-FLOAT");
  public static final Symbol MOST_POSITIVE_DOUBLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-POSITIVE-DOUBLE-FLOAT");
  public static final Symbol MOST_POSITIVE_FIXNUM =
    PACKAGE_CL.addExternalSymbol("MOST-POSITIVE-FIXNUM");
  public static final Symbol MOST_POSITIVE_LONG_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-POSITIVE-LONG-FLOAT");
  public static final Symbol MOST_POSITIVE_SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-POSITIVE-SHORT-FLOAT");
  public static final Symbol MOST_POSITIVE_SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("MOST-POSITIVE-SINGLE-FLOAT");
  public static final Symbol MUFFLE_WARNING =
    PACKAGE_CL.addExternalSymbol("MUFFLE-WARNING");
  public static final Symbol MULTIPLE_VALUE_BIND =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUE-BIND");
  public static final Symbol MULTIPLE_VALUE_CALL =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUE-CALL");
  public static final Symbol MULTIPLE_VALUE_LIST =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUE-LIST");
  public static final Symbol MULTIPLE_VALUE_PROG1 =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUE-PROG1");
  public static final Symbol MULTIPLE_VALUE_SETQ =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUE-SETQ");
  public static final Symbol MULTIPLE_VALUES_LIMIT =
    PACKAGE_CL.addExternalSymbol("MULTIPLE-VALUES-LIMIT");
  public static final Symbol NAME_CHAR =
    PACKAGE_CL.addExternalSymbol("NAME-CHAR");
  public static final Symbol NAMESTRING =
    PACKAGE_CL.addExternalSymbol("NAMESTRING");
  public static final Symbol NBUTLAST =
    PACKAGE_CL.addExternalSymbol("NBUTLAST");
  public static final Symbol NCONC =
    PACKAGE_CL.addExternalSymbol("NCONC");
  public static final Symbol NEXT_METHOD_P =
    PACKAGE_CL.addExternalSymbol("NEXT-METHOD-P");
  // NIL is a special case.
  //     public static final Symbol NIL =
  //         PACKAGE_CL.addExternalSymbol("NIL");
  public static final Symbol NINTERSECTION =
    PACKAGE_CL.addExternalSymbol("NINTERSECTION");
  public static final Symbol NINTH =
    PACKAGE_CL.addExternalSymbol("NINTH");
  public static final Symbol NO_APPLICABLE_METHOD =
    PACKAGE_CL.addExternalSymbol("NO-APPLICABLE-METHOD");
  public static final Symbol NO_NEXT_METHOD =
    PACKAGE_CL.addExternalSymbol("NO-NEXT-METHOD");
  public static final Symbol NOT =
    PACKAGE_CL.addExternalSymbol("NOT");
  public static final Symbol NOTANY =
    PACKAGE_CL.addExternalSymbol("NOTANY");
  public static final Symbol NOTEVERY =
    PACKAGE_CL.addExternalSymbol("NOTEVERY");
  public static final Symbol NOTINLINE =
    PACKAGE_CL.addExternalSymbol("NOTINLINE");
  public static final Symbol NRECONC =
    PACKAGE_CL.addExternalSymbol("NRECONC");
  public static final Symbol NREVERSE =
    PACKAGE_CL.addExternalSymbol("NREVERSE");
  public static final Symbol NSET_DIFFERENCE =
    PACKAGE_CL.addExternalSymbol("NSET-DIFFERENCE");
  public static final Symbol NSET_EXCLUSIVE_OR =
    PACKAGE_CL.addExternalSymbol("NSET-EXCLUSIVE-OR");
  public static final Symbol NSTRING_CAPITALIZE =
    PACKAGE_CL.addExternalSymbol("NSTRING-CAPITALIZE");
  public static final Symbol NSTRING_DOWNCASE =
    PACKAGE_CL.addExternalSymbol("NSTRING-DOWNCASE");
  public static final Symbol NSTRING_UPCASE =
    PACKAGE_CL.addExternalSymbol("NSTRING-UPCASE");
  public static final Symbol NSUBLIS =
    PACKAGE_CL.addExternalSymbol("NSUBLIS");
  public static final Symbol NSUBST =
    PACKAGE_CL.addExternalSymbol("NSUBST");
  public static final Symbol NSUBST_IF =
    PACKAGE_CL.addExternalSymbol("NSUBST-IF");
  public static final Symbol NSUBST_IF_NOT =
    PACKAGE_CL.addExternalSymbol("NSUBST-IF-NOT");
  public static final Symbol NSUBSTITUTE =
    PACKAGE_CL.addExternalSymbol("NSUBSTITUTE");
  public static final Symbol NSUBSTITUTE_IF =
    PACKAGE_CL.addExternalSymbol("NSUBSTITUTE-IF");
  public static final Symbol NSUBSTITUTE_IF_NOT =
    PACKAGE_CL.addExternalSymbol("NSUBSTITUTE-IF-NOT");
  public static final Symbol NTH =
    PACKAGE_CL.addExternalSymbol("NTH");
  public static final Symbol NTH_VALUE =
    PACKAGE_CL.addExternalSymbol("NTH-VALUE");
  public static final Symbol NTHCDR =
    PACKAGE_CL.addExternalSymbol("NTHCDR");
  public static final Symbol NULL =
    PACKAGE_CL.addExternalSymbol("NULL");
  public static final Symbol NUMBER =
    PACKAGE_CL.addExternalSymbol("NUMBER");
  public static final Symbol NUMBERP =
    PACKAGE_CL.addExternalSymbol("NUMBERP");
  public static final Symbol NUMERATOR =
    PACKAGE_CL.addExternalSymbol("NUMERATOR");
  public static final Symbol NUNION =
    PACKAGE_CL.addExternalSymbol("NUNION");
  public static final Symbol ODDP =
    PACKAGE_CL.addExternalSymbol("ODDP");
  public static final Symbol OPEN =
    PACKAGE_CL.addExternalSymbol("OPEN");
  public static final Symbol OPEN_STREAM_P =
    PACKAGE_CL.addExternalSymbol("OPEN-STREAM-P");
  public static final Symbol OPTIMIZE =
    PACKAGE_CL.addExternalSymbol("OPTIMIZE");
  public static final Symbol OR =
    PACKAGE_CL.addExternalSymbol("OR");
  public static final Symbol OTHERWISE =
    PACKAGE_CL.addExternalSymbol("OTHERWISE");
  public static final Symbol OUTPUT_STREAM_P =
    PACKAGE_CL.addExternalSymbol("OUTPUT-STREAM-P");
  public static final Symbol PACKAGE =
    PACKAGE_CL.addExternalSymbol("PACKAGE");
  public static final Symbol PACKAGE_ERROR =
    PACKAGE_CL.addExternalSymbol("PACKAGE-ERROR");
  public static final Symbol PACKAGE_ERROR_PACKAGE =
    PACKAGE_CL.addExternalSymbol("PACKAGE-ERROR-PACKAGE");
  public static final Symbol PACKAGE_NAME =
    PACKAGE_CL.addExternalSymbol("PACKAGE-NAME");
  public static final Symbol PACKAGE_NICKNAMES =
    PACKAGE_CL.addExternalSymbol("PACKAGE-NICKNAMES");
  public static final Symbol PACKAGE_SHADOWING_SYMBOLS =
    PACKAGE_CL.addExternalSymbol("PACKAGE-SHADOWING-SYMBOLS");
  public static final Symbol PACKAGE_USE_LIST =
    PACKAGE_CL.addExternalSymbol("PACKAGE-USE-LIST");
  public static final Symbol PACKAGE_USED_BY_LIST =
    PACKAGE_CL.addExternalSymbol("PACKAGE-USED-BY-LIST");
  public static final Symbol PACKAGEP =
    PACKAGE_CL.addExternalSymbol("PACKAGEP");
  public static final Symbol PAIRLIS =
    PACKAGE_CL.addExternalSymbol("PAIRLIS");
  public static final Symbol PARSE_ERROR =
    PACKAGE_CL.addExternalSymbol("PARSE-ERROR");
  public static final Symbol PARSE_INTEGER =
    PACKAGE_CL.addExternalSymbol("PARSE-INTEGER");
  public static final Symbol PARSE_NAMESTRING =
    PACKAGE_CL.addExternalSymbol("PARSE-NAMESTRING");
  public static final Symbol PATHNAME =
    PACKAGE_CL.addExternalSymbol("PATHNAME");
  public static final Symbol PATHNAME_DEVICE =
    PACKAGE_CL.addExternalSymbol("PATHNAME-DEVICE");
  public static final Symbol PATHNAME_DIRECTORY =
    PACKAGE_CL.addExternalSymbol("PATHNAME-DIRECTORY");
  public static final Symbol PATHNAME_HOST =
    PACKAGE_CL.addExternalSymbol("PATHNAME-HOST");
  public static final Symbol PATHNAME_MATCH_P =
    PACKAGE_CL.addExternalSymbol("PATHNAME-MATCH-P");
  public static final Symbol PATHNAME_NAME =
    PACKAGE_CL.addExternalSymbol("PATHNAME-NAME");
  public static final Symbol PATHNAME_TYPE =
    PACKAGE_CL.addExternalSymbol("PATHNAME-TYPE");
  public static final Symbol PATHNAME_VERSION =
    PACKAGE_CL.addExternalSymbol("PATHNAME-VERSION");
  public static final Symbol PATHNAMEP =
    PACKAGE_CL.addExternalSymbol("PATHNAMEP");
  public static final Symbol PEEK_CHAR =
    PACKAGE_CL.addExternalSymbol("PEEK-CHAR");
  public static final Symbol PHASE =
    PACKAGE_CL.addExternalSymbol("PHASE");
  public static final Symbol PI =
    PACKAGE_CL.addExternalSymbol("PI");
  public static final Symbol PLUSP =
    PACKAGE_CL.addExternalSymbol("PLUSP");
  public static final Symbol POP =
    PACKAGE_CL.addExternalSymbol("POP");
  public static final Symbol POSITION =
    PACKAGE_CL.addExternalSymbol("POSITION");
  public static final Symbol POSITION_IF =
    PACKAGE_CL.addExternalSymbol("POSITION-IF");
  public static final Symbol POSITION_IF_NOT =
    PACKAGE_CL.addExternalSymbol("POSITION-IF-NOT");
  public static final Symbol PPRINT =
    PACKAGE_CL.addExternalSymbol("PPRINT");
  public static final Symbol PPRINT_DISPATCH =
    PACKAGE_CL.addExternalSymbol("PPRINT-DISPATCH");
  public static final Symbol PPRINT_EXIT_IF_LIST_EXHAUSTED =
    PACKAGE_CL.addExternalSymbol("PPRINT-EXIT-IF-LIST-EXHAUSTED");
  public static final Symbol PPRINT_FILL =
    PACKAGE_CL.addExternalSymbol("PPRINT-FILL");
  public static final Symbol PPRINT_INDENT =
    PACKAGE_CL.addExternalSymbol("PPRINT-INDENT");
  public static final Symbol PPRINT_LINEAR =
    PACKAGE_CL.addExternalSymbol("PPRINT-LINEAR");
  public static final Symbol PPRINT_LOGICAL_BLOCK =
    PACKAGE_CL.addExternalSymbol("PPRINT-LOGICAL-BLOCK");
  public static final Symbol PPRINT_NEWLINE =
    PACKAGE_CL.addExternalSymbol("PPRINT-NEWLINE");
  public static final Symbol PPRINT_POP =
    PACKAGE_CL.addExternalSymbol("PPRINT-POP");
  public static final Symbol PPRINT_TAB =
    PACKAGE_CL.addExternalSymbol("PPRINT-TAB");
  public static final Symbol PPRINT_TABULAR =
    PACKAGE_CL.addExternalSymbol("PPRINT-TABULAR");
  public static final Symbol PRIN1 =
    PACKAGE_CL.addExternalSymbol("PRIN1");
  public static final Symbol PRIN1_TO_STRING =
    PACKAGE_CL.addExternalSymbol("PRIN1-TO-STRING");
  public static final Symbol PRINC =
    PACKAGE_CL.addExternalSymbol("PRINC");
  public static final Symbol PRINC_TO_STRING =
    PACKAGE_CL.addExternalSymbol("PRINC-TO-STRING");
  public static final Symbol PRINT =
    PACKAGE_CL.addExternalSymbol("PRINT");
  public static final Symbol PRINT_NOT_READABLE =
    PACKAGE_CL.addExternalSymbol("PRINT-NOT-READABLE");
  public static final Symbol PRINT_NOT_READABLE_OBJECT =
    PACKAGE_CL.addExternalSymbol("PRINT-NOT-READABLE-OBJECT");
  public static final Symbol PRINT_OBJECT =
    PACKAGE_CL.addExternalSymbol("PRINT-OBJECT");
  public static final Symbol PRINT_UNREADABLE_OBJECT =
    PACKAGE_CL.addExternalSymbol("PRINT-UNREADABLE-OBJECT");
  public static final Symbol PROBE_FILE =
    PACKAGE_CL.addExternalSymbol("PROBE-FILE");
  public static final Symbol PROCLAIM =
    PACKAGE_CL.addExternalSymbol("PROCLAIM");
  public static final Symbol PROG =
    PACKAGE_CL.addExternalSymbol("PROG");
  public static final Symbol PROG_STAR =
    PACKAGE_CL.addExternalSymbol("PROG*");
  public static final Symbol PROG1 =
    PACKAGE_CL.addExternalSymbol("PROG1");
  public static final Symbol PROG2 =
    PACKAGE_CL.addExternalSymbol("PROG2");
  public static final Symbol PROGN =
    PACKAGE_CL.addExternalSymbol("PROGN");
  public static final Symbol PROGRAM_ERROR =
    PACKAGE_CL.addExternalSymbol("PROGRAM-ERROR");
  public static final Symbol PROGV =
    PACKAGE_CL.addExternalSymbol("PROGV");
  public static final Symbol PROVIDE =
    PACKAGE_CL.addExternalSymbol("PROVIDE");
  public static final Symbol PSETF =
    PACKAGE_CL.addExternalSymbol("PSETF");
  public static final Symbol PSETQ =
    PACKAGE_CL.addExternalSymbol("PSETQ");
  public static final Symbol PUSH =
    PACKAGE_CL.addExternalSymbol("PUSH");
  public static final Symbol PUSHNEW =
    PACKAGE_CL.addExternalSymbol("PUSHNEW");
  public static final Symbol QUOTE =
    PACKAGE_CL.addExternalSymbol("QUOTE");
  public static final Symbol RANDOM =
    PACKAGE_CL.addExternalSymbol("RANDOM");
  public static final Symbol RANDOM_STATE =
    PACKAGE_CL.addExternalSymbol("RANDOM-STATE");
  public static final Symbol RANDOM_STATE_P =
    PACKAGE_CL.addExternalSymbol("RANDOM-STATE-P");
  public static final Symbol RASSOC =
    PACKAGE_CL.addExternalSymbol("RASSOC");
  public static final Symbol RASSOC_IF =
    PACKAGE_CL.addExternalSymbol("RASSOC-IF");
  public static final Symbol RASSOC_IF_NOT =
    PACKAGE_CL.addExternalSymbol("RASSOC-IF-NOT");
  public static final Symbol RATIO =
    PACKAGE_CL.addExternalSymbol("RATIO");
  public static final Symbol RATIONAL =
    PACKAGE_CL.addExternalSymbol("RATIONAL");
  public static final Symbol RATIONALIZE =
    PACKAGE_CL.addExternalSymbol("RATIONALIZE");
  public static final Symbol RATIONALP =
    PACKAGE_CL.addExternalSymbol("RATIONALP");
  public static final Symbol READ =
    PACKAGE_CL.addExternalSymbol("READ");
  public static final Symbol READ_BYTE =
    PACKAGE_CL.addExternalSymbol("READ-BYTE");
  public static final Symbol READ_CHAR =
    PACKAGE_CL.addExternalSymbol("READ-CHAR");
  public static final Symbol READ_CHAR_NO_HANG =
    PACKAGE_CL.addExternalSymbol("READ-CHAR-NO-HANG");
  public static final Symbol READ_DELIMITED_LIST =
    PACKAGE_CL.addExternalSymbol("READ-DELIMITED-LIST");
  public static final Symbol READ_FROM_STRING =
    PACKAGE_CL.addExternalSymbol("READ-FROM-STRING");
  public static final Symbol READ_LINE =
    PACKAGE_CL.addExternalSymbol("READ-LINE");
  public static final Symbol READ_PRESERVING_WHITESPACE =
    PACKAGE_CL.addExternalSymbol("READ-PRESERVING-WHITESPACE");
  public static final Symbol READ_SEQUENCE =
    PACKAGE_CL.addExternalSymbol("READ-SEQUENCE");
  public static final Symbol READER_ERROR =
    PACKAGE_CL.addExternalSymbol("READER-ERROR");
  public static final Symbol READTABLE =
    PACKAGE_CL.addExternalSymbol("READTABLE");
  public static final Symbol READTABLE_CASE =
    PACKAGE_CL.addExternalSymbol("READTABLE-CASE");
  public static final Symbol READTABLEP =
    PACKAGE_CL.addExternalSymbol("READTABLEP");
  public static final Symbol REAL =
    PACKAGE_CL.addExternalSymbol("REAL");
  public static final Symbol REALP =
    PACKAGE_CL.addExternalSymbol("REALP");
  public static final Symbol REALPART =
    PACKAGE_CL.addExternalSymbol("REALPART");
  public static final Symbol REDUCE =
    PACKAGE_CL.addExternalSymbol("REDUCE");
  public static final Symbol REINITIALIZE_INSTANCE =
    PACKAGE_CL.addExternalSymbol("REINITIALIZE-INSTANCE");
  public static final Symbol REM =
    PACKAGE_CL.addExternalSymbol("REM");
  public static final Symbol REMF =
    PACKAGE_CL.addExternalSymbol("REMF");
  public static final Symbol REMHASH =
    PACKAGE_CL.addExternalSymbol("REMHASH");
  public static final Symbol REMOVE =
    PACKAGE_CL.addExternalSymbol("REMOVE");
  public static final Symbol REMOVE_DUPLICATES =
    PACKAGE_CL.addExternalSymbol("REMOVE-DUPLICATES");
  public static final Symbol REMOVE_IF =
    PACKAGE_CL.addExternalSymbol("REMOVE-IF");
  public static final Symbol REMOVE_IF_NOT =
    PACKAGE_CL.addExternalSymbol("REMOVE-IF-NOT");
  public static final Symbol REMOVE_METHOD =
    PACKAGE_CL.addExternalSymbol("REMOVE-METHOD");
  public static final Symbol REMPROP =
    PACKAGE_CL.addExternalSymbol("REMPROP");
  public static final Symbol RENAME_FILE =
    PACKAGE_CL.addExternalSymbol("RENAME-FILE");
  public static final Symbol RENAME_PACKAGE =
    PACKAGE_CL.addExternalSymbol("RENAME-PACKAGE");
  public static final Symbol REPLACE =
    PACKAGE_CL.addExternalSymbol("REPLACE");
  public static final Symbol REQUIRE =
    PACKAGE_CL.addExternalSymbol("REQUIRE");
  public static final Symbol REST =
    PACKAGE_CL.addExternalSymbol("REST");
  public static final Symbol RESTART =
    PACKAGE_CL.addExternalSymbol("RESTART");
  public static final Symbol RESTART_BIND =
    PACKAGE_CL.addExternalSymbol("RESTART-BIND");
  public static final Symbol RESTART_CASE =
    PACKAGE_CL.addExternalSymbol("RESTART-CASE");
  public static final Symbol RESTART_NAME =
    PACKAGE_CL.addExternalSymbol("RESTART-NAME");
  public static final Symbol RETURN =
    PACKAGE_CL.addExternalSymbol("RETURN");
  public static final Symbol RETURN_FROM =
    PACKAGE_CL.addExternalSymbol("RETURN-FROM");
  public static final Symbol REVAPPEND =
    PACKAGE_CL.addExternalSymbol("REVAPPEND");
  public static final Symbol REVERSE =
    PACKAGE_CL.addExternalSymbol("REVERSE");
  public static final Symbol ROOM =
    PACKAGE_CL.addExternalSymbol("ROOM");
  public static final Symbol ROTATEF =
    PACKAGE_CL.addExternalSymbol("ROTATEF");
  public static final Symbol ROUND =
    PACKAGE_CL.addExternalSymbol("ROUND");
  public static final Symbol ROW_MAJOR_AREF =
    PACKAGE_CL.addExternalSymbol("ROW-MAJOR-AREF");
  public static final Symbol RPLACA =
    PACKAGE_CL.addExternalSymbol("RPLACA");
  public static final Symbol RPLACD =
    PACKAGE_CL.addExternalSymbol("RPLACD");
  public static final Symbol SAFETY =
    PACKAGE_CL.addExternalSymbol("SAFETY");
  public static final Symbol SATISFIES =
    PACKAGE_CL.addExternalSymbol("SATISFIES");
  public static final Symbol SBIT =
    PACKAGE_CL.addExternalSymbol("SBIT");
  public static final Symbol SCALE_FLOAT =
    PACKAGE_CL.addExternalSymbol("SCALE-FLOAT");
  public static final Symbol SCHAR =
    PACKAGE_CL.addExternalSymbol("SCHAR");
  public static final Symbol SEARCH =
    PACKAGE_CL.addExternalSymbol("SEARCH");
  public static final Symbol SECOND =
    PACKAGE_CL.addExternalSymbol("SECOND");
  public static final Symbol SEQUENCE =
    PACKAGE_CL.addExternalSymbol("SEQUENCE");
  public static final Symbol SERIOUS_CONDITION =
    PACKAGE_CL.addExternalSymbol("SERIOUS-CONDITION");
  public static final Symbol SET =
    PACKAGE_CL.addExternalSymbol("SET");
  public static final Symbol SET_DIFFERENCE =
    PACKAGE_CL.addExternalSymbol("SET-DIFFERENCE");
  public static final Symbol SET_DISPATCH_MACRO_CHARACTER =
    PACKAGE_CL.addExternalSymbol("SET-DISPATCH-MACRO-CHARACTER");
  public static final Symbol SET_EXCLUSIVE_OR =
    PACKAGE_CL.addExternalSymbol("SET-EXCLUSIVE-OR");
  public static final Symbol SET_MACRO_CHARACTER =
    PACKAGE_CL.addExternalSymbol("SET-MACRO-CHARACTER");
  public static final Symbol SET_PPRINT_DISPATCH =
    PACKAGE_CL.addExternalSymbol("SET-PPRINT-DISPATCH");
  public static final Symbol SET_SYNTAX_FROM_CHAR =
    PACKAGE_CL.addExternalSymbol("SET-SYNTAX-FROM-CHAR");
  public static final Symbol SETF =
    PACKAGE_CL.addExternalSymbol("SETF");
  public static final Symbol SETQ =
    PACKAGE_CL.addExternalSymbol("SETQ");
  public static final Symbol SEVENTH =
    PACKAGE_CL.addExternalSymbol("SEVENTH");
  public static final Symbol SHADOW =
    PACKAGE_CL.addExternalSymbol("SHADOW");
  public static final Symbol SHADOWING_IMPORT =
    PACKAGE_CL.addExternalSymbol("SHADOWING-IMPORT");
  public static final Symbol SHARED_INITIALIZE =
    PACKAGE_CL.addExternalSymbol("SHARED-INITIALIZE");
  public static final Symbol SHIFTF =
    PACKAGE_CL.addExternalSymbol("SHIFTF");
  public static final Symbol SHORT_FLOAT =
    PACKAGE_CL.addExternalSymbol("SHORT-FLOAT");
  public static final Symbol SHORT_FLOAT_EPSILON =
    PACKAGE_CL.addExternalSymbol("SHORT-FLOAT-EPSILON");
  public static final Symbol SHORT_FLOAT_NEGATIVE_EPSILON =
    PACKAGE_CL.addExternalSymbol("SHORT-FLOAT-NEGATIVE-EPSILON");
  public static final Symbol SHORT_SITE_NAME =
    PACKAGE_CL.addExternalSymbol("SHORT-SITE-NAME");
  public static final Symbol SIGNAL =
    PACKAGE_CL.addExternalSymbol("SIGNAL");
  public static final Symbol SIGNED_BYTE =
    PACKAGE_CL.addExternalSymbol("SIGNED-BYTE");
  public static final Symbol SIGNUM =
    PACKAGE_CL.addExternalSymbol("SIGNUM");
  public static final Symbol SIMPLE_ARRAY =
    PACKAGE_CL.addExternalSymbol("SIMPLE-ARRAY");
  public static final Symbol SIMPLE_BASE_STRING =
    PACKAGE_CL.addExternalSymbol("SIMPLE-BASE-STRING");
  public static final Symbol SIMPLE_BIT_VECTOR =
    PACKAGE_CL.addExternalSymbol("SIMPLE-BIT-VECTOR");
  public static final Symbol SIMPLE_BIT_VECTOR_P =
    PACKAGE_CL.addExternalSymbol("SIMPLE-BIT-VECTOR-P");
  public static final Symbol SIMPLE_CONDITION =
    PACKAGE_CL.addExternalSymbol("SIMPLE-CONDITION");
  public static final Symbol SIMPLE_CONDITION_FORMAT_ARGUMENTS =
    PACKAGE_CL.addExternalSymbol("SIMPLE-CONDITION-FORMAT-ARGUMENTS");
  public static final Symbol SIMPLE_CONDITION_FORMAT_CONTROL =
    PACKAGE_CL.addExternalSymbol("SIMPLE-CONDITION-FORMAT-CONTROL");
  public static final Symbol SIMPLE_ERROR =
    PACKAGE_CL.addExternalSymbol("SIMPLE-ERROR");
  public static final Symbol SIMPLE_STRING =
    PACKAGE_CL.addExternalSymbol("SIMPLE-STRING");
  public static final Symbol SIMPLE_STRING_P =
    PACKAGE_CL.addExternalSymbol("SIMPLE-STRING-P");
  public static final Symbol SIMPLE_TYPE_ERROR =
    PACKAGE_CL.addExternalSymbol("SIMPLE-TYPE-ERROR");
  public static final Symbol SIMPLE_VECTOR =
    PACKAGE_CL.addExternalSymbol("SIMPLE-VECTOR");
  public static final Symbol SIMPLE_VECTOR_P =
    PACKAGE_CL.addExternalSymbol("SIMPLE-VECTOR-P");
  public static final Symbol SIMPLE_WARNING =
    PACKAGE_CL.addExternalSymbol("SIMPLE-WARNING");
  public static final Symbol SIN =
    PACKAGE_CL.addExternalSymbol("SIN");
  public static final Symbol SINGLE_FLOAT =
    PACKAGE_CL.addExternalSymbol("SINGLE-FLOAT");
  public static final Symbol SINGLE_FLOAT_EPSILON =
    PACKAGE_CL.addExternalSymbol("SINGLE-FLOAT-EPSILON");
  public static final Symbol SINGLE_FLOAT_NEGATIVE_EPSILON =
    PACKAGE_CL.addExternalSymbol("SINGLE-FLOAT-NEGATIVE-EPSILON");
  public static final Symbol SINH =
    PACKAGE_CL.addExternalSymbol("SINH");
  public static final Symbol SIXTH =
    PACKAGE_CL.addExternalSymbol("SIXTH");
  public static final Symbol SLEEP =
    PACKAGE_CL.addExternalSymbol("SLEEP");
  public static final Symbol SLOT_BOUNDP =
    PACKAGE_CL.addExternalSymbol("SLOT-BOUNDP");
  public static final Symbol SLOT_EXISTS_P =
    PACKAGE_CL.addExternalSymbol("SLOT-EXISTS-P");
  public static final Symbol SLOT_MAKUNBOUND =
    PACKAGE_CL.addExternalSymbol("SLOT-MAKUNBOUND");
  public static final Symbol SLOT_MISSING =
    PACKAGE_CL.addExternalSymbol("SLOT-MISSING");
  public static final Symbol SLOT_UNBOUND =
    PACKAGE_CL.addExternalSymbol("SLOT-UNBOUND");
  public static final Symbol SLOT_VALUE =
    PACKAGE_CL.addExternalSymbol("SLOT-VALUE");
  public static final Symbol SOFTWARE_TYPE =
    PACKAGE_CL.addExternalSymbol("SOFTWARE-TYPE");
  public static final Symbol SOFTWARE_VERSION =
    PACKAGE_CL.addExternalSymbol("SOFTWARE-VERSION");
  public static final Symbol SOME =
    PACKAGE_CL.addExternalSymbol("SOME");
  public static final Symbol SORT =
    PACKAGE_CL.addExternalSymbol("SORT");
  public static final Symbol SPACE =
    PACKAGE_CL.addExternalSymbol("SPACE");
  public static final Symbol SPECIAL =
    PACKAGE_CL.addExternalSymbol("SPECIAL");
  public static final Symbol SPECIAL_OPERATOR_P =
    PACKAGE_CL.addExternalSymbol("SPECIAL-OPERATOR-P");
  public static final Symbol SPEED =
    PACKAGE_CL.addExternalSymbol("SPEED");
  public static final Symbol SQRT =
    PACKAGE_CL.addExternalSymbol("SQRT");
  public static final Symbol STABLE_SORT =
    PACKAGE_CL.addExternalSymbol("STABLE-SORT");
  public static final Symbol STANDARD =
    PACKAGE_CL.addExternalSymbol("STANDARD");
  public static final Symbol STANDARD_CHAR =
    PACKAGE_CL.addExternalSymbol("STANDARD-CHAR");
  public static final Symbol STANDARD_CHAR_P =
    PACKAGE_CL.addExternalSymbol("STANDARD-CHAR-P");
  public static final Symbol STANDARD_CLASS =
    PACKAGE_CL.addExternalSymbol("STANDARD-CLASS");
  public static final Symbol STANDARD_GENERIC_FUNCTION =
    PACKAGE_CL.addExternalSymbol("STANDARD-GENERIC-FUNCTION");
  public static final Symbol STANDARD_METHOD =
    PACKAGE_CL.addExternalSymbol("STANDARD-METHOD");
  public static final Symbol STANDARD_OBJECT =
    PACKAGE_CL.addExternalSymbol("STANDARD-OBJECT");
  public static final Symbol STEP =
    PACKAGE_CL.addExternalSymbol("STEP");
  public static final Symbol STORAGE_CONDITION =
    PACKAGE_CL.addExternalSymbol("STORAGE-CONDITION");
  public static final Symbol STORE_VALUE =
    PACKAGE_CL.addExternalSymbol("STORE-VALUE");
  public static final Symbol STREAM =
    PACKAGE_CL.addExternalSymbol("STREAM");
  public static final Symbol STREAM_ELEMENT_TYPE =
    PACKAGE_CL.addExternalSymbol("STREAM-ELEMENT-TYPE");
  public static final Symbol STREAM_ERROR =
    PACKAGE_CL.addExternalSymbol("STREAM-ERROR");
  public static final Symbol STREAM_ERROR_STREAM =
    PACKAGE_CL.addExternalSymbol("STREAM-ERROR-STREAM");
  public static final Symbol STREAM_EXTERNAL_FORMAT =
    PACKAGE_CL.addExternalSymbol("STREAM-EXTERNAL-FORMAT");
  public static final Symbol STREAMP =
    PACKAGE_CL.addExternalSymbol("STREAMP");
  public static final Symbol STRING =
    PACKAGE_CL.addExternalSymbol("STRING");
  public static final Symbol STRING_CAPITALIZE =
    PACKAGE_CL.addExternalSymbol("STRING-CAPITALIZE");
  public static final Symbol STRING_DOWNCASE =
    PACKAGE_CL.addExternalSymbol("STRING-DOWNCASE");
  public static final Symbol STRING_EQUAL =
    PACKAGE_CL.addExternalSymbol("STRING-EQUAL");
  public static final Symbol STRING_GREATERP =
    PACKAGE_CL.addExternalSymbol("STRING-GREATERP");
  public static final Symbol STRING_LEFT_TRIM =
    PACKAGE_CL.addExternalSymbol("STRING-LEFT-TRIM");
  public static final Symbol STRING_LESSP =
    PACKAGE_CL.addExternalSymbol("STRING-LESSP");
  public static final Symbol STRING_NOT_EQUAL =
    PACKAGE_CL.addExternalSymbol("STRING-NOT-EQUAL");
  public static final Symbol STRING_NOT_GREATERP =
    PACKAGE_CL.addExternalSymbol("STRING-NOT-GREATERP");
  public static final Symbol STRING_NOT_LESSP =
    PACKAGE_CL.addExternalSymbol("STRING-NOT-LESSP");
  public static final Symbol STRING_RIGHT_TRIM =
    PACKAGE_CL.addExternalSymbol("STRING-RIGHT-TRIM");
  public static final Symbol STRING_STREAM =
    PACKAGE_CL.addExternalSymbol("STRING-STREAM");
  public static final Symbol STRING_TRIM =
    PACKAGE_CL.addExternalSymbol("STRING-TRIM");
  public static final Symbol STRING_UPCASE =
    PACKAGE_CL.addExternalSymbol("STRING-UPCASE");
  public static final Symbol STRING_NE =
    PACKAGE_CL.addExternalSymbol("STRING/=");
  public static final Symbol STRING_LT =
    PACKAGE_CL.addExternalSymbol("STRING<");
  public static final Symbol STRING_LE =
    PACKAGE_CL.addExternalSymbol("STRING<=");
  public static final Symbol STRING_EQUALS =
    PACKAGE_CL.addExternalSymbol("STRING=");
  public static final Symbol STRING_GT =
    PACKAGE_CL.addExternalSymbol("STRING>");
  public static final Symbol STRING_GE =
    PACKAGE_CL.addExternalSymbol("STRING>=");
  public static final Symbol STRINGP =
    PACKAGE_CL.addExternalSymbol("STRINGP");
  public static final Symbol STRUCTURE =
    PACKAGE_CL.addExternalSymbol("STRUCTURE");
  public static final Symbol STRUCTURE_CLASS =
    PACKAGE_CL.addExternalSymbol("STRUCTURE-CLASS");
  public static final Symbol STRUCTURE_OBJECT =
    PACKAGE_CL.addExternalSymbol("STRUCTURE-OBJECT");
  public static final Symbol STYLE_WARNING =
    PACKAGE_CL.addExternalSymbol("STYLE-WARNING");
  public static final Symbol SUBLIS =
    PACKAGE_CL.addExternalSymbol("SUBLIS");
  public static final Symbol SUBSEQ =
    PACKAGE_CL.addExternalSymbol("SUBSEQ");
  public static final Symbol SUBSETP =
    PACKAGE_CL.addExternalSymbol("SUBSETP");
  public static final Symbol SUBST =
    PACKAGE_CL.addExternalSymbol("SUBST");
  public static final Symbol SUBST_IF =
    PACKAGE_CL.addExternalSymbol("SUBST-IF");
  public static final Symbol SUBST_IF_NOT =
    PACKAGE_CL.addExternalSymbol("SUBST-IF-NOT");
  public static final Symbol SUBSTITUTE =
    PACKAGE_CL.addExternalSymbol("SUBSTITUTE");
  public static final Symbol SUBSTITUTE_IF =
    PACKAGE_CL.addExternalSymbol("SUBSTITUTE-IF");
  public static final Symbol SUBSTITUTE_IF_NOT =
    PACKAGE_CL.addExternalSymbol("SUBSTITUTE-IF-NOT");
  public static final Symbol SUBTYPEP =
    PACKAGE_CL.addExternalSymbol("SUBTYPEP");
  public static final Symbol SVREF =
    PACKAGE_CL.addExternalSymbol("SVREF");
  public static final Symbol SXHASH =
    PACKAGE_CL.addExternalSymbol("SXHASH");
  public static final Symbol SYMBOL =
    PACKAGE_CL.addExternalSymbol("SYMBOL");
  public static final Symbol SYMBOL_FUNCTION =
    PACKAGE_CL.addExternalSymbol("SYMBOL-FUNCTION");
  public static final Symbol SYMBOL_MACROLET =
    PACKAGE_CL.addExternalSymbol("SYMBOL-MACROLET");
  public static final Symbol SYMBOL_NAME =
    PACKAGE_CL.addExternalSymbol("SYMBOL-NAME");
  public static final Symbol SYMBOL_PACKAGE =
    PACKAGE_CL.addExternalSymbol("SYMBOL-PACKAGE");
  public static final Symbol SYMBOL_PLIST =
    PACKAGE_CL.addExternalSymbol("SYMBOL-PLIST");
  public static final Symbol SYMBOL_VALUE =
    PACKAGE_CL.addExternalSymbol("SYMBOL-VALUE");
  public static final Symbol SYMBOLP =
    PACKAGE_CL.addExternalSymbol("SYMBOLP");
  public static final Symbol SYNONYM_STREAM =
    PACKAGE_CL.addExternalSymbol("SYNONYM-STREAM");
  public static final Symbol SYNONYM_STREAM_SYMBOL =
    PACKAGE_CL.addExternalSymbol("SYNONYM-STREAM-SYMBOL");
  public static final Symbol T =
    PACKAGE_CL.addExternalSymbol("T");
  public static final Symbol TAGBODY =
    PACKAGE_CL.addExternalSymbol("TAGBODY");
  public static final Symbol TAILP =
    PACKAGE_CL.addExternalSymbol("TAILP");
  public static final Symbol TAN =
    PACKAGE_CL.addExternalSymbol("TAN");
  public static final Symbol TANH =
    PACKAGE_CL.addExternalSymbol("TANH");
  public static final Symbol TENTH =
    PACKAGE_CL.addExternalSymbol("TENTH");
  public static final Symbol TERPRI =
    PACKAGE_CL.addExternalSymbol("TERPRI");
  public static final Symbol THE =
    PACKAGE_CL.addExternalSymbol("THE");
  public static final Symbol THIRD =
    PACKAGE_CL.addExternalSymbol("THIRD");
  public static final Symbol THROW =
    PACKAGE_CL.addExternalSymbol("THROW");
  public static final Symbol TIME =
    PACKAGE_CL.addExternalSymbol("TIME");
  public static final Symbol TRACE =
    PACKAGE_CL.addExternalSymbol("TRACE");
  public static final Symbol TRANSLATE_LOGICAL_PATHNAME =
    PACKAGE_CL.addExternalSymbol("TRANSLATE-LOGICAL-PATHNAME");
  public static final Symbol TRANSLATE_PATHNAME =
    PACKAGE_CL.addExternalSymbol("TRANSLATE-PATHNAME");
  public static final Symbol TREE_EQUAL =
    PACKAGE_CL.addExternalSymbol("TREE-EQUAL");
  public static final Symbol TRUENAME =
    PACKAGE_CL.addExternalSymbol("TRUENAME");
  public static final Symbol TRUNCATE =
    PACKAGE_CL.addExternalSymbol("TRUNCATE");
  public static final Symbol TWO_WAY_STREAM =
    PACKAGE_CL.addExternalSymbol("TWO-WAY-STREAM");
  public static final Symbol TWO_WAY_STREAM_INPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("TWO-WAY-STREAM-INPUT-STREAM");
  public static final Symbol TWO_WAY_STREAM_OUTPUT_STREAM =
    PACKAGE_CL.addExternalSymbol("TWO-WAY-STREAM-OUTPUT-STREAM");
  public static final Symbol TYPE =
    PACKAGE_CL.addExternalSymbol("TYPE");
  public static final Symbol TYPE_ERROR =
    PACKAGE_CL.addExternalSymbol("TYPE-ERROR");
  public static final Symbol TYPE_ERROR_DATUM =
    PACKAGE_CL.addExternalSymbol("TYPE-ERROR-DATUM");
  public static final Symbol TYPE_ERROR_EXPECTED_TYPE =
    PACKAGE_CL.addExternalSymbol("TYPE-ERROR-EXPECTED-TYPE");
  public static final Symbol TYPE_OF =
    PACKAGE_CL.addExternalSymbol("TYPE-OF");
  public static final Symbol TYPECASE =
    PACKAGE_CL.addExternalSymbol("TYPECASE");
  public static final Symbol TYPEP =
    PACKAGE_CL.addExternalSymbol("TYPEP");
  public static final Symbol UNBOUND_SLOT =
    PACKAGE_CL.addExternalSymbol("UNBOUND-SLOT");
  public static final Symbol UNBOUND_SLOT_INSTANCE =
    PACKAGE_CL.addExternalSymbol("UNBOUND-SLOT-INSTANCE");
  public static final Symbol UNBOUND_VARIABLE =
    PACKAGE_CL.addExternalSymbol("UNBOUND-VARIABLE");
  public static final Symbol UNDEFINED_FUNCTION =
    PACKAGE_CL.addExternalSymbol("UNDEFINED-FUNCTION");
  public static final Symbol UNEXPORT =
    PACKAGE_CL.addExternalSymbol("UNEXPORT");
  public static final Symbol UNINTERN =
    PACKAGE_CL.addExternalSymbol("UNINTERN");
  public static final Symbol UNION =
    PACKAGE_CL.addExternalSymbol("UNION");
  public static final Symbol UNLESS =
    PACKAGE_CL.addExternalSymbol("UNLESS");
  public static final Symbol UNREAD_CHAR =
    PACKAGE_CL.addExternalSymbol("UNREAD-CHAR");
  public static final Symbol UNSIGNED_BYTE =
    PACKAGE_CL.addExternalSymbol("UNSIGNED-BYTE");
  public static final Symbol UNTRACE =
    PACKAGE_CL.addExternalSymbol("UNTRACE");
  public static final Symbol UNUSE_PACKAGE =
    PACKAGE_CL.addExternalSymbol("UNUSE-PACKAGE");
  public static final Symbol UNWIND_PROTECT =
    PACKAGE_CL.addExternalSymbol("UNWIND-PROTECT");
  public static final Symbol UPDATE_INSTANCE_FOR_DIFFERENT_CLASS =
    PACKAGE_CL.addExternalSymbol("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS");
  public static final Symbol UPDATE_INSTANCE_FOR_REDEFINED_CLASS =
    PACKAGE_CL.addExternalSymbol("UPDATE-INSTANCE-FOR-REDEFINED-CLASS");
  public static final Symbol UPGRADED_ARRAY_ELEMENT_TYPE =
    PACKAGE_CL.addExternalSymbol("UPGRADED-ARRAY-ELEMENT-TYPE");
  public static final Symbol UPGRADED_COMPLEX_PART_TYPE =
    PACKAGE_CL.addExternalSymbol("UPGRADED-COMPLEX-PART-TYPE");
  public static final Symbol UPPER_CASE_P =
    PACKAGE_CL.addExternalSymbol("UPPER-CASE-P");
  public static final Symbol USE_PACKAGE =
    PACKAGE_CL.addExternalSymbol("USE-PACKAGE");
  public static final Symbol USE_VALUE =
    PACKAGE_CL.addExternalSymbol("USE-VALUE");
  public static final Symbol USER_HOMEDIR_PATHNAME =
    PACKAGE_CL.addExternalSymbol("USER-HOMEDIR-PATHNAME");
  public static final Symbol VALUES =
    PACKAGE_CL.addExternalSymbol("VALUES");
  public static final Symbol VALUES_LIST =
    PACKAGE_CL.addExternalSymbol("VALUES-LIST");
  public static final Symbol VARIABLE =
    PACKAGE_CL.addExternalSymbol("VARIABLE");
  public static final Symbol VECTOR =
    PACKAGE_CL.addExternalSymbol("VECTOR");
  public static final Symbol VECTOR_POP =
    PACKAGE_CL.addExternalSymbol("VECTOR-POP");
  public static final Symbol VECTOR_PUSH =
    PACKAGE_CL.addExternalSymbol("VECTOR-PUSH");
  public static final Symbol VECTOR_PUSH_EXTEND =
    PACKAGE_CL.addExternalSymbol("VECTOR-PUSH-EXTEND");
  public static final Symbol VECTORP =
    PACKAGE_CL.addExternalSymbol("VECTORP");
  public static final Symbol WARN =
    PACKAGE_CL.addExternalSymbol("WARN");
  public static final Symbol WARNING =
    PACKAGE_CL.addExternalSymbol("WARNING");
  public static final Symbol WHEN =
    PACKAGE_CL.addExternalSymbol("WHEN");
  public static final Symbol WILD_PATHNAME_P =
    PACKAGE_CL.addExternalSymbol("WILD-PATHNAME-P");
  public static final Symbol WITH_ACCESSORS =
    PACKAGE_CL.addExternalSymbol("WITH-ACCESSORS");
  public static final Symbol WITH_COMPILATION_UNIT =
    PACKAGE_CL.addExternalSymbol("WITH-COMPILATION-UNIT");
  public static final Symbol WITH_CONDITION_RESTARTS =
    PACKAGE_CL.addExternalSymbol("WITH-CONDITION-RESTARTS");
  public static final Symbol WITH_HASH_TABLE_ITERATOR =
    PACKAGE_CL.addExternalSymbol("WITH-HASH-TABLE-ITERATOR");
  public static final Symbol WITH_INPUT_FROM_STRING =
    PACKAGE_CL.addExternalSymbol("WITH-INPUT-FROM-STRING");
  public static final Symbol WITH_OPEN_FILE =
    PACKAGE_CL.addExternalSymbol("WITH-OPEN-FILE");
  public static final Symbol WITH_OPEN_STREAM =
    PACKAGE_CL.addExternalSymbol("WITH-OPEN-STREAM");
  public static final Symbol WITH_OUTPUT_TO_STRING =
    PACKAGE_CL.addExternalSymbol("WITH-OUTPUT-TO-STRING");
  public static final Symbol WITH_PACKAGE_ITERATOR =
    PACKAGE_CL.addExternalSymbol("WITH-PACKAGE-ITERATOR");
  public static final Symbol WITH_SIMPLE_RESTART =
    PACKAGE_CL.addExternalSymbol("WITH-SIMPLE-RESTART");
  public static final Symbol WITH_SLOTS =
    PACKAGE_CL.addExternalSymbol("WITH-SLOTS");
  public static final Symbol WITH_STANDARD_IO_SYNTAX =
    PACKAGE_CL.addExternalSymbol("WITH-STANDARD-IO-SYNTAX");
  public static final Symbol WRITE =
    PACKAGE_CL.addExternalSymbol("WRITE");
  public static final Symbol WRITE_BYTE =
    PACKAGE_CL.addExternalSymbol("WRITE-BYTE");
  public static final Symbol WRITE_CHAR =
    PACKAGE_CL.addExternalSymbol("WRITE-CHAR");
  public static final Symbol WRITE_LINE =
    PACKAGE_CL.addExternalSymbol("WRITE-LINE");
  public static final Symbol WRITE_SEQUENCE =
    PACKAGE_CL.addExternalSymbol("WRITE-SEQUENCE");
  public static final Symbol WRITE_STRING =
    PACKAGE_CL.addExternalSymbol("WRITE-STRING");
  public static final Symbol WRITE_TO_STRING =
    PACKAGE_CL.addExternalSymbol("WRITE-TO-STRING");
  public static final Symbol Y_OR_N_P =
    PACKAGE_CL.addExternalSymbol("Y-OR-N-P");
  public static final Symbol YES_OR_NO_P =
    PACKAGE_CL.addExternalSymbol("YES-OR-NO-P");
  public static final Symbol ZEROP =
    PACKAGE_CL.addExternalSymbol("ZEROP");
  // End of CL symbols.

  // Extensions.
  public static final Symbol MOST_POSITIVE_JAVA_LONG =
    PACKAGE_EXT.addExternalSymbol("MOST-POSITIVE-JAVA-LONG");
  public static final Symbol MOST_NEGATIVE_JAVA_LONG=
    PACKAGE_EXT.addExternalSymbol("MOST-NEGATIVE-JAVA-LONG");
  public static final Symbol SINGLE_FLOAT_POSITIVE_INFINITY =
    PACKAGE_EXT.addExternalSymbol("SINGLE-FLOAT-POSITIVE-INFINITY");
  public static final Symbol SINGLE_FLOAT_NEGATIVE_INFINITY =
    PACKAGE_EXT.addExternalSymbol("SINGLE-FLOAT-NEGATIVE-INFINITY");
  public static final Symbol DOUBLE_FLOAT_POSITIVE_INFINITY =
    PACKAGE_EXT.addExternalSymbol("DOUBLE-FLOAT-POSITIVE-INFINITY");
  public static final Symbol DOUBLE_FLOAT_NEGATIVE_INFINITY =
    PACKAGE_EXT.addExternalSymbol("DOUBLE-FLOAT-NEGATIVE-INFINITY");
  public static final Symbol STYLE_WARN =
    PACKAGE_EXT.addExternalSymbol("STYLE-WARN");
  public static final Symbol MEMQ =
    PACKAGE_EXT.addExternalSymbol("MEMQ");
  public static final Symbol MEMQL =
    PACKAGE_EXT.addExternalSymbol("MEMQL");
  public static final Symbol NIL_VECTOR =
    PACKAGE_EXT.addExternalSymbol("NIL-VECTOR");
  public static final Symbol COMPILER_ERROR =
    PACKAGE_EXT.addExternalSymbol("COMPILER-ERROR");
  public static final Symbol INTERNAL_COMPILER_ERROR =
    PACKAGE_EXT.addExternalSymbol("INTERNAL-COMPILER-ERROR");
  public static final Symbol COMPILER_UNSUPPORTED_FEATURE_ERROR =
    PACKAGE_EXT.addExternalSymbol("COMPILER-UNSUPPORTED-FEATURE-ERROR");
  public static final Symbol MAILBOX =
    PACKAGE_EXT.addExternalSymbol("MAILBOX");
  public static final Symbol MUTEX =
    PACKAGE_EXT.addExternalSymbol("MUTEX");
  public static final Symbol SUPPRESS_COMPILER_WARNINGS =
    PACKAGE_EXT.addExternalSymbol("*SUPPRESS-COMPILER-WARNINGS*");
  public static final Symbol NEQ =
    PACKAGE_EXT.addExternalSymbol("NEQ");
  public static final Symbol ADJOIN_EQL =
    PACKAGE_EXT.addExternalSymbol("ADJOIN-EQL");
  public static final Symbol CHARACTER_DESIGNATOR =
    PACKAGE_EXT.addExternalSymbol("CHARACTER-DESIGNATOR");
  public static final Symbol INTERRUPT_LISP =
    PACKAGE_EXT.addExternalSymbol("INTERRUPT-LISP");
  public static final Symbol GETENV =
    PACKAGE_EXT.addExternalSymbol("GETENV");
  public static final Symbol MACROEXPAND_ALL =
    PACKAGE_EXT.addExternalSymbol("MACROEXPAND-ALL");
  public static final Symbol LOAD_TRUENAME_FASL =
    PACKAGE_EXT.addExternalSymbol("*LOAD-TRUENAME-FASL*");
  public static final Symbol SLIME_INPUT_STREAM =
    PACKAGE_EXT.addExternalSymbol("SLIME-INPUT-STREAM");
  public static final Symbol SLIME_OUTPUT_STREAM =
    PACKAGE_EXT.addExternalSymbol("SLIME-OUTPUT-STREAM");
  public static final Symbol JAR_PATHNAME =
    PACKAGE_EXT.addExternalSymbol("JAR-PATHNAME");
  public static final Symbol URL_PATHNAME =
    PACKAGE_EXT.addExternalSymbol("URL-PATHNAME");
  public static final Symbol WEAK_REFERENCE =
    PACKAGE_EXT.addExternalSymbol("WEAK-REFERENCE");

  // MOP.
  public static final Symbol CLASS_LAYOUT =
    PACKAGE_MOP.addInternalSymbol("CLASS-LAYOUT");
  public static final Symbol CLASS_PRECEDENCE_LIST =
    PACKAGE_MOP.addInternalSymbol("CLASS-PRECEDENCE-LIST");
  public static final Symbol STANDARD_READER_METHOD =
    PACKAGE_MOP.addExternalSymbol("STANDARD-READER-METHOD");
  public static final Symbol DIRECT_SLOT_DEFINITION =
    PACKAGE_MOP.addExternalSymbol("DIRECT-SLOT-DEFINITION");
  public static final Symbol EFFECTIVE_SLOT_DEFINITION =
    PACKAGE_MOP.addExternalSymbol("EFFECTIVE-SLOT-DEFINITION");
  public static final Symbol STANDARD_SLOT_DEFINITION =
    PACKAGE_MOP.addExternalSymbol("STANDARD-SLOT-DEFINITION");
  public static final Symbol STANDARD_DIRECT_SLOT_DEFINITION =
    PACKAGE_MOP.addExternalSymbol("STANDARD-DIRECT-SLOT-DEFINITION");
  public static final Symbol STANDARD_EFFECTIVE_SLOT_DEFINITION =
    PACKAGE_MOP.addExternalSymbol("STANDARD-EFFECTIVE-SLOT-DEFINITION");

  // Java interface.
  public static final Symbol JAVA_EXCEPTION =
    PACKAGE_JAVA.addExternalSymbol("JAVA-EXCEPTION");
  public static final Symbol JAVA_EXCEPTION_CAUSE =
    PACKAGE_JAVA.addExternalSymbol("JAVA-EXCEPTION-CAUSE");
  public static final Symbol JAVA_OBJECT =
    PACKAGE_JAVA.addExternalSymbol("JAVA-OBJECT");
  public static final Symbol JAVA_CLASS =
      PACKAGE_JAVA.addExternalSymbol("JAVA-CLASS");
  public static final Symbol JCALL =
    PACKAGE_JAVA.addExternalSymbol("JCALL");
  public static final Symbol JCALL_RAW =
    PACKAGE_JAVA.addExternalSymbol("JCALL-RAW");
  public static final Symbol JCLASS =
    PACKAGE_JAVA.addExternalSymbol("JCLASS");
  public static final Symbol JCLASS_NAME =
    PACKAGE_JAVA.addExternalSymbol("JCLASS-NAME");
  public static final Symbol JCLASS_OF =
    PACKAGE_JAVA.addExternalSymbol("JCLASS-OF");
  public static final Symbol JMETHOD_RETURN_TYPE =
    PACKAGE_JAVA.addExternalSymbol("JMETHOD-RETURN-TYPE");
  public static final Symbol JRESOLVE_METHOD =
    PACKAGE_JAVA.addExternalSymbol("JRESOLVE-METHOD");

  // External symbols in SYSTEM package.
  public static final Symbol _ENABLE_AUTOCOMPILE_ =
    PACKAGE_SYS.addExternalSymbol("*ENABLE-AUTOCOMPILE*");
  public static final Symbol AUTOCOMPILE =
    PACKAGE_SYS.addExternalSymbol("AUTOCOMPILE");
  public static final Symbol ENVIRONMENT =
    PACKAGE_SYS.addExternalSymbol("ENVIRONMENT");
  public static final Symbol FORWARD_REFERENCED_CLASS =
    PACKAGE_SYS.addExternalSymbol("FORWARD-REFERENCED-CLASS");
  public static final Symbol FLOAT_UNDERFLOW_MODE =
    PACKAGE_SYS.addExternalSymbol("FLOAT-UNDERFLOW-MODE");
  public static final Symbol FLOAT_OVERFLOW_MODE =
    PACKAGE_SYS.addExternalSymbol("FLOAT-OVERFLOW-MODE");
  public static final Symbol CLASS_BYTES =
    PACKAGE_SYS.addExternalSymbol("CLASS-BYTES");
  public static final Symbol _CLASS_SLOTS =
    PACKAGE_SYS.addExternalSymbol("%CLASS-SLOTS");
  public static final Symbol COMPILED_LISP_FUNCTION_P =
    PACKAGE_SYS.addExternalSymbol("COMPILED-LISP-FUNCTION-P");
  public static final Symbol LAYOUT =
    PACKAGE_SYS.addExternalSymbol("LAYOUT");
  public static final Symbol NAMED_LAMBDA =
    PACKAGE_SYS.addExternalSymbol("NAMED-LAMBDA");
  public static final Symbol OUTPUT_OBJECT =
    PACKAGE_SYS.addExternalSymbol("OUTPUT-OBJECT");
  public static final Symbol _SET_CLASS_SLOTS =
    PACKAGE_SYS.addExternalSymbol("%SET-CLASS-SLOTS");
  public static final Symbol SETF_FUNCTION =
    PACKAGE_SYS.addExternalSymbol("SETF-FUNCTION");
  public static final Symbol SETF_INVERSE =
    PACKAGE_SYS.addExternalSymbol("SETF-INVERSE");
  public static final Symbol SLOT_DEFINITION =
    PACKAGE_SYS.addExternalSymbol("SLOT-DEFINITION");
  public static final Symbol _SLOT_DEFINITION_NAME =
    PACKAGE_SYS.addExternalSymbol("%SLOT-DEFINITION-NAME");
  public static final Symbol _SLOT_DEFINITION_INITARGS =
    PACKAGE_SYS.addExternalSymbol("%SLOT-DEFINITION-INITARGS");
  public static final Symbol _SLOT_DEFINITION_INITFUNCTION =
    PACKAGE_SYS.addExternalSymbol("%SLOT-DEFINITION-INITFUNCTION");
  public static final Symbol _DOCUMENTATION =
    PACKAGE_SYS.addExternalSymbol("%DOCUMENTATION");
  public static final Symbol STD_SLOT_BOUNDP =
    PACKAGE_SYS.addExternalSymbol("STD-SLOT-BOUNDP");
  public static final Symbol STD_SLOT_VALUE =
    PACKAGE_SYS.addExternalSymbol("STD-SLOT-VALUE");
  public static final Symbol SET_STD_SLOT_VALUE =
    PACKAGE_SYS.addExternalSymbol("SET-STD-SLOT-VALUE");
  public static final Symbol SUBCLASSP =
    PACKAGE_SYS.addExternalSymbol("SUBCLASSP");
  public static final Symbol GETHASH1 =
    PACKAGE_SYS.addExternalSymbol("GETHASH1");
  public static final Symbol PUTHASH =
    PACKAGE_SYS.addExternalSymbol("PUTHASH");
  public static final Symbol UNDEFINED_FUNCTION_CALLED =
    PACKAGE_SYS.addExternalSymbol("UNDEFINED-FUNCTION-CALLED");
  public static final Symbol SET_CHAR =
    PACKAGE_SYS.addExternalSymbol("SET-CHAR");
  public static final Symbol SET_SCHAR =
    PACKAGE_SYS.addExternalSymbol("SET-SCHAR");
  public static final Symbol JAR_STREAM =
    PACKAGE_SYS.addExternalSymbol("JAR-STREAM");
  public static final Symbol URL_STREAM =
    PACKAGE_SYS.addExternalSymbol("URL-STREAM");


  // Internal symbols in SYSTEM package.
  public static final Symbol BACKQUOTE_MACRO =
    PACKAGE_SYS.addInternalSymbol("BACKQUOTE-MACRO");
  public static final Symbol CASE_FROB_STREAM =
    PACKAGE_SYS.addInternalSymbol("CASE-FROB-STREAM");
  public static final Symbol CAUSE =
    PACKAGE_SYS.addInternalSymbol("CAUSE");
  public static final Symbol COMMA_MACRO =
    PACKAGE_SYS.addInternalSymbol("COMMA-MACRO");
  public static final Symbol DATUM =
    PACKAGE_SYS.addInternalSymbol("DATUM");
  public static final Symbol DEFTYPE_DEFINITION =
    PACKAGE_SYS.addInternalSymbol("DEFTYPE-DEFINITION");
  public static final Symbol EXPECTED_TYPE =
    PACKAGE_SYS.addInternalSymbol("EXPECTED-TYPE");
  public static final Symbol FORMAT_ARGUMENTS =
    PACKAGE_SYS.addInternalSymbol("FORMAT-ARGUMENTS");
  public static final Symbol FORMAT_CONTROL =
    PACKAGE_SYS.addInternalSymbol("FORMAT-CONTROL");
  public static final Symbol FSET =
    PACKAGE_SYS.addInternalSymbol("FSET");
  public static final Symbol FUNCTION_PRELOAD =
    PACKAGE_SYS.addInternalSymbol("FUNCTION-PRELOAD");
  public static final Symbol INSTANCE =
    PACKAGE_SYS.addInternalSymbol("INSTANCE");
  public static final Symbol MACROEXPAND_MACRO =
    PACKAGE_SYS.addInternalSymbol("MACROEXPAND-MACRO");
  public static final Symbol MAKE_FUNCTION_PRELOADING_CONTEXT =
    PACKAGE_SYS.addInternalSymbol("MAKE-FUNCTION-PRELOADING-CONTEXT");
  public static final Symbol NAME =
    PACKAGE_SYS.addInternalSymbol("NAME");
  public static final Symbol OBJECT =
    PACKAGE_SYS.addInternalSymbol("OBJECT");
  public static final Symbol OPERANDS =
    PACKAGE_SYS.addInternalSymbol("OPERANDS");
  public static final Symbol OPERATION =
    PACKAGE_SYS.addInternalSymbol("OPERATION");
  public static final Symbol PROXY_PRELOADED_FUNCTION =
    PACKAGE_SYS.addInternalSymbol("PROXY-PRELOADED-FUNCTION");
  public static final Symbol _SOURCE =
    PACKAGE_SYS.addInternalSymbol("%SOURCE");
  public static final Symbol SOCKET_STREAM =
    PACKAGE_SYS.addInternalSymbol("SOCKET-STREAM");
  public static final Symbol STRING_INPUT_STREAM =
    PACKAGE_SYS.addInternalSymbol("STRING-INPUT-STREAM");
  public static final Symbol STRING_OUTPUT_STREAM =
    PACKAGE_SYS.addInternalSymbol("STRING-OUTPUT-STREAM");
  public static final Symbol SYSTEM_STREAM =
    PACKAGE_SYS.addInternalSymbol("SYSTEM-STREAM");
  public static final Symbol STACK_FRAME =
    PACKAGE_SYS.addInternalSymbol("STACK-FRAME");
  public static final Symbol LISP_STACK_FRAME =
    PACKAGE_SYS.addInternalSymbol("LISP-STACK-FRAME");
  public static final Symbol JAVA_STACK_FRAME =
    PACKAGE_SYS.addInternalSymbol("JAVA-STACK-FRAME");

  // CDR6
  public static final Symbol _INSPECTOR_HOOK_ =
    PACKAGE_EXT.addExternalSymbol("*INSPECTOR-HOOK*");

  public static final Symbol COMPILER_LET =
    PACKAGE_LISP.addExternalSymbol("COMPILER-LET");

  // THREADS
  public static final Symbol THREAD =
    PACKAGE_THREADS.addExternalSymbol("THREAD");

}
