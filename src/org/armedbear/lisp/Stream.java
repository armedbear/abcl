/*
 * Stream.java
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.BitSet;


/** The stream class
 * 
 * A base class for all Lisp built-in streams.
 * 
 */
public class Stream extends LispObject
{
  protected LispObject elementType;
  protected boolean isInputStream;
  protected boolean isOutputStream;
  protected boolean isCharacterStream;
  protected boolean isBinaryStream;

  private boolean pastEnd = false;
  private boolean interactive;
  private boolean open = true;
  
  // Character input.
  protected PushbackReader reader;
  protected int offset;
  protected int lineNumber;

  // Character output.
  private Writer writer;

  /** The number of characters on the current line of output
   * 
   * Used to determine whether additional line feeds are
   * required when calling FRESH-LINE
   */
  protected int charPos;
  
  public enum EolStyle {
    RAW,
    CR,
    CRLF,
    LF
  }

  static final protected Symbol keywordDefault = Packages.internKeyword("DEFAULT");
  
  static final private Symbol keywordCodePage = Packages.internKeyword("CODE-PAGE");
  static final private Symbol keywordID = Packages.internKeyword("ID");

  static final private Symbol keywordEolStyle = Packages.internKeyword("EOL-STYLE");
  static final private Symbol keywordCR = Packages.internKeyword("CR");
  static final private Symbol keywordLF = Packages.internKeyword("LF");
  static final private Symbol keywordCRLF = Packages.internKeyword("CRLF");
  static final private Symbol keywordRAW = Packages.internKeyword("RAW");
    
  public final static EolStyle platformEolStyle = Utilities.isPlatformWindows ? EolStyle.CRLF : EolStyle.LF;
    
  protected EolStyle eolStyle = platformEolStyle;
  protected char eolChar = (eolStyle == EolStyle.CR) ? '\r' : '\n';
  protected LispObject externalFormat = LispObject.NIL;
  protected String encoding = null;
  protected char lastChar = 0;
  
  // Binary input.
  private InputStream in;

  // Binary output.
  private OutputStream out;

  protected Stream()
  {
  }

  public Stream(InputStream inputStream, LispObject elementType)
    {
      this(inputStream, elementType, keywordDefault);
    }


  // Input stream constructors.
    public Stream(InputStream inputStream, LispObject elementType, LispObject format)
  {
    this.elementType = elementType;
    setExternalFormat(format);
    
    if (elementType == Symbol.CHARACTER || elementType == Symbol.BASE_CHAR)
      {
        InputStreamReader inputStreamReader =
            (encoding == null) ?
                new InputStreamReader(inputStream)
                : new InputStreamReader(inputStream,
                    Charset.forName(encoding).newDecoder());
        initAsCharacterInputStream(new BufferedReader(inputStreamReader));
      }
    else
      {
        isBinaryStream = true;
        InputStream stream = new BufferedInputStream(inputStream);
	initAsBinaryInputStream(stream);
      }
  }

  public Stream(InputStream inputStream, LispObject elementType, boolean interactive)
  {
    this(inputStream, elementType);
    setInteractive(interactive);
  }

  public Stream(OutputStream outputStream, LispObject elementType)
    {
      this(outputStream, elementType, keywordDefault);
    }
    
  // Output stream constructors.
  public Stream(OutputStream outputStream, LispObject elementType, LispObject format)
  {
    this.elementType = elementType;
    setExternalFormat(format);
    if (elementType == Symbol.CHARACTER || elementType == Symbol.BASE_CHAR)
      {
	Writer w =
            (encoding == null) ?
                new OutputStreamWriter(outputStream)
                : new OutputStreamWriter(outputStream, 
                    Charset.forName(encoding).newEncoder());
	initAsCharacterOutputStream(w);
      }
    else
      {
        OutputStream stream = new BufferedOutputStream(outputStream);
	initAsBinaryOutputStream(stream);
      }
  }

  public Stream(OutputStream outputStream, LispObject elementType,
                boolean interactive)
  {
    this(outputStream, elementType);
    setInteractive(interactive);
  }

  protected void initAsCharacterInputStream(Reader reader)
  {
    if (! (reader instanceof PushbackReader))
        this.reader = new PushbackReader(reader, 5);
    else
        this.reader = (PushbackReader)reader;
    
    isInputStream = true;
    isCharacterStream = true;
  }

  protected void initAsBinaryInputStream(InputStream in) {
    this.in = in;
    isInputStream = true;
    isBinaryStream = true;
  }

  protected void initAsCharacterOutputStream(Writer writer) {
    this.writer = writer;
    isOutputStream = true;
    isCharacterStream = true;
  }

  protected void initAsBinaryOutputStream(OutputStream out) {
    this.out = out;
    isOutputStream = true;
    isBinaryStream = true;
  }

  public boolean isInputStream() throws ConditionThrowable
  {
    return isInputStream;
  }

  public boolean isOutputStream() throws ConditionThrowable
  {
    return isOutputStream;
  }

  public boolean isCharacterInputStream() throws ConditionThrowable
  {
    return isCharacterStream && isInputStream;
  }

  public boolean isBinaryInputStream() throws ConditionThrowable
  {
    return isBinaryStream && isInputStream;
  }

  public boolean isCharacterOutputStream() throws ConditionThrowable
  {
    return isCharacterStream && isOutputStream;
  }

  public boolean isBinaryOutputStream() throws ConditionThrowable
  {
    return isBinaryStream && isOutputStream;
  }

  public boolean isInteractive()
  {
    return interactive;
  }

  public void setInteractive(boolean b)
  {
    interactive = b;
  }

  public LispObject getExternalFormat() {
      return externalFormat;
  }
  
  public String getEncoding() {
      return encoding;
  }
  
  public void setExternalFormat(LispObject format) {
    if (format == keywordDefault) {
      encoding = null;
      eolStyle = platformEolStyle;
      eolChar = (eolStyle == EolStyle.CR) ? '\r' : '\n';
      externalFormat = format;
      return;
    }
      
    try {
      LispObject enc;
      boolean encIsCp = false;
      
      if (format instanceof Cons) {
          // meaning a non-empty list
          enc = format.car();

          if (enc == keywordCodePage) {
              encIsCp = true;

              enc = LispObject.getf(format.cdr(), keywordID, null);
          }
          
          LispObject eol = LispObject.getf(format.cdr(), keywordEolStyle, keywordRAW);
          if (eol == keywordCR)
              eolStyle = EolStyle.CR;
          else if (eol == keywordLF)
              eolStyle = EolStyle.LF;
          else if (eol == keywordCRLF)
              eolStyle = EolStyle.CRLF;
          else if (eol != keywordRAW)
              ; //###FIXME: raise an error
          
      } else
        enc = format;
      
      if (enc.numberp())
          encoding = enc.toString();
      else if (enc instanceof AbstractString)
          encoding = enc.getStringValue();
      else if (enc == keywordDefault)
          // This allows the user to use the encoding determined by
          // Java to be the default for the current environment
          // while still being able to set other stream options
          // (e.g. :EOL-STYLE)
          encoding = null;
      else if (enc instanceof Symbol)
          encoding = ((Symbol)enc).getName();
      else
          ; //###FIXME: raise an error!
      
      if (encIsCp)
          encoding = "Cp" + encoding;
    }
    catch (ConditionThrowable ct) { }
    
    eolChar = (eolStyle == EolStyle.CR) ? '\r' : '\n';
    externalFormat = format;
  }
  
  public boolean isOpen()
  {
    return open;
  }

  public void setOpen(boolean b)
  {
    open = b;
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.STREAM;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.STREAM;
  }

  @Override
  public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
  {
    if (typeSpecifier == Symbol.STREAM)
      return T;
    if (typeSpecifier == BuiltInClass.STREAM)
      return T;
    return super.typep(typeSpecifier);
  }

  public LispObject getElementType() throws ConditionThrowable
  {
    return elementType;
  }

  // Character input.
  public int getOffset()
  {
    return offset;
  }

  // Character input.
  public final int getLineNumber()
  {
    return lineNumber;
  }

  protected void setWriter(Writer writer)
  {
    this.writer = writer;
  }

  // Character output.
  public int getCharPos()
  {
    return charPos;
  }

  // Character output.
  public void setCharPos(int n)
  {
    charPos = n;
  }

  public LispObject read(boolean eofError, LispObject eofValue,
                         boolean recursive, LispThread thread)
    throws ConditionThrowable
  {
    LispObject result = readPreservingWhitespace(eofError, eofValue,
                                                 recursive, thread);
    if (result != eofValue && !recursive)
      {
        if (_charReady())
          {
            int n = _readChar();
            if (n >= 0)
              {
                char c = (char) n;
                Readtable rt = (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
                if (!rt.isWhitespace(c))
                  _unreadChar(c);
              }
          }
      }
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    else
      return result;
  }

  // ### *sharp-equal-alist*
  // internal symbol
  private static final Symbol _SHARP_EQUAL_ALIST_ =
    internSpecial("*SHARP-EQUAL-ALIST*", PACKAGE_SYS, NIL);

  public LispObject readPreservingWhitespace(boolean eofError,
                                             LispObject eofValue,
                                             boolean recursive,
                                             LispThread thread)
    throws ConditionThrowable
  {
    if (recursive)
      {
        final Readtable rt = (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
        while (true)
          {
            int n = _readChar();
            if (n < 0)
              {
                if (eofError)
                  return error(new EndOfFile(this));
                else
                  return eofValue;
              }
            char c = (char) n;
            if (rt.isWhitespace(c))
              continue;
            LispObject result = processChar(c, rt);
            if (result != null)
              return result;
          }
      }
    else
      {
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        thread.bindSpecial(_SHARP_EQUAL_ALIST_, NIL);
        try
          {
            return readPreservingWhitespace(eofError, eofValue, true, thread);
          }
        finally
          {
            thread.lastSpecialBinding = lastSpecialBinding;
          }
      }
  }

  public LispObject faslRead(boolean eofError, LispObject eofValue,
                             boolean recursive, LispThread thread)
    throws ConditionThrowable
  {
    LispObject result = faslReadPreservingWhitespace(eofError, eofValue,
                                                     recursive, thread);
    if (result != eofValue && !recursive)
      {
        if (_charReady())
          {
            int n = _readChar();
            if (n >= 0)
              {
                char c = (char) n;
                Readtable rt = FaslReadtable.getInstance();
                if (!rt.isWhitespace(c))
                  _unreadChar(c);
              }
          }
      }
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    else
      return result;
  }

  private final LispObject faslReadPreservingWhitespace(boolean eofError,
                                                        LispObject eofValue,
                                                        boolean recursive,
                                                        LispThread thread)
    throws ConditionThrowable
  {
    if (recursive)
      {
        final Readtable rt = FaslReadtable.getInstance();
        while (true)
          {
            int n = _readChar();
            if (n < 0)
              {
                if (eofError)
                  return error(new EndOfFile(this));
                else
                  return eofValue;
              }
            char c = (char) n;
            if (rt.isWhitespace(c))
              continue;
            LispObject result = processChar(c, rt);
            if (result != null)
              return result;
          }
      }
    else
      {
        thread.bindSpecial(_SHARP_EQUAL_ALIST_, NIL);
        return faslReadPreservingWhitespace(eofError, eofValue, true, thread);
      }
  }

  private final LispObject processChar(char c, Readtable rt)
    throws ConditionThrowable
  {
    final LispObject handler = rt.getReaderMacroFunction(c);
    if (handler instanceof ReaderMacroFunction)
      return ((ReaderMacroFunction)handler).execute(this, c);
    if (handler != null && handler != NIL)
      return handler.execute(this, LispCharacter.getInstance(c));
    return readToken(c, rt);
  }

  public LispObject readPathname() throws ConditionThrowable
  {
    LispObject obj = read(true, NIL, false, LispThread.currentThread());
    if (obj instanceof AbstractString)
      return Pathname.parseNamestring((AbstractString)obj);
    if (obj.listp())
      return Pathname.makePathname(obj);
    return error(new TypeError("#p requires a string or list argument."));
  }

  public LispObject faslReadPathname() throws ConditionThrowable
  {
    LispObject obj = faslRead(true, NIL, false, LispThread.currentThread());
    if (obj instanceof AbstractString)
      return Pathname.parseNamestring((AbstractString)obj);
    if (obj.listp())
      return Pathname.makePathname(obj);
    return error(new TypeError("#p requires a string or list argument."));
  }

  public LispObject readSymbol() throws ConditionThrowable
  {
    final Readtable rt =
      (Readtable) Symbol.CURRENT_READTABLE.symbolValue(LispThread.currentThread());
    FastStringBuffer sb = new FastStringBuffer();
    _readToken(sb, rt);
    return new Symbol(sb.toString());
  }

  public LispObject readSymbol(Readtable rt) throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer();
    _readToken(sb, rt);
    return new Symbol(sb.toString());
  }

  public LispObject readStructure() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = read(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (obj.listp())
      {
        Symbol structure = checkSymbol(obj.car());
        LispClass c = LispClass.findClass(structure);
        if (!(c instanceof StructureClass))
          return error(new ReaderError(structure.getName() +
                                        " is not a defined structure type.",
                                        this));
        LispObject args = obj.cdr();
        Symbol DEFSTRUCT_DEFAULT_CONSTRUCTOR =
          PACKAGE_SYS.intern("DEFSTRUCT-DEFAULT-CONSTRUCTOR");
        LispObject constructor =
          DEFSTRUCT_DEFAULT_CONSTRUCTOR.getSymbolFunctionOrDie().execute(structure);
        final int length = args.length();
        if ((length % 2) != 0)
          return error(new ReaderError("Odd number of keyword arguments following #S: " +
                                        obj.writeToString(),
                                        this));
        LispObject[] array = new LispObject[length];
        LispObject rest = args;
        for (int i = 0; i < length; i += 2)
          {
            LispObject key = rest.car();
            if (key instanceof Symbol && ((Symbol)key).getPackage() == PACKAGE_KEYWORD)
              {
                array[i] = key;
              }
            else
              {
                array[i] = PACKAGE_KEYWORD.intern(javaString(key));
              }
            array[i + 1] = rest.cadr();
            rest = rest.cddr();
          }
        return funcall(constructor.getSymbolFunctionOrDie(), array,
                       thread);
      }
    return error(new ReaderError("Non-list following #S: " +
                                  obj.writeToString(),
                                  this));
  }

  public LispObject faslReadStructure() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = faslRead(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (obj.listp())
      {
        Symbol structure = checkSymbol(obj.car());
        LispClass c = LispClass.findClass(structure);
        if (!(c instanceof StructureClass))
          return error(new ReaderError(structure.getName() +
                                        " is not a defined structure type.",
                                        this));
        LispObject args = obj.cdr();
        Symbol DEFSTRUCT_DEFAULT_CONSTRUCTOR =
          PACKAGE_SYS.intern("DEFSTRUCT-DEFAULT-CONSTRUCTOR");
        LispObject constructor =
          DEFSTRUCT_DEFAULT_CONSTRUCTOR.getSymbolFunctionOrDie().execute(structure);
        final int length = args.length();
        if ((length % 2) != 0)
          return error(new ReaderError("Odd number of keyword arguments following #S: " +
                                        obj.writeToString(),
                                        this));
        LispObject[] array = new LispObject[length];
        LispObject rest = args;
        for (int i = 0; i < length; i += 2)
          {
            LispObject key = rest.car();
            if (key instanceof Symbol && ((Symbol)key).getPackage() == PACKAGE_KEYWORD)
              {
                array[i] = key;
              }
            else
              {
                array[i] = PACKAGE_KEYWORD.intern(javaString(key));
              }
            array[i + 1] = rest.cadr();
            rest = rest.cddr();
          }
        return funcall(constructor.getSymbolFunctionOrDie(), array,
                       thread);
      }
    return error(new ReaderError("Non-list following #S: " +
                                  obj.writeToString(),
                                  this));
  }

  public LispObject readList(boolean requireProperList, boolean useFaslReadtable)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    Cons first = null;
    Cons last = null;
    Readtable rt = null;
    if (useFaslReadtable)
      rt = FaslReadtable.getInstance();
    while (true)
      {
        if (!useFaslReadtable)
          rt = (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
        char c = flushWhitespace(rt);
        if (c == ')')
          {
            return first == null ? NIL : first;
          }
        if (c == '.')
          {
            int n = _readChar();
            if (n < 0)
              return error(new EndOfFile(this));
            char nextChar = (char) n;
            if (isTokenDelimiter(nextChar, rt))
              {
                if (last == null)
                  {
                    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
                      return NIL;
                    else
                      return error(new ReaderError("Nothing appears before . in list.",
                                                    this));
                  }
                _unreadChar(nextChar);
                LispObject obj = read(true, NIL, true, thread);
                if (requireProperList)
                  {
                    if (!obj.listp())
                      error(new ReaderError("The value " +
                                             obj.writeToString() +
                                             " is not of type " +
                                             Symbol.LIST.writeToString() + ".",
                                             this));
                  }
                last.cdr = obj;
                continue;
              }
            // normal token beginning with '.'
            _unreadChar(nextChar);
          }
        LispObject obj = processChar(c, rt);
        if (obj == null)
          {
            // A comment.
            continue;
          }
        if (first == null)
          {
            first = new Cons(obj);
            last = first;
          }
        else
          {
            Cons newCons = new Cons(obj);
            last.cdr = newCons;
            last = newCons;
          }
      }
  }

  private static final boolean isTokenDelimiter(char c, Readtable rt)
    throws ConditionThrowable
  {
    switch (c)
      {
      case '"':
      case '\'':
      case '(':
      case ')':
      case ',':
      case ';':
      case '`':
        return true;
      default:
        return rt.isWhitespace(c);
      }
  }

  public LispObject readDispatchChar(char dispChar, boolean useFaslReadtable)
    throws ConditionThrowable
  {
    int numArg = -1;
    char c;
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          return error(new EndOfFile(this));
        c = (char) n;
        if (c < '0' || c > '9')
          break;
        if (numArg < 0)
          numArg = 0;
        numArg = numArg * 10 + c - '0';
      }
    final LispThread thread = LispThread.currentThread();
    final Readtable rt;
    if (useFaslReadtable)
      rt = FaslReadtable.getInstance();
    else
      rt = (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
    LispObject fun = rt.getDispatchMacroCharacter(dispChar, c);
    if (fun instanceof DispatchMacroFunction)
      return ((DispatchMacroFunction)fun).execute(this, c, numArg);
    if (fun != NIL)
      {
        LispObject result =
          thread.execute(fun, this, LispCharacter.getInstance(c),
                         (numArg < 0) ? NIL : Fixnum.getInstance(numArg));
        LispObject[] values = thread._values;
        if (values != null && values.length == 0)
          result = null;
        thread._values = null;
        return result;
      }
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return null;
    return error(new ReaderError("No dispatch function defined for #\\" + c,
                                  this));
  }

  public LispObject readCharacterLiteral(Readtable rt, LispThread thread)
    throws ConditionThrowable
  {
    int n = _readChar();
    if (n < 0)
      return error(new EndOfFile(this));
    char c = (char) n;
    FastStringBuffer sb = new FastStringBuffer(c);
    while (true)
      {
        n = _readChar();
        if (n < 0)
          break;
        c = (char) n;
        if (rt.isWhitespace(c))
          break;
        if (c == '(' || c == ')')
          {
            _unreadChar(c);
            break;
          }
        sb.append(c);
      }
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (sb.length() == 1)
      return LispCharacter.getInstance(sb.charAt(0));
    String token = sb.toString();
    n = LispCharacter.nameToChar(token);
    if (n >= 0)
      return LispCharacter.getInstance((char)n);
    return error(new LispError("Unrecognized character name: \"" + token + '"'));
  }

  public void skipBalancedComment() throws ConditionThrowable
  {
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          return;
        if (n == '|')
          {
            n = _readChar();
            if (n == '#')
              return;
            else
              _unreadChar(n);
          }
        else if (n == '#')
          {
            n = _readChar();
            if (n == '|')
              skipBalancedComment(); // Nested comment. Recurse!
            else
              _unreadChar(n);
          }
      }
  }

  public LispObject readArray(int rank) throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = read(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    switch (rank)
      {
      case -1:
        return error(new ReaderError("No dimensions argument to #A.", this));
      case 0:
        return new ZeroRankArray(T, obj, false);
      case 1:
        {
          if (obj.listp() || obj instanceof AbstractVector)
            return new SimpleVector(obj);
          return error(new ReaderError(obj.writeToString() + " is not a sequence.",
                                        this));
        }
      default:
        return new SimpleArray_T(rank, obj);
      }
  }

  public LispObject faslReadArray(int rank) throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = faslRead(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    switch (rank)
      {
      case -1:
        return error(new ReaderError("No dimensions argument to #A.", this));
      case 0:
        return new ZeroRankArray(T, obj, false);
      case 1:
        {
          if (obj.listp() || obj instanceof AbstractVector)
            return new SimpleVector(obj);
          return error(new ReaderError(obj.writeToString() + " is not a sequence.",
                                        this));
        }
      default:
        return new SimpleArray_T(rank, obj);
      }
  }

  public LispObject readComplex() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = read(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (obj instanceof Cons && obj.length() == 2)
      return Complex.getInstance(obj.car(), obj.cadr());
    // Error.
    FastStringBuffer sb = new FastStringBuffer("Invalid complex number format");
    if (this instanceof FileStream)
      {
        Pathname p = ((FileStream)this).getPathname();
        if (p != null)
          {
            String namestring = p.getNamestring();
            if (namestring != null)
              {
                sb.append(" in #P\"");
                sb.append(namestring);
                sb.append('"');
              }
          }
        sb.append(" at offset ");
        sb.append(_getFilePosition());
      }
    sb.append(": #C");
    sb.append(obj.writeToString());
    return error(new ReaderError(sb.toString(), this));
  }

  public LispObject faslReadComplex() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject obj = faslRead(true, NIL, true, thread);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (obj instanceof Cons && obj.length() == 2)
      return Complex.getInstance(obj.car(), obj.cadr());
    // Error.
    FastStringBuffer sb = new FastStringBuffer("Invalid complex number format");
    if (this instanceof FileStream)
      {
        Pathname p = ((FileStream)this).getPathname();
        if (p != null)
          {
            String namestring = p.getNamestring();
            if (namestring != null)
              {
                sb.append(" in #P\"");
                sb.append(namestring);
                sb.append('"');
              }
          }
        sb.append(" at offset ");
        sb.append(_getFilePosition());
      }
    sb.append(": #C");
    sb.append(obj.writeToString());
    return error(new ReaderError(sb.toString(), this));
  }

  private String readMultipleEscape(Readtable rt) throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer();
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          {
            error(new EndOfFile(this));
            // Not reached.
            return null;
          }
        char c = (char) n;
        byte syntaxType = rt.getSyntaxType(c);
        if (syntaxType == Readtable.SYNTAX_TYPE_SINGLE_ESCAPE)
          {
            n = _readChar();
            if (n < 0)
              {
                error(new EndOfFile(this));
                // Not reached.
                return null;
              }
            sb.append((char)n);
            continue;
          }
        if (syntaxType == Readtable.SYNTAX_TYPE_MULTIPLE_ESCAPE)
          break;
        sb.append(c);
      }
    return sb.toString();
  }

  private static final int findUnescapedSingleColon(String s, BitSet flags)
  {
    if (flags == null)
      return s.indexOf(':');
    final int limit = s.length();
    for (int i = 0; i < limit; i++)
      {
        if (s.charAt(i) == ':' && !flags.get(i))
          {
            return i;
          }
      }
    return -1;
  }

  private static final int findUnescapedDoubleColon(String s, BitSet flags)
  {
    if (flags == null)
      return s.indexOf("::");
    final int limit = s.length() - 1;
    for (int i = 0; i < limit; i++)
      {
        if (s.charAt(i) == ':' && !flags.get(i))
          {
            if (s.charAt(i + 1) == ':' && !flags.get(i + 1))
              {
                return i;
              }
          }
      }
    return -1;
  }

  private final LispObject readToken(char c, Readtable rt)
    throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer(c);
    final LispThread thread = LispThread.currentThread();
    BitSet flags = _readToken(sb, rt);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    final LispObject readtableCase = rt.getReadtableCase();
    final String token;
    if (readtableCase == Keyword.INVERT)
      token = invert(sb.toString(), flags);
    else
      token = sb.toString();
    final int length = token.length();
    if (length > 0)
      {
        final char firstChar = token.charAt(0);
        if (flags == null)
          {
            if (firstChar == '.')
              {
                // Section 2.3.3: "If a token consists solely of dots (with
                // no escape characters), then an error of type READER-
                // ERROR is signaled, except in one circumstance: if the
                // token is a single dot and appears in a situation where
                // dotted pair notation permits a dot, then it is accepted
                // as part of such syntax and no error is signaled."
                boolean ok = false;
                for (int i = length; i-- > 1;)
                  {
                    if (token.charAt(i) != '.')
                      {
                        ok = true;
                        break;
                      }
                  }
                if (!ok)
                  {
                    final String message;
                    if (length > 1)
                      message = "Too many dots.";
                    else
                      message = "Dot context error.";
                    return error(new ReaderError(message, this));
                  }
              }
            final int radix = getReadBase(thread);
            if ("+-.0123456789".indexOf(firstChar) >= 0)
              {
                LispObject number = makeNumber(token, length, radix);
                if (number != null)
                  return number;
              }
            else if (Character.digit(firstChar, radix) >= 0)
              {
                LispObject number = makeNumber(token, length, radix);
                if (number != null)
                  return number;
              }
          }
        if (firstChar == ':')
          if (flags == null || !flags.get(0))
            return PACKAGE_KEYWORD.intern(token.substring(1));
        int index = findUnescapedDoubleColon(token, flags);
        if (index > 0)
          {
            String packageName = token.substring(0, index);
            String symbolName = token.substring(index + 2);
            Package pkg = Packages.findPackage(packageName);
            if (pkg == null)
              return error(new LispError("Package \"" + packageName +
                                          "\" not found."));
            return pkg.intern(symbolName);
          }
        index = findUnescapedSingleColon(token, flags);
        if (index > 0)
          {
            final String packageName = token.substring(0, index);
            Package pkg = Packages.findPackage(packageName);
            if (pkg == null)
              return error(new PackageError("Package \"" + packageName +
                                             "\" not found."));
            final String symbolName = token.substring(index + 1);
            final SimpleString s = new SimpleString(symbolName);
            Symbol symbol = pkg.findExternalSymbol(s);
            if (symbol != null)
              return symbol;
            // Error!
            if (pkg.findInternalSymbol(s) != null)
              return error(new ReaderError("The symbol \"" + symbolName +
                                            "\" is not external in package " +
                                            packageName + '.',
                                            this));
            else
              return error(new ReaderError("The symbol \"" + symbolName +
                                            "\" was not found in package " +
                                            packageName + '.',
                                            this));
          }
      }
    // Intern token in current package.
    return ((Package)Symbol._PACKAGE_.symbolValue(thread)).intern(new SimpleString(token));
  }

  private final BitSet _readToken(FastStringBuffer sb, Readtable rt)
    throws ConditionThrowable
  {
    BitSet flags = null;
    final LispObject readtableCase = rt.getReadtableCase();
    if (sb.length() > 0)
      {
        Debug.assertTrue(sb.length() == 1);
        char c = sb.charAt(0);
        byte syntaxType = rt.getSyntaxType(c);
        if (syntaxType == Readtable.SYNTAX_TYPE_SINGLE_ESCAPE)
          {
            int n = _readChar();
            if (n < 0)
              {
                error(new EndOfFile(this));
                // Not reached.
                return flags;
              }
            sb.setCharAt(0, (char) n);
            flags = new BitSet(1);
            flags.set(0);
          }
        else if (syntaxType == Readtable.SYNTAX_TYPE_MULTIPLE_ESCAPE)
          {
            sb.setLength(0);
            sb.append(readMultipleEscape(rt));
            flags = new BitSet(sb.length());
            for (int i = sb.length(); i-- > 0;)
              flags.set(i);
          }
        else if (rt.isInvalid(c))
          {
            rt.checkInvalid(c, this); // Signals a reader-error.
          }
        else if (readtableCase == Keyword.UPCASE)
          {
            sb.setCharAt(0, LispCharacter.toUpperCase(c));
          }
        else if (readtableCase == Keyword.DOWNCASE)
          {
            sb.setCharAt(0, LispCharacter.toLowerCase(c));
          }
      }
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          break;
        char c = (char) n;
        if (rt.isWhitespace(c))
          {
            _unreadChar(n);
            break;
          }
        byte syntaxType = rt.getSyntaxType(c);
        if (syntaxType == Readtable.SYNTAX_TYPE_TERMINATING_MACRO)
          {
            _unreadChar(c);
            break;
          }
        rt.checkInvalid(c, this);
        if (syntaxType == Readtable.SYNTAX_TYPE_SINGLE_ESCAPE)
          {
            n = _readChar();
            if (n < 0)
              break;
            sb.append((char)n);
            if (flags == null)
              flags = new BitSet(sb.length());
            flags.set(sb.length() - 1);
            continue;
          }
        if (syntaxType == Readtable.SYNTAX_TYPE_MULTIPLE_ESCAPE)
          {
            int begin = sb.length();
            sb.append(readMultipleEscape(rt));
            int end = sb.length();
            if (flags == null)
              flags = new BitSet(sb.length());
            for (int i = begin; i < end; i++)
              flags.set(i);
            continue;
          }
        if (readtableCase == Keyword.UPCASE)
          c = LispCharacter.toUpperCase(c);
        else if (readtableCase == Keyword.DOWNCASE)
          c = LispCharacter.toLowerCase(c);
        sb.append(c);
      }
    return flags;
  }

  public static final String invert(String s, BitSet flags)
  {
    // Section 23.1.2: "When the readtable case is :INVERT, then if all of
    // the unescaped letters in the extended token are of the same case,
    // those (unescaped) letters are converted to the opposite case."
    final int limit = s.length();
    final int LOWER = 1;
    final int UPPER = 2;
    int state = 0;
    for (int i = 0; i < limit; i++)
      {
        // We only care about unescaped characters.
        if (flags != null && flags.get(i))
          continue;
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
    FastStringBuffer sb = new FastStringBuffer(limit);
    for (int i = 0; i < limit; i++)
      {
        char c = s.charAt(i);
        if (flags != null && flags.get(i)) // Escaped.
          sb.append(c);
        else if (Character.isUpperCase(c))
          sb.append(Character.toLowerCase(c));
        else if (Character.isLowerCase(c))
          sb.append(Character.toUpperCase(c));
        else
          sb.append(c);
      }
    return sb.toString();
  }

  private static final int getReadBase(LispThread thread)
    throws ConditionThrowable
  {
    final int readBase;
    try
      {
        readBase = ((Fixnum)Symbol.READ_BASE.symbolValue(thread)).value;
      }
    catch (ClassCastException e)
      {
        // The value of *READ-BASE* is not a Fixnum.
        error(new LispError("The value of *READ-BASE* is not of type '(INTEGER 2 36)."));
        // Not reached.
        return 10;
      }
    if (readBase < 2 || readBase > 36)
      {
        error(new LispError("The value of *READ-BASE* is not of type '(INTEGER 2 36)."));
        // Not reached.
        return 10;
      }
    return readBase;
  }

  private final LispObject makeNumber(String token, int length, int radix)
    throws ConditionThrowable
  {
    if (length == 0)
      return null;
    if (token.indexOf('/') >= 0)
      return makeRatio(token, radix);
    if (token.charAt(length - 1) == '.')
      {
        radix = 10;
        token = token.substring(0, --length);
      }
    boolean numeric = true;
    if (radix == 10)
      {
        for (int i = length; i-- > 0;)
          {
            char c = token.charAt(i);
            if (c < '0' || c > '9')
              {
                if (i > 0 || (c != '-' && c != '+'))
                  {
                    numeric = false;
                    break;
                  }
              }
          }
      }
    else
      {
        for (int i = length; i-- > 0;)
          {
            char c = token.charAt(i);
            if (Character.digit(c, radix) < 0)
              {
                if (i > 0 || (c != '-' && c != '+'))
                  {
                    numeric = false;
                    break;
                  }
              }
          }
      }
    if (!numeric) // Can't be an integer.
      return makeFloat(token, length);
    if (token.charAt(0) == '+')
      token = token.substring(1);
    try
      {
        int n = Integer.parseInt(token, radix);
        return (n >= 0 && n <= 255) ? Fixnum.constants[n] : Fixnum.getInstance(n);
      }
    catch (NumberFormatException e) {}
    // parseInt() failed.
    try
      {
        return new Bignum(token, radix);
      }
    catch (NumberFormatException e) {}
    // Not a number.
    return null;
  }

  private final LispObject makeRatio(String token, int radix)
    throws ConditionThrowable
  {
    final int index = token.indexOf('/');
    if (index < 0)
      return null;
    try
      {
        BigInteger numerator =
          new BigInteger(token.substring(0, index), radix);
        BigInteger denominator =
          new BigInteger(token.substring(index + 1), radix);
        // Check the denominator here, before calling number(), so we can
        // signal a READER-ERROR, as required by ANSI, instead of DIVISION-
        // BY-ZERO.
        if (denominator.signum() == 0)
          error(new ReaderError("Division by zero.", this));
        return number(numerator, denominator);
      }
    catch (NumberFormatException e)
      {
        return null;
      }
  }

  private static final LispObject makeFloat(final String token,
                                            final int length)
    throws ConditionThrowable
  {
    if (length == 0)
      return null;
    FastStringBuffer sb = new FastStringBuffer();
    int i = 0;
    boolean maybe = false;
    char marker = 0;
    char c = token.charAt(i);
    if (c == '-' || c == '+')
      {
        sb.append(c);
        ++i;
      }
    while (i < length)
      {
        c = token.charAt(i);
        if (c == '.' || (c >= '0' && c <= '9'))
          {
            if (c == '.')
              maybe = true;
            sb.append(c);
            ++i;
          }
        else
          break;
      }
    if (i < length)
      {
        c = token.charAt(i);
        if ("esfdlESFDL".indexOf(c) >= 0)
          {
            // Exponent marker.
            maybe = true;
            marker = LispCharacter.toUpperCase(c);
            if (marker == 'S')
              marker = 'F';
            else if (marker == 'L')
              marker = 'D';
            else if (marker == 'E')
              {
                LispObject format = Symbol.READ_DEFAULT_FLOAT_FORMAT.symbolValue();
                if (format == Symbol.SINGLE_FLOAT || format == Symbol.SHORT_FLOAT)
                  marker = 'F';
                else
                  marker = 'D';
              }
            sb.append('E');
            ++i;
          }
      }
    if (!maybe)
      return null;
    // Append rest of token.
    sb.append(token.substring(i));
    try
      {
        if (marker == 0)
          {
            LispObject format = Symbol.READ_DEFAULT_FLOAT_FORMAT.symbolValue();
            if (format == Symbol.SINGLE_FLOAT || format == Symbol.SHORT_FLOAT)
              marker = 'F';
            else
              marker = 'D';
          }
        if (marker == 'D')
          return new DoubleFloat(Double.parseDouble(sb.toString()));
        else
          return new SingleFloat(Float.parseFloat(sb.toString()));
      }
    catch (NumberFormatException e)
      {
        return null;
      }
  }

  public LispObject readRadix(int radix) throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer();
    final LispThread thread = LispThread.currentThread();
    final Readtable rt =
      (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
    boolean escaped = (_readToken(sb, rt) != null);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (escaped)
      return error(new ReaderError("Illegal syntax for number.", this));
    String s = sb.toString();
    if (s.indexOf('/') >= 0)
      return makeRatio(s, radix);
    // Integer.parseInt() below handles a prefixed '-' character correctly, but
    // does not accept a prefixed '+' character, so we skip over it here
    if (s.charAt(0) == '+')
      s = s.substring(1);
    try
      {
        int n = Integer.parseInt(s, radix);
        return (n >= 0 && n <= 255) ? Fixnum.constants[n] : Fixnum.getInstance(n);
      }
    catch (NumberFormatException e) {}
    // parseInt() failed.
    try
      {
        return new Bignum(s, radix);
      }
    catch (NumberFormatException e) {}
    // Not a number.
    return error(new LispError());
  }

  public LispObject faslReadRadix(int radix) throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer();
    final LispThread thread = LispThread.currentThread();
    final Readtable rt = FaslReadtable.getInstance();
    boolean escaped = (_readToken(sb, rt) != null);
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    if (escaped)
      return error(new ReaderError("Illegal syntax for number.", this));
    String s = sb.toString();
    if (s.indexOf('/') >= 0)
      return makeRatio(s, radix);
    try
      {
        int n = Integer.parseInt(s, radix);
        return (n >= 0 && n <= 255) ? Fixnum.constants[n] : Fixnum.getInstance(n);
      }
    catch (NumberFormatException e) {}
    // parseInt() failed.
    try
      {
        return new Bignum(s, radix);
      }
    catch (NumberFormatException e) {}
    // Not a number.
    return error(new LispError());
  }

  private char flushWhitespace(Readtable rt) throws ConditionThrowable
  {
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          {
            error(new EndOfFile(this));
            // Not reached.
            return 0;
          }
        char c = (char) n;
        if (!rt.isWhitespace(c))
          return c;
      }
  }

  public LispObject readDelimitedList(char delimiter)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    LispObject result = NIL;
    while (true)
      {
        Readtable rt = (Readtable) Symbol.CURRENT_READTABLE.symbolValue(thread);
        char c = flushWhitespace(rt);
        if (c == delimiter)
          break;
        LispObject obj = processChar(c, rt);
        if (obj != null)
          result = new Cons(obj, result);
      }
    if (Symbol.READ_SUPPRESS.symbolValue(thread) != NIL)
      return NIL;
    else
      return result.nreverse();
  }

  // read-line &optional stream eof-error-p eof-value recursive-p
  // => line, missing-newline-p
  // recursive-p is ignored
  public LispObject readLine(boolean eofError, LispObject eofValue)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    FastStringBuffer sb = new FastStringBuffer();
    while (true)
      {
        int n = _readChar();
        if (n < 0)
          {
            if (sb.length() == 0)
              {
                if (eofError)
                  return error(new EndOfFile(this));
                return thread.setValues(eofValue, T);
              }
            return thread.setValues(new SimpleString(sb), T);
          }
        if (n == '\n')
          return thread.setValues(new SimpleString(sb), NIL);
        else
          sb.append((char)n);
      }
  }

  // read-char &optional stream eof-error-p eof-value recursive-p => char
  // recursive-p is ignored
  public LispObject readChar() throws ConditionThrowable
  {
    int n = _readChar();
    if (n < 0)
      return error(new EndOfFile(this));
    return LispCharacter.getInstance((char)n);
  }

  public LispObject readChar(boolean eofError, LispObject eofValue)
    throws ConditionThrowable
  {
    int n = _readChar();
    if (n < 0)
      {
        if (eofError)
          return error(new EndOfFile(this));
        else
          return eofValue;
      }
    return LispCharacter.getInstance((char)n);
  }

  // read-char-no-hang &optional stream eof-error-p eof-value recursive-p => char
  // recursive-p is ignored
  public LispObject readCharNoHang(boolean eofError, LispObject eofValue)
    throws ConditionThrowable
  {
    return _charReady() ? readChar(eofError, eofValue) : NIL;
  }


  // unread-char character &optional input-stream => nil
  public LispObject unreadChar(LispCharacter c) throws ConditionThrowable
  {
    _unreadChar(c.value);
    return NIL;
  }

  public LispObject finishOutput() throws ConditionThrowable
  {
    _finishOutput();
    return NIL;
  }

  // clear-input &optional input-stream => nil
  public LispObject clearInput() throws ConditionThrowable
  {
    _clearInput();
    return NIL;
  }

  public LispObject getFilePosition() throws ConditionThrowable
  {
    long pos = _getFilePosition();
    return pos >= 0 ? number(pos) : NIL;
  }

  public LispObject setFilePosition(LispObject arg) throws ConditionThrowable
  {
    return _setFilePosition(arg) ? T : NIL;
  }

  // close stream &key abort => result
  // Must return true if stream was open, otherwise implementation-dependent.
  public LispObject close(LispObject abort) throws ConditionThrowable
  {
    _close();
    return T;
  }

  @Override
  public String toString()
  {
    return unreadableString("STREAM");
  }

  // read-byte stream &optional eof-error-p eof-value => byte
  // Reads an 8-bit byte.
  public LispObject readByte(boolean eofError, LispObject eofValue)
    throws ConditionThrowable
  {
    int n = _readByte();
    if (n < 0)
      {
        if (eofError)
          return error(new EndOfFile(this));
        else
          return eofValue;
      }
    return Fixnum.constants[n];
  }

  public LispObject terpri() throws ConditionThrowable
  {
    _writeChar('\n');
    return NIL;
  }

  public LispObject freshLine() throws ConditionThrowable
  {
    if (charPos == 0)
      return NIL;
    _writeChar('\n');
    return T;
  }

  public void print(char c) throws ConditionThrowable
  {
    _writeChar(c);
  }

  // PRIN1 produces output suitable for input to READ.
  // Binds *PRINT-ESCAPE* to true.
  public void prin1(LispObject obj) throws ConditionThrowable
  {
    LispThread thread = LispThread.currentThread();
    SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
    thread.bindSpecial(Symbol.PRINT_ESCAPE, T);
    try
      {
        _writeString(obj.writeToString());
      }
    finally
      {
        thread.lastSpecialBinding = lastSpecialBinding;
      }
  }

  public LispObject listen() throws ConditionThrowable
  {
    if (pastEnd)
      return NIL;
    
    if (! _charReady())
      return NIL;
    
    int n = _readChar();
    if (n < 0)
      return NIL;

    _unreadChar(n);
    
    return T;
  }

  public LispObject fileLength() throws ConditionThrowable
  {
    return type_error(this, Symbol.FILE_STREAM);
  }

  public LispObject fileStringLength(LispObject arg) throws ConditionThrowable
  {
    if (arg instanceof LispCharacter)
      {
        if (Utilities.isPlatformWindows)
          {
            if (((LispCharacter)arg).value == '\n')
              return Fixnum.TWO;
          }
        return Fixnum.ONE;
      }
    if (arg instanceof AbstractString)
      {
        if (Utilities.isPlatformWindows)
          {
            int fileStringLength = 0;
            char[] chars = ((AbstractString)arg).getStringChars();
            for (int i = chars.length; i-- > 0;)
              {
                if (chars[i] == '\n')
                  fileStringLength += 2;
                else
                  ++fileStringLength;
              }
            return number(fileStringLength);

          }
        return number(arg.length());
      }
    return error(new TypeError(arg.writeToString() +
                                " is neither a string nor a character."));
  }

  /** Reads a character off an underlying stream
   * 
   * @return a character, or -1 at end-of-file
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  protected int _readChar() throws ConditionThrowable
  {
    if (pastEnd)
      return -1;
    
    try
      {
        int n = reader.read();
        
        if (n < 0) {
            pastEnd = true;
            return -1;
        }
        
        ++offset;
        if (eolStyle == EolStyle.CRLF && n == '\r') {
            n = _readChar();
            if (n != '\n') {
                _unreadChar(n);
                return '\r';
            }
            else
              return '\n';
        }

        if (n == eolChar) {
          ++lineNumber;
          return '\n';
        }

        return n;
      }
    catch (NullPointerException e)
      {
        // reader is null
        streamNotCharacterInputStream();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
    // Not reached.
    return -1;
  }

  /** Puts a character back into the (underlying) stream
   * 
   * @param n
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  protected void _unreadChar(int n) throws ConditionThrowable
  {
    try
      {
        reader.unread(n);
        --offset;
        pastEnd = false;
        if (n == eolChar)
          --lineNumber;
      }
    catch (NullPointerException e)
      {
        // reader is null
        streamNotCharacterInputStream();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  /** Returns a boolean indicating input readily available
   * 
   * @return true if a character is available
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  protected boolean _charReady() throws ConditionThrowable
  {
    try
      {
        return reader.ready();
      }
    catch (NullPointerException e)
      {
        // reader is null
        streamNotCharacterInputStream();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
    // Not reached.
    return false;
  }

  /** Writes a character into the underlying stream,
   * updating charPos while doing so
   * 
   * @param c
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _writeChar(char c) throws ConditionThrowable
  {
    try
      {
        if (c == '\n') {
	  if (eolStyle == EolStyle.CRLF && lastChar != '\r')
              writer.write('\r');

          writer.write(eolChar);
          lastChar = eolChar;
          writer.flush();
          charPos = 0;
        } else {
          writer.write(c);
          lastChar = c;
          ++charPos;
        }
      }
    catch (NullPointerException e)
      {
        // writer is null
        streamNotCharacterOutputStream();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  /** Writes a series of characters in the underlying stream,
   * updating charPos while doing so
   * 
   * @param chars
   * @param start
   * @param end
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _writeChars(char[] chars, int start, int end)
    throws ConditionThrowable
  {
    try
      {
        if (eolStyle != EolStyle.RAW) {
          for (int i = start; i < end; i++)
            //###FIXME: the number of writes can be greatly reduced by
            // writing the space between newlines as chunks.
            _writeChar(chars[i]);
          return;
        }
        
        writer.write(chars, start, end - start);
        if (start < end)
          lastChar = chars[end-1];
        
        int index = -1;
        for (int i = end; i-- > start;)
          {
            if (chars[i] == '\n')
              {
                index = i;
                break;
	  }
	}
        if (index < 0)
          {
            // No newline.
            charPos += (end - start);
	      }
        else
          {
            charPos = end - (index + 1);
              writer.flush();
	    }
	  }
    catch (NullPointerException e)
      {
        if (writer == null)
          streamNotCharacterOutputStream();
        else
          throw e;
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  /** Writes a string to the underlying stream,
   * updating charPos while doing so
   * 
   * @param s
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _writeString(String s) throws ConditionThrowable
  {
    try
      {
	_writeChars(s.toCharArray(), 0, s.length());
      }
    catch (NullPointerException e)
      {
        if (writer == null)
          streamNotCharacterOutputStream();
        else
          throw e;
      }
  }

  /** Writes a string to the underlying stream, appending
   * a new line and updating charPos while doing so
   * 
   * @param s
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _writeLine(String s) throws ConditionThrowable
  {
    try
      {
        _writeString(s);
        _writeChar('\n');
      }
    catch (NullPointerException e)
      {
        // writer is null
        streamNotCharacterOutputStream();
      }
  }

  // Reads an 8-bit byte.
  /** Reads an 8-bit byte off the underlying stream
   * 
   * @return
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public int _readByte() throws ConditionThrowable
  {
    try
      {
        int n = in.read();
        if (n < 0)
          pastEnd = true;
        
        return n; // Reads an 8-bit byte.
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
        // Not reached.
        return -1;
      }
  }

  // Writes an 8-bit byte.
  /** Writes an 8-bit byte off the underlying stream
   * 
   * @param n
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _writeByte(int n) throws ConditionThrowable
  {
    try
      {
        out.write(n); // Writes an 8-bit byte.
      }
    catch (NullPointerException e)
      {
        // out is null
        streamNotBinaryOutputStream();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  /** Flushes any buffered output in the (underlying) stream
   * 
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _finishOutput() throws ConditionThrowable
  {
    try
      {
        if (writer != null)
          writer.flush();
        if (out != null)
          out.flush();
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  /** Reads all input from the underlying stream,
   * until _charReady() indicates no more input to be available
   * 
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _clearInput() throws ConditionThrowable
  {
    if (reader != null)
      {
        int c = 0;
        while (_charReady() && (c >= 0))
          c = _readChar();
      }
    else if (in != null)
      {
        try
          {
            int n = 0;
            while (in.available() > 0)
              n = in.read();
            
            if (n < 0)
              pastEnd = true;
          }
        catch (IOException e)
          {
            error(new StreamError(this, e));
          }
      }
  }

  /** Returns a (non-negative) file position integer or a negative value
   * if the position cannot be determined.
   * 
   * @return non-negative value as a position spec
   * @return negative value for 'unspecified'
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  protected long _getFilePosition() throws ConditionThrowable
  {
    return -1;
  }

  /** Sets the file position based on a position designator passed in arg
   * 
   * @param arg File position specifier as described in the CLHS
   * @return true on success, false on failure
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  protected boolean _setFilePosition(LispObject arg) throws ConditionThrowable
  {
    return false;
  }

  /** Closes the stream and underlying streams
   * 
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public void _close() throws ConditionThrowable
  {
    try
      {
        if (reader != null)
          reader.close();
        if (in != null)
          in.close();
        if (writer != null)
          writer.close();
        if (out != null)
          out.close();
        setOpen(false);
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  public void printStackTrace(Throwable t) throws ConditionThrowable
  {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    try
      {
        writer.write(sw.toString());
        writer.write('\n');
        lastChar = '\n';
        writer.flush();
        charPos = 0;
      }
    catch (IOException e)
      {
        error(new StreamError(this, e));
      }
  }

  protected LispObject streamNotInputStream() throws ConditionThrowable
  {
    return error(new StreamError(this, writeToString() + " is not an input stream."));
  }

  protected LispObject streamNotCharacterInputStream() throws ConditionThrowable
  {
    return error(new StreamError(this, writeToString() + " is not a character input stream."));
  }

  protected LispObject streamNotOutputStream() throws ConditionThrowable
  {
    return error(new StreamError(this, writeToString() + " is not an output stream."));
  }

  protected LispObject streamNotBinaryOutputStream() throws ConditionThrowable
  {
    return error(new StreamError(this, writeToString() + " is not a binary output stream."));
  }

  protected LispObject streamNotCharacterOutputStream() throws ConditionThrowable
  {
    return error(new StreamError(this, writeToString() + " is not a character output stream."));
  }

  // ### %stream-write-char character output-stream => character
  // OUTPUT-STREAM must be a real stream, not an output stream designator!
  private static final Primitive _WRITE_CHAR =
    new Primitive("%stream-write-char", PACKAGE_SYS, true,
                  "character output-stream")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((Stream)second)._writeChar(((LispCharacter)first).value);
          }
        catch (ClassCastException e)
          {
            if (second instanceof Stream)
              return type_error(first, Symbol.CHARACTER);
            else
              return type_error(second, Symbol.STREAM);
          }
        return first;
      }
    };

  // ### %write-char character output-stream => character
  private static final Primitive _STREAM_WRITE_CHAR =
    new Primitive("%write-char", PACKAGE_SYS, false,
                  "character output-stream")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final char c;
        try
          {
            c = ((LispCharacter)first).value;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.CHARACTER);
          }
        if (second == T)
          second = Symbol.TERMINAL_IO.symbolValue();
        else if (second == NIL)
          second = Symbol.STANDARD_OUTPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.STREAM);
          }
        stream._writeChar(c);
        return first;
      }
    };

  // ### %write-string string output-stream start end => string
  private static final Primitive _WRITE_STRING =
    new Primitive("%write-string", PACKAGE_SYS, false,
                  "string output-stream start end")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        final AbstractString s;
        try
          {
            s = (AbstractString) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STRING);
          }
        char[] chars = s.chars();
        final Stream out;
        try
          {
            if (second == T)
              out = (Stream) Symbol.TERMINAL_IO.symbolValue();
            else if (second == NIL)
              out = (Stream) Symbol.STANDARD_OUTPUT.symbolValue();
            else
              out = (Stream) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.STREAM);
          }
        final int start;
        try
          {
            start = ((Fixnum)third).value;
          }
        catch (ClassCastException e)
          {
            return type_error(third, Symbol.FIXNUM);
          }
        final int end;
        if (fourth == NIL)
          end = chars.length;
        else
          {
            try
              {
                end = ((Fixnum)fourth).value;
              }
            catch (ClassCastException e)
              {
                return type_error(fourth, Symbol.FIXNUM);
              }
          }
        checkBounds(start, end, chars.length);
        out._writeChars(chars, start, end);
        return first;
      }
    };

  // ### %finish-output output-stream => nil
  private static final Primitive _FINISH_OUTPUT =
    new Primitive("%finish-output", PACKAGE_SYS, false, "output-stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return finishOutput(arg);
      }
    };

  // ### %force-output output-stream => nil
  private static final Primitive _FORCE_OUTPUT =
    new Primitive("%force-output", PACKAGE_SYS, false, "output-stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return finishOutput(arg);
      }
    };

  private static final LispObject finishOutput(LispObject arg)
    throws ConditionThrowable
  {
    final Stream out;
    try
      {
        if (arg == T)
          out = (Stream) Symbol.TERMINAL_IO.symbolValue();
        else if (arg == NIL)
          out = (Stream) Symbol.STANDARD_OUTPUT.symbolValue();
        else
          out = (Stream) arg;
      }
    catch (ClassCastException e)
      {
        return type_error(arg, Symbol.STREAM);
      }
    return out.finishOutput();
  }

  // ### clear-input &optional input-stream => nil
  private static final Primitive CLEAR_INPUT =
    new Primitive(Symbol.CLEAR_INPUT, "&optional input-stream")
    {
      @Override
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        if (args.length > 1)
          return error(new WrongNumberOfArgumentsException(this));
        final Stream in;
        if (args.length == 0)
          in = checkCharacterInputStream(Symbol.STANDARD_INPUT.symbolValue());
        else
          in = inSynonymOf(args[0]);
        in.clearInput();
        return NIL;
      }
    };

  // ### %clear-output output-stream => nil
  // "If any of these operations does not make sense for output-stream, then
  // it does nothing."
  private static final Primitive _CLEAR_OUTPUT =
    new Primitive("%clear-output", PACKAGE_SYS, false, "output-stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg == T) // *TERMINAL-IO*
          return NIL;
        if (arg == NIL) // *STANDARD-OUTPUT*
          return NIL;
        if (arg instanceof Stream)
          return NIL;
        return type_error(arg, Symbol.STREAM);
      }
    };

  // ### close stream &key abort => result
  private static final Primitive CLOSE =
    new Primitive(Symbol.CLOSE, "stream &key abort")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((Stream)arg).close(NIL);
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STREAM);
          }
      }

      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        if (second == Keyword.ABORT)
          return stream.close(third);
        return error(new ProgramError("Unrecognized keyword argument " +
                                       second.writeToString() + "."));
      }
    };

  // ### out-synonym-of stream-designator => stream
  private static final Primitive OUT_SYNONYM_OF =
    new Primitive("out-synonym-of", PACKAGE_SYS, true, "stream-designator")
    {
      @Override
      public LispObject execute (LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof Stream)
          return arg;
        if (arg == T)
          return Symbol.TERMINAL_IO.symbolValue();
        if (arg == NIL)
          return Symbol.STANDARD_OUTPUT.symbolValue();
        return arg;
      }
    };

  // ### write-8-bits
  // write-8-bits byte stream => nil
  private static final Primitive WRITE_8_BITS =
    new Primitive("write-8-bits", PACKAGE_SYS, true, "byte stream")
    {
      @Override
      public LispObject execute (LispObject first, LispObject second)
        throws ConditionThrowable
      {
        int n;
        try
          {
            n = ((Fixnum)first).value;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.FIXNUM);
          }
        if (n < 0 || n > 255)
          return type_error(first, UNSIGNED_BYTE_8);
        try
          {
            ((Stream)second)._writeByte(n);
            return NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.STREAM);
          }
      }
    };

  // ### read-8-bits
  // read-8-bits stream &optional eof-error-p eof-value => byte
  private static final Primitive READ_8_BITS =
    new Primitive("read-8-bits", PACKAGE_SYS, true,
                  "stream &optional eof-error-p eof-value")
    {
      @Override
      public LispObject execute (LispObject first, LispObject second,
                                 LispObject third)
        throws ConditionThrowable
      {
        return checkBinaryInputStream(first).readByte((second != NIL),
                                                      third);
      }

      @Override
      public LispObject execute (LispObject[] args) throws ConditionThrowable
      {
        int length = args.length;
        if (length < 1 || length > 3)
          return error(new WrongNumberOfArgumentsException(this));
        final Stream in = checkBinaryInputStream(args[0]);
        boolean eofError = length > 1 ? (args[1] != NIL) : true;
        LispObject eofValue = length > 2 ? args[2] : NIL;
        return in.readByte(eofError, eofValue);
      }
    };

  // ### read-line &optional input-stream eof-error-p eof-value recursive-p
  // => line, missing-newline-p
  private static final Primitive READ_LINE =
    new Primitive(Symbol.READ_LINE,
                  "&optional input-stream eof-error-p eof-value recursive-p")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        final LispObject obj = Symbol.STANDARD_INPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) obj;
          }
        catch (ClassCastException e)
          {
            return type_error(obj, Symbol.STREAM);
          }
        return stream.readLine(true, NIL);
      }
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg == T)
          arg = Symbol.TERMINAL_IO.symbolValue();
        else if (arg == NIL)
          arg = Symbol.STANDARD_INPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STREAM);
          }
        return stream.readLine(true, NIL);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue();
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.readLine(second != NIL, NIL);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue();
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.readLine(second != NIL, third);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        // recursive-p is ignored
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue();
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue();
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.readLine(second != NIL, third);
      }
    };

  // ### %read-from-string string eof-error-p eof-value start end preserve-whitespace
  // => object, position
  private static final Primitive _READ_FROM_STRING =
    new Primitive("%read-from-string", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth,
                                LispObject fifth, LispObject sixth)
        throws ConditionThrowable
      {
        String s = first.getStringValue();
        boolean eofError = (second != NIL);
        boolean preserveWhitespace = (sixth != NIL);
        final int startIndex;
        if (fourth != NIL)
          startIndex = Fixnum.getValue(fourth);
        else
          startIndex = 0;
        final int endIndex;
        if (fifth != NIL)
          endIndex = Fixnum.getValue(fifth);
        else
          endIndex = s.length();
        StringInputStream in =
          new StringInputStream(s, startIndex, endIndex);
        final LispThread thread = LispThread.currentThread();
        LispObject result;
        if (preserveWhitespace)
          result = in.readPreservingWhitespace(eofError, third, false,
                                               thread);
        else
          result = in.read(eofError, third, false, thread);
        return thread.setValues(result, Fixnum.getInstance(in.getOffset()));
      }
    };

  // ### read &optional input-stream eof-error-p eof-value recursive-p => object
  private static final Primitive READ =
    new Primitive(Symbol.READ,
                  "&optional input-stream eof-error-p eof-value recursive-p")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        final LispObject obj = Symbol.STANDARD_INPUT.symbolValue(thread);
        final Stream stream;
        try
          {
            stream = (Stream) obj;
          }
        catch (ClassCastException e)
          {
            return type_error(obj, Symbol.STREAM);
          }
        return stream.read(true, NIL, false, thread);
      }
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        if (arg == T)
          arg = Symbol.TERMINAL_IO.symbolValue(thread);
        else if (arg == NIL)
          arg = Symbol.STANDARD_INPUT.symbolValue(thread);
        final Stream stream;
        try
          {
            stream = (Stream) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STREAM);
          }
        return stream.read(true, NIL, false, thread);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue(thread);
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue(thread);
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.read(second != NIL, NIL, false, thread);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue(thread);
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue(thread);
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.read(second != NIL, third, false, thread);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        if (first == T)
          first = Symbol.TERMINAL_IO.symbolValue(thread);
        else if (first == NIL)
          first = Symbol.STANDARD_INPUT.symbolValue(thread);
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.read(second != NIL, third, fourth != NIL, thread);
      }
    };

  // ### read-preserving-whitespace
  // &optional input-stream eof-error-p eof-value recursive-p => object
  private static final Primitive READ_PRESERVING_WHITESPACE =
    new Primitive(Symbol.READ_PRESERVING_WHITESPACE,
                  "&optional input-stream eof-error-p eof-value recursive-p")
    {
      @Override
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        int length = args.length;
        if (length > 4)
          return error(new WrongNumberOfArgumentsException(this));
        Stream stream =
          length > 0 ? inSynonymOf(args[0]) : getStandardInput();
        boolean eofError = length > 1 ? (args[1] != NIL) : true;
        LispObject eofValue = length > 2 ? args[2] : NIL;
        boolean recursive = length > 3 ? (args[3] != NIL) : false;
        return stream.readPreservingWhitespace(eofError, eofValue,
                                               recursive,
                                               LispThread.currentThread());
      }
    };

  // ### read-char &optional input-stream eof-error-p eof-value recursive-p
  // => char
  private static final Primitive READ_CHAR =
    new Primitive(Symbol.READ_CHAR,
                  "&optional input-stream eof-error-p eof-value recursive-p")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        return checkCharacterInputStream(Symbol.STANDARD_INPUT.symbolValue()).readChar();
      }
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return inSynonymOf(arg).readChar();
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        return inSynonymOf(first).readChar(second != NIL, NIL);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        return inSynonymOf(first).readChar(second != NIL, third);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        return inSynonymOf(first).readChar(second != NIL, third);
      }
    };

  // ### read-char-no-hang &optional input-stream eof-error-p eof-value
  // recursive-p => char
  private static final Primitive READ_CHAR_NO_HANG =
    new Primitive("read-char-no-hang", "&optional input-stream eof-error-p eof-value recursive-p") {

      @Override
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        int length = args.length;
        if (length > 4)
            error(new WrongNumberOfArgumentsException(this));
        Stream stream =
            length > 0 ? inSynonymOf(args[0]) : getStandardInput();
        boolean eofError = length > 1 ? (args[1] != NIL) : true;
        LispObject eofValue = length > 2 ? args[2] : NIL;
        // recursive-p is ignored
        // boolean recursive = length > 3 ? (args[3] != NIL) : false;
        return stream.readCharNoHang(eofError, eofValue);
      }
  };

  // ### read-delimited-list char &optional input-stream recursive-p => list
  private static final Primitive READ_DELIMITED_LIST =
    new Primitive("read-delimited-list", "char &optional input-stream recursive-p") {

      @Override
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        int length = args.length;
        if (length < 1 || length > 3)
            error(new WrongNumberOfArgumentsException(this));
        char c = LispCharacter.getValue(args[0]);
        Stream stream =
            length > 1 ? inSynonymOf(args[1]) : getStandardInput();
        return stream.readDelimitedList(c);
      }
  };


  // ### unread-char character &optional input-stream => nil
  private static final Primitive UNREAD_CHAR =
    new Primitive(Symbol.UNREAD_CHAR, "character &optional input-stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return getStandardInput().unreadChar(checkCharacter(arg));
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Stream stream = inSynonymOf(second);
        return stream.unreadChar(checkCharacter(first));
      }
    };

  // ### write-vector-unsigned-byte-8
  private static final Primitive WRITE_VECTOR_UNSIGNED_BYTE_8 =
    new Primitive("write-vector-unsigned-byte-8", PACKAGE_SYS, true,
                  "vector stream start end")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        final AbstractVector v = checkVector(first);
        final Stream stream;
        try
          {
            stream = (Stream) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.STREAM);
          }
        int start = Fixnum.getValue(third);
        int end = Fixnum.getValue(fourth);
        for (int i = start; i < end; i++)
          stream._writeByte(v.aref(i));
        return v;
      }
    };

  // ### read-vector-unsigned-byte-8 vector stream start end => position
  private static final Primitive READ_VECTOR_UNSIGNED_BYTE_8 =
    new Primitive("read-vector-unsigned-byte-8", PACKAGE_SYS, true,
                  "vector stream start end")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
        throws ConditionThrowable
      {
        AbstractVector v = checkVector(first);
        Stream stream = checkBinaryInputStream(second);
        int start = Fixnum.getValue(third);
        int end = Fixnum.getValue(fourth);
        if (!v.getElementType().equal(UNSIGNED_BYTE_8))
          return type_error(first, list(Symbol.VECTOR,
                                              UNSIGNED_BYTE_8));
        for (int i = start; i < end; i++)
          {
            int n = stream._readByte();
            if (n < 0)
              {
                // End of file.
                return Fixnum.getInstance(i);
              }
            v.aset(i, n);
          }
        return fourth;
      }
    };

  // ### file-position
  private static final Primitive FILE_POSITION =
    new Primitive("file-position", "stream &optional position-spec")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final Stream stream;
        try
          {
            stream = (Stream) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STREAM);
          }
        return stream.getFilePosition();
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final Stream stream;
        try
          {
            stream = (Stream) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STREAM);
          }
        return stream.setFilePosition(second);
      }
    };

  // ### stream-line-number
  private static final Primitive STREAM_LINE_NUMBER =
    new Primitive("stream-line-number", PACKAGE_SYS, false, "stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Stream stream = checkStream(arg);
        return Fixnum.getInstance(stream.getLineNumber() + 1);
      }
    };

  // ### stream-offset
  private static final Primitive STREAM_OFFSET =
    new Primitive("stream-offset", PACKAGE_SYS, false, "stream")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Stream stream = checkStream(arg);
        return number(stream.getOffset());
      }
    };

  // ### stream-charpos stream => position
  private static final Primitive STREAM_CHARPOS =
    new Primitive("stream-charpos", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Stream stream = checkCharacterOutputStream(arg);
        return Fixnum.getInstance(stream.getCharPos());
      }
    };

  // ### stream-%set-charpos stream newval => newval
  private static final Primitive STREAM_SET_CHARPOS =
    new Primitive("stream-%set-charpos", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Stream stream = checkCharacterOutputStream(first);
        stream.setCharPos(Fixnum.getValue(second));
        return second;
      }
    };
}
