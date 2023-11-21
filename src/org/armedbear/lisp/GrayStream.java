package org.armedbear.lisp;

import java.util.HashMap;
import static org.armedbear.lisp.Lisp.*;

/**
   The Java stub for Gray streams which wraps the reference of the CLOS
   object corresponding to the stream.
*/
public class GrayStream 
  extends Stream
{
  LispObject clos = null;
  
  // objects are created via memoization via findOrCreate(LispObject)
  private GrayStream(LispObject clos) {
    super(Symbol.CLOS_STREAM);
    this.clos = clos;
  }

  // TODO: can we make this a weak hash?  But the value contains a
  // reference to the key, so use a weak reference to clos?
  static HashMap<LispObject, GrayStream> objects
    = new HashMap<LispObject, GrayStream>();
  
  synchronized // ???
    static 
  public GrayStream findOrCreate(LispObject o) {
    GrayStream wrappedStream
      = objects.get(o);
    if (wrappedStream == null) {
      wrappedStream = new GrayStream(o);
      objects.put(o, wrappedStream);
    }
    return wrappedStream;
  }

  //
  // do what we can for Java code that wants to determine our valence(s)
  //
  public static final Symbol INPUT_STREAM_P
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/INPUT-STREAM-P");
  public boolean isInputStream() {
    Function f = checkFunction(INPUT_STREAM_P.getSymbolFunction());
    return f.execute(clos).getBooleanValue();
  }

  public static final Symbol OUTPUT_STREAM_P
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/OUTPUT-STREAM-P");
  public boolean isOutputStream() {
    Function f = checkFunction(OUTPUT_STREAM_P.getSymbolFunction());
    return f.execute(clos).getBooleanValue();
  }

  public static final Symbol INTERACTIVE_STREAM_P
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/INTERACTIVE-STREAM-P");
  public boolean isInteractive() {
    Function f = checkFunction(INTERACTIVE_STREAM_P.getSymbolFunction());
    return f.execute(clos).getBooleanValue();
  }

  public static final Symbol OPEN_STREAM_P
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/OPEN-STREAM-P");
  public boolean isOpen() {
    Function f = checkFunction(OPEN_STREAM_P.getSymbolFunction());
    return f.execute(clos).getBooleanValue();
  }

  public boolean isCharacterStream() {
    Function SUBTYPEP
      = (Function)Symbol.SUBTYPEP.getSymbolFunction();
    Package pkg
      = getCurrentPackage().findPackage("COMMON-LISP");
    Symbol s
      = (Symbol) pkg.findSymbol("CHARACTER");
    return SUBTYPEP.execute(getElementType(), s).getBooleanValue();
  }

  public boolean isBinaryStream() {
    return !isCharacterStream();
  }

  public boolean isCharacterInputStream() {
    return isCharacterStream() && isInputStream();
  }

  public boolean isCharacterOutputStream() {
    return isCharacterStream() && isOutputStream();
  }

  public boolean isBinaryInputStream() {
    return isBinaryStream() && isInputStream();
  }

  public boolean isBinaryOutputStream() {
    return isBinaryStream() && isOutputStream();
  }

  // TODO
  //   return Lisp type as Stream or as clos object?
  //
  /*
  public LispObject typeOf() {}
  public LispObject classOf() {}
  public LispObject typep(LispObject typeSpecifier) {}
  */

  //
  // methods forwarded to the possible Gray stream implementation
  //

  public static final Symbol ELEMENT_TYPE
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/ELEMENT-TYPE");
  public LispObject getElementType() {
    Function f = checkFunction(ELEMENT_TYPE.getSymbolFunction());
    return f.execute(clos);
  }
  
  public static final Symbol FORCE_OUTPUT
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FORCE-OUTPUT");
  public void _forceOutput() {
    Function f = checkFunction(FORCE_OUTPUT.getSymbolFunction());
    f.execute(clos);
  }

  public static final Symbol WRITE_STRING
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/WRITE-STRING");
  public void _writeString(String string) {
    Function f = checkFunction(WRITE_STRING.getSymbolFunction());
    SimpleString s = new SimpleString(string);
    f.execute(clos, s);
  }

  public static final Symbol WRITE_CHAR
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/WRITE-CHAR");
  public void _writeChar(char c) {
    Function f = checkFunction(WRITE_CHAR.getSymbolFunction());
    f.execute(clos, LispCharacter.getInstance(c));
  }


  public static final Symbol WRITE_CHARS
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/WRITE-CHARS");
  public void _writeChars(char[] chars, int start, int end) {
    Function f = checkFunction(WRITE_CHARS.getSymbolFunction());
    SimpleString string = new SimpleString(chars); // XXX under an encoding?
    f.execute(clos, string,
              LispInteger.getInstance(start),
              LispInteger.getInstance(end));
  }

  public static final Symbol FRESH_LINE
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FRESH-LINE");
  public LispObject freshLine() {
    Function f = checkFunction(FRESH_LINE.getSymbolFunction());
    return f.execute(clos);
  }

  public static final Symbol READ_CHAR
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/READ-CHAR");
  public int _readChar() {
    Function f = checkFunction(READ_CHAR.getSymbolFunction());
    LispObject result = f.execute(clos);
    if (result instanceof LispCharacter) {
      return checkCharacter(result).getValue();
    }
    return -1;
  }

  public static final Symbol UNREAD_CHAR
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/UNREAD-CHAR");
  public void _unreadChar(int n) {
    Function f = checkFunction(UNREAD_CHAR.getSymbolFunction());
    f.execute(clos, LispCharacter.getInstance((char)n));
  }

  public static final Symbol STREAM_LISTEN
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/LISTEN");
  protected boolean _charReady()
  {
    Function f = checkFunction(STREAM_LISTEN.getSymbolFunction());
    if (STREAM_LISTEN.execute(clos).equals(T)) {
      return true;
    }
    return false; 
  }

  protected boolean _byteReady()
  {
    simple_error("unimplemented _byteReady()");
    return false; // unreached
  }

  public static final Symbol READ_BYTE
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/READ-BYTE");
  public int _readByte() {
    Function f = checkFunction(READ_BYTE.getSymbolFunction());
    LispObject result = f.execute(clos);
    return result.intValue();
  }

  public static final Symbol WRITE_BYTE
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/WRITE-BYTE");
  public void _writeByte(int n) {
    Function f = checkFunction(WRITE_BYTE.getSymbolFunction());
    f.execute(clos, LispInteger.getInstance(n));
  }

  public static final Symbol FINISH_OUTPUT
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FINISH-OUTPUT");
  public void _finishOutput() {
    Function f = checkFunction(FINISH_OUTPUT.getSymbolFunction());
    f.execute(clos);
  }

  public static final Symbol FILE_POSITION
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FILE-POSITION");
  public long _getFilePosition() {
    Function f = checkFunction(FILE_POSITION.getSymbolFunction());
    LispObject result = f.execute(clos);
    return result.longValue();
  }

  public static final Symbol FILE_LENGTH
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FILE-LENGTH");
  @Override
  public LispObject fileLength() {
    Function f = checkFunction(FILE_LENGTH.getSymbolFunction());
    return f.execute(clos);
  }

  public static final Symbol LINE_COLUMN
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/LINE-COLUMN");
  public int getCharPos() {
    Function f = checkFunction(LINE_COLUMN.getSymbolFunction());
    LispObject result = f.execute(clos);
    return result.intValue();
  }

  //
  // unimplemented interfaces of parent class
  //
  // we stub these to return Lisp-side errors
  public void setInteractive(boolean b) {
    simple_error("unimplemented setInteractive(boolean)");
  }

  public LispObject getExternalFormat() {
    simple_error("unimplemented getExternalFormat()");
    return null;  // unreached
  }

  public String getEncoding() {
    simple_error("unimplemented getEncoding()");
    return null;  // unreached
  }

  public void setExternalFormat(LispObject format) {
    simple_error("unimplemented setExternalFormat()");
  }

  public void setOpen(boolean b) {
    simple_error("unimplemented setOpen()");;
  }

  public int getOffset() {
    simple_error("unimplemented getOffset()");
    return 0; // unreached
  }

  public final int getLineNumber() {
    simple_error("unimplemented getLineNumber()");
    return 0;  // unreached 
  }

  public void _clearInput() {
    // inherited implementation uses available bytes on stream
    simple_error("unimplemented _clearInput()");
  }

// TODO figure out why we can't add these to autoloads.lisp
  static {
    Autoload.autoloadFile(GrayStream.ELEMENT_TYPE, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FORCE_OUTPUT, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.WRITE_STRING, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.WRITE_CHAR, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.WRITE_CHARS, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FRESH_LINE, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.READ_CHAR, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.UNREAD_CHAR, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.STREAM_LISTEN, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.READ_BYTE, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.WRITE_BYTE, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FINISH_OUTPUT, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FILE_POSITION, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FILE_LENGTH, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.LINE_COLUMN, "gray-streams-java");
  }
}
