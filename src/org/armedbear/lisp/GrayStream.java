package org.armedbear.lisp;

import java.util.HashMap;
import static org.armedbear.lisp.Lisp.*;

/**
   The Java proxy for Gray streams which wraps the reference of the
   CLOS object corresponding to the stream, and trampolines calls to
   overloaded Stream.java methods through the generic method
   machinery.
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
    Function subtypep = checkFunction(Symbol.SUBTYPEP.getSymbolFunction());
    return subtypep.execute(getElementType(), Symbol.CHARACTER).getBooleanValue();
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
  @Override
  public LispObject getElementType() {
    Function f = checkFunction(ELEMENT_TYPE.getSymbolFunction());
    return f.execute(clos);
  }
  
  public static final Symbol EXTERNAL_FORMAT
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/EXTERNAL-FORMAT");
  @Override
  public LispObject getExternalFormat() {
    Function f = checkFunction(EXTERNAL_FORMAT.getSymbolFunction());
    return f.execute(clos);
  }

  public static final Symbol SET_EXTERNAL_FORMAT
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/SET-EXTERNAL-FORMAT");
  @Override
  public void setExternalFormat(LispObject format) {
    Function f = checkFunction(SET_EXTERNAL_FORMAT.getSymbolFunction());
    f.execute(clos, format);
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

  public static final Symbol CLEAR_INPUT
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/CLEAR-INPUT");
  public void _clearInput() {
    Function f = checkFunction(CLEAR_INPUT.getSymbolFunction());
    f.execute(clos);
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

  public static final Symbol SET_FILE_POSITION
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/SET-FILE-POSITION");
  public boolean _setFilePosition(LispObject arg) {
    Function f = checkFunction(SET_FILE_POSITION.getSymbolFunction());
    return f.execute(clos, arg).getBooleanValue();
  }

  public static final Symbol FILE_LENGTH
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FILE-LENGTH");
  @Override
  public LispObject fileLength() {
    Function f = checkFunction(FILE_LENGTH.getSymbolFunction());
    return f.execute(clos);
  }

  public static final Symbol FILE_STRING_LENGTH
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/FILE-STRING-LENGTH");
  @Override
  public LispObject fileStringLength(LispObject arg) {
    Function f = checkFunction(FILE_STRING_LENGTH.getSymbolFunction());
    return f.execute(clos, arg);
  }

  public static final Symbol PATHNAME
    = PACKAGE_GRAY_STREAMS_JAVA.addExternalSymbol("JAVA/PATHNAME");
  @Override
  public Pathname getPathname() {
    Function f = checkFunction(PATHNAME.getSymbolFunction());
    return (Pathname)f.execute(clos);
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

  public String getEncoding() {
    simple_error("unimplemented getEncoding()");
    return null;  // unreached
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

  // TODO figure out why we can't add these to autoloads.lisp
  static {
    Autoload.autoloadFile(GrayStream.INPUT_STREAM_P, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.OUTPUT_STREAM_P, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.INTERACTIVE_STREAM_P, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.OPEN_STREAM_P, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.ELEMENT_TYPE, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.EXTERNAL_FORMAT, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.SET_EXTERNAL_FORMAT, "gray-streams-java");
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
    Autoload.autoloadFile(GrayStream.CLEAR_INPUT, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FINISH_OUTPUT, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FILE_POSITION, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.SET_FILE_POSITION, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FILE_LENGTH, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.FILE_STRING_LENGTH, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.PATHNAME, "gray-streams-java");
    Autoload.autoloadFile(GrayStream.LINE_COLUMN, "gray-streams-java");
  }
}
