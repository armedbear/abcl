package org.armedbear.lisp;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileWriter;
import org.junit.Test;
import java.io.IOException;

public class StreamTest
{
  @Test
  public void readLispObject() {
    File file = null;
    try {
      file = File.createTempFile("foo", "lisp");
      FileWriter output = new FileWriter(file);
      String contents = "(defun foo () 42)";
      output.append(contents);
      output.close();
    } catch (IOException e) {
      System.out.println("Failed to create temp file" + e);
      return;
    }
    Pathname pathname = Pathname.makePathname(file);
    Stream in = new Stream(Symbol.SYSTEM_STREAM, pathname.getInputStream(), Symbol.CHARACTER);
    LispObject o = in.read(false, Lisp.EOF, false,
                           LispThread.currentThread(), Stream.currentReadtable);
    assertFalse(o.equals(Lisp.NIL));
    in._close();
    file.delete();
  }
}   