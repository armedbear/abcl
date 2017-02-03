package org.armedbear.lisp;

import static org.junit.Assert.*;

import org.junit.Test;

public class SeekableStringWriterTest
{
  @Test
  public void writeAndSeek() {
    SeekableStringWriter writer = new SeekableStringWriter();
    String buf = "sdf";
    writer.append('a').append(buf).append(buf, 1, 2);
    assertEquals("asdfd", writer.toString());
    writer.seek(0);
    writer.append("meow");
    assertEquals("meowd", writer.toString());
  }
  
   @Test
   public void writeAndClear() {
     SeekableStringWriter writer = new SeekableStringWriter();
     String buf1 = "empus";
     String buf2 = "  fugit";
     writer.append('t').append(buf1).append(buf2, 1, 7);
     assertEquals("tempus fugit", writer.toString());
     String result = writer.toStringAndClear();
     assertEquals("tempus fugit", result);
     assertEquals("", writer.toString());
   }
}
