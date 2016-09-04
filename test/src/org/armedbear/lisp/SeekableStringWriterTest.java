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
}
