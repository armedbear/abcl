package org.armedbear.lisp;

import java.net.MalformedURLException;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.runner.JUnitCore;


import java.net.URL;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;

public class PathnameTest
{
  public static void main(final String args[]) {
    JUnitCore.main("org.armedbear.lisp.PathnameTest");
  }

  @Test
  public void constructorURL()
  {
    URL url = null;
    try {
       url = new URL("file:///Users/evenson/work/abcl/build/classes/org/armedbear/lisp/boot.lisp");
    } catch (MalformedURLException e) {
        System.out.println(e.getMessage());
    }
    Pathname pathname = new Pathname(url);
    assertNotNull(pathname);
    assertNotNull(pathname.getNamestring());
    assertNotNull(pathname.name);
    assertNotNull(pathname.type);
    assertNotNull(pathname.directory);
  }
  
  @Test
  public void getInputStream() throws IOException {
    File file = File.createTempFile("foo", "lisp");
    FileWriter output = new FileWriter(file);
    String contents = "(defun foo () 42)";
    output.append(contents);
    output.close();
    Pathname pathname = Pathname.makePathname(file);
    InputStream input = pathname.getInputStream();
    InputStreamReader reader = new InputStreamReader(input);
    char[] buffer = new char[1024];
    StringBuilder result = new StringBuilder();
    int i;
    while((i = reader.read(buffer, 0, buffer.length)) != -1) {
      result.append(buffer, 0, i);
    }
    assertEquals(contents, result.toString());
    file.delete();
  }
}
