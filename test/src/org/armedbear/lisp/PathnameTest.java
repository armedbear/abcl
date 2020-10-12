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
    Pathname pathname = (Pathname)URLPathname.create(url);
    assertNotNull(pathname);
    assertNotNull(pathname.getNamestring());
    assertNotNull(pathname.getName());
    assertNotNull(pathname.getType());
    assertNotNull(pathname.getDirectory());
  }
  
  @Test
  public void getInputStream() throws IOException {
    File file = File.createTempFile("foo", ".lisp");
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
    input.close();
    file.delete();
  }

  @Test
  public void copyConstructor() {
    Pathname orig = (Pathname)Pathname.create("/a/b/c/d/e/foo.lisp");
    Pathname copy = (Pathname)Pathname.create(orig.getNamestring());
    assertTrue(orig.getNamestring().equals(copy.getNamestring()));
  }

  @Test
  public void mergePathnames1() {
    Pathname p = (Pathname)Pathname.create("a/b/c/d/foo.lisp");
    Pathname d = (Pathname)Pathname.create("/foo/bar/there");
    Pathname r = (Pathname)Pathname.mergePathnames(p, d);
    String s = r.getNamestring();
    assertTrue(s.equals("/foo/bar/a/b/c/d/foo.lisp"));
  }

  @Test
  public void mergePathnames2() {
    Pathname p = (Pathname)Pathname.create("/a/b/c/d/foo.lisp");
    Pathname d = (Pathname)Pathname.create("/foo/bar/there");
    Pathname r = (Pathname)Pathname.mergePathnames(p, d);
    assertTrue(r.getNamestring().equals("/a/b/c/d/foo.lisp"));
  }

  @Test
  public void mergePathnames3() {
      LispObject args = Lisp.NIL;
      args = args.push(Keyword.TYPE);
      args = args.push(new SimpleString("abcl-tmp"));
      args = args.nreverse();
      Pathname p = (Pathname)Pathname.makePathname(args);
      Pathname d = (Pathname)Pathname.create("/foo/bar.abcl");
      Pathname r = (Pathname)Pathname.mergePathnames(p, d);
      assertTrue(r.getNamestring().equals("/foo/bar.abcl-tmp"));
  }

// Currently we disallow construction of relative pathname JARs
//  @Test
//  public void mergePathnames4() {
//    Pathname p = (Pathname)Pathname.create("jar:file:foo.jar!/bar.abcl");
//    Pathname d = (Pathname)Pathname.create("/a/b/c/");
//    Pathname r = (Pathname)Pathname.mergePathnames(p, d);
//    String s = r.getNamestring();
//    assertTrue(s.equals("jar:file:///a/b/c/foo.jar!/bar.abcl"));
//  }
  @Test 
  public void constructorFileDirectory() {
    Pathname p = (Pathname)Pathname.create("file:///tmp/");
    assertTrue(p.getNamestring().endsWith("/"));
  }
  @Test 
  public void constructorFileWindowsDevice() {
    Pathname p = (Pathname)Pathname.create("file:c://tmp/");
    LispObject device = p.getDevice();
    if (Utilities.isPlatformWindows) {
      assert(device != Lisp.NIL);
    }
  }
  // Necessary for ASDF output translations to work
  @Test
  public void wildInferiorsJars() {
    String namestring = "jar:file:///**/*.jar!/**/*.*";
    Pathname p = (Pathname)Pathname.create(namestring);
    String parsedNamestring = p.getNamestring();
    assertTrue(parsedNamestring.equals(namestring));
  }
  
  @Test
  public void equality() {
    Pathname p1 = (Pathname)Pathname.create("file:///tmp/");
    Pathname p2 = (Pathname)Pathname.create("file:///tmp/");
    boolean result = p1.equals(p2);
    assertTrue("Java equals() for Pathname", result);

    JarPathname p3 = (JarPathname)Pathname.create("jar:file:///abcl.jar!/tmp/");
    JarPathname p4 = (JarPathname)Pathname.create("jar:file:///abcl.jar!/tmp/");
    result = p3.equals(p4);
    assertTrue("Java equals() for PathnameJar", result);
  }
}
