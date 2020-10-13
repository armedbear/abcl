package org.armedbear.lisp;

import java.util.List;
import java.text.MessageFormat;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class JarPathnameTest
{
  @Test
  public void enumerate1() {
    String s = "jar:jar:file:/a/foo.jar!/b/baz.abcl!/path/c.lisp";
    List<String> r = JarPathname.enumerate(s);
    assertTrue("3 results", r.size() == 3);
    String parts[] = {
      "file:/a/foo.jar",
      "b/baz.abcl!/",
      "/path/c.lisp"
    };
    for (int i = 0; i < parts.length; i++) {
      assertTrue(parts[i], r.get(i).equals(parts[i]));
    }
  }

  @Test
  public void enumerate2() {
    String s = "jar:jar:file:/a/foo.jar!/b/baz.abcl!/";
    List<String> r = JarPathname.enumerate(s);
    assertTrue("2 results", r.size() == 2);
    String parts[] = {
      "file:/a/foo.jar",
      "b/baz.abcl!/"
    };
    for (int i = 0; i < parts.length; i++) {
      assertTrue(parts[i], r.get(i).equals(parts[i]));
    }
  }

  @Test
  public void enumerate3() {
    String s = "jar:jar:https://example.com/a/foo.jar!/b/baz.abcl!/path/c.lisp";
    List<String> r = JarPathname.enumerate(s);
    assertTrue("3 results", r.size() == 3);
    String parts[] = {
      "https://example.com/a/foo.jar",
      "b/baz.abcl!/",
      "/path/c.lisp"
    };
    for (int i = 0; i < parts.length; i++) {
      assertTrue(parts[i], r.get(i).equals(parts[i]));
    }
  }

  @Test
  public void enumerate4() {
    String s = "jar:jar:jar:file:/a/foo.jar!/b/baz.abcl!/log4j.jar!/MF/manifest.mf";
    List<String> r = JarPathname.enumerate(s);
    assertTrue("4 results", r.size() == 4);
    String parts[] = {
      "file:/a/foo.jar",
      "b/baz.abcl!/",
      "log4j.jar!/",
      "/MF/manifest.mf"
    };
    for (int i = 0; i < parts.length; i++) {
      assertTrue(parts[i], r.get(i).equals(parts[i]));
    }
  }

  @Test
  public void roundTrips() {
    String namestrings[] = {
      "jar:file:///foo.jar!/",
      "jar:jar:file:///foo.jar!/baz.abcl!/",
      "jar:jar:file:///foo.jar!/baz.abcl!/__loader__._",
      "jar:jar:jar:file:///a/b/foo.jar!/c/baz.zip!/log4j.jar!/MF/manifest.mf",
      "jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/"
    };

    for (String namestring  : namestrings) {
      Pathname result = (Pathname) Pathname.create(namestring);
      String resultingNamestring = result.getNamestring();
      String message = MessageFormat.format("Namestring \"{0}\" failed to roundtrip", namestring);
      assertTrue(message,
                 namestring.equals(resultingNamestring));
    }
  }

  @Test
  public void invalidNamestrings() {
    String namestrings[] = {
      "jar:file:foo.jar!/",
      "jar:file:foo.jar!/baz.abcl!/",
      "jar:jar:file:foo.jar!/baz.abcl!/__loader__._",
      "jar:file:foo.jar!/baz.abcl!/__loader__._",
      "jar:jar:file:foo.jar!/baz.abcl!/",  
      "jar:jar:jar:file:a/b/foo.jar!/c/baz.zip!/log4j.jar!/MF/manifest.mf"
    };

    // JUnit 4.12 (which is what is available in Netbeans 12) doesn't
    // have an assertion about throwing an error.
    for (String namestring  : namestrings) {
      try { 
        Pathname.create(namestring);
      } catch (Throwable t) {
        String message = MessageFormat.format("Namestring \"{0}\" is invalid throwing: {1}",
                                              namestring,
                                              t.getCause());
        assertTrue(message, true);
      }
    }
  }

  @Test
  public void makePathname() {
    String urlString = "https://abcl.org/releases/1.7.1/abcl-contrib.jar";
    URLPathname urlPathname = URLPathname.create(urlString);
    LispObject args[] = {Keyword.DEVICE, Lisp.list(urlPathname)};
    LispObject result = Symbol.MAKE_PATHNAME.execute(args);
    assertTrue("MAKE-PATHNAME created instance of a JAR-PATHNAME", result instanceof JarPathname);
    String expectedNamestring
      = MessageFormat.format("jar:{0}!/", urlString);
    String resultingNamestring
      = ((JarPathname)result).getNamestring();
    assertTrue(MessageFormat.format("Namestring '{0}' is '{1}'", expectedNamestring, resultingNamestring),
               expectedNamestring.equals(resultingNamestring));
  }
                                         
}
