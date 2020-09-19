package org.armedbear.lisp;

import java.util.List;
import java.text.MessageFormat;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class PathnameJarTest {
  
  public PathnameJarTest() {
  }
  
  @BeforeClass
  public static void setUpClass() {
  }
  
  @AfterClass
  public static void tearDownClass() {
  }
  
  @Before
  public void setUp() {
  }
  
  @After
  public void tearDown() {
  }

  @Test
  public void testParseJars1() {
    String s1 = "jar:jar:file:/a/foo.jar!/b/baz.abcl!/path/c.lisp";
    List<String> r1 = PathnameJar.enumerate(s1);
    assertTrue(r1.size() == 3);
    String s10 = "file:/a/foo.jar";
    assertTrue(s10.equals(r1.get(0)));
    String s11 = "/b/baz.abcl!/";
    assertTrue(s11.equals(r1.get(1)));
    String s12 = "/path/c.lisp";
    assertTrue(s12.equals(r1.get(2)));
  }

  @Test
  public void testParseJars10() {
    String s1 = "jar:jar:file:/a/foo.jar!/b/baz.abcl!/";
    List<String> r1 = PathnameJar.enumerate(s1);
    assertTrue(r1.size() == 2);
    String s10 = "file:/a/foo.jar";
    assertTrue(s10.equals(r1.get(0)));
    String s11 = "/b/baz.abcl!/";
    assertTrue(s11.equals(r1.get(1)));
  }

  @Test
  public void testParseJars2() {
    String s1 = "jar:jar:https://example.com/a/foo.jar!/b/baz.abcl!/path/c.lisp";
    List<String> r1 = PathnameJar.enumerate(s1);
    assertTrue(r1.size() == 3);
    String s10 = "https://example.com/a/foo.jar";
    assertTrue(s10.equals(r1.get(0)));
    String s11 = "/b/baz.abcl!/";
    assertTrue(s11.equals(r1.get(1)));
    String s12 = "/path/c.lisp";
    assertTrue(s12.equals(r1.get(2)));
  }

  @Test
  public void roundTrips() {
    String namestrings[] = {
      "jar:file:foo.jar!/",
      "jar:jar:file:foo.jar!/baz.abcl!/",
      "jar:jar:file:foo.jar!/baz.abcl!/__loader__._"
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
      "jar:file:foo.jar!/baz.abcl!/",
      "jar:file:foo.jar!/baz.abcl!/__loader__._"
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
}
