package org.armedbear.lisp;

import java.util.List;
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

  // TODO add test methods here.
  // The methods must be annotated with annotation @Test. For example:
  //
  // @Test
  // public void hello() {}
  @Test
  public void testParseJars() {
    String s1 = "jar:jar:file:/a/foo.jar!/b/baz.abcl!/path/c.lisp";
    List<String> r1 = PathnameJar.enumerateJarURIs(s1);
    assertTrue(r1.size() == 2);
    String s10 = "file:/a/foo.jar!/";
    assertTrue(s10.equals(r1.get(0)));
    String s11 = "b/baz.abcl!/path/c.lisp";
    assertTrue(s11.equals(r1.get(1)));
  }
}
