/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.armedbear.lisp;

import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author evenson
 */
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
  }
}
