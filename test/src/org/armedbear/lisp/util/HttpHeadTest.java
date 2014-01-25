/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.armedbear.lisp.util;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
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
public class HttpHeadTest {
  
  public HttpHeadTest() {
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

  /**
   * Test of get method, of class HttpHead.
   */
  @Test
  public void testGet() {
    System.out.println("get");
    URL url = null;
    try {
      url = new URL("http://abcl-dynamic-install.googlecode.com/files/baz-20130403a.jar");
    } catch (MalformedURLException ex) {
      Logger.getLogger(HttpHeadTest.class.getName()).log(Level.SEVERE, null, ex);
    }
    String key = "Last-Modified";
    String expResult = "";
    String result = HttpHead.get(url, key);
    assertEquals(expResult, result);
    System.out.println("Last-Modifed result was "+ result);
    fail("The test case is a prototype.");
  }

  /**
   * Test of main method, of class HttpHead.
   */
  @Test
  public void testMain() {
    System.out.println("main");
    String[] argv = null;
    HttpHead.main(argv);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }
  
}
