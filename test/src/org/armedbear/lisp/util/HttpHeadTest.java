package org.armedbear.lisp.util;

import java.io.IOException;
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

  @Test
  public void testGet() {
    URL url = null;
    try {
      url = new URL("http://abcl.org/fasl/42/baz-20140105a-fasl-42.jar");
    } catch (MalformedURLException ex) {
      Logger.getLogger(HttpHeadTest.class.getName()).log(Level.SEVERE, null, ex);
    }
    String key = "Last-Modified";
    String result = null;
    try {
      result = HttpHead.get(url, key);
    } catch (IOException ex) {
      Logger.getLogger(HttpHeadTest.class.getName()).log(Level.SEVERE, null, ex);
    }
    assertNotNull(result);
    System.out.println("Last-Modified result was "+ result);
  }

  @Test
  public void testMain() {
    System.out.println("main");
    String[] argv = {"http://google.com/"};
    HttpHead.main(argv);
  }
  
}
