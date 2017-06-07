/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.abcl.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
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
public class StreamTaskTest {
  
  public StreamTaskTest() {
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
   * Test of run method, of class StreamTask.
   */
  @Test
  public void testRun() {
    System.out.println("run");
    byte inputBytes[] = {65, 66, 66, 90} ;
    ByteArrayInputStream input = new ByteArrayInputStream(inputBytes);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    StreamTask instance = new StreamTask(input, output);
    instance.run();
    assertArrayEquals(inputBytes, output.toByteArray());
  }
  
}
