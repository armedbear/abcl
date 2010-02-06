package org.armedbear.lisp;

import java.io.FileNotFoundException;
import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import org.junit.Test;
import java.io.IOException;
import java.io.InputStream;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.junit.Before;

public class UtilitiesTest
{
   File zipFile;


   @Before
   public void setup() {
       // XXX currently created by the ABCL Lisp based tests
       zipFile = new File("test/lisp/abcl/baz.jar");
       assertTrue(zipFile.canRead());
   }


  @Test
  public void getZipEntry() throws FileNotFoundException, IOException {
      FileInputStream inputFile = new FileInputStream(zipFile);
      ZipInputStream input = new ZipInputStream(inputFile);
      ZipEntry entry = Utilities.getEntry(input, "a/b/bar.abcl");
      assertNotNull(entry);
      input.close();
      inputFile.close();
  }

  @Test
  public void getZipInputStreamZipEntry() throws FileNotFoundException, IOException {
      JarFile jar = new JarFile(zipFile);
      Pathname pathname = new Pathname("a/b/bar.abcl");
      InputStream entryInputStream = Utilities.getInputStream(jar, pathname);
      assertNotNull(entryInputStream);
      ZipInputStream zip = new ZipInputStream(entryInputStream);
      assertNotNull(zip);
      ZipEntry entry = Utilities.getEntry(zip, "bar._");
      assertNotNull(entry);
  }
    
}   