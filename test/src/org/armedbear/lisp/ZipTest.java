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

public class ZipTest
{
  // FIXME These need to be created as part of executing the tests
  String zipFile = "/Users/evenson/work/abcl/dist/abcl-contrib.jar";
  // created via
  //   (require :abcl-contrib)
  //   (asdf:load-system :asdf-jar)
  //   (asdf-jar:package :cl-ppcre)
  String nestedJarFile = "/var/tmp/cl-ppcre-all-2.1.1.jar";
  JarPathname zip;
  JarPathname nestedJar;

  @Before
  public void setup() {
    zip = (JarPathname) JarPathname.createFromFile(zipFile);
    nestedJar = (JarPathname) JarPathname.createFromFile(nestedJarFile);
  }

  @Test
  public void getArchive() {
    ZipCache.Archive archive1 = ZipCache.getArchive(zip);
    assertTrue("Get ZipArchive from pathname",
               archive1 instanceof ZipCache.ArchiveFile
               && ((ZipCache.ArchiveFile)archive1).file != null);
    JarPathname zip2
      = (JarPathname) JarPathname.createFromFile(zipFile);
    ZipCache.Archive archive2 = ZipCache.getArchive(zip2);
    assertTrue("Get cached ZipArchive from pathname",
               archive2 instanceof ZipCache.ArchiveFile
               && ((ZipCache.ArchiveFile)archive2).file != null);
    assertTrue("Cached ZipArchive refers to same entry",
               archive2.equals(archive1));
  }

  @Test
  public void getEntry() {
    String entryPath = "abcl-asdf/abcl-asdf-tests.asd";
    JarPathname entryPathname
      = (JarPathname) JarPathname.createEntryFromFile(zipFile, entryPath);
    ZipEntry entry = ZipCache.getZipEntry(entryPathname);
    assertTrue("Getting entry from jar",
               entry.getName().equals(entryPath));
    JarPathname entryPathname2
      = (JarPathname) JarPathname.createEntryFromFile(zipFile, entryPath);
    ZipEntry entry2 = ZipCache.getZipEntry(entryPathname2);
    assertTrue("Cached ZipEntry returns same object",
               entry.equals(entry2));
  }

  @Test
  public void getNestedJar() {
    String nestedNamestring = "jar:jar:file:/var/tmp/cl-ppcre-all-2.1.1.jar!/cl-ppcre/packages.abcl!/";
    JarPathname nested = (JarPathname)JarPathname.create(nestedNamestring);
    ZipCache.Archive archive = ZipCache.getArchive(nested);
    assertTrue("Able to retrieve nested jar archive",
               !archive.equals(null));
  }

  //  @Test
  public void getNestedJarEntry() {
    String nestedNamestring = "jar:jar:file:/var/tmp/cl-ppcre-all-2.1.1.jar!/cl-ppcre/packages.abcl!/__loader__._";
  }

  


  // @Test
  // public void getZipEntry() throws FileNotFoundException, IOException {
  //     FileInputStream inputFile = new FileInputStream(zipFile);
  //     ZipInputStream input = new ZipInputStream(inputFile);
  //     ZipEntry entry = ZipCache.getEntry(input, "a/b/bar.abcl");
  //     assertNotNull(entry);
  //     input.close();
  //     inputFile.close();
  // }

  // @Test
  // public void getZipInputStreamZipEntry() throws FileNotFoundException, IOException {
  //     JarFile jar = new JarFile(zipFile);
  //     Pathname pathname = (Pathname)Pathname.create("a/b/bar.abcl");
  //     InputStream entryInputStream = ZipCache.getInputStream(jar, pathname);
  //     assertNotNull(entryInputStream);
  //     ZipInputStream zip = new ZipInputStream(entryInputStream);
  //     assertNotNull(zip);
  //     ZipEntry entry = ZipCache.getEntry(zip, "bar._");
  //     assertNotNull(entry);
  // }
  
    
}   
