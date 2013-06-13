/*
 * Version.java
 *
 * Copyright (C) 2003-2008 Peter Graves
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

public final class Version
{
  private Version() {}
  
  static final String baseVersion = "1.3.0-dev";
  
  static void init() {
    try {
      InputStream input = Version.class.getResourceAsStream("version");
      BufferedReader reader = new BufferedReader(new InputStreamReader(input));
      String v = reader.readLine().trim();
      version = v;
    } catch (Throwable t) {
      version = baseVersion;
    } 
  }
  
  static String version = "";
  public synchronized static String getVersion()
  {
    if ("".equals(version)) {
      init();
    }
    return version;
  }

  public static void main(String args[]) {
    System.out.println(Version.getVersion());
  }
}
