/*
 * Site.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

import static org.armedbear.lisp.Lisp.*;

import java.net.URL;


public final class Site
{
    private static LispObject LISP_HOME;

    private static void init() {
        String s = System.getProperty("abcl.home");
        if (s != null) {
            String fileSeparator = System.getProperty("file.separator");
            if (!s.endsWith(fileSeparator)) {
                s += fileSeparator;
            }
            LISP_HOME = new Pathname(s);
            return;
        }
        URL url = Lisp.class.getResource("boot.lisp");
        if (url != null) {
            if (!Pathname.isSupportedProtocol(url.getProtocol())) {
                LISP_HOME = NIL;
            } else {
                Pathname p = new Pathname(url);
                p.name = NIL;
                p.type = NIL;
                p.invalidateNamestring();
                LISP_HOME = p;
            }
            return;
        }
        Debug.trace("Unable to determine LISP_HOME.");
    }

    public static final LispObject getLispHome()
    {
      if (LISP_HOME == null) {
        init();
      }
      return LISP_HOME;
    }

    // ### *lisp-home*
    private static final Symbol _LISP_HOME_ =
        exportSpecial("*LISP-HOME*", PACKAGE_EXT, NIL);

    static {
        LispObject p  = Site.getLispHome();
        if (p != null)
            _LISP_HOME_.setSymbolValue(p);
    }
}
