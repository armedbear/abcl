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

import java.io.File;
import java.net.URL;
import java.net.URLDecoder;

public final class Site
{
    private static final String LISP_HOME;

    static {
        String lispHome = null;
        URL url = Lisp.class.getResource("boot.lisp");
        if (url != null) {
            String protocol = url.getProtocol();
            if (protocol != null && protocol.equals("file")) {
                String path = url.getPath();
                try {
                    path = URLDecoder.decode(path, "UTF-8");
                }
                catch (java.io.UnsupportedEncodingException uee) {
                    // can't happen: Java implementations are required to
                    // support UTF-8
                }
                int index = path.lastIndexOf('/');
                if (index >= 0) {
                    lispHome = path.substring(0, index + 1);
                    if (Utilities.isPlatformWindows) {
                        if (lispHome.length() > 0 && lispHome.charAt(0) == '/')
                            lispHome = lispHome.substring(1);
                    }
                }
            }
        } else
            lispHome = System.getProperty("abcl.home");
        LISP_HOME = lispHome;
    }

    public static final String getLispHome()
    {
        return LISP_HOME;
    }

    // ### *lisp-home*
    private static final Symbol _LISP_HOME_ =
        exportSpecial("*LISP-HOME*", PACKAGE_EXT, NIL);

    static {
        try {
            String s = Site.getLispHome();
            if (s != null)
                _LISP_HOME_.setSymbolValue(new Pathname(s));
        }
        catch (Throwable t) {
            Debug.trace(t);
        }
    }
}
