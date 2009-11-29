/*
 * machine_version.java
 *
 * Copyright (C) 2004 Peter Graves
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

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

// ### machine-version
public final class machine_version extends Primitive
{
    private machine_version()
    {
        super("machine-version");
    }

    @Override
    public LispObject execute()
    {
        String osName = System.getProperty("os.name");
        if (osName != null && osName.toLowerCase().startsWith("linux")) {
            try {
                FileInputStream in = new FileInputStream("/proc/cpuinfo");
                if (in != null) {
                    BufferedReader reader =
                        new BufferedReader(new InputStreamReader(in));
                    try {
                        String s;
                        while ((s = reader.readLine()) != null) {
                            int start = s.indexOf("model name");
                            if (start >= 0) {
                                start = s.indexOf(':', start);
                                if (start >= 0) {
                                    return new SimpleString(s.substring(start + 1).trim());
                                }
                            }
                        }
                    }
                    finally {
                        reader.close();
                    }
                }
            }
            catch (IOException e) {}
        }
        return NIL;
    }

    private static final Primitive MACHINE_VERSION = new machine_version();
}
