/*
 * disassemble_class_bytes.java
 *
 * Copyright (C) 2005 Peter Graves
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
import java.io.FileOutputStream;
import java.io.IOException;

// ### disassemble-class-bytes
public final class disassemble_class_bytes extends Primitive
{
    private disassemble_class_bytes()
    {
        super("disassemble-class-bytes", PACKAGE_SYS, true, "java-object");
    }

    @Override
    public LispObject execute(LispObject arg)
    {
        if (arg instanceof JavaObject) {
            byte[] bytes = (byte[]) ((JavaObject)arg).getObject();
            try {
                File file = File.createTempFile("abcl", ".class", null);
                FileOutputStream out = new FileOutputStream(file);
                out.write(bytes);
                out.close();
                LispObject disassembler = _DISASSEMBLER_.symbolValue();
                StringBuffer command = new StringBuffer();
                if (disassembler instanceof AbstractString) {
                    command.append(disassembler.getStringValue());
                    command.append(" ");
                    command.append(file.getPath());
                } else if (disassembler instanceof Operator) {
                    Pathname p = Pathname.makePathname(file);
                    LispObject commandResult = disassembler.execute(p);
                    command.append(commandResult.getStringValue());
                } else {
                    return new SimpleString("No disassembler is available.");
                }                        
                ShellCommand sc = new ShellCommand(command.toString(), null, null);
                sc.run();
                file.delete();
                return new SimpleString(sc.getOutput());
            } catch (IOException e) {
                Debug.trace(e);
            }
        }
        return NIL;
    }

    private static final Primitive DISASSEMBLE_CLASS_BYTES =
        new disassemble_class_bytes();
}
