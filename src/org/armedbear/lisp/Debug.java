/*
 * Debug.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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
import java.io.PrintWriter;
import java.io.StringWriter;

public final class Debug
{
    
    public static final void assertTrue(boolean b)
    {
        if (!b) {
            String msg = "ABCL Debug.assertTrue() assertion failed!";
            System.err.println(msg);
            Error e = new Error(msg);
            e.printStackTrace(System.err);
	    
	    StringBuffer buffer = new StringBuffer();
	    final String CR = "\n";
	    buffer.append(msg).append(CR);
	    StackTraceElement[] stack = e.getStackTrace();
	    for (int i = 0; i < stack.length; i++) {
		buffer.append(stack[i].toString()).append(CR);
	    }
            throw new Error(buffer.toString());
        }
    }
    public static final void assertViolation(String msg) {
	final String m = "Assert violation: " + msg;
	Error e = new Error(m);

	System.err.println(m);
	e.printStackTrace(System.err);

	StringBuffer buffer = new StringBuffer();
	final String CR = "\n";
	buffer.append(msg).append(CR);
	StackTraceElement[] stack = e.getStackTrace();
	for (int i = 0; i < stack.length; i++) {
	    buffer.append(stack[i].toString()).append(CR);
	}
	throw new Error(buffer.toString());
    }

    // Does not throw an exception.
    public static void bug()
    {
        trace(new Exception("BUG!"));
    }

    public static final void trace(String s)
    {
        System.err.println(s);
    }

    @SuppressWarnings("CallToThreadDumpStack")
    public static final void trace(Throwable t)
    {
        t.printStackTrace();
    }

    public static final void trace(String message, Throwable t)
    {
       trace(message);
       trace(t);
    }
    public static final Symbol _DEBUG_WARN_
        = exportSpecial("*DEBUG-WARN*", PACKAGE_SYS, NIL);

    public static void setDebugWarnings(boolean flag) {
        if (flag) {
            _DEBUG_WARN_.setSymbolValue(T);
        } else {
            _DEBUG_WARN_.setSymbolValue(NIL);
        }
    }
    
    public static final void warn(String s) {
        if (_DEBUG_WARN_.getSymbolValue() != null) {
            trace(s);
        }
    }
}
