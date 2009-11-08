/*
 * ControlTransfer.java
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

/** This class is the parent class of all non-local transfer of
 * control events in ABCL. The classes inheriting from this class each
 * represent a transfer of control event as it is available in the
 * standard: GO (represented by Go), RETURN (by Return) and THROW (by Throw).
 *
 * Please note that you should <b>only</b> be using these classes in case
 * you've establisched a corresponding TAGBODY, BLOCK or CATCH-like
 * construct in your code.
 *
 * Otherwise, be aware that if you are mixing Lisp and Java code,
 * Lisp code being called into might throw one of the three exception types
 * and cause execution to be transferred to the nearest handler - presumably
 * outside your Java code.
 *
 */
abstract public class ControlTransfer extends RuntimeException
{
    public ControlTransfer()
    {
    }
    
    /**
     * Overridden in order to make ControlTransfer construct
     * faster. This avoids gathering stack trace information.
     */
    @Override
    public Throwable fillInStackTrace()
    {
        return this;
    }

    public ControlTransfer(String message)
    {
        super(message);
    }

    public abstract LispObject getCondition();
}
