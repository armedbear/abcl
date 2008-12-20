/*
 * cxr.java
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

public final class cxr extends Lisp
{
  // ### set-car
  private static final Primitive SET_CAR =
    new Primitive("set-car", PACKAGE_SYS, true)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        first.setCar(second);
        return second;
      }
    };

  // ### set-cdr
  private static final Primitive SET_CDR =
    new Primitive("set-cdr", PACKAGE_SYS, true)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        first.setCdr(second);
        return second;
      }
    };

  // ### car
  private static final Primitive CAR = new Primitive(Symbol.CAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car();
      }
    };

  // ### cdr
  private static final Primitive CDR = new Primitive(Symbol.CDR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr();
      }
    };

  // ### caar
  private static final Primitive CAAR = new Primitive(Symbol.CAAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().car();
      }
    };

  // ### cadr
  private static final Primitive CADR = new Primitive(Symbol.CADR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cadr();
      }
    };

  // ### cdar
  private static final Primitive CDAR = new Primitive(Symbol.CDAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().cdr();
      }
    };

  // ### cddr
  private static final Primitive CDDR = new Primitive(Symbol.CDDR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr().cdr();
      }
    };

  // ### caddr
  private static final Primitive CADDR = new Primitive(Symbol.CADDR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.caddr();
      }
    };

  // ### caadr
  private static final Primitive CAADR = new Primitive(Symbol.CAADR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr().car().car();
      }
    };

  // ### caaar
  private static final Primitive CAAAR = new Primitive(Symbol.CAAAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().car().car();
      }
    };

  // ### cdaar
  private static final Primitive CDAAR = new Primitive(Symbol.CDAAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().car().cdr();
      }
    };

  // ### cddar
  private static final Primitive CDDAR = new Primitive(Symbol.CDDAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().cdr().cdr();
      }
    };

  // ### cdddr
  private static final Primitive CDDDR = new Primitive(Symbol.CDDDR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr().cdr().cdr();
      }
    };

  // ### cadar
  private static final Primitive CADAR = new Primitive(Symbol.CADAR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car().cdr().car();
      }
    };

  // ### cdadr
  private static final Primitive CDADR = new Primitive(Symbol.CDADR, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr().car().cdr();
      }
    };

  // ### first
  private static final Primitive FIRST = new Primitive(Symbol.FIRST, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.car();
      }
    };

  // ### second
  private static final Primitive SECOND = new Primitive(Symbol.SECOND, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cadr();
      }
    };

  // ### third
  private static final Primitive THIRD = new Primitive(Symbol.THIRD, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.caddr();
      }
    };

  // ### fourth
  private static final Primitive FOURTH = new Primitive(Symbol.FOURTH, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr().cdr().cadr();
      }
    };

  // ### rest
  private static final Primitive REST = new Primitive(Symbol.REST, "list")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg.cdr();
      }
    };
}
