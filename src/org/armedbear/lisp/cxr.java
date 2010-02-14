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

import static org.armedbear.lisp.Lisp.*;

public final class cxr {
    // ### set-car
    private static final Primitive SET_CAR = new pf_set_car();
    private static final class pf_set_car extends Primitive {
        pf_set_car() {
            super("set-car", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setCar(second);
            return second;
        }
    };

    // ### set-cdr
    private static final Primitive SET_CDR = new pf_set_cdr();
    private static final class pf_set_cdr extends Primitive {
        pf_set_cdr() {
            super("set-cdr", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setCdr(second);
            return second;
        }
    };

    // ### car
    private static final Primitive CAR = new pf_car();
    private static final class pf_car extends Primitive {
        pf_car() {
            super(Symbol.CAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car();
        }
    };

    // ### cdr
    private static final Primitive CDR = new pf_cdr();
    private static final class pf_cdr extends Primitive {
        pf_cdr() {
            super(Symbol.CDR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr();
        }
    };

    // ### caar
    private static final Primitive CAAR = new pf_caar();
    private static final class pf_caar extends Primitive {
        pf_caar() {
            super(Symbol.CAAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().car();
        }
    };

    // ### cadr
    private static final Primitive CADR = new pf_cadr();
    private static final class pf_cadr extends Primitive {
        pf_cadr() {
            super(Symbol.CADR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cadr();
        }
    };

    // ### cdar
    private static final Primitive CDAR = new pf_cdar();
    private static final class pf_cdar extends Primitive {
        pf_cdar() {
            super(Symbol.CDAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().cdr();
        }
    };

    // ### cddr
    private static final Primitive CDDR = new pf_cddr();
    private static final class pf_cddr extends Primitive {
        pf_cddr() {
            super(Symbol.CDDR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr().cdr();
        }
    };

    // ### caddr
    private static final Primitive CADDR = new pf_caddr();
    private static final class pf_caddr extends Primitive {
        pf_caddr() {
            super(Symbol.CADDR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.caddr();
        }
    };

    // ### caadr
    private static final Primitive CAADR = new pf_caadr();
    private static final class pf_caadr extends Primitive {
        pf_caadr() {
            super(Symbol.CAADR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr().car().car();
        }
    };

    // ### caaar
    private static final Primitive CAAAR = new pf_caaar();
    private static final class pf_caaar extends Primitive {
        pf_caaar() {
            super(Symbol.CAAAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().car().car();
        }
    };

    // ### cdaar
    private static final Primitive CDAAR = new pf_cdaar();
    private static final class pf_cdaar extends Primitive {
        pf_cdaar() {
            super(Symbol.CDAAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().car().cdr();
        }
    };

    // ### cddar
    private static final Primitive CDDAR = new pf_cddar();
    private static final class pf_cddar extends Primitive {
        pf_cddar() {
            super(Symbol.CDDAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().cdr().cdr();
        }
    };

    // ### cdddr
    private static final Primitive CDDDR = new pf_cdddr();
    private static final class pf_cdddr extends Primitive {
        pf_cdddr() {
            super(Symbol.CDDDR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr().cdr().cdr();
        }
    };

    // ### cadar
    private static final Primitive CADAR = new pf_cadar();
    private static final class pf_cadar extends Primitive {
        pf_cadar() {
            super(Symbol.CADAR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car().cdr().car();
        }
    };

    // ### cdadr
    private static final Primitive CDADR = new pf_cdadr();
    private static final class pf_cdadr extends Primitive {
        pf_cdadr() {
            super(Symbol.CDADR, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr().car().cdr();
        }
    };

    // ### first
    private static final Primitive FIRST = new pf_first();
    private static final class pf_first extends Primitive {
        pf_first() {
            super(Symbol.FIRST, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.car();
        }
    };

    // ### second
    private static final Primitive SECOND = new pf_second();
    private static final class pf_second extends Primitive {
        pf_second() {
            super(Symbol.SECOND, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cadr();
        }
    };

    // ### third
    private static final Primitive THIRD = new pf_third();
    private static final class pf_third extends Primitive {
        pf_third() {
            super(Symbol.THIRD, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.caddr();
        }
    };

    // ### fourth
    private static final Primitive FOURTH = new pf_fourth();
    private static final class pf_fourth extends Primitive {
        pf_fourth() {
            super(Symbol.FOURTH, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr().cdr().cadr();
        }
    };

    // ### rest
    private static final Primitive REST = new pf_rest();
    private static final class pf_rest extends Primitive {
        pf_rest() {
            super(Symbol.REST, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.cdr();
        }
    };
}
