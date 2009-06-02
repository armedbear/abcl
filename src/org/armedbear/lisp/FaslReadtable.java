/*
 * FaslReadtable.java
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

public final class FaslReadtable extends Readtable
{
    public FaslReadtable()
    {
        super();
    }

    @Override
    protected void initialize()
    {
        syntax[9]    = SYNTAX_TYPE_WHITESPACE; // tab
        syntax[10]   = SYNTAX_TYPE_WHITESPACE; // linefeed
        syntax[12]   = SYNTAX_TYPE_WHITESPACE; // form feed
        syntax[13]   = SYNTAX_TYPE_WHITESPACE; // return
        syntax[' ']  = SYNTAX_TYPE_WHITESPACE;

        syntax['"']  = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax['\''] = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax['(']  = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax[')']  = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax[',']  = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax[';']  = SYNTAX_TYPE_TERMINATING_MACRO;
        syntax['`']  = SYNTAX_TYPE_TERMINATING_MACRO;

        syntax['#']  = SYNTAX_TYPE_NON_TERMINATING_MACRO;

        syntax['\\'] = SYNTAX_TYPE_SINGLE_ESCAPE;
        syntax['|']  = SYNTAX_TYPE_MULTIPLE_ESCAPE;

        readerMacroFunctions[';']  = FaslReader.FASL_READ_COMMENT;
        readerMacroFunctions['"']  = FaslReader.FASL_READ_STRING;
        readerMacroFunctions['(']  = FaslReader.FASL_READ_LIST;
        readerMacroFunctions[')']  = FaslReader.FASL_READ_RIGHT_PAREN;
        readerMacroFunctions['\''] = FaslReader.FASL_READ_QUOTE;
        readerMacroFunctions['#']  = FaslReader.FASL_READ_DISPATCH_CHAR;

        // BACKQUOTE-MACRO and COMMA-MACRO are defined in backquote.lisp.
        readerMacroFunctions['`']  = Symbol.BACKQUOTE_MACRO;
        readerMacroFunctions[',']  = Symbol.COMMA_MACRO;

        DispatchTable dt = new DispatchTable();
        dt.functions['(']  = FaslReader.FASL_SHARP_LEFT_PAREN;
        dt.functions['*']  = FaslReader.FASL_SHARP_STAR;
        dt.functions['.']  = FaslReader.FASL_SHARP_DOT;
        dt.functions[':']  = FaslReader.FASL_SHARP_COLON;
        dt.functions['A']  = FaslReader.FASL_SHARP_A;
        dt.functions['B']  = FaslReader.FASL_SHARP_B;
        dt.functions['C']  = FaslReader.FASL_SHARP_C;
        dt.functions['O']  = FaslReader.FASL_SHARP_O;
        dt.functions['P']  = FaslReader.FASL_SHARP_P;
        dt.functions['R']  = FaslReader.FASL_SHARP_R;
        dt.functions['S']  = FaslReader.FASL_SHARP_S;
        dt.functions['X']  = FaslReader.FASL_SHARP_X;
        dt.functions['\''] = FaslReader.FASL_SHARP_QUOTE;
        dt.functions['\\'] = FaslReader.FASL_SHARP_BACKSLASH;
        dt.functions['|']  = FaslReader.FASL_SHARP_VERTICAL_BAR;
        dt.functions[')']  = FaslReader.FASL_SHARP_ILLEGAL;
        dt.functions['<']  = FaslReader.FASL_SHARP_ILLEGAL;
        dt.functions[' ']  = FaslReader.FASL_SHARP_ILLEGAL;
        dt.functions[8]    = FaslReader.FASL_SHARP_ILLEGAL; // backspace
        dt.functions[9]    = FaslReader.FASL_SHARP_ILLEGAL; // tab
        dt.functions[10]   = FaslReader.FASL_SHARP_ILLEGAL; // newline, linefeed
        dt.functions[12]   = FaslReader.FASL_SHARP_ILLEGAL; // page
        dt.functions[13]   = FaslReader.FASL_SHARP_ILLEGAL; // return
        dispatchTables['#'] = dt;

        readtableCase = Keyword.PRESERVE;
    }

    private static final FaslReadtable instance = new FaslReadtable();

    public static final FaslReadtable getInstance()
    {
        return instance;
    }
}
