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
    	Byte[] syntax = this.syntax.constants;
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

        LispObject[] readerMacroFunctions = this.readerMacroFunctions.constants;
        readerMacroFunctions[';']  = LispReader.READ_COMMENT;
        readerMacroFunctions['"']  = FaslReader.FASL_READ_STRING;
        readerMacroFunctions['(']  = FaslReader.FASL_READ_LIST;
        readerMacroFunctions[')']  = LispReader.READ_RIGHT_PAREN;
        readerMacroFunctions['\''] = FaslReader.FASL_READ_QUOTE;
        readerMacroFunctions['#']  = FaslReader.FASL_READ_DISPATCH_CHAR;

        // BACKQUOTE-MACRO and COMMA-MACRO are defined in backquote.lisp.
        readerMacroFunctions['`']  = Symbol.BACKQUOTE_MACRO;
        readerMacroFunctions[',']  = Symbol.COMMA_MACRO;

        DispatchTable dt = new DispatchTable();
        LispObject[] dtfunctions = dt.functions.constants;
        dtfunctions['(']  = FaslReader.FASL_SHARP_LEFT_PAREN;
        dtfunctions['*']  = FaslReader.FASL_SHARP_STAR;
        dtfunctions['.']  = FaslReader.FASL_SHARP_DOT;
        dtfunctions[':']  = FaslReader.FASL_SHARP_COLON;
        dtfunctions['A']  = FaslReader.FASL_SHARP_A;
        dtfunctions['B']  = FaslReader.FASL_SHARP_B;
        dtfunctions['C']  = FaslReader.FASL_SHARP_C;
        dtfunctions['O']  = FaslReader.FASL_SHARP_O;
        dtfunctions['P']  = FaslReader.FASL_SHARP_P;
        dtfunctions['R']  = FaslReader.FASL_SHARP_R;
        dtfunctions['S']  = FaslReader.FASL_SHARP_S;
        dtfunctions['X']  = FaslReader.FASL_SHARP_X;
        dtfunctions['\''] = FaslReader.FASL_SHARP_QUOTE;
        dtfunctions['\\'] = FaslReader.FASL_SHARP_BACKSLASH;
        dtfunctions['|']  = LispReader.SHARP_VERTICAL_BAR;
        dtfunctions[')']  = LispReader.SHARP_ILLEGAL;
        dtfunctions['<']  = LispReader.SHARP_ILLEGAL;
        dtfunctions[' ']  = LispReader.SHARP_ILLEGAL;
        dtfunctions[8]    = LispReader.SHARP_ILLEGAL; // backspace
        dtfunctions[9]    = LispReader.SHARP_ILLEGAL; // tab
        dtfunctions[10]   = LispReader.SHARP_ILLEGAL; // newline, linefeed
        dtfunctions[12]   = LispReader.SHARP_ILLEGAL; // page
        dtfunctions[13]   = LispReader.SHARP_ILLEGAL; // return
        dtfunctions['?']  = FaslReader.FASL_SHARP_QUESTION_MARK;
        dispatchTables.constants['#'] = dt;

        readtableCase = Keyword.PRESERVE;
        // after all, all symbols will have been uppercased by the reader,
        // if applicable, when reading the source file; so, any lower-case
        // symbols are really meant to be lower case, even if printed without
        // pipe characters, which may happen if  the READTABLE-CASE of the
        // current readtable is :PRESERVE when printing the symbols
    }

    private static final FaslReadtable instance = new FaslReadtable();

    public static final FaslReadtable getInstance()
    {
        return instance;
    }
}
