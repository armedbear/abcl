/*
 * Constants.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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
 */

package org.armedbear.j;

import java.awt.event.InputEvent;

public interface Constants
{
    char EOL = '\n';

    int COMMAND_NOTHING          =  0;
    int COMMAND_KILL             =  1;
    int COMMAND_PASTE            =  2;
    int COMMAND_PASTE_COMMENT    =  3;
    int COMMAND_UP               =  4;
    int COMMAND_DOWN             =  5;
    int COMMAND_PAGE_UP          =  6;
    int COMMAND_PAGE_DOWN        =  7;
    int COMMAND_LEFT             =  8;
    int COMMAND_RIGHT            =  9;
    int COMMAND_HOME             = 10;
    int COMMAND_HOME_HOME        = 11;
    int COMMAND_END              = 12;
    int COMMAND_END_END          = 13;
    int COMMAND_SELECT_HOME      = 14;
    int COMMAND_SELECT_HOME_HOME = 15;
    int COMMAND_SELECT_END       = 16;
    int COMMAND_SELECT_END_END   = 17;
    int COMMAND_UNDO             = 18;
    int COMMAND_EXPAND           = 19;
    int COMMAND_HISTORY          = 20;
    int COMMAND_WINDOW_UP        = 21;
    int COMMAND_WINDOW_DOWN      = 22;

    int FILETYPE_UNKNOWN         = -1;
    int FILETYPE_TEXT            = 0;
    int FILETYPE_XML             = 1;
    int FILETYPE_SHELLSCRIPT     = 2;
    int FILETYPE_PERL            = 3;
    int FILETYPE_PHP             = 4;
    int FILETYPE_BINARY          = 5;
    int FILETYPE_ZIP             = 6;
    int FILETYPE_GZIP            = 7;
    int FILETYPE_WORD            = 8;
    int FILETYPE_JPEG            = 9;

    int COMPRESSION_NONE         = 0;
    int COMPRESSION_ZIP          = 1;
    int COMPRESSION_GZIP         = 2;

    int LANGUAGE_JAVA            = 0;
    int LANGUAGE_JAVASCRIPT      = 1;
    int LANGUAGE_C               = 2;
    int LANGUAGE_CPP             = 3;
    int LANGUAGE_OBJC            = 4;

    int LOAD_COMPLETED           = 1;
    int LOAD_PENDING             = 2;
    int LOAD_FAILED              = 3;

    // LocalTag types.
    int TAG_UNKNOWN              = 0;
    int TAG_INTERFACE            = 1;
    int TAG_CLASS                = 2;
    int TAG_METHOD               = 3;
    int TAG_FIELD                = 4;
    int TAG_EXTENDS              = 5;
    int TAG_IMPLEMENTS           = 6;
    int TAG_EXPLICIT             = 7;

    int TAG_FUNCTION             = TAG_METHOD;

    // Additional LocalTag types for Lisp.
    int TAG_CONSTANT             = 8;
    int TAG_CONDITION            = 9;
    int TAG_DEFUN                = 10;
    int TAG_GENERIC_FUNCTION     = 11;
    int TAG_MACRO                = 12;
    int TAG_PARAMETER            = 13;
    int TAG_STRUCT               = 14;
    int TAG_TYPE                 = 15;
    int TAG_VAR                  = 16;
    int TAG_TEST                 = 17;

    // Visibility values (stored in LocalTag flags field).
    int TAG_PUBLIC               = 0x0001;
    int TAG_PROTECTED            = 0x0002;
    int TAG_PRIVATE              = 0x0004;

    int TAG_VISIBILITY_MASK      = 0x0007;

    // Version control.
    int VC_CVS                   = 1;
    int VC_P4                    = 2;
    int VC_DARCS                 = 3;

    String CHECK_SAVE_PROMPT  = "Buffer is modified; save it first?";

    // The following values are arbitrary and must not conflict with any
    // VK_ values defined by Sun!
    int VK_MOUSE_1               = 0xe001;
    int VK_DOUBLE_MOUSE_1        = 0xe002;
    int VK_MOUSE_2               = 0xe003;
    int VK_DOUBLE_MOUSE_2        = 0xe004;
    int VK_MOUSE_3               = 0xe005;
    int VK_DOUBLE_MOUSE_3        = 0xe006;

    // Buffer list bits are in high word.
    int SIDEBAR_BUFFER_LIST_CHANGED      = 0x0100;
    int SIDEBAR_REPAINT_BUFFER_LIST      = 0x0200;
    int SIDEBAR_SET_BUFFER               = 0x0400;
    int SIDEBAR_MODIFIED_BUFFER_COUNT    = 0x0800;

    // Navigation component bits are in low word.
    int SIDEBAR_POSITION                 = 0x0001;

    int SIDEBAR_ALL                      = 0xffff;
    int SIDEBAR_BUFFER_LIST_ALL          = 0xff00; // High word.
    int SIDEBAR_NAVIGATION_COMPONENT_ALL = 0x00ff; // Low word.

    // Display update flag bits.
    int REFRAME = 0x0001;
    int REPAINT = 0x0002; // Full repaint.

    // Modes.
    int ARCHIVE_MODE            =  1;   String ARCHIVE_MODE_NAME = "Archive";
    int ASM_MODE                =  2;   String ASM_MODE_NAME = "Assembly";
    int AUTOCONF_MODE           =  3;   String AUTOCONF_MODE_NAME = "Autoconf";
    int BEANSHELL_MODE          =  4;   String BEANSHELL_MODE_NAME = "BeanShell";
    int BINARY_MODE             =  5;   String BINARY_MODE_NAME = "Binary";
    int CHECKIN_MODE            =  6;   String CHECKIN_MODE_NAME = "Checkin";
    int COMPILATION_MODE        =  7;   String COMPILATION_MODE_NAME = "Compilation";
    int CPP_MODE                =  8;   String CPP_MODE_NAME = "C++";
    int CSS_MODE                =  9;   String CSS_MODE_NAME = "CSS";
    int C_MODE                  = 10;   String C_MODE_NAME = "C";
    int DIFF_MODE               = 11;   String DIFF_MODE_NAME = "Diff";
    int DIRECTORY_MODE          = 12;   String DIRECTORY_MODE_NAME = "Directory";
    int HTML_MODE               = 13;   String HTML_MODE_NAME = "HTML";
    int IMAGE_MODE              = 14;   String IMAGE_MODE_NAME = "Image";
    int JAVASCRIPT_MODE         = 15;   String JAVASCRIPT_MODE_NAME = "JavaScript";
    int JAVA_MODE               = 16;   String JAVA_MODE_NAME = "Java";
    int JDB_MODE                = 17;   String JDB_MODE_NAME = "JDB";
    int LISP_MODE               = 18;   String LISP_MODE_NAME = "Lisp";
    int LISP_SHELL_MODE         = 19;   String LISP_SHELL_MODE_NAME = "Lisp Shell";
    int LIST_OCCURRENCES_MODE   = 20;   String LIST_OCCURRENCES_MODE_NAME = "List Occurrences";
    int LIST_REGISTERS_MODE     = 21;   String LIST_REGISTERS_MODE_NAME = "List Registers";
    int LIST_TAGS_MODE          = 22;   String LIST_TAGS_MODE_NAME = "List Tags";
    int MAILBOX_MODE            = 23;   String MAILBOX_MODE_NAME = "Mailbox";
    int MAKEFILE_MODE           = 24;   String MAKEFILE_MODE_NAME = "Makefile";
    int MAN_MODE                = 25;   String MAN_MODE_NAME = "Man";
    int MESSAGE_MODE            = 26;   String MESSAGE_MODE_NAME = "Message";
    int NEWS_GROUPS_MODE        = 27;   String NEWS_GROUPS_MODE_NAME = "Groups";
    int NEWS_GROUP_SUMMARY_MODE = 28;   String NEWS_GROUP_SUMMARY_MODE_NAME = "Summary";
    int OBJC_MODE               = 29;   String OBJC_MODE_NAME = "Objective C";
    int PERL_MODE               = 30;   String PERL_MODE_NAME = "Perl";
    int PHP_MODE                = 31;   String PHP_MODE_NAME = "PHP";
    int PLAIN_TEXT_MODE         = 32;   String PLAIN_TEXT_MODE_NAME = "Plain Text";
    int PROPERTIES_MODE         = 33;   String PROPERTIES_MODE_NAME = "Properties";
    int PYTHON_MODE             = 34;   String PYTHON_MODE_NAME = "Python";
    int RUBY_MODE               = 35;   String RUBY_MODE_NAME = "Ruby";
    int SCHEME_MODE             = 36;   String SCHEME_MODE_NAME = "Scheme";
    int SEND_MAIL_MODE          = 37;   String SEND_MAIL_MODE_NAME = "Send Mail";
    int SHELL_MODE              = 38;   String SHELL_MODE_NAME = "Shell";
    int SHELL_SCRIPT_MODE       = 39;   String SHELL_SCRIPT_MODE_NAME = "Shell-script";
    int TCL_MODE                = 40;   String TCL_MODE_NAME = "Tcl";
    int VERILOG_MODE            = 41;   String VERILOG_MODE_NAME = "Verilog";
    int VHDL_MODE               = 42;   String VHDL_MODE_NAME = "VHDL";
    int WEB_MODE                = 43;   String WEB_MODE_NAME = "Web";
    int WORD_MODE               = 44;   String WORD_MODE_NAME = "Word";
    int XML_MODE                = 45;   String XML_MODE_NAME = "XML";

    int SUCCESS    =  0;
    int ERROR      = -1;
    int CANCELLED  =  1;

    int SHIFT_MASK = InputEvent.SHIFT_MASK;
    int CTRL_MASK  = InputEvent.CTRL_MASK;
    int ALT_MASK   = InputEvent.ALT_MASK;
    int META_MASK  = InputEvent.META_MASK;

    int RESPONSE_YES        = 0;
    int RESPONSE_NO         = 1;
    int RESPONSE_YES_TO_ALL = 2;
    int RESPONSE_CANCEL     = 4;

    // States for Formatter.parseBuffer(). May be stored in the line flags.
    int STATE_NEUTRAL         =  0;
    int STATE_COMMENT         =  1;
    int STATE_QUOTE           =  2;
    int STATE_SINGLEQUOTE     =  3;
    int STATE_IDENTIFIER      =  4;
    int STATE_TAG             =  5;
    int STATE_SCRIPT          =  6;
    int STATE_SCRIPT_TAG      =  7;
    int STATE_HTML_COMMENT    =  8;
    int STATE_OPERATOR        =  9;
    int STATE_BRACE           = 10;
    int STATE_NUMBER          = 11;
    int STATE_HEXNUMBER       = 12;
    int STATE_PREPROCESSOR    = 13;
    int STATE_DISABLED        = 14;
    int STATE_FUNCTION        = 15;
    int STATE_INPUT           = 16;
    int STATE_OUTPUT          = 17;
    int STATE_PROMPT          = 18;
    int STATE_PASSWORD_PROMPT = 19;
    int STATE_CDATA           = 20;

    int STATE_LAST            = 20;

    String DEFAULT_SHELL_PROMPT_PATTERN = "^[^#$%>]*[#$%>] *|^: *";

    String DEFAULT_CMD_EXE_PROMPT_PATTERN = "^[A-Z]:[^>]*>";

    String EXPLICIT_FOLD_START = "{{{";
    String EXPLICIT_FOLD_END   = "}}}";
}
