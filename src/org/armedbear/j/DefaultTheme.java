/*
 * DefaultTheme.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.awt.Color;
import java.awt.Font;

public final class DefaultTheme
{
    public static final Color getColor(String thing)
    {
        return getColor(null, thing);
    }

    // Returns null if mode/thing not found.
    public static final Color getColor(String mode, String thing)
    {
        if (thing == null)
            return null;
        thing = thing.intern();
        if (mode != null) {
            mode = mode.intern();
            if (mode == "CSSMode") {
                if (thing == "selector")
                    return new Color(0, 0, 0);
                if (thing == "property")
                    return new Color(0, 0, 204);
            } else if (mode == "DiffMode") {
                if (thing == "file")
                    return new Color(0, 0, 0);
                if (thing == "header")
                    return new Color(0, 102, 0);
                if (thing == "context")
                    return new Color(0, 0, 0);
                if (thing == "inserted")
                    return new Color(153, 0, 0);
                if (thing == "deleted")
                    return new Color(0, 0, 153);
            } else if (mode == "DirectoryMode") {
                if (thing == "directory")
                    return new Color(0, 0, 0);
                if (thing == "symlink")
                    return new Color(0, 0, 255);
                if (thing == "marked")
                    return new Color(153, 0, 0);
            } else if (mode == "HtmlMode") {
                if (thing == "tag")
                    return new Color(0, 0, 153);
                if (thing == "anchor")
                    return new Color(51, 153, 51);
                if (thing == "image")
                    return new Color(204, 102, 0);
                if (thing == "table")
                    return new Color(204, 0, 0);
                if (thing == "tableRow")
                    return new Color(153, 0, 0);
                if (thing == "tableData")
                    return new Color(153, 51, 0);
                if (thing == "comment")
                    return new Color(128, 128, 128);
                if (thing == "script")
                    return new Color(0, 0, 255);
            } else if (mode == "ListOccurrencesMode") {
                if (thing == "headerName")
                    return new Color(0, 0, 153);
                if (thing == "headerValue")
                    return new Color(0, 0, 255);
            } else if (mode == "MailboxMode") {
                if (thing == "to")
                    return new Color(0, 0, 0);
                if (thing == "flags")
                    return new Color(0, 0, 0);
                if (thing == "date")
                    return new Color(51, 51, 51);
                if (thing == "from")
                    return new Color(0, 0, 0);
                if (thing == "size")
                    return new Color(51, 51, 51);
                if (thing == "subject")
                    return new Color(51, 102, 102);
                if (thing == "flaggedTo")
                    return new Color(204, 51, 0);
                if (thing == "flaggedFlags")
                    return new Color(0, 0, 0);
                if (thing == "flaggedDate")
                    return new Color(0, 0, 0);
                if (thing == "flaggedFrom")
                    return new Color(204, 51, 0);
                if (thing == "flaggedSize")
                    return new Color(0, 0, 0);
                if (thing == "flaggedSubject")
                    return new Color(204, 51, 0);
                if (thing == "marked")
                    return new Color(153, 0, 0);
                if (thing == "deleted")
                    return new Color(153, 153, 153);
            } else if (mode == "MessageMode") {
                if (thing == "headerName")
                    return new Color(0, 0, 153);
                if (thing == "headerValue")
                    return new Color(51, 102, 102);
                if (thing == "signature")
                    return new Color(102, 102, 102);
                if (thing == "string")
                    return new Color(0, 102, 0);
                if (thing == "comment")
                    return new Color(102, 102, 102);
            } else if (mode == "WebMode") {
                if (thing == "headerValue")
                    return new Color(51, 102, 102);
            } else if (mode == "LispMode") {
                if (thing == "substitution")
                    return new Color(153, 0, 153);
                if (thing == "punctuation")
                    return new Color(102, 102, 102);
                if (thing == "parenthesis")
                    return new Color(102, 102, 102);
                if (thing == "secondaryKeyword")
                    return new Color(0, 102, 153);
            } else if (mode == "PerlMode") {
                if (thing == "scalar")
                    return new Color(51, 51, 0);
                if (thing == "list")
                    return new Color(0, 51, 51);
            } else if (mode == "PHPMode") {
                if (thing == "var")
                    return new Color(51, 51, 0);
                if (thing == "tag")
                    return new Color(0, 0, 0);
                if (thing == "attribute")
                    return new Color(0, 0, 128);
                if (thing == "equals")
                    return new Color(0, 153, 153);
            } else if (mode == "TclMode") {
                if (thing == "brace")
                    return new Color(153, 0, 51);
                if (thing == "bracket")
                    return new Color(204, 102, 0);
            } else if (mode == "VHDLMode") {
                if (thing == "type")
                    return new Color(0, 0, 255);
            } else if (mode == "PropertiesMode") {
                if (thing == "section")
                    return new Color(0, 0, 153);
            } else if (mode == "XmlMode") {
                if (thing == "attribute")
                    return new Color(0, 0, 128);
                if (thing == "equals")
                    return new Color(0, 153, 153);
                if (thing == "namespace")
                    return new Color(0, 0, 0);
                if (thing == "tag")
                    return new Color(0, 0, 0);
            }
        }

        if (thing == "text")
            return new Color(0, 0, 0);
        if (thing == "background")
            return new Color(255, 255, 224);
        if (thing == "caret")
            return new Color(255, 0, 0);
        if (thing == "verticalRule")
            return new Color(204, 204, 204);
        if (thing == "selectionBackground")
            return new Color(153, 204, 255);
        if (thing == "matchingBracketBackground")
            return new Color(153, 204, 255);
        if (thing == "preprocessor")
            return new Color(255, 0, 0);
        if (thing == "comment")
            return new Color(0, 102, 0);
        if (thing == "keyword")
            return new Color(0, 0, 153);
        if (thing == "brace")
            return new Color(0, 128, 128);
        if (thing == "number")
            return new Color(153, 102, 51);
        if (thing == "currentLineBackground")
            return new Color(235, 235, 204);
        if (thing == "function")
            return new Color(0, 0, 0);
        if (thing == "string")
            return new Color(153, 51, 0);
        if (thing == "operator")
            return new Color(0, 0, 255);
        if (thing == "disabled")
            return new Color(153, 153, 153);
        if (thing == "change")
            return new Color(255, 164, 0);
        if (thing == "savedChange")
            return new Color(180, 180, 180);
        if (thing == "lineNumber")
            return new Color(153, 153, 153);
        if (thing == "gutterBorder")
            return new Color(153, 153, 153);
        if (thing == "prompt")
            return new Color(0, 0, 0);
        if (thing == "input")
            return new Color(0, 0, 255);
        if (thing == "matchingText")
            return new Color(204, 102, 0);
        if (thing == "status")
            return new Color(0, 0, 153);
        if (thing == "key")
            return new Color(0, 0, 153);
        if (thing == "value")
            return new Color(128, 0, 0);
        if (thing == "delimiter")
            return new Color(0, 153, 153);

        // Makefile mode.
        if (thing == "target")
            return new Color(0, 0, 0);

        // Web mode.
        if (thing == "link")
            return new Color(0, 0, 255);

        // List Registers mode.
        if (thing == "registerPrefix")
            return new Color(0, 0, 153);
        if (thing == "registerName")
            return new Color(204, 102, 0);

        // Not found.
        return null;
    }

    // Font.PLAIN is 0, Font.BOLD is 1, Font.ITALIC is 2.
    // Returns -1 if mode/thing not found.
    public static final int getStyle(String mode, String thing)
    {
        if (thing == null)
            return -1;
        thing = thing.intern();
        if (mode != null) {
            mode = mode.intern();
            if (mode == "CSSMode") {
                if (thing == "selector")
                    return Font.BOLD;
                if (thing == "property")
                    return Font.PLAIN;
            } else if (mode == "DiffMode") {
                if (thing == "file")
                    return Font.BOLD;
                if (thing == "header")
                    return Font.ITALIC;
            } else if (mode == "MailboxMode") {
                if (thing == "to")
                    return Font.BOLD;
                if (thing == "date")
                    return Font.PLAIN;
                if (thing == "from")
                    return Font.BOLD;
                if (thing == "subject")
                    return Font.BOLD;
                if (thing == "flaggedTo")
                    return Font.BOLD;
                if (thing == "flaggedFrom")
                    return Font.BOLD;
                if (thing == "flaggedSubject")
                    return Font.BOLD;
                if (thing == "marked")
                    return Font.BOLD;
            } else if (mode == "MessageMode") {
                if (thing == "headerName")
                    return Font.BOLD;
                if (thing == "headerValue")
                    return Font.BOLD;
                if (thing == "comment")
                    return Font.PLAIN;
            } else if (mode == "WebMode") {
                if (thing == "headerValue")
                    return Font.BOLD;
            } else if (mode == "ListOccurrencesMode") {
                if (thing == "headerName")
                    return Font.BOLD;
            } else if (mode == "PropertiesMode") {
                if (thing == "section")
                    return Font.BOLD;
                else if (thing == "comment")
                    return Font.ITALIC;
                else
                    return Font.PLAIN;
            } else if (mode == "DirectoryMode") {
                if (thing == "directory")
                    return Font.BOLD;
                if (thing == "marked")
                    return Font.BOLD;
            } else if (mode == "TclMode") {
                if (thing == "brace")
                    return Font.BOLD;
                if (thing == "bracket")
                    return Font.BOLD;
            } else if (mode == "XmlMode" || mode == "PHPMode" ) {
                if (thing == "tag")
                    return Font.BOLD;
            } else if (mode == "MakefileMode") {
                if (thing == "target")
                    return Font.BOLD;
            }
        }

        if (thing == "keyword")
            return Font.BOLD;
        if (thing == "function")
            return Font.BOLD;
        if (thing == "prompt")
            return Font.BOLD;
        if (thing == "comment")
            return Font.ITALIC;
        if (thing == "matchingText")
            return Font.BOLD;
        if (thing == "status")
            return Font.ITALIC;
        if (thing == "key")
            return Font.PLAIN;
        if (thing == "delimiter")
            return Font.BOLD;

        // List Registers mode.
        if (thing == "registerPrefix")
            return Font.BOLD;
        if (thing == "registerName")
            return Font.BOLD;

        // Not found.
        return -1;
    }
}
