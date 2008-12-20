/*
 * CMode.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.awt.event.KeyEvent;
import java.util.HashSet;

public class CMode extends JavaMode implements Constants, Mode
{
    private static final String[] cConditionals = {
        "if",
        "else",
        "do",
        "while",
        "for",
        "switch"
    };

    private static CMode mode;

    private CMode()
    {
        super(C_MODE, C_MODE_NAME);
        keywords = new Keywords(this);
        conditionals = cConditionals;
    }

    protected CMode(int id, String displayName)
    {
        super(id, displayName);
    }

    // Don't construct the singleton class instance until we actually need it,
    // to avoid unnecessary overhead for CppMode which is derived from this
    // class.
    public static Mode getMode()
    {
        if (mode == null)
            mode = new CMode();
        return mode;
    }

    public String getCommentStart()
    {
        return "/*";
    }

    public String getCommentEnd()
    {
        return "*/";
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new CFormatter(buffer, LANGUAGE_C);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        super.setKeyMapDefaults(km);
        km.mapKey('#', "electricPound");
        km.mapKey(KeyEvent.VK_M, CTRL_MASK, "cppFindMatch");
        km.mapKey(KeyEvent.VK_F6, CTRL_MASK, "iList");
    }

    public void populateModeMenu(Editor editor, Menu menu)
    {
        menu.add(editor, "Compile...", 'C', "compile");
        menu.add(editor, "Recompile", 'R', "recompile");
        boolean enabled = CompilationCommands.getCompilationBuffer() != null;
        menu.addSeparator();
        menu.add(editor, "Next Error", 'N', "nextError", enabled);
        menu.add(editor, "Previous Error", 'P', "previousError", enabled);
        menu.add(editor, "Show Error Message", 'M', "showMessage", enabled);
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return new CTagger(buffer);
    }

    public boolean hasQualifiedNames()
    {
        return false;
    }

    public boolean isQualifiedName(String s)
    {
        return false;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        if (line.trim().startsWith("#"))
            return 0; // Preprocessor directive.

        return super.getCorrectIndentation(line, buffer);
    }

    protected static String getPreprocessorToken(Line line)
    {
        String s = line.trim();
        if (s.length() == 0 || s.charAt(0) != '#')
            return null;
        final int limit = s.length();
        int i;
        for (i = 1; i < limit; i++) {
            char c = s.charAt(i);
            if (c != ' ' && c != '\t')
                break;
        }
        FastStringBuffer sb = new FastStringBuffer();
        for (; i < limit; i++) {
            char c = s.charAt(i);
            if (c >= 'a' && c <='z')
                sb.append(c);
            else
                break;
        }
        return sb.toString();
    }

    // Used only by findMatchPreprocessor. This needs to persist between calls
    // so we can handle #else/#elif correctly in successive calls.
    private static boolean matchBackwards = false;

    public static Line findMatchPreprocessor(Line startLine)
    {
        final String patternIf = "if";
        final String patternElse = "el";
        final String patternEndif = "endif";
        String token = null;
        String match = null;
        boolean searchBackwards = false;
        String s = getPreprocessorToken(startLine);
        if (s == null)
            return null;
        if (s.startsWith(patternIf)) {
            token = patternIf;
            match = patternEndif;
            matchBackwards = false;
        } else if (s.startsWith(patternEndif)) {
            token = patternEndif;
            match = patternIf;
            matchBackwards = true;
        } else if (s.startsWith(patternElse)) {
            if (matchBackwards) {
                token = patternEndif;
                match = patternIf;
            } else {
                token = patternIf;
                match = patternEndif;
            }
        } else
            return null;
        int count = 1;
        Line line = startLine;
        while (true) {
            if (matchBackwards)
                line = line.previous();
            else
                line = line.next();
            if (line == null) break;
            s = getPreprocessorToken(line);
            if (s != null) {
                if (count == 1 && s.startsWith(patternElse))
                    return line;
                if (s.startsWith(token))
                    ++count;
                else if (s.startsWith(match))
                    --count;
                if (count == 0)
                    return line;
            }
        }
        return null;
    }

    public boolean isIdentifierStart(char c)
    {
        if (c >= 'a' && c <= 'z')
            return true;
        if (c >='A' && c <= 'Z')
            return true;
        if (c == '_')
            return true;
        return false;
    }

    public boolean isIdentifierPart(char c)
    {
        if (c >= 'a' && c <= 'z')
            return true;
        if (c >='A' && c <= 'Z')
            return true;
        if (c >= '0' && c <= '9')
            return true;
        if (c == '_')
            return true;
        return false;
    }
}
