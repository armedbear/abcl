/*
 * JavaTag.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.util.StringTokenizer;

public final class JavaTag extends LocalTag
{
    private final JavaClass parent;

    public JavaTag(String name, Position pos, int type, int flags)
    {
        this(name, pos, type, flags, null);
    }

    public JavaTag(String name, Position pos, int type, int flags,
        JavaClass parent)
    {
        super(name, pos, type, flags);
        this.parent = parent;
        switch (type) {
            case TAG_METHOD:
                canonicalSignature = parseCanonicalSignatureForMethod();
                break;
            case TAG_EXPLICIT:
                canonicalSignature = pos.getLine().trim();
                break;
        }
    }

    public final JavaClass getParent()
    {
        return parent;
    }

    public String getMethodName()
    {
        return getShortName();
    }

    private String getShortName()
    {
        int index = name.lastIndexOf('.');
        if (index >= 0)
            return name.substring(index+1);
        else
            return name;
    }

    public String getLongName()
    {
        if (name.startsWith("class "))
            return name;
        if (canonicalSignature != null)
            return canonicalSignature;
        if (getType() == TAG_FIELD) {
            // Since we don't save information about fields in tag files, we
            // don't need to initialize a field's canonical signature in the
            // JavaTag constructor.
            return canonicalSignature = parseCanonicalSignatureForField();
        }
        String s = signature.trim();
        // Strip comment if any.
        int index = s.indexOf("//");
        if (index >= 0)
            s = s.substring(0, index).trim();
        index = s.indexOf('=');
        if (index >= 0)
            s = s.substring(0, index).trim();
        index = s.indexOf(';');
        if (index >= 0)
            s = s.substring(0, index).trim();
        return s;
    }

    public String getClassName()
    {
        if (parent != null)
            return parent.getName();
        return null;
    }

    private String parseCanonicalSignatureForField()
    {
        String s = signature.trim();
        // Strip comment if any.
        int index = s.indexOf("//");
        if (index >= 0)
            s = s.substring(0, index).trim();
        // There might be multiple declarations on the same line:
        //     "private int x = 1, y = 2, z = 3;"
        index = s.indexOf(',');
        if (index >= 0)
            s = s.substring(0, index).trim();
        index = s.indexOf('=');
        if (index >= 0)
            s = s.substring(0, index).trim();
        index = s.indexOf(';');
        if (index >= 0)
            s = s.substring(0, index).trim();
        // If there were multiple declarations on the same line, what we have
        // now is basically the first of them ("private int x"), even though
        // we might actually want one of the others. So we copy all but the
        // last token and then append the short name of this tag. This also
        // lets us convert tabs to spaces and remove excess whitespace from
        // the canonical signature.
        StringTokenizer st = new StringTokenizer(s);
        int count = st.countTokens();
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 0; i < count-1; i++) {
            sb.append(st.nextToken());
            sb.append(' ');
        }
        sb.append(getShortName());
        return sb.toString();
    }

    private String parseCanonicalSignatureForMethod()
    {
        FastStringBuffer sb = new FastStringBuffer();
        Position pos = getPosition().copy();
        pos.setOffset(0);
        while (Character.isWhitespace(pos.getChar()))
            if (!pos.next())
                return null;
        char lastChar = 0;
        char c;
        while ((c = pos.getChar()) != ')') {
            if (c == '/') {
                if (!pos.next())
                    return null;
                skipComment(pos);
                if (pos.atEnd())
                    return null;
                continue;
            }
            if (c == '\n' || c == '\t')
                c = ' ';
            if (c != ' ' || lastChar != ' ') {
                sb.append(c);
                lastChar = c;
            }
            if (!pos.next())
                return null;
        }
        if (c == ')')
            sb.append(c);
        return sb.toString();
    }

    // On entry, pos points at second char of "//" or "/*" (which might not
    // actually be '/' or '*').
    private static void skipComment(Position pos)
    {
        char c = pos.getChar();
        if (!pos.next())
            return;
        if (c == '/') {
            while ((c = pos.getChar()) != '\n')
                if (!pos.next())
                    return;
            pos.next();
        } else if (c == '*') {
            while (!pos.lookingAt("*/"))
                if (!pos.next())
                    return;
            pos.skip(2);
        }
    }

    public String getSidebarText()
    {
        switch (getType()) {
            case TAG_EXTENDS:
                return "extends ".concat(getMethodName());
            case TAG_IMPLEMENTS:
                return "implements ".concat(getMethodName());
            default:
                return getMethodName();
        }
    }

    public String getToolTipText()
    {
        switch (getType()) {
            case TAG_EXTENDS:
                return "class ".concat(getMethodName());
            case TAG_IMPLEMENTS:
                return "interface ".concat(getMethodName());
            default:
                return getLongName();
        }
    }

    public void gotoTag(Editor editor)
    {
        switch (getType()) {
            case TAG_EXTENDS:
            case TAG_IMPLEMENTS:
                if (!TagCommands.findClass(editor, getMethodName(), false))
                    super.gotoTag(editor);
                break;
            default:
                super.gotoTag(editor);
                break;
        }
    }
}
