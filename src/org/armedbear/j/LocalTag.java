/*
 * LocalTag.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.awt.Component;
import java.awt.Graphics;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.undo.CompoundEdit;

public class LocalTag extends Tag implements Constants
{
    private final Position pos;
    private final int type;
    private int flags;

    protected LocalTag(String name, Line line)
    {
        super(name, line.getText());
        pos = new Position(line, 0);
        type = TAG_METHOD;
    }

    protected LocalTag(String name, Position pos)
    {
        super(name, pos.getLine().getText());
        this.pos = new Position(pos);
        type = TAG_METHOD;
    }

    protected LocalTag(String name, Position pos, int type, int flags)
    {
        super(name, pos.getLine().getText());
        this.pos = new Position(pos);
        this.type = type;
        this.flags = flags;
    }

    protected LocalTag(String name, Position pos, int type)
    {
        super(name, pos.getLine().getText());
        this.pos = new Position(pos);
        this.type = type;
    }

    public String getMethodName()
    {
        return name;
    }

    public String getLongName()
    {
        return name;
    }

    public String getClassName()
    {
        return null;
    }

    public final Position getPosition()
    {
        return pos;
    }

    public final Line getLine()
    {
        return pos.getLine();
    }

    public final int lineNumber()
    {
        return pos.lineNumber();
    }

    public final int getType()
    {
        return type;
    }

    public final boolean isPublic()
    {
        return (flags & TAG_VISIBILITY_MASK) == TAG_PUBLIC;
    }

    public final boolean isProtected()
    {
        return (flags & TAG_VISIBILITY_MASK) == TAG_PROTECTED;
    }

    public final boolean isPrivate()
    {
        return (flags & TAG_VISIBILITY_MASK) == TAG_PRIVATE;
    }

    private static ImageIcon interfaceIcon;
    private static ImageIcon classIcon;
    private static ImageIcon methodIcon;
    private static ImageIcon fieldIcon;

    private static ImageIcon publicIcon;
    private static ImageIcon protectedIcon;
    private static ImageIcon privateIcon;

    public Icon getIcon()
    {
        ImageIcon base = null;
        ImageIcon overlay = null;
        switch (type) {
            case TAG_INTERFACE:
            case TAG_IMPLEMENTS:
            case TAG_TYPE:      // Lisp
                if (interfaceIcon == null)
                    interfaceIcon = Utilities.getIconFromFile("interface.png");
                base = interfaceIcon;
                break;
            case TAG_CLASS:
            case TAG_EXTENDS:
            case TAG_CONDITION: // Lisp
            case TAG_STRUCT:    // Lisp
                if (classIcon == null)
                    classIcon = Utilities.getIconFromFile("class.png");
                base = classIcon;
                break;
            case TAG_METHOD:
            case TAG_MACRO:     // Lisp
            case TAG_DEFUN:     // Lisp
            default:
                if (methodIcon == null)
                    methodIcon = Utilities.getIconFromFile("method.png");
                base = methodIcon;
                break;
            case TAG_FIELD:
            case TAG_CONSTANT:  // Lisp
            case TAG_PARAMETER: // Lisp
            case TAG_VAR:       // Lisp
                if (fieldIcon == null)
                    fieldIcon = Utilities.getIconFromFile("field.png");
                base = fieldIcon;
                break;
        }
        if (isPublic()) {
            if (publicIcon == null)
                publicIcon = Utilities.getIconFromFile("public.png");
            overlay = publicIcon;
        } else if (isProtected()) {
            if (protectedIcon == null)
                protectedIcon = Utilities.getIconFromFile("protected.png");
            overlay = protectedIcon;
        } else if (isPrivate()) {
            if (privateIcon == null)
                privateIcon = Utilities.getIconFromFile("private.png");
            overlay = privateIcon;
        }
        return new OverlayIcon(base, overlay);
    }

    public String toString()
    {
        return getMethodName();
    }

    public String getSidebarText()
    {
        return getMethodName();
    }

    public String getToolTipText()
    {
        return getLongName();
    }

    public void gotoTag(Editor editor)
    {
        if (editor.getBuffer().contains(pos.getLine())) {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.addUndo(SimpleEdit.FOLD);
            editor.unfoldMethod(pos.getLine());
            editor.moveDotTo(pos);
            TagCommands.centerTag(editor);
            editor.endCompoundEdit(compoundEdit);
            editor.getBuffer().repaint();
            editor.updateDisplay();
        }
    }

    private static class OverlayIcon extends ImageIcon
    {
        private ImageIcon base;
        private ImageIcon overlay;

        private OverlayIcon(ImageIcon base, ImageIcon overlay)
        {
            this.base = base;
            this.overlay = overlay;
        }

        public synchronized void paintIcon(Component c, Graphics g, int x, int y)
        {
            if (base != null)
                base.paintIcon(c, g, x, y);
            if (overlay != null)
                overlay.paintIcon(c, g, x, y);
        }

        public final int getIconWidth()
        {
            return base.getIconWidth();
        }

        public final int getIconHeight()
        {
            return base.getIconHeight();
        }
    }
}
