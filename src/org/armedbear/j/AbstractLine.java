/*
 * AbstractLine.java
 *
 * Copyright (C) 1999-2002 Peter Graves
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

public abstract class AbstractLine implements Line
{
    private Line prev;
    private Line next;
    private int lineNumber = -1;
    private int originalLineNumber = -1;
    private int hidden;
    private Annotation annotation;

    public final synchronized Line previous()
    {
        return prev;
    }

    public final synchronized void setPrevious(Line line)
    {
        prev = line;
    }

    public final synchronized Line next()
    {
        return next;
    }

    public final synchronized void setNext(Line line)
    {
        next = line;
    }

    public final synchronized void insertAfter(Line line)
    {
        if (line != null) {
            Line n = line.next();
            setNext(n);
            if (n != null)
                n.setPrevious(this);
            line.setNext(this);
            setPrevious(line);
        } else
            Debug.bug();
    }

    public final synchronized int lineNumber()
    {
        return lineNumber;
    }

    public final synchronized void setLineNumber(int n)
    {
        lineNumber = n;
    }

    public final synchronized int originalLineNumber()
    {
        return originalLineNumber;
    }

    public final synchronized void setOriginalLineNumber(int n)
    {
        originalLineNumber = n;
    }

    public int getHeight()
    {
        return Display.getCharHeight();
    }

    public int getWidth()
    {
        return 0;
    }

    // Derived classes override this!
    public String getText()
    {
        return null;
    }

    // Derived classes override this!
    public String getOriginalText()
    {
        return null;
    }

    // Derived classes override this!
    public void setOriginalText(String s)
    {
        // Do nothing.
    }

    // Derived classes override this!
    public boolean isModified()
    {
        return false;
    }

    // Derived classes override this!
    public boolean isNew()
    {
        return false;
    }

    // Derived classes override this!
    public void setNew(boolean b)
    {
        // Do nothing.
    }

    // Derived classes override this!
    public boolean isSaved()
    {
        return false;
    }

    // Derived classes override this!
    public void setSaved(boolean b)
    {
        // Do nothing.
    }

    // Derived classes override this!
    public void unmodified()
    {
        // Do nothing.
    }

    // Returns offset (not column) of first non-whitespace character.
    public int getIndentation()
    {
        String text = getText();
        if (text == null)
            return 0;
        int limit = text.length();
        for (int i = 0; i < limit; i++)
            if (!Character.isWhitespace(text.charAt(i)))
                return i;
        return limit;
    }

    public final boolean isHidden()
    {
        return hidden > 0;
    }

    public final void hide()
    {
        ++hidden;
    }

    public final void unhide()
    {
        --hidden;
        if (Editor.isDebugEnabled() && hidden < 0)
            Debug.bug("hidden < 0");
    }

    public final void show()
    {
        hidden = 0;
    }

    public final int getHidden()
    {
        return hidden;
    }

    public final void setHidden(int hidden)
    {
        this.hidden = hidden;
    }

    public final synchronized Line previousVisible()
    {
        Line line = previous();
        while (line != null && line.isHidden())
            line = line.previous();
        return line;
    }

    public final synchronized Line nextVisible()
    {
        Line line = next();
        while (line != null && line.isHidden())
            line = line.next();
        return line;
    }

    public final boolean isBefore(Line line)
    {
        return lineNumber < line.lineNumber();
    }

    // Derived classes override this!
    public Line copy()
    {
        return null;
    }

    // Derived classes override this!
    public void copy(Line line)
    {
        // Do nothing.
    }

    public final Annotation getAnnotation()
    {
        return annotation;
    }

    public final void setAnnotation(Annotation annotation)
    {
        this.annotation = annotation;
    }

    public final String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        sb.append("line ");
        sb.append(lineNumber()+1);
        String s = getText();
        if (s != null) {
            sb.append(" text = \"");
            sb.append(s);
            sb.append('"');
        } else
            sb.append(" text = null");
        return sb.toString();
    }
}
