/*
 * ImageLine.java
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

import java.awt.Image;
import java.awt.Rectangle;

public final class ImageLine extends AbstractLine implements Line
{
    private Image image;
    private final int imageHeight;
    private final int imageWidth;
    private final int height;
    private final Rectangle rect;

    public ImageLine(Image image, Rectangle r)
    {
        this.image = image;
        rect = new Rectangle(r);
        height = Math.max(r.height, Display.getCharHeight());;
        imageHeight = r.height;
        imageWidth = r.width;
    }

    public final Image getImage()
    {
        return image;
    }

    public final Rectangle getRect()
    {
        return rect;
    }

    public final int getImageHeight()
    {
        return imageHeight;

    }

    public final int getImageWidth()
    {
        return imageWidth;
    }

    public final int getHeight()
    {
        return height;
    }

    public final int getWidth()
    {
        return getImageWidth();
    }

    public final int flags()
    {
        return 0;
    }

    public final void setFlags(int flags)
    {
    }

    public String getText()
    {
        return null;
    }

    public final void setText(String s)
    {
    }

    public final char charAt(int i)
    {
        return '\0';
    }

    public final String substring(int beginIndex)
    {
        return null;
    }

    public final String substring(int beginIndex, int endIndex)
    {
        return null;
    }

    public final String trim()
    {
        return null;
    }

    public final int length()
    {
        return 0;
    }

    public final byte[] getBytes(String encoding)
    {
        return null;
    }

    public final boolean isBlank()
    {
        return false;
    }
    
    public final void flushImage()
    {
        if (image != null) {
            image.flush();
            image = null;
        }        
    }
    
    protected void finalize() throws Throwable
    {
        flushImage();
        super.finalize();
    }
}
