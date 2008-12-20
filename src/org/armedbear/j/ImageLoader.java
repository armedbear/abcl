/*
 * ImageLoader.java
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
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.lang.reflect.Method;

public final class ImageLoader
{
    private File file;
    private MediaTracker mt;
    private Image image;

    public ImageLoader(File file)
    {
        this.file = file;
    }

    public Image loadImage()
    {
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        image = Toolkit.getDefaultToolkit().createImage(file.canonicalPath());
        mt = new MediaTracker(editor);
        try {
            mt.addImage(image, 0);
            mt.waitForID(0);
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (mt.isErrorAny())
            image = null;
        if (image == null) {
            // Try again using JIMI.
            try {
                Class c = Class.forName("com.sun.jimi.core.Jimi");
                Class[] parameterTypes = new Class[1];
                parameterTypes[0] = Class.forName("java.lang.String");
                Method method = c.getMethod("getImage", parameterTypes);
                Object[] args = new Object[1];
                args[0] = file.canonicalPath();
                Object returned = method.invoke(null, args);
                if (returned instanceof Image)
                    image = (Image) returned;
            }
            catch (ClassNotFoundException e) {
                // JIMI not found.
            }
            catch (Exception e) {
                Log.error(e);
            }
            mt = new MediaTracker(editor);
            try {
                mt.addImage(image , 0);
                mt.waitForID(0);
            }
            catch (Exception e) {
                Log.error(e);
            }
            if (mt.isErrorAny())
                image = null;
        }
        editor.setDefaultCursor();
        return image;
    }

    public void dispose()
    {
        if (image != null && mt != null) {
            mt.removeImage(image);
            image.flush();
            image = null;
            mt = null;
        }
    }
}
