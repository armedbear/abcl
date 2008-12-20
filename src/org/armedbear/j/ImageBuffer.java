/*
 * ImageBuffer.java
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
import java.awt.Cursor;
import java.awt.Image;
import java.awt.MediaTracker;

public class ImageBuffer extends Buffer implements Constants
{
    private static Color backgrounds[];

    static {
        backgrounds = new Color[3];
        backgrounds[0] = new Color(153, 153, 153);
        backgrounds[1] = Color.black;
        backgrounds[2] = Color.white;
    }

    private int backgroundIndex;
    private Image currentImage;
    private Image originalImage;
    private int originalWidth;
    private int originalHeight;
    private int currentWidth;
    private int currentHeight;
    private ImageLoader loader;

    private ImageBuffer(File file, File cache, String listing)
    {
        super();
        mode = Editor.getModeList().getMode(IMAGE_MODE);
        setFile(file);
        supportsUndo = false;
        readOnly = true;
        autosaveEnabled = false;
        type = TYPE_IMAGE;
        setCache(cache);
        setListing(listing);
        setInitialized(true);
    }

    public static ImageBuffer createImageBuffer(File file, File cache, String listing)
    {
        // Load the image before creating the buffer, so we don't have to
        // unlink the buffer if we can't load the image.
        File toBeLoaded = cache != null ? cache : file;
        ImageLoader loader = new ImageLoader(toBeLoaded);
        Image img = loader.loadImage();
        if (img != null) {
            // The image was loaded successfully. Now create the buffer.
            ImageBuffer ib = new ImageBuffer(file, cache, listing);
            ib.loader = loader;
            ib.currentImage = ib.originalImage = img;
            ib.currentWidth = ib.originalWidth = img.getWidth(null);
            ib.currentHeight = ib.originalHeight = img.getHeight(null);
            ib.setLastModified(toBeLoaded.lastModified());
            ib.setLoaded(true);
            return ib;
        } else
            return null;
    }

    public final Position getInitialDotPos()
    {
        return null;
    }

    public final boolean needsParsing()
    {
        return false;
    }

    public final Image getImage()
    {
        return currentImage;
    }

    public final int getDisplayHeight()
    {
        if (getModeId() == IMAGE_MODE)
            return currentImage.getHeight(null) + Display.getImageBorderHeight() * 2;
        else
            return super.getDisplayHeight();
    }

    public final int getDisplayWidth()
    {
        if (getModeId() == IMAGE_MODE)
            return currentImage.getWidth(null) + Display.getImageBorderWidth() * 2;
        else
            return super.getDisplayWidth();
    }

    public int load()
    {
        if (!isLoaded()) {
            Debug.assertTrue(loader == null);
            final File toBeLoaded = getCache() != null ? getCache() : getFile();
            loader = new ImageLoader(toBeLoaded);
            Image img = loader.loadImage();
            if (img != null) {
                currentImage = originalImage = img;
                currentWidth = originalWidth = img.getWidth(null);
                currentHeight = originalHeight = img.getHeight(null);
            } else
                MessageDialog.showMessageDialog("Error loading image", "Error");
            setLastModified(toBeLoaded.lastModified());
            setLoaded(true);
        }
        return LOAD_COMPLETED;
    }

    public void reload()
    {
        switch (getModeId()) {
            case BINARY_MODE:
                if (loader != null) {
                    loader.dispose();
                    loader = null;
                }
                currentImage = null;
                originalImage = null;
                super.reload();
                return;
            case IMAGE_MODE:
                if (getFile().isLocal())
                    reloadLocal();
                else
                    reloadRemote();
                return;
            default:
                Debug.bug();
        }
    }

    private void reloadLocal()
    {
        empty();
        if (loader != null) {
            loader.dispose();
            loader = null;
        }
        currentImage = null;
        originalImage = null;
        load();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == ImageBuffer.this) {
                ed.getDisplay().repaint();
                ed.updateDisplay();
            }
        }
    }

    private void reloadRemote()
    {
        final File file = getFile();
        LoadProcess p = null;
        if (file instanceof FtpFile) {
            FtpSession session = FtpSession.getSession((FtpFile)file);
            p = new FtpLoadProcess(this, (FtpFile)file, session);
        } else if (file instanceof HttpFile) {
            p = new HttpLoadProcess(this, (HttpFile)file);
        } else {
            Debug.bug();
            return;
        }
        final LoadProcess loadProcess = p;
        Runnable successRunnable = new Runnable() {
            public void run()
            {
                final File cache = getCache();
                if (cache != null && cache.isFile())
                    cache.delete();
                setCache(loadProcess.getCache());
                reloadLocal();
                setBusy(false);
            }
        };
        ErrorRunnable errorRunnable = new ErrorRunnable("Reload failed");
        loadProcess.setProgressNotifier(new StatusBarProgressNotifier(this));
        loadProcess.setSuccessRunnable(successRunnable);
        loadProcess.setErrorRunnable(errorRunnable);
        loadProcess.start();
    }

    public final Color getBackgroundColor()
    {
        return getBackground(backgroundIndex);
    }

    public static final Color getDefaultBackgroundColor()
    {
        return getBackground(0);
    }

    public final int getImageWidth()
    {
        return originalWidth;
    }

    public final int getImageHeight()
    {
        return originalHeight;
    }

    public final void cycleBackground()
    {
        if (++backgroundIndex >= getBackgroundCount())
            backgroundIndex = 0;
    }

    private static final int getBackgroundCount()
    {
        return backgrounds.length;
    }

    private static Color getBackground(int index)
    {
        if (index >= 0 && index < backgrounds.length)
            return backgrounds[index];
        else
            return backgrounds[0];
    }

    public void zoomIn()
    {
        int w = currentWidth * 2;
        int h = currentHeight * 2;
        if (w > 0 && h > 0)
            resize(w, h);
    }

    public void zoomOut()
    {
        int w = currentWidth / 2;
        int h = currentHeight / 2;
        if (w > 0 && h > 0)
            resize(w, h);
    }

    public void fit()
    {
        if (originalWidth == 0 || originalHeight == 0)
            return;
        Editor editor = Editor.currentEditor();
        Display display = editor.getDisplay();
        int displayWidth = display.getWidth() - Display.getImageBorderWidth() * 2;
        int displayHeight = display.getHeight() - Display.getImageBorderHeight() * 2;
        float factor = (float) displayWidth / (float) originalWidth;
        if (factor * originalHeight > displayHeight)
            factor = (float) displayHeight / (float) originalHeight;
        int w = (int) (originalWidth * factor);
        int h = (int) (originalHeight * factor);
        resize(w, h);
    }

    public void restore()
    {
        resize(originalWidth, originalHeight);
    }

    private void resize(int w, int h)
    {
        Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        Image img = null;
        MediaTracker mt = null;

        try
        {
            long pixels = w * h;
            if (pixels > originalWidth * originalHeight && pixels > 2592000) {
                // 1800x1440 (an arbitrary limit)
                editor.status("Too many pixels!");
                return;
            }
            long bytesRequired = pixels * 4; // 4 bytes (32 bits) per pixel
            Runtime runtime = Runtime.getRuntime();
            long bytesFree = runtime.freeMemory();
            if (bytesRequired > bytesFree) {
                runtime.gc();
                bytesFree = runtime.freeMemory();
                if (bytesRequired > bytesFree) {
                    editor.status("Not enough memory");
                    return;
                }
            }
            img = originalImage.getScaledInstance(w, h, Image.SCALE_DEFAULT);
            mt = new MediaTracker(editor);
            mt.addImage(img , 0);
            mt.waitForID(0);
        }
        catch (Exception e) {
            Log.error(e);
        }
        finally {
            editor.setDefaultCursor();
        }

        if (mt == null || mt.isErrorAny())
            img = null;

        if (img != null) {
            if (currentImage != originalImage)
                currentImage.flush();
            currentImage = img;
            currentWidth = img.getWidth(null);
            currentHeight = img.getHeight(null);
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.getDisplay().repaint();
                    status(ed);
                }
            }
        }
    }

    public boolean reactivate()
    {
        final File file = getFile();
        if (file == null || file.isRemote() || !file.isFile())
            return false;
        if (isLoaded() && file.lastModified() != getLastModified()) {
            reload();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.getDisplay().repaint();
                    status(ed);
                }
            }
            return true;
        }
        return false;
    }

    private void status(Editor editor)
    {
        int percent =
            (int) ((float) currentWidth * 100 / (float) originalWidth + 0.5);
        editor.status(String.valueOf(percent) + '%');
    }

    public void dispose()
    {
        if (loader != null)
            loader.dispose();
        super.dispose();
    }

    public Cursor getDefaultCursor()
    {
        return Cursor.getDefaultCursor();
    }

    public Cursor getDefaultCursor(Position pos)
    {
        return Cursor.getDefaultCursor();
    }

    public void saveView(Editor editor)
    {
        // Nothing to do.
    }

    public String getStatusText(Editor editor)
    {
        FastStringBuffer sb = new FastStringBuffer(String.valueOf(getImageWidth()));
        sb.append('x');
        sb.append(String.valueOf(getImageHeight()));
        sb.append(" pixels");
        return sb.toString();
    }
}
