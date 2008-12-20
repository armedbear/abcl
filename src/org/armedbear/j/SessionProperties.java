/*
 * SessionProperties.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;

public final class SessionProperties
{
    private Properties properties;
    private File file;
    private File backupFile;

    public SessionProperties()
    {
        properties = new Properties();
        file = File.getInstance(Directories.getEditorDirectory(), "props");
        backupFile = File.getInstance(Directories.getEditorDirectory(), "props~");
        try {
            InputStream in = null;
            if (file != null && file.isFile()) {
                in = file.getInputStream();
            } else if (backupFile != null && backupFile.isFile()) {
                Log.debug("SessionProperties constructor loading backup file");
                in = backupFile.getInputStream();
            }
            if (in != null) {
                properties.load(in);
                in.close();
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public void saveWindowPlacement()
    {
        for (int i = 0; i < Editor.getFrameCount(); i++)
            saveWindowPlacement(Editor.getFrame(i));
    }

    private void saveWindowPlacement(Frame frame)
    {
        saveSidebarState(frame);
        String prefix = getPrefix(frame);
        if (prefix != null) {
            setBooleanProperty(prefix + "toolbar.show", frame.getShowToolbar());
            Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
            Rectangle r = frame.getRect();
            if (screen != null && r != null) {
                if (r.x >= 0 && r.y >= 0 && r.width < screen.width && r.height < screen.height) {
                    setIntegerProperty(prefix + "x", r.x);
                    setIntegerProperty(prefix + "y", r.y);
                    setIntegerProperty(prefix + "width", r.width);
                    setIntegerProperty(prefix + "height", r.height);
                }
            }
            setIntegerProperty(prefix + "extendedState", frame.retrieveExtendedState());
        }
    }

    public Rectangle getWindowPlacement(int index)
    {
        Rectangle r = new Rectangle();
        String prefix = getPrefix(index);
        if (prefix != null) {
            r.x = getIntegerProperty(prefix + "x", 0);
            r.y = getIntegerProperty(prefix + "y", 0);
            r.width = getIntegerProperty(prefix + "width", 0);
            r.height = getIntegerProperty(prefix + "height", 0);
        }
        return r;
    }

    public int getExtendedState(int index)
    {
        String prefix = getPrefix(index);
        return getIntegerProperty(prefix + "extendedState", 0);
    }

    public void saveSidebarState(Frame frame)
    {
        String prefix = getPrefix(frame);
        if (prefix != null) {
            if (frame.getSidebar() != null) {
                int width = frame.getSidebarSplitPane().getDividerLocation();
                if (width > 0)
                    setIntegerProperty(prefix.concat("sidebar.width"), width);
                int dividerLocation = frame.getSidebar().getDividerLocation();
                if (dividerLocation >= 0) {
                    // If < 0, sidebar has no divider.
                    setIntegerProperty(prefix.concat("sidebar.dividerLocation"),
                        dividerLocation);
                }
            }
            setBooleanProperty(prefix.concat("sidebar.show"),
                frame.getSidebar() != null);
        }
    }

    public boolean getShowSidebar(Frame frame)
    {
        int index = Editor.indexOf(frame);

        // By default, only show sidebar in primary frame.
        boolean toBeReturned = index == 0;

        String prefix = getPrefix(frame);
        if (prefix != null)
            toBeReturned = getBooleanProperty(prefix.concat("sidebar.show"),
                toBeReturned);
        return toBeReturned;
    }

    public int getSidebarWidth(Frame frame)
    {
        int toBeReturned = 150;
        String prefix = getPrefix(frame);
        if (prefix != null)
            toBeReturned = getIntegerProperty(prefix.concat("sidebar.width"),
                toBeReturned);
        return toBeReturned;
    }

    public int getSidebarDividerLocation(Frame frame)
    {
        int toBeReturned = 200;
        String prefix = getPrefix(frame);
        if (prefix != null)
            toBeReturned = getIntegerProperty(prefix.concat("sidebar.dividerLocation"),
                toBeReturned);
        return toBeReturned;
    }

    public boolean getShowToolbar(Frame frame)
    {
        String prefix = getPrefix(frame);
        if (prefix != null)
            return getBooleanProperty(prefix.concat("toolbar.show"), true);
        return true;
    }

    public void setShowToolbar(Frame frame, boolean show)
    {
        String prefix = getPrefix(frame);
        if (prefix != null)
            setBooleanProperty(prefix.concat("toolbar.show"), show);
    }

    public int getIntegerProperty(String key, int defaultValue)
    {
        try {
            String s = properties.getProperty(key);
            if (s != null)
                return Integer.parseInt(s);
        }
        catch (NumberFormatException e) {}
        return defaultValue;
    }

    public void setBooleanProperty(String key, boolean value)
    {
        properties.put(key, value ? "true" : "false");
    }

    public boolean getBooleanProperty(String key, boolean defaultValue)
    {
        String s = properties.getProperty(key);
        if (s != null) {
            if (s.equals("true"))
                return true;
            if (s.equals("false"))
                return false;
        }
        return defaultValue;
    }

    public void setIntegerProperty(String key, int value)
    {
        properties.put(key, String.valueOf(value));
    }

    public float getFloatProperty(String key, float defaultValue)
    {
        try {
            String s = properties.getProperty(key);
            if (s != null)
                return Float.parseFloat(s);
        }
        catch (NumberFormatException e) {}
        return defaultValue;
    }

    public void setFloatProperty(String key, float value)
    {
        properties.put(key, String.valueOf(value));
    }

    public String getStringProperty(String key, String defaultValue)
    {
        String s = properties.getProperty(key);
        if (s != null)
            return s;
        return defaultValue;
    }

    public void setStringProperty(String key, String value)
    {
        if (value != null)
            properties.put(key, value);
        else
            properties.remove(key);
    }

    public void save()
    {
        try {
            File tempFile = Utilities.getTempFile();
            OutputStream out = tempFile.getOutputStream();
            properties.store(out, null);
            out.flush();
            out.close();
            if (file.exists() && !Utilities.deleteRename(file, backupFile)) {
                Log.error("SessionProperties.save deleteRename failed");
                Log.error("source = " + file);
                Log.error("destination = " + backupFile);
            }
            if (!Utilities.deleteRename(tempFile, file)) {
                Log.error("SessionProperties.save deleteRename failed");
                Log.error("source = " + tempFile);
                Log.error("destination = " + file);
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private String getPrefix(Frame frame)
    {
        return getPrefix(Editor.indexOf(frame));
    }

    private String getPrefix(int index)
    {
        if (index < 0) {
            // Should never happen.
            Debug.assertTrue(false);
            return null;
        }
        return "frame." + index + ".";
    }
}
