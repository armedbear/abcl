/*
 * FormatTable.java
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

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;

public final class FormatTable
{
    private static final Preferences preferences = Editor.preferences();

    private String modeName;
    private ArrayList list;
    private FormatTableEntry[] array;
    private boolean initialized;

    public FormatTable(String modeName)
    {
        this.modeName = modeName;
        list = new ArrayList();
    }

    public synchronized final void setModeName(String s)
    {
        modeName = s;
    }

    public synchronized FormatTableEntry lookup(int format)
    {
        if (array != null) {
            try {
                return array[format];
            }
            catch (ArrayIndexOutOfBoundsException e) {
                Log.error(e);
                // Fall through...
            }
        }
        if (!initialized) {
            initialized = true;
            boolean ok = true;
            int largest = -1;
            for (int i = list.size()-1; i >= 0; i--) {
                final FormatTableEntry entry = (FormatTableEntry) list.get(i);
                final int f = entry.getFormat();
                if (f < 0) {
                    ok = false;
                    break;
                }
                if (f > largest)
                    largest = f;
            }
            if (ok && largest < 128) {
                array = new FormatTableEntry[largest+1];
                for (int i = list.size()-1; i >= 0; i--) {
                    FormatTableEntry entry = (FormatTableEntry) list.get(i);
                    array[entry.getFormat()] = entry;
                }
                list = null; // We don't need it any more.
                try {
                    return array[format];
                }
                catch (ArrayIndexOutOfBoundsException e) {
                    Log.error(e);
                    return null;
                }
            } else
                Debug.bug("FormatTableEntry.lookup unable to build array");
        }
        if (list != null) {
            for (int i = list.size()-1; i >= 0; i--) {
                FormatTableEntry entry = (FormatTableEntry) list.get(i);
                if (entry.getFormat() == format)
                    return entry;
            }
        }
        return null;
    }

    public synchronized void addEntryFromPrefs(int format, String thing)
    {
        addEntryFromPrefs(format, thing, null);
    }

    public synchronized void addEntryFromPrefs(int format, String thing, String fallback)
    {
        // Color.
        Color color = null;
        String key;
        if (modeName != null) {
            // "JavaMode.color.comment"
            key = modeName + ".color." + thing;
            color = preferences.getColorProperty(key);
        }
        if (color == null) {
            // "color.comment"
            key = "color." + thing;
            color = preferences.getColorProperty(key);
        }
        // Use fallback if there's no entry for thing.
        if (color == null && fallback != null) {
            if (modeName != null) {
                key = modeName + ".color." + fallback;
                color = preferences.getColorProperty(key);
            }
            if (color == null) {
                key = "color." + fallback;
                color = preferences.getColorProperty(key);
            }
        }
        if (color == null) {
            color = DefaultTheme.getColor(modeName, thing);
            if (color == null && fallback != null) {
                color = DefaultTheme.getColor(modeName, fallback);
                if (color == null)
                    color = DefaultTheme.getColor("text");
            }
        }

        // Style.
        int style = -1;
        String value = null;
        if (modeName != null) {
            // "JavaMode.style.comment"
            key = modeName + ".style." + thing;
            value = preferences.getStringProperty(key);
        }
        if (value == null) {
            // "style.comment"
            key = "style." + thing;
            value = preferences.getStringProperty(key);
        }
        // Use fallback if there's no entry for thing.
        if (value == null && fallback != null) {
            if (modeName != null) {
                key = modeName + ".style." + fallback;
                value = preferences.getStringProperty(key);
            }
            if (value == null) {
                key = "style." + fallback;
                value = preferences.getStringProperty(key);
            }
        }
        if (value != null) {
            try {
                style = Integer.parseInt(value);
            }
            catch (NumberFormatException e) {}
        }
        if (style != Font.PLAIN && style != Font.BOLD && style != Font.ITALIC) {
            style = DefaultTheme.getStyle(modeName, thing);
            if (style < 0) {
                if (fallback != null)
                    style = DefaultTheme.getStyle(modeName, fallback);
                if (style < 0)
                    style = Font.PLAIN;
            }
        }
        addEntry(format, thing, color, style);
    }

    // Only called from synchronized methods.
    private void addEntry(int format, String name, Color color, int style)
    {
        FormatTableEntry entry = new FormatTableEntry(format, color, style);
        int index = indexOf(format);
        if (index >= 0)
            list.set(index, entry);
        else
            list.add(entry);
    }

    // Only called from synchronized methods.
    private int indexOf(int format)
    {
        for (int i = 0; i < list.size(); i++) {
            FormatTableEntry entry = (FormatTableEntry) list.get(i);
            if (entry.getFormat() == format)
                return i;
        }
        return -1;
    }
}
