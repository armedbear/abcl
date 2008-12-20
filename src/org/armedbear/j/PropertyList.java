/*
 * PropertyList.java
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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

public final class PropertyList
{
    private HashMap map;

    public PropertyList()
    {
    }

    public Object getProperty(Property property)
    {
        if (map != null)
            return map.get(property);
        return null;
    }

    public void setProperty(Property property, Object value)
    {
        if (map == null)
            map = new HashMap();
        map.put(property, value);
    }

    public void setProperty(Property property, boolean value)
    {
        setProperty(property, value ? Boolean.TRUE : Boolean.FALSE);
    }

    public void setProperty(Property property, int value)
    {
        setProperty(property, new Integer(value));
    }

    public boolean setPropertyFromString(Property property, String value)
    {
        if (property.isBooleanProperty()) {
            if (value.equals("true") || value.equals("1")) {
                setProperty(property, true);
                return true;
            }
            if (value.equals("false") || value.equals("0")) {
                setProperty(property, false);
                return true;
            }
            return false; // Invalid boolean value.
        }
        if (property.isIntegerProperty()) {
            try {
                setProperty(property, Integer.parseInt(value));
                return true;
            }
            catch (NumberFormatException e) {
                return false; // Invalid integer value.
            }
        }
        setProperty(property, value);
        return true;
    }

    public boolean removeProperty(Property property)
    {
        return map.remove(property) != null;
    }

    public boolean getBooleanProperty(Property property)
    {
        Object value = getProperty(property);
        if (!(value instanceof Boolean))
            value = property.getDefaultValue();
        return ((Boolean)value).booleanValue();
    }

    public int getIntegerProperty(Property property)
    {
        Object value = getProperty(property);
        if (!(value instanceof Integer))
            value = property.getDefaultValue();
        return ((Integer)value).intValue();
    }

    public String getStringProperty(Property property)
    {
        Object value = getProperty(property);
        if (!(value instanceof String))
            value = property.getDefaultValue();
        return (String) value;
    }

    public Iterator keyIterator()
    {
        if (map != null)
            return map.keySet().iterator();
        return null;
    }

    public Set keySet()
    {
        if (map != null)
            return map.keySet();
        return null;
    }

    public int size()
    {
        return map != null ? map.size() : 0;
    }

    public void putAll(PropertyList other)
    {
        if (other.map != null && other.map.size() > 0) {
            if (map == null)
                map = new HashMap();
            map.putAll(other.map);
        }
    }
}
