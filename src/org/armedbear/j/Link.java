/*
 * Link.java
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

/**
 * Link is an abstraction of an anchor tag in a web page, with
 * <code>target</code> as the <code>href<code> target.
 */
public class Link
{
    private final String target;

    /**
     * Creates a link that points to the given target.
     *
     * @param target The target for this Link.
     */
    public Link(String target)
    {
        this.target = target;
    }

    /**
     * Returns the target that this Link was constructed with.
     *
     * @return The target that this Link was constructed with.
     */
    public final String getTarget()
    {
        return target;
    }
}
