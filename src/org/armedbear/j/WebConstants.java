/*
 * WebConstants.java
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

public interface WebConstants
{
    // Bit flags.
    int FORMAT_TEXT         = 0x00;
    int FORMAT_LINK         = 0x01;
    int FORMAT_WHITESPACE   = 0x02;
    int FORMAT_DISABLED     = 0x04;
    int FORMAT_HEADER_NAME  = 0x08;
    int FORMAT_HEADER_VALUE = 0x10;
    int FORMAT_ITALIC       = 0x40;
    int FORMAT_BOLD         = 0x80;
}
