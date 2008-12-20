/*
 * JdbConstants.java
 *
 * Copyright (C) 2003 Peter Graves
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

package org.armedbear.j.jdb;

public interface JdbConstants
{
    int JDB_BREAK       =  1;
    int JDB_CATCH       =  2;
    int JDB_CLEAR       =  3;
    int JDB_CONTINUE    =  4;
    int JDB_FINISH      =  5;
    int JDB_LOCALS      =  6;
    int JDB_NEXT        =  7;
    int JDB_PRINT       =  8;
    int JDB_QUIT        =  9;
    int JDB_RESTART     = 10;
    int JDB_STDIN       = 11;
    int JDB_STEP        = 12;
    int JDB_SUSPEND     = 13;
    int JDB_TBREAK      = 14;

    int CATCH_NONE      =  0;
    int CATCH_UNCAUGHT  =  1;
    int CATCH_ALL       =  2;
}
