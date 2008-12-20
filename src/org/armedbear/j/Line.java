/*
 * Line.java
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

import java.io.UnsupportedEncodingException;

public interface Line
{
    Line previous();

    void setPrevious(Line line);

    Line next();

    void setNext(Line line);

    void insertAfter(Line line);

    String getText();

    void setText(String s);

    String getOriginalText();

    void setOriginalText(String s);

    boolean isModified();

    boolean isNew();

    void setNew(boolean b);

    boolean isSaved();

    void setSaved(boolean b);

    void unmodified();

    int lineNumber();

    void setLineNumber(int n);

    int originalLineNumber();

    void setOriginalLineNumber(int n);

    int getHeight();

    int getWidth();

    int flags();

    void setFlags(int flags);

    char charAt(int i);

    String substring(int beginIndex);

    String substring(int beginIndex, int endIndex);

    String trim();

    int length();

    byte[] getBytes(String encoding) throws UnsupportedEncodingException;

    boolean isBlank();

    int getIndentation();

    boolean isHidden();

    void hide();

    void unhide();

    void show();

    int getHidden();

    void setHidden(int hidden);

    Line previousVisible();

    Line nextVisible();

    boolean isBefore(Line line);

    Line copy();

    void copy(Line line);

    Annotation getAnnotation();

    void setAnnotation(Annotation annotation);
}
