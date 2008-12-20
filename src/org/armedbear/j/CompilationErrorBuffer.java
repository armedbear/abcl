/*
 * CompilationErrorBuffer.java
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

package org.armedbear.j;

import javax.swing.Icon;

public abstract class CompilationErrorBuffer extends Buffer
{
    private CompilationError currentError;

    protected CompilationErrorBuffer()
    {
        supportsUndo  = false;
        mode = PlainTextMode.getMode();
        formatter = new PlainTextFormatter(this);
        lineSeparator = System.getProperty("line.separator");
        readOnly = true;
        setTransient(true);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
        setInitialized(true);
    }

    public int load()
    {
        if (!isLoaded()) {
            if (getFirstLine() == null) {
                try {
                    lockWrite();
                }
                catch (InterruptedException e) {
                    Log.debug(e);
                    return LOAD_FAILED; // Shouldn't happen.
                }
                try {
                    appendLine("");
                    renumber();
                }
                finally {
                    unlockWrite();
                }
            }
            setLoaded(true);
        }
        return LOAD_COMPLETED;
    }

    public CompilationError getCurrentError()
    {
        return currentError;
    }

    public void setCurrentError(CompilationError ce)
    {
        currentError = ce;
    }

    protected CompilationError nextError()
    {
        Line line;
        if (currentError != null) {
            line = currentError.getErrorLine();
            if (line != null)
                line = line.next();
        } else
            line = getFirstLine();
        while (line != null) {
            CompilationError ce =
                CompilationError.parseLineAsErrorMessage(line);
            if (ce != null) {
                currentError = ce;
                return ce;
            }
            line = line.next();
        }
        return null;
    }

    protected CompilationError previousError()
    {
        Line line;
        if (currentError != null) {
            line = currentError.getErrorLine();
            if (line != null)
                line = line.previous();
        } else
            line = getLastLine();
        while (line != null) {
            CompilationError ce =
                CompilationError.parseLineAsErrorMessage(line);
            if (ce != null) {
                currentError = ce;
                return ce;
            }
            line = line.previous();
        }
        return null;
    }

    public String getMessage()
    {
        if (currentError != null) {
            String message = currentError.getMessage();
            if (message != null)
                return message;
            // Message on following line.
            Line line = currentError.getErrorLine();
            if (line != null && line.next() != null)
                return line.next().trim();
        }
        return null;
    }

    public boolean isModified()
    {
        return false;
    }

    // For the buffer list.
    public Icon getIcon()
    {
        return Utilities.getIconFromFile("jpty.png");
    }
}
