/*
 * Tagger.java
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

import java.io.IOException;
import java.io.Writer;
import java.util.List;

public abstract class Tagger implements Constants, Runnable
{
    public static final char separatorChar = 0;

    protected SystemBuffer buffer;

    protected Tagger(SystemBuffer buffer)
    {
        this.buffer = buffer;
    }

    public void writeTags(Writer writer)
    {
        if (buffer == null)
            return;
        List tags = buffer.getTags();
        if (tags == null)
            return;
        File file = buffer.getFile();
        if (file == null)
            return;
        final String canonicalPath = file.canonicalPath();
        try {
            for (int i = 0, limit = tags.size(); i < limit; i++) {
                LocalTag localTag = (LocalTag) tags.get(i);
                if (localTag != null) {
                    switch (localTag.getType()) {
                        case TAG_INTERFACE:
                        case TAG_CLASS:
                        case TAG_METHOD:
                        case TAG_EXPLICIT:
                        case TAG_DEFUN: // Lisp.
                        case TAG_GENERIC_FUNCTION: // Lisp.
                        case TAG_MACRO: // Lisp.
                        case TAG_STRUCT: // Lisp.
                            writer.write(localTag.getName());
                            writer.write(separatorChar);
                            writer.write(canonicalPath);
                            writer.write(separatorChar);
                            writer.write(localTag.getLine().getText());
                            final String canonicalSignature =
                                localTag.getCanonicalSignature();
                            if (canonicalSignature != null) {
                                writer.write(separatorChar);
                                writer.write(canonicalSignature);
                            }
                            writer.write('\n');
                            break;
                        default:
                            break;
                    }
                }
            }
            writer.flush();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public abstract void run();
}
