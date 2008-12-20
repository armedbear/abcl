/*
 * CheckPath.java
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Stack;

public final class CheckPath implements Constants
{
    private final Editor editor;
    private final boolean showAll;
    private final Buffer buffer;
    private FastStringBuffer sb = new FastStringBuffer(16384);
    private String path;
    private File currentDirectory;
    private HashSet checkedFiles = new HashSet(256);
    private Stack stack = new Stack();
    private int depthDisplayed;

    private CheckPath(Editor editor, boolean showAll)
    {
        this.editor = editor;
        this.showAll = showAll;
        buffer = editor.getBuffer();
        path = buffer.getStringProperty(Property.INCLUDE_PATH);
        currentDirectory = editor.getCurrentDirectory();
    }

    private String getOutput()
    {
        return sb.toString();
    }

    private void run()
    {
        sb.append("File: ");
        sb.append(buffer.getFile().netPath());
        sb.append('\n');
        sb.append("Include path: ");
        sb.append(path);
        sb.append('\n');
        if (showAll)
            sb.append("Included files:\n");
        else
            sb.append("The following included files were not found:\n");
        checkBuffer(buffer, 0);
    }

    private void checkBuffer(Buffer b, int depth)
    {
        for (Line line = b.getFirstLine(); line != null; line = line.next()) {
            String s = Utilities.extractInclude(line.getText());
            if (s != null) {
                int result = checkFile(s, depth);
                if (showAll) {
                    if (result == NOT_FOUND)
                        sb.append("  NOT FOUND");
                    else if (result == ALREADY_LISTED)
                        sb.append("  (Already listed)");
                    if (sb.length() == 0 || sb.charAt(sb.length()-1) != '\n')
                        sb.append('\n');
                } else if (result == NOT_FOUND) {
                    sb.append(s);
                    sb.append("  NOT FOUND\n");
                }
            }
        }
    }

    private static final int NOT_FOUND      = 0;
    private static final int FOUND          = 1;
    private static final int ALREADY_LISTED = 2;

    private int checkFile(final String s, final int depth)
    {
        if (showAll) {
            sb.append(spaces(depth));
            sb.append(s);
        } else if (depth < depthDisplayed)
            depthDisplayed = depth;
        File file = Utilities.findInclude(s, path, currentDirectory);
        if (file == null)
            return NOT_FOUND;
        if (checkedFiles.contains(file))
            return ALREADY_LISTED;
        checkedFiles.add(file);
        int count = 0;
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(file.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                String name = Utilities.extractInclude(line);
                if (name != null) {
                    if (showAll && count == 0) {
                        sb.append('\n');
                        sb.append(spaces(depth));
                        sb.append(getDisplayName(file));
                        sb.append(" -->\n");
                    }
                    // Recurse!
                    stack.push(file);
                    int result = checkFile(name, depth+1);
                    if (showAll) {
                        if (result == NOT_FOUND)
                            sb.append("  NOT FOUND");
                        else if (result == ALREADY_LISTED)
                            sb.append("  (Already listed)");
                        if (sb.length() == 0 || sb.charAt(sb.length()-1) != '\n')
                            sb.append('\n');
                    } else if (result == NOT_FOUND) {
                        while (depthDisplayed < stack.size()) {
                            sb.append(spaces(depthDisplayed));
                            sb.append(getDisplayName((File)stack.get(depthDisplayed)));
                            sb.append(" -->\n");
                            ++depthDisplayed;
                        }
                        sb.append(spaces(depth+1));
                        sb.append(name);
                        sb.append("  NOT FOUND\n");
                    }
                    stack.pop();
                    ++count;
                }
            }
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
        return FOUND;
    }

    private String getDisplayName(File file)
    {
        File dir = file.getParentFile();
        if (dir.equals(currentDirectory))
            return file.getName();
        return file.canonicalPath();
    }

    private static String spaces(int depth)
    {
        return Utilities.spaces(depth*2);
    }

    public static void checkPath()
    {
        checkPathInternal(false);
    }

    public static void listIncludes()
    {
        checkPathInternal(true);
    }

    private static void checkPathInternal(boolean showAll)
    {
        final Editor editor = Editor.currentEditor();
        final int modeId = editor.getModeId();
        if (modeId != C_MODE && modeId != CPP_MODE)
            return;
        editor.setWaitCursor();
        CheckPath cp = new CheckPath(editor, showAll);
        cp.run();
        editor.setDefaultCursor();
        Buffer buf = OutputBuffer.getOutputBuffer(cp.getOutput());
        buf.setFormatter(new CheckPathFormatter(buf));
        FastStringBuffer sb = new FastStringBuffer();
        if (showAll)
            sb.append("listIncludes ");
        else
            sb.append("checkPath");
        sb.append(editor.getBuffer().getFile().getName());
        buf.setTitle(sb.toString());
        editor.makeNext(buf);
        editor.activateInOtherWindow(buf);
    }
}
