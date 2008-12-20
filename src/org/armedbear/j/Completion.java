/*
 * Completion.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.util.List;
import java.util.ArrayList;

public final class Completion
{
    private String input;
    private String toBeCompleted;
    private boolean ignoreCase;
    private boolean cygnify;

    private ArrayList list = new ArrayList();

    public Completion(File dir, String input, String shellCommand)
    {
        if (Platform.isPlatformWindows()) {
            if (shellCommand != null && shellCommand.toLowerCase().indexOf("cmd.exe") < 0)
                cygnify = true;
        }
        final char separatorChar = getSeparatorChar();
        this.input = input;
        toBeCompleted = input;
        if (Platform.isPlatformWindows()) {
            if (cygnify) {
                if (toBeCompleted.startsWith("~/")) {
                    String home = Utilities.getUserHome();
                    if (!home.startsWith("/"))
                        home = Utilities.cygnify(home);
                    toBeCompleted = Utilities.uncygnify(home + toBeCompleted.substring(1));
                } else if (toBeCompleted.startsWith("../"))
                    toBeCompleted = File.normalize(toBeCompleted);
                else
                    toBeCompleted = Utilities.uncygnify(toBeCompleted);
            } else {
                if (toBeCompleted.startsWith("~/")) {
                    String home = Utilities.getUserHome();
                    toBeCompleted = home + toBeCompleted.substring(1);
                } else
                    toBeCompleted = File.normalize(toBeCompleted);
            }
            ignoreCase = true;
        }
        FilenameCompletion c = new FilenameCompletion(dir, toBeCompleted, null,
            ignoreCase);
        List files = c.listFiles();
        if (files != null) {
            String home = Utilities.getUserHome() + "/";
            String prefix = dir.canonicalPath() + LocalFile.getSeparator();
            int skip = prefix.length();
            String toBeAdded;
            int limit = files.size();
            for (int i = 0; i < limit; i++) {
                File file = (File) files.get(i);
                toBeAdded = file.getAbsolutePath();
                if (!Utilities.isFilenameAbsolute(toBeCompleted)) {
                    if (toBeAdded.startsWith(prefix)) {
                        toBeAdded = toBeAdded.substring(skip);
                        if (input.startsWith("./"))
                            toBeAdded = "./" + toBeAdded;
                    }
                }
                if (cygnify)
                    toBeAdded = Utilities.cygnify(toBeAdded);
                if (input.startsWith("~/")) {
                    if (toBeAdded.startsWith(home)) {
                        toBeAdded = "~/" + toBeAdded.substring(home.length());
                    }
                } else if (input.startsWith("..")) {
                    String remaining = input;
                    File parentDir = dir;
                    String parentPrefix = "";
                    while (remaining.startsWith("../")) {
                        parentDir = parentDir.getParentFile();
                        parentPrefix += "../";
                        remaining = remaining.substring(3);
                    }
                    String parentDirName = parentDir.canonicalPath();
                    if (cygnify)
                        parentDirName = Utilities.cygnify(parentDirName);
                    if (!parentDirName.endsWith("/"))
                        parentDirName += "/";
                    if (toBeAdded.startsWith(parentDirName))
                        toBeAdded = parentPrefix + toBeAdded.substring(parentDirName.length());
                }
                toBeAdded = escapeSpaces(toBeAdded);
                if (file.isDirectory())
                    toBeAdded += separatorChar;
                list.add(toBeAdded);
            }
        }
    }

    // Converts "this is a test" into "this\ is\ a\ test".
    private static final String escapeSpaces(String s)
    {
        final int length = s.length();
        FastStringBuffer sb = new FastStringBuffer(length * 2);
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            if (c == ' ')
                sb.append('\\');
            sb.append(c);
        }
        return sb.toString();
    }

    private final char getSeparatorChar()
    {
        if (Platform.isPlatformWindows() && !cygnify)
            return '\\';
        return '/';
    }

    public final List getCompletions()
    {
        return list;
    }

    private boolean isUnique()
    {
        return list.size() == 1;
    }

    private String getLongestCommonPrefix()
    {
        String s = input;
        if (list.size() != 0) {
            if (list.size() == 1) {
                s = (String) list.get(0);
            } else {
                String first = (String) list.get(0);
                int length = toBeCompleted.length() + 1;
                while (true) {
                    if (length > first.length())
                        return s;
                    String maybe = first.substring(0, length);
                    for (int i = 1; i < list.size(); i++) {
                        String toBeChecked = (String) list.get(i);
                        if (!maybe.regionMatches(ignoreCase, 0, toBeChecked, 0, length)) {
                            if (cygnify)
                                s = Utilities.cygnify(s);
                            return s;
                        }
                    }
                    s = maybe;
                    ++length;
                }
            }
        }
        if (cygnify)
            s = Utilities.cygnify(s);
        return s;
    }

    public String toString()
    {
        if (isUnique()) {
            String s = (String) list.get(0);
            // Directories have file separator already appended.
            if (!s.endsWith("/") && !s.endsWith("\\"))
                s += ' ';
            return s;
        }
        return getLongestCommonPrefix();
    }
}
