/*
 * JDKHelp.java
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

import java.awt.AWTEvent;
import java.awt.event.MouseEvent;
import java.util.List;

public final class JDKHelp implements Constants
{
    public static void jdkHelp()
    {
        final String className = getClassNameInCurrentEditor();
        if (className != null && className.length() > 0)
            jdkHelp(className);
    }

    public static void jdkHelp(final String className)
    {
        if (className == null || className.length() == 0)
            return;
        final String jdkDocPath =
            Editor.preferences().getStringProperty(Property.JDK_DOC_PATH);
        if (jdkDocPath == null)
            return;
        final List dirnames = getDirectoriesInPath(jdkDocPath);
        if (dirnames == null)
            return;
        final Editor editor = Editor.currentEditor();
        final Frame frame = editor.getFrame();
        final Buffer buffer = editor.getBuffer();
        frame.setWaitCursor();
        final String[] imports = JavaSource.getImports(buffer);
        final int size = dirnames.size();
        for (int i = 0; i < size; i++) {
            final String dirname = (String) dirnames.get(i);
            final File dir = File.getInstance(dirname);
            File file = JavaSource.findImport(className, imports, dir,
                ".html");
            if (file == null || !file.isFile()) {
                // Not found. Look in "api" subdirectory if it exists.
                final File apiDir = File.getInstance(dir, "api");
                if (apiDir != null && apiDir.isDirectory()) {
                    file = JavaSource.findImport(className, imports, apiDir,
                        ".html");
                }
            }
            if (file != null && file.isFile()) {
                Buffer buf = null;
                // Look for existing buffer.
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer b = it.nextBuffer();
                    if (b instanceof WebBuffer && b.getFile().equals(file)) {
                        buf = b;
                        break;
                    }
                }
                if (buf == null) {
                    buf = WebBuffer.createWebBuffer(file, null, null);
                    buf.setTransient(true);
                }
                if (editor.getBuffer() != buf) {
                    editor.makeNext(buf);
                    editor.activateInOtherWindow(buf);
                }
                frame.setDefaultCursor();
                return;
            }
        }
        frame.setDefaultCursor();
        MessageDialog.showMessageDialog(editor,
            "No help available for ".concat(className), "JDK Help");
    }

    public static void source()
    {
        final String className = getClassNameInCurrentEditor();
        if (className != null && className.length() > 0)
            source(className);
    }

    public static void source(final String className)
    {
        if (className == null || className.length() == 0)
            return;
        final Editor editor = Editor.currentEditor();
        final Frame frame = editor.getFrame();
        frame.setWaitCursor();
        File file = JavaSource.findSource(editor.getBuffer(), className, false);
        if (file != null) {
            Buffer buf = null;
            // Look for existing buffer.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer b = it.nextBuffer();
                if (file.equals(b.getFile())) {
                    buf = b;
                    break;
                }
            }
            if (buf == null)
                buf = Buffer.createBuffer(file);
            if (editor.getBuffer() != buf) {
                editor.makeNext(buf);
                editor.activate(buf);
            }
            frame.setDefaultCursor();
        } else {
            frame.setDefaultCursor();
            MessageDialog.showMessageDialog(editor,
                "No source available for ".concat(className), "Source");
        }
    }

    private static List getDirectoriesInPath(String path)
    {
        final List dirNames = Utilities.getDirectoriesInPath(path);
        final int size = dirNames.size();
        if (size == 0) {
            MessageDialog.showMessageDialog("Empty path", "Error");
            return null;
        }
        for (int i = 0; i < size; i++) {
            final String dirName = (String) dirNames.get(i);
            final File dir = File.getInstance(dirName);
            if (!dir.isDirectory()) {
                FastStringBuffer sb = new FastStringBuffer("Directory \"");
                sb.append(dir.canonicalPath());
                sb.append("\" does not exist");
                MessageDialog.showMessageDialog(sb.toString(), "Error");
            }
        }
        return dirNames;
    }

    private static String getClassNameInCurrentEditor()
    {
        final Editor editor = Editor.currentEditor();
        AWTEvent e = editor.getDispatcher().getLastEvent();
        if (e instanceof MouseEvent)
            editor.mouseMoveDotToPoint((MouseEvent)e);
        String className = editor.getSelectionOnCurrentLine();
        if (className == null || className.length() == 0)
            className = getClassNameAtPosition(editor.getDot());
        return className;
    }

    // Supports both simple names ("String") and canonical names
    // ("java.lang.String").
    private static String getClassNameAtPosition(Position pos)
    {
        if (pos == null)
            return null;
        final Line line = pos.getLine();
        final int limit = line.length();
        int offset = pos.getOffset();
        if (offset >= limit)
            return null;
        char c = line.charAt(offset);
        if (!Character.isJavaIdentifierPart(c) && c != '.')
            return null;
        // Go left until we encounter a character that can't be part of the name.
        while (offset > 0) {
            c = line.charAt(--offset);
            if (!Character.isJavaIdentifierPart(c) && c != '.')
                break;
        }
        // Go right to find start of name.
        while (offset < limit && !Character.isJavaIdentifierStart(line.charAt(offset)))
            ++offset;
        if (offset == limit)
            return null; // Nothing left.
        // Now we're looking at the first char of the name.
        Debug.assertTrue(Character.isJavaIdentifierStart(line.charAt(offset)));
        FastStringBuffer sb = new FastStringBuffer();
        sb.append(line.charAt(offset));
        while (++offset < limit) {
            c = line.charAt(offset);
            if (Character.isJavaIdentifierPart(c) || c == '.')
                sb.append(c);
            else
                break; // Reached end.
        }
        return sb.toString();
    }
}
