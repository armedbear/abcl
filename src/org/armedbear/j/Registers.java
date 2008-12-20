/*
 * Registers.java
 *
 * Copyright (C) 2002-2006 Peter Graves
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
import java.io.OutputStreamWriter;

public final class Registers
{
    public static final void saveToRegister()
    {
        saveToRegister(null);
    }

    public static final void saveToRegister(String name)
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDot() == null)
            return;
        if (editor.getMark() == null) {
            MessageDialog.showMessageDialog(editor, "No region selected",
                "Error");
            return;
        }
        if (editor.isColumnSelection()) {
            editor.notSupportedForColumnSelections();
            return;
        }
        if (name == null) {
            name = getName(editor, "Save To Register");
            if (name == null)
                return;
        }
        if (!validateName(name))
            return;
        Region r = new Region(editor);
        String text = r.toString();
        File file =
            File.getInstance(Directories.getRegistersDirectory(), name);
        if (file == null)
            return; // Shouldn't happen.
        try {
            OutputStreamWriter writer =
                new OutputStreamWriter(file.getOutputStream());
            writer.write(text);
            writer.flush();
            writer.close();
            ListRegistersBuffer buf = findListRegistersBuffer();
            if (buf != null)
                buf.reload();
            editor.status("Region saved to register ".concat(name));
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public static final void insertRegister()
    {
        insertRegister(null);
    }

    public static final void insertRegister(String name)
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        if (editor.getDot() == null)
            return;
        if (name == null) {
            name = getName(editor, "Insert Register");
            if (name == null)
                return;
        }
        String text = getText(name);
        if (text != null)
            editor.paste(text);
    }

    public static final void editRegister()
    {
        editRegister(null);
    }

    public static final void editRegister(String name)
    {
        final Editor editor = Editor.currentEditor();
        if (name == null) {
            name = getName(editor, "Edit Register");
            if (name == null)
                return;
        }
        File file =
            File.getInstance(Directories.getRegistersDirectory(), name);
        if (file != null && file.isFile()) {
            Buffer buf = Editor.getBuffer(file);
            if (buf != null) {
                Editor ed = editor.getOtherEditor();
                if (ed != null)
                    ed.makeNext(buf);
                else
                    editor.makeNext(buf);
                editor.activateInOtherWindow(buf);
            }
        }
    }

    public static final void clearRegister()
    {
        clearRegister(null);
    }

    public static final void clearRegister(String name)
    {
        final Editor editor = Editor.currentEditor();
        if (name == null) {
            name = getName(editor, "Clear Register");
            if (name == null)
                return;
        }
        File file =
            File.getInstance(Directories.getRegistersDirectory(), name);
        if (file.isFile()) {
            file.delete();
            ListRegistersBuffer buf = findListRegistersBuffer();
            if (buf != null)
                buf.reload();
        }
    }

    public static final void listRegisters()
    {
        Buffer buf = findListRegistersBuffer();
        if (buf == null)
            buf = new ListRegistersBuffer();
        final Editor editor = Editor.currentEditor();
        if (editor.getBuffer() == buf)
            return;
        Editor other = editor.getOtherEditor();
        if (other != null)
            other.makeNext(buf);
        else
            editor.makeNext(buf);
        editor.activateInOtherWindow(buf);
    }

    public static final ListRegistersBuffer findListRegistersBuffer()
    {
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof ListRegistersBuffer)
                return (ListRegistersBuffer) buf;
        }
        return null;
    }

    private static final String getName(Editor editor, String title)
    {
        SelectRegisterDialog d = new SelectRegisterDialog(editor, "Register:", title, null);
        editor.centerDialog(d);
        d.show();
        return d.getInput();
    }

    private static final boolean validateName(String name)
    {
        boolean lenOk = false;
        boolean charsOk = true;
        int len = name.length();
        // Keep the names of the registers between 1 and 25 chars
        if (len > 0 && len <= 25) {
            lenOk = true;
            for (int x = 0; x < len; x++) {
                char c = name.charAt(x);
                // Allow alphanumeric chars plus "-" and "_" only, with no spaces.
                if (!Character.isLetterOrDigit(c) &&
                    c != '-' && c != '_' ) {
                    charsOk = false;
                    break;
                }
            }
        }
        if (lenOk && charsOk)
            return true;
        FastStringBuffer sb = new FastStringBuffer('"');
        sb.append(name);
        sb.append("\" is not a valid register name. ");
        sb.append("Register names must be between 1 and 25 characters long ");
        sb.append("and contain only the characters \"a-z\", \"A-Z\", \"0-9\", \"-\", or \"_\".");
        String message = sb.toString();
        if (message.length() > 65)
            message = Utilities.wrap(message, 65, 8);
        MessageDialog.showMessageDialog(message, "Error");
        return false;
    }

    /*package*/ static final String getText(String name)
    {
        return getText(name, 0);
    }

    // If maxLines > 0, return at most maxLines lines of text.
    /*package*/ static final String getText(String name, int maxLines)
    {
        File file =
            File.getInstance(Directories.getRegistersDirectory(), name);
        if (!file.isFile())
            return null;
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                file.getInputStream()));
            FastStringBuffer sb = new FastStringBuffer();
            int lineCount = 0;
            int c;
            while ((c = reader.read()) > 0) {
                sb.append((char)c);
                if (c == '\n')
                    ++lineCount;
                if (maxLines > 0 && lineCount == maxLines)
                    break;
            }
            reader.close();
            return sb.toString();
        }
        catch (IOException e) {
            Log.error(e);
            return null;
        }
    }
}

