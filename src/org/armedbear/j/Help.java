/*
 * Help.java
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collections;
import java.util.List;
import org.armedbear.lisp.Closure;
import org.armedbear.lisp.LispObject;

public final class Help
{
    public static final void help()
    {
        help(null);
    }

    public static void help(String arg)
    {
        final Editor editor = Editor.currentEditor();
        final Frame frame = editor.getFrame();
        final File dir = getDocumentationDirectory();
        if (dir == null)
            return;
        frame.setWaitCursor();
        try {
            String fileName = "contents.html";
            String ref = null;
            if (arg == null || arg.length() == 0)
                ;
            else if (arg.endsWith(".html")) {
                File file = File.getInstance(dir, arg);
                if (file != null && file.isFile())
                    fileName = arg;
            } else {
                Command command = CommandTable.getCommand(arg);
                if (command != null) {
                    fileName = "commands.html";
                    ref = command.getName();
                } else {
                    Property property = Property.findProperty(arg);
                    if (property != null) {
                        fileName = "preferences.html";
                        ref = property.getDisplayName();
                    }
                }
            }
            File file = File.getInstance(dir, fileName);
            if (file == null || !file.isFile())
                return;
            Buffer buf = null;
            // Look for existing help buffer.
            if (isHelpBuffer(editor.getBuffer()))
                buf = editor.getBuffer();
            else {
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer b = it.nextBuffer();
                    if (isHelpBuffer(b)) {
                        buf = b;
                        break;
                    }
                }
            }
            if (buf != null) {
                // Found existing help buffer.
                // Save history.
                int offset = 0;
                if (editor.getBuffer() == buf) {
                    offset = buf.getAbsoluteOffset(editor.getDot());
                } else {
                    View view = buf.getLastView();
                    if (view != null) {
                        Position dot = view.getDot();
                        if (dot != null)
                            offset = buf.getAbsoluteOffset(dot);
                    }
                }
                ((WebBuffer)buf).saveHistory(buf.getFile(), offset,
                    ((WebBuffer)buf).getContentType());
                if (!buf.getFile().equals(file)) {
                    // Existing buffer is not looking at the right file.
                    ((WebBuffer)buf).go(file, 0, null);
                }
                Position pos = ((WebBuffer) buf).findRef(ref);
                if (editor.getBuffer() == buf) {
                    if (pos != null)
                        editor.moveDotTo(pos);
                } else {
                    editor.makeNext(buf);
                    Editor ed = editor.activateInOtherWindow(buf);
                    if (pos != null) {
                        ed.moveDotTo(pos);
                        ed.updateDisplay();
                    }
                }
            } else {
                buf = WebBuffer.createWebBuffer(file, null, ref);
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null) {
                    buf.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                    otherEditor.makeNext(buf);
                } else {
                    buf.setUnsplitOnClose(true);
                    editor.makeNext(buf);
                }
                Editor ed = editor.activateInOtherWindow(buf);
                ed.updateDisplay();
            }
        }
        finally {
            frame.setDefaultCursor();
        }
    }

    private static boolean isHelpBuffer(Buffer buffer)
    {
        if (!(buffer instanceof WebBuffer))
            return false;
        File file = buffer.getFile();
        if (file != null) {
            File dir = file.getParentFile();
            if (dir != null && dir.equals(getDocumentationDirectory()))
                return true;
        }
        return false;
    }

    public static final File getBindingsFile()
    {
        return File.getInstance(Directories.getTempDirectory(), "bindings.html");
    }

    public static void describeBindings()
    {
        final Editor editor = Editor.currentEditor();
        final Frame frame = editor.getFrame();
        frame.setWaitCursor();
        try {
            File file = getBindingsFile();
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
            writer.write("<html>\n<head>\n<title>Keyboard Bindings</title>\n</head>\n<body>\n");
            File docDir = getDocumentationDirectory();
            writer.write("<b>");
            writer.write("Local Bindings (");
            writer.write(editor.getMode().toString());
            writer.write(" mode)");
            writer.write("</b><br><br>");
            addBindingsFromKeyMap(editor.getBuffer().getKeyMapForMode(), docDir,
                                  writer, "");
            writer.write("<br>");
            writer.write("<b>");
            writer.write("Global Bindings");
            writer.write("</b><br><br>");
            addBindingsFromKeyMap(KeyMap.getGlobalKeyMap(), docDir, writer, "");
            writer.write("</body>\n</html>\n");
            writer.flush();
            writer.close();
            if (isListBindingsBuffer(editor.getBuffer())) {
                ((WebBuffer)editor.getBuffer()).go(file, 0, "text/html");
            } else {
                Buffer buf = null;
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer b = it.nextBuffer();
                    if (isListBindingsBuffer(b)) {
                        buf = b;
                        break;
                    }
                }
                if (buf != null)
                    ((WebBuffer)buf).go(file, 0, "text/html");
                else
                    buf = WebBuffer.createWebBuffer(file, null, null);
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null) {
                    buf.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                    otherEditor.makeNext(buf);
                } else {
                    buf.setUnsplitOnClose(true);
                    editor.makeNext(buf);
                }
                editor.activateInOtherWindow(buf);
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        finally {
            frame.setDefaultCursor();
        }
    }

    private static void addBindingsFromKeyMap(KeyMap keyMap, File docDir,
                                              Writer writer, String prefix)
        throws IOException
    {
        KeyMapping[] mappings = keyMap.getMappings();
        int count = mappings.length;
        if (count == 0) {
            writer.write("[None]<br>\n");
            return;
        }
        int prefixLength = prefix.length();
        final String sanitizedPrefix =
            (prefixLength > 0) ? sanitize(prefix) : null;
        final int spaces = 32 - prefixLength;
        ArrayList submappings = null;
        for (int i = 0; i < count; i++) {
            KeyMapping mapping = mappings[i];
            FastStringBuffer sb = new FastStringBuffer();
            if (sanitizedPrefix != null)
                sb.append(sanitizedPrefix);
            final String keytext = mapping.getKeyText();
            sb.append(sanitize(keytext));
            for (int j = spaces - keytext.length(); j-- > 0;)
                sb.append("&nbsp;");
            Object command = mapping.getCommand();
            if (command instanceof String) {
                String commandString = (String) command;
                if (docDir != null) {
                    sb.append("<a href=\"");
                    sb.append(docDir.canonicalPath());
                    sb.append(LocalFile.getSeparatorChar());
                    sb.append("commands.html#");
                    sb.append(commandString);
                    sb.append("\">");
                    sb.append(commandString);
                    sb.append("</a>");
                } else
                    sb.append(commandString);
            } else if (command instanceof LispObject) {
                try {
                    String s = ((LispObject)command).writeToString();
                    sb.append(sanitize(s));
                }
                catch (Throwable t) {
                    Log.debug(t);
                }
            } else if (command instanceof KeyMap) {
                sb.append(mapping.getKeyText());
                sb.append(" prefix command");
                if (submappings == null)
                    submappings = new ArrayList();
                submappings.add(mapping);
            }
            sb.append("<br>\n");
            writer.write(sb.toString());
        }
        if (submappings != null) {
            for (int i = 0; i < submappings.size(); i++) {
                writer.write("<br>\n");
                KeyMapping mapping = (KeyMapping) submappings.get(i);
                String keytext = mapping.getKeyText();
                KeyMap submap = (KeyMap) mapping.getCommand();
                FastStringBuffer sb = new FastStringBuffer();
                if (prefixLength > 0) {
                    sb.append(prefix);
                    sb.append(' ');
                }
                sb.append(keytext);
                sb.append(' ');
                addBindingsFromKeyMap(submap, docDir, writer, sb.toString());
            }
        }
    }

    private static String sanitize(String s)
    {
        FastStringBuffer sb = null;
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            switch (c) {
                case '<':
                    if (sb == null) {
                        sb = new FastStringBuffer();
                        sb.append(s.substring(0, i));
                    }
                    sb.append("&lt;");
                    break;
                case '>':
                    if (sb == null) {
                        sb = new FastStringBuffer();
                        sb.append(s.substring(0, i));
                    }
                    sb.append("&gt;");
                    break;
                case '&':
                    if (sb == null) {
                        sb = new FastStringBuffer();
                        sb.append(s.substring(0, i));
                    }
                    sb.append("&amp;");
                    break;
                default:
                    if (sb != null)
                        sb.append(c);
                    break;
            }
        }
        return (sb != null) ? sb.toString() : s;
    }

    private static boolean isListBindingsBuffer(Buffer buffer)
    {
        if (!(buffer instanceof WebBuffer))
            return false;
        return buffer.getFile().equals(getBindingsFile());
    }

    public static void apropos()
    {
        final Editor editor = Editor.currentEditor();
        InputDialog d = new InputDialog(editor, "Apropos:", "Apropos", null);
        d.setHistory(new History("apropos"));
        editor.centerDialog(d);
        d.show();
        String arg = d.getInput();
        if (arg == null)
            return;
        arg = arg.trim();
        if (arg.length() == 0)
            return;
        apropos(arg);
    }

    public static void apropos(String arg)
    {
        final File dir = getDocumentationDirectory();
        if (dir == null)
            return;
        final Editor editor = Editor.currentEditor();
        final Frame frame = editor.getFrame();
        frame.setWaitCursor();
        try {
            File file = getAproposFile();
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
            writer.write("<html>\n<head>\n<title>");
            writer.write("Apropos ");
            writer.write('"');
            writer.write(arg);
            writer.write('"');
            writer.write("</title>\n</head>\n<body>\n");
            writer.write("<b>");
            writer.write("Commands");
            writer.write("</b><br><br>");
            File helpFile = File.getInstance(dir, "commands.html");
            if (helpFile != null && !helpFile.isFile())
                helpFile = null;
            List commands = CommandTable.apropos(arg);
            sort(commands);
            addAproposEntries(commands, helpFile, writer);
            writer.write("<br>");
            writer.write("<b>");
            writer.write("Preferences");
            writer.write("</b><br><br>");
            helpFile = File.getInstance(dir, "preferences.html");
            if (helpFile != null && !helpFile.isFile())
                helpFile = null;
            List properties = Property.apropos(arg);
            sort(properties);
            addAproposEntries(properties, helpFile, writer);
            writer.write("</body>\n</html>\n");
            writer.flush();
            writer.close();
            Editor ed;
            if (isAproposBuffer(editor.getBuffer())) {
                ((WebBuffer)editor.getBuffer()).go(file, 0, "text/html");
                ed = editor;
            } else {
                Buffer buf = null;
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer b = it.nextBuffer();
                    if (isAproposBuffer(b)) {
                        buf = b;
                        break;
                    }
                }
                if (buf != null)
                    ((WebBuffer)buf).go(file, 0, "text/html");
                else {
                    buf = WebBuffer.createWebBuffer(file, null, null);
                    buf.setTransient(true);
                }
                Editor otherEditor = editor.getOtherEditor();
                if (otherEditor != null) {
                    buf.setUnsplitOnClose(otherEditor.getBuffer().unsplitOnClose());
                    otherEditor.makeNext(buf);
                } else {
                    buf.setUnsplitOnClose(true);
                    editor.makeNext(buf);
                }
                ed = editor.activateInOtherWindow(buf);
            }
            for (Line line = ed.getBuffer().getFirstLine(); line != null; line = line.next()) {
                if (WebBuffer.findLink(line, 0) != null) {
                    int offset = 0;
                    while (offset < line.length()) {
                        char c = line.charAt(offset);
                        if (!Character.isWhitespace(c) && c != 160)
                            break;
                        ++offset;
                    }
                    ed.moveDotTo(line, offset);
                    ed.updateDisplay();
                    break;
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        finally {
            frame.setDefaultCursor();
        }
    }

    private static Comparator comparator;

    private static void sort(List list)
    {
        if (comparator == null) {
            comparator = new Comparator() {
                public int compare(Object o1, Object o2)
                {
                    return o1.toString().compareToIgnoreCase(o2.toString());
                }
            };
        }
        Collections.sort(list, comparator);
    }

    private static void addAproposEntries(List list, File helpFile,
        Writer writer) throws IOException
    {
        int size = 0;
        if (list != null)
            size = list.size();
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                String s = (String) list.get(i);
                if (helpFile != null) {
                    writer.write("&nbsp;&nbsp;<a href=\"");
                    writer.write(helpFile.canonicalPath());
                    writer.write('#');
                    writer.write(s);
                    writer.write("\">");
                }
                writer.write(s);
                if (helpFile != null)
                    writer.write("</a>");
                writer.write("<br>\n");
            }
        } else
            writer.write("&nbsp;&nbsp;<i>None</i><br>\n");
    }

    private static boolean isAproposBuffer(Buffer buffer)
    {
        if (!(buffer instanceof WebBuffer))
            return false;
        return buffer.getFile().equals(getAproposFile());
    }

    private static final File getAproposFile()
    {
        return File.getInstance(Directories.getTempDirectory(), "apropos.html");
    }

    public static File getDocumentationDirectory()
    {
        String s = Editor.preferences().getStringProperty(Property.DOC_PATH);
        if (s != null) {
            Path path = new Path(s);
            String[] array = path.list();
            if (array != null) {
                for (int i = 0; i < array.length; i++) {
                    File dir = File.getInstance(array[i]);
                    if (isDocDir(dir))
                        return dir;
                }
            }
        }
        s = System.getProperty("java.class.path");
        if (s != null) {
            final File userDir =
                File.getInstance(System.getProperty("user.dir"));
            Path path = new Path(s);
            String[] array = path.list();
            if (array != null) {
                for (int i = 0; i < array.length; i++) {
                    String filename = array[i];
                    if (filename.toLowerCase().endsWith("j.jar")) {
                        File jarFile = File.getInstance(userDir, filename);
                        if (jarFile != null && jarFile.isFile()) {
                            File jarDir = jarFile.getParentFile();
                            if (jarDir != null && jarDir.isDirectory()) {
                                File docDir = File.getInstance(jarDir, "doc");
                                if (isDocDir(docDir))
                                    return docDir;
                            }
                        }
                    } else if (filename.toLowerCase().endsWith("src")) {
                        // "~/j/src"
                        File srcDir = File.getInstance(userDir, filename);
                        if (srcDir != null && srcDir.isDirectory()) {
                            File parentDir = srcDir.getParentFile(); // "~/j"
                            if (parentDir != null && parentDir.isDirectory()) {
                                File docDir = File.getInstance(parentDir, "doc"); // "~/j/doc"
                                if (isDocDir(docDir))
                                    return docDir;
                            }
                        }
                    } else {
                        String suffix = LocalFile.getSeparator() + "j" + LocalFile.getSeparator() + "j.jar";
                        if (filename.endsWith(suffix)) {
                            // "/usr/local/share/j/j.jar"
                            File dataDir = File.getInstance(filename.substring(0, filename.length() - suffix.length())); // "/usr/local/share"
                            File docDir = File.getInstance(dataDir, "doc" + LocalFile.getSeparator() + "j"); // "/usr/local/share/doc/j"
                            if (isDocDir(docDir))
                                 return docDir;
                        }
                    }
                }
            }
        }
        // As a last resort, a couple of hard-coded possibilities...
        if (Platform.isPlatformUnix()) {
            File dir = File.getInstance("/usr/local/share/doc/j");
            if (isDocDir(dir))
                return dir;
            dir = File.getInstance("/usr/share/doc/j");
            if (isDocDir(dir))
                return dir;
        } else if (Platform.isPlatformWindows()) {
            String dirname = cygpath("/usr/local/share/doc/j");
            if (dirname != null) {
                File dir = File.getInstance(dirname);
                if (isDocDir(dir))
                    return dir;
            }
        }
        return null;
    }

    private static String cygpath(String s)
    {
        String[] cmdarray = {"cygpath", "-w", s};
        try {
            Process process = Runtime.getRuntime().exec(cmdarray);
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            return reader.readLine();
        }
        catch (Throwable t) {
            return null;
        }
    }

    // Return true if dir seems to contain j's documentation.
    private static boolean isDocDir(File dir)
    {
        if (dir == null ||!dir.isDirectory())
            return false;
        File check = File.getInstance(dir, "commands.html");
        if (check == null || !check.isFile())
            return false;
        return true;
    }
}
