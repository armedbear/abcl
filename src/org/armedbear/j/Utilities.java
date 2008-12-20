/*
 * Utilities.java
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.KeyEvent;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

public final class Utilities implements Constants
{
    // Returns false if the string contains any upper case letters, true
    // otherwise. "abc123" and "123" are lower case.
    public static boolean isLowerCase(String s)
    {
        for (int i = s.length()-1; i >= 0; i--) {
            if (Character.isUpperCase(s.charAt(i)))
                return false;
        }
        return true;
    }

    // For a string to be upper case, it must have at least one upper case
    // letter, and no lower case letters. "ABC123" is upper case, but "123" is
    // not.
    public static boolean isUpperCase(String s)
    {
        boolean containsLetter = false;
        for (int i = s.length()-1; i >= 0; i--) {
            char c = s.charAt(i);
            if (Character.isLetter(c)) {
                if (!Character.isUpperCase(c))
                    return false;
                containsLetter = true;
            }
        }
        // We didn't encounter any letters that weren't upper case.
        return containsLetter;
    }

    public static boolean isWhitespace(String s)
    {
        for (int i = s.length()-1; i >= 0; i--) {
            if (!Character.isWhitespace(s.charAt(i)))
                return false;
        }
        return true;
    }

    public static int countLines(String s)
    {
        int count = 0;
        for (int i = s.length(); i-- > 0;)
            if (s.charAt(i) == '\n')
                ++count;
        return count;
    }

    public static boolean isDelimited(Buffer buffer, Position pos, int length)
    {
        if (buffer != null)
            return isDelimited(buffer.getMode(), pos, length);
        else
            return isDelimited(pos, length);
    }

    public static boolean isDelimited(Mode mode, Position pos, int length)
    {
        if (mode == null)
            return isDelimited(pos, length);
        return mode.isDelimited(pos, length);
    }

    public static boolean isDelimited(Position pos, int length)
    {
        final int before = pos.getOffset() - 1;
        if (before >= 0 && Character.isJavaIdentifierPart(pos.getLine().charAt(before)))
            return false;
        final int after = pos.getOffset() + length;
        if (after < pos.getLineLength() && Character.isJavaIdentifierPart(pos.getLine().charAt(after)))
            return false;
        return true;
    }

    public static boolean isDelimited(String s, int index, int length)
    {
        final int before = index - 1;
        if (before >= 0 && Character.isJavaIdentifierPart(s.charAt(before)))
            return false;
        final int after = index + length;
        if (after < s.length() && Character.isJavaIdentifierPart(s.charAt(after)))
            return false;
        return true;
    }

    public static boolean isDelimited(String s, int index, int length,
                                      Mode mode)
    {
        if (mode == null)
            return isDelimited(s, index, length);
        final int before = index - 1;
        if (before >= 0 && mode.isIdentifierPart(s.charAt(before)))
            return false;
        final int after = index + length;
        if (after < s.length() && mode.isIdentifierPart(s.charAt(after)))
            return false;
        return true;
    }

    public static boolean isDelimited(Mode mode, String s,
                                      int startIndex, int endIndex)
    {
        if (mode == null)
            mode = JavaMode.getMode();
        final int before = startIndex - 1;
        if (before >= 0 && mode.isIdentifierPart(s.charAt(before)))
            return false;
        if (endIndex < s.length() && mode.isIdentifierPart(s.charAt(endIndex)))
            return false;
        return true;
    }

    // Returns extension including leading '.'
    public static String getExtension(String filename)
    {
        int indexOfLastDot = filename.lastIndexOf('.');
        if (indexOfLastDot < 0)
            return null;
        int indexOfLastFileSeparatorChar = filename.lastIndexOf(LocalFile.getSeparatorChar());
        if (indexOfLastDot < indexOfLastFileSeparatorChar)
            return null; // Last dot was in path prefix.
        else
            return filename.substring(indexOfLastDot);
    }

    // Returns extension including leading '.'
    public static String getExtension(File file)
    {
        // Only consider last component of filename.
        String name = file.getName();
        int index = name.lastIndexOf('.');
        if (index < 0)
            return null;
        else
            return name.substring(index);
    }

    public static final File getTempFile()
    {
        return getTempFile(Directories.getTempDirectory());
    }

    public static final File getTempFile(File dir)
    {
        return getTempFile(dir, "");
    }

    public static final File getTempFile(String dir)
    {
        return getTempFile(File.getInstance(dir), "");
    }

    // Implementation.
    // Guarantee that getTempFile() will generate unique filenames even if
    // it's called twice with the same value of System.currentTimeMillis().
    private static int tempFileCount = 0;

    public static synchronized File getTempFile(File dir, String extension)
    {
        if (dir == null)
            return null;

        if (extension == null) {
            extension = "";
        } else if (extension.length() > 0) {
            if (extension.charAt(0) != '.')
                extension = ".".concat(extension);
        }

        long date = System.currentTimeMillis();

        for (int i = 0; i < 100; i++) {
            File file = File.getInstance(dir,
                String.valueOf(date + tempFileCount).concat(extension));
            ++tempFileCount;
            if (!file.exists())
                return file;
        }

        return null;
    }

    public static boolean isDirectoryWritable(File dir)
    {
        boolean isWritable = false;
        File file = getTempFile(dir);
        if (file != null) {
            try {
                // getOutputStream() will throw FileNotFoundException if
                // directory is not writable.
                FileOutputStream out = file.getOutputStream();
                out.close();
                if (file.isFile()) {
                    isWritable = true;
                    file.delete();
                }
            }
            catch (IOException e) {}
        }
        return isWritable;
    }

    public static String detab(String s, int tabWidth, int startCol)
    {
        if (tabWidth <= 0)
            return s;
        int limit = s.length();
        int i;
        for (i = 0; i < limit; i++) {
            if (s.charAt(i) == '\t')
                break;
        }
        if (i == limit)
            return s; // No tab characters in string.

        // If we get here, we're looking at the first tab character in the string.

        // Copy the first part of the string to a char array.
        char[] charArray = new char[limit * tabWidth];
        s.getChars(0, i, charArray, 0);
        int col = startCol + i;
        // Handle the first tab character.
        do {
            charArray[col - startCol] = ' ';
            ++col;
        } while ((col % tabWidth) != 0);
        // SKip past the tab char in the input string.
        ++i;
        // Process the rest of the string.
        while (i < limit) {
            char c = s.charAt(i++);
            if (c == '\t') {
                for (int j = tabWidth - col % tabWidth - 1; j >= 0; j--) {
                    charArray[col - startCol] = ' ';
                    ++col;
                }
            } else {
                charArray[col - startCol] = c;
                ++col;
            }
        }
        return charArray == null ? s : new String(charArray, 0, col - startCol);
    }

    public static final String detab(String s, int tabWidth)
    {
        return detab(s, tabWidth, 0);
    }

    public static String entab(String s, int tabWidth, int startCol)
    {
        if (tabWidth <= 0)
            return s;
        int limit = s.length();
        if (limit < tabWidth)
            return s;
        s = detab(s, tabWidth, startCol); // Clean start.

        // The length may have changed when we detabbed the string.
        limit = s.length();

        FastStringBuffer sb = new FastStringBuffer(limit);
        int i = 0;
        while (i < limit) {
            char c = s.charAt(i);
            if (c == ' ') {
                int nextTabStop = ((startCol + i) / tabWidth + 1) * tabWidth;
                if (nextTabStop < startCol + limit && nextTabStop - (startCol + i) > 1) {
                    boolean replace = true;
                    int j = i + 1;
                    while (j < nextTabStop - startCol) {
                        if (s.charAt(j++) != ' ') {
                            replace = false;
                            break;
                        }
                    }
                    if (replace) {
                        sb.append('\t');
                        i = j;
                        continue;
                    }
                }
            }
            sb.append(c);
            i++;
        }
        return sb.toString();
    }

    public static final String entab(String s, int tabWidth)
    {
        return entab(s, tabWidth, 0);
    }

    public static String makeTabsVisible(String s, int tabWidth)
    {
        if (tabWidth <= 0)
            return s;
        int limit = s.length();
        if (limit == 0)
            return s;
        final char tabChar = '^';
        FastStringBuffer sb = new FastStringBuffer(limit);
        int col = 0;
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (c == '\t') {
                sb.append(tabChar);
                ++col;
                while ((col % tabWidth) != 0) {
                    sb.append(' ');
                    ++col;
                }
            } else {
                sb.append(c);
                ++col;
            }
        }
        return sb.toString();
    }

    public static String wrap(String s, int wrapCol, int tabWidth)
    {
        FastStringBuffer sb = new FastStringBuffer();
        int i = 0;
        final int limit = s.length();
        int startOffs = 0;
        while (i < limit) {
            int col = 0;
            int breakOffs = 0;
            startOffs = i;
            boolean inUrl = false;
            while (i < limit) {
                char c = s.charAt(i++);
                if (c == '\n') {
                    sb.append(trimTrailing(s.substring(startOffs, i)));
                    startOffs = i;
                    break;
                }
                if (c == '\t')
                    col += tabWidth - col % tabWidth;
                else
                    ++col;
                if (!inUrl && col > wrapCol) {
                    // Need to break.
                    if (breakOffs <= startOffs)
                        breakOffs = i;
                    sb.append(trimTrailing(s.substring(startOffs, breakOffs)));
                    sb.append('\n');
                    i = breakOffs;
                    // Skip whitespace.
                    while (i < limit && s.charAt(i) == ' ')
                        ++i;
                    break;
                }
                if (c == ' ' || c == '\t') {
                    // We've already incremented i to point past the whitespace char.
                    breakOffs = i;
                    inUrl = false;
                } else if (c == 'h' && lookingAt(s, i, "ttp://", false)) {
                    inUrl = true;
                    i += 6;
                } else if (c == 'h' && lookingAt(s, i, "ttps://", false)) {
                    inUrl = true;
                    i += 7;
                } else if (c == 'f' && lookingAt(s, i, "tp://", false)) {
                    inUrl = true;
                    i += 5;
                }
            }
        }
        if (i > startOffs)
            sb.append(trimTrailing(s.substring(startOffs, i)));
        return sb.toString();
    }

    public static int getDetabbedLength(String s, int tabWidth)
    {
        final int limit = s.length();
        int detabbedLength = 0;
        for (int i = 0; i < limit; i++) {
            if (s.charAt(i) == '\t')
                detabbedLength += tabWidth - detabbedLength % tabWidth;
            else
                ++detabbedLength;
        }
        return detabbedLength;
    }

    // Trims leading space characters only, not all whitespace.
    public static String trimLeading(String s)
    {
        final int length = s.length();
        int i = 0;
        while (i < length) {
            if (s.charAt(i) != ' ')
                break;
            ++i;
        }
        return s.substring(i);
    }

    // Trims trailing space characters only, not all whitespace.
    public static String trimTrailing(String s)
    {
        int length = s.length();
        if (length == 0 || s.charAt(length-1) != ' ')
            return s;
        do {
            --length;
        } while (length > 0 && s.charAt(length-1) == ' ');
        return s.substring(0, length);
    }

    // Returns true if the string to be inserted looks like a block of lines.
    public static boolean isLinePaste(String s)
    {
        final char c = s.charAt(s.length()-1);
        return c == '\r' || c == '\n';
    }

    public static int getIntegerProperty(Properties props, String key, int defaultValue)
    {
        try {
            String s = props.getProperty(key);
            if (s != null)
                return Integer.parseInt(s);
        }
        catch (NumberFormatException e) {}
        return defaultValue;
    }

    /**
     * Extracts the name of an include file, enclosed in quotes or angle
     * brackets, from a #include line in a C or C++ source file.
     *
     * @param line      the #include line
     * @return          the name of the file, enclosed in quotes or angle
     *                  brackets as the case may be, or <code>null</code> if
     *                  the line in question is not a valid #include line.
     * @since           0.16.2
     */
    public static String extractInclude(String line)
    {
        String s = line.trim();
        if (s.length() < 12) // "#include <a>"
            return null;
        if (s.charAt(0) != '#')
            return null;
        REMatch match = includeRE.getMatch(s);
        if (match == null)
            return null;
        s = s.substring(match.getEndIndex()).trim();
        if (s.length() < 2)
            return null;
        char c = s.charAt(0);
        int lastIndex = -1;
        if (c == '"')
            lastIndex = s.indexOf('"', 1);
        else if (c == '<')
            lastIndex = s.indexOf('>', 1);
        if (lastIndex < 0)
            return null;
        return s.substring(0, lastIndex+1);
    }

    private static final RE includeRE = new UncheckedRE("#[ \t]*include[ \t]");

    /**
     * Finds an include file (C or C++).
     * <p>
     * If the filename is enclosed in quotes, we look for it in the current
     * directory first.
     *
     * @param s                 the name of the file, enclosed in quotes or
     *                          angle brackets as the case may be
     * @param path              the include path
     * @param currentDirectory  the current directory
     * @return                  the file, or <code>null</code> if not found
     * @since                   0.16.2
     */
    public static File findInclude(String s, String path,
        File currentDirectory)
    {
        char c = s.charAt(0);
        final String fileName = s.substring(1, s.length()-1);
        if (c == '"') {
            // The filename is enclosed in quotes. Look in the current
            // directory first.
            File file = File.getInstance(currentDirectory, fileName);
            if (file != null && file.isLocal() && file.isFile())
                return file;
        }
        return Utilities.findFileInPath(fileName, path, currentDirectory);
    }

    // We pass in currentDirectory in order to support relative paths.
    public static File findFileInPath(String filename, String path,
        File currentDirectory)
    {
        if (path != null) {
            int index;
            do {
                index = path.indexOf(LocalFile.getPathSeparatorChar());
                String dirname;
                if (index < 0) {
                    dirname = path;
                } else {
                    dirname = path.substring(0, index);
                    path = path.substring(index + 1);
                }
                File dir;
                if (currentDirectory != null && currentDirectory.isLocal())
                    dir = File.getInstance(currentDirectory, dirname);
                else
                    dir = File.getInstance(dirname);
                if (dir != null) {
                    File file = File.getInstance(dir, filename);
                    if (file != null && file.isLocal() && file.isFile())
                        return file;
                }
            } while (index >= 0);
        }
        return null;
    }

    // Returns a list of strings.
    public static List getDirectoriesInPath(String path)
    {
        ArrayList list = new ArrayList();
        if (path != null) {
            final char sep = LocalFile.getPathSeparatorChar();
            int begin = 0;
            int end;
            do {
                end = path.indexOf(sep, begin);
                String dir;
                if (end < 0) {
                    dir = path.substring(begin);
                } else {
                    dir = path.substring(begin, end);
                    begin = end + 1;
                }
                if (dir.length() != 0)
                    list.add(dir);
            } while (end >= 0);
        }
        return list;
    }

    // Looks for specified file in the source path and (if applicable) the
    // include path.
    public static File findFile(Editor editor, String filename)
    {
        final Buffer buffer = editor.getBuffer();
        final File currentDirectory =
            buffer != null ? buffer.getCurrentDirectory() : null;

        // Look for .h files in the include path.
        if (filename.toLowerCase().endsWith(".h")) {
            String includePath;
            if (buffer != null)
                includePath = buffer.getStringProperty(Property.INCLUDE_PATH);
            else
                includePath = Editor.preferences().getStringProperty(Property.INCLUDE_PATH);
            if (includePath != null) {
                File file = findFileInPath(filename, includePath, currentDirectory);
                if (file != null)
                    return file;
            }
        }

        // Try the source path.
        String sourcePath;
        if (buffer != null)
            sourcePath = buffer.getStringProperty(Property.SOURCE_PATH);
        else
            sourcePath = Editor.preferences().getStringProperty(Property.SOURCE_PATH);
        if (sourcePath != null) {
            File file = findFileInPath(filename, sourcePath, currentDirectory);
            if (file != null)
                return file;
        }

        // Try the source path of the default mode for the file we're looking
        // for.
        Mode mode = Editor.getModeList().getModeForFileName(filename);
        if (mode != null) {
            sourcePath = mode.getStringProperty(Property.SOURCE_PATH);
            if (sourcePath != null) {
                File file = findFileInPath(filename, sourcePath, currentDirectory);
                if (file != null)
                    return file;
            }
        }

        // Try global source path.
        sourcePath = Editor.preferences().getStringProperty(Property.SOURCE_PATH);
        if (sourcePath != null) {
            File file = findFileInPath(filename, sourcePath, currentDirectory);
            if (file != null)
                return file;
        }

        return null;
    }

    // If parent directory exists, returns true; otherwise displays standard
    // error dialog using context as title and returns false.
    public static boolean checkParentDirectory(File file, String context)
    {
        File parent = file.getParentFile();
        if (parent != null && parent.isDirectory())
            return true;
        FastStringBuffer sb = new FastStringBuffer("Invalid path \"");
        sb.append(file.netPath());
        sb.append('"');
        MessageDialog.showMessageDialog(sb.toString(), context);
        return false;
    }

    public static boolean isFilenameAbsolute(String filename)
    {
        final int length = filename.length();
        if (length > 0) {
            char c0 = filename.charAt(0);
            if (c0 == '\\' || c0 == '/')
                return true;
            if (length > 2) {
                if (Platform.isPlatformWindows()) {
                    // Check for drive letter.
                    char c1 = filename.charAt(1);
                    if (c1 == ':') {
                        if (c0 >= 'a' && c0 <= 'z')
                            return true;
                        if (c0 >= 'A' && c0 <= 'Z')
                            return true;
                    }
                } else if (Platform.isPlatformUnix()) {
                    if (filename.equals("~") || filename.startsWith("~/"))
                        return true;
                }
                if (File.hasRemotePrefix(filename))
                    return true;
                if (File.hasLocalPrefix(filename))
                    return true;
            }
        }
        return false;
    }

    // Helper for getFileType.
    private static String getStringFromUnicodeBytes(byte[] bytes, int start,
        int length, boolean isLittleEndian)
    {
        FastStringBuffer sb = new FastStringBuffer(length);
        int i = start;
        int limit = start + length;
        while (i < limit - 1) {
            byte b1 = bytes[i++];
            byte b2 = bytes[i++];
            if (isLittleEndian)
                sb.append((char) ((b2 << 16) + b1));
            else
                sb.append((char) ((b1 << 16) + b2));
        }
        return sb.toString();
    }

    // Returns FILETYPE_UNKNOWN if file is null or does not exist.
    public static int getFileType(File file)
    {
        if (file == null)
            return FILETYPE_UNKNOWN;
        int fileType = FILETYPE_UNKNOWN;
        try {
            InputStream in = file.getInputStream();
            byte[] bytes = new byte[4096];
            int bytesRead = in.read(bytes);
            in.close();
            boolean isUnicode = false;
            boolean isLittleEndian = false;
            if (bytesRead >= 2) {
                if (bytes[0] == (byte)0xfe && bytes[1] == (byte)0xff) {
                    isUnicode = true;
                    isLittleEndian = false;
                } else if (bytes[0] == (byte)0xff && bytes[1] == (byte)0xfe)
                    isUnicode = true;
            }
            if (!isUnicode) {
                for (int i = 0; i < bytesRead; i++) {
                    if (bytes[i] == 0) {
                        fileType = FILETYPE_BINARY;
                        break;
                    }
                }
            }
            if (fileType == FILETYPE_BINARY) {
                if (bytesRead > 2) {
                    if (bytes[0] == (byte) 0xd0 && bytes[1] == (byte) 0xcf)
                        fileType = FILETYPE_WORD;
                    else if (bytes[0] == (byte) 0xff && bytes[1] == (byte) 0xd8)
                        fileType = FILETYPE_JPEG;
                    else if (bytes[0] == (byte) 'P' && bytes[1] == 'K') {
                        // Looks like a zip file.
                        try {
                            ZipInputStream istream =
                                new ZipInputStream(file.getInputStream());
                            fileType = FILETYPE_ZIP;
                            istream.close();
                        }
                        catch (Exception e) {}
                    } else {
                        try {
                            GZIPInputStream istream =
                                new GZIPInputStream(file.getInputStream());
                            fileType = FILETYPE_GZIP;
                            istream.close();
                        }
                        catch (IOException e) {}
                    }
                }
            } else {
                // Not binary.
                fileType = FILETYPE_TEXT;
                String s;
                if (isUnicode)
                    s = getStringFromUnicodeBytes(bytes, 2, bytesRead,
                                                  isLittleEndian);
                else
                    s = new String(bytes, 0, bytesRead);
                if (s.length() >= 3) {
                    if (s.charAt(0) == '#' && s.charAt(1) == '!') {
                        // Only consider the first line.
                        int index = s.indexOf('\n');
                        if (index >= 0)
                            s = s.substring(0, index);
                        index = s.indexOf('\r');
                        if (index >= 0)
                            s = s.substring(0, index);
                        if (s.indexOf("/bin/sh") >=0 ||
                            s.indexOf("/bin/bash") >= 0 ||
                            s.indexOf("/bin/tcsh") >= 0)
                            fileType = FILETYPE_SHELLSCRIPT;
                        else if (s.indexOf("/bin/perl") >= 0)
                            fileType = FILETYPE_PERL;
                    } else if (s.startsWith("<?xml")) {
                        fileType = FILETYPE_XML;
                    } else if (s.startsWith("<?php")) {
                        fileType = FILETYPE_PHP;
                    } else if (s.startsWith("<?") &&
                        Character.isWhitespace(s.charAt(2))) {
                        fileType = FILETYPE_PHP;
                    }
                }
            }
        }
        catch (Exception e) {}
        return fileType;
    }

    public static boolean deleteRename(File source, File destination)
    {
        if (!source.isFile()) {
            Log.warn("deleteRename source file " + source + " does not exist");
            return false;
        }
        // The delete/rename operation sometimes fails on NT at first, but then
        // succeeds later, so we do some retries here if necessary.
        for (int i = 0; i < 5; i++) {
            if (destination.exists())
                destination.delete();
            if (source.renameTo(destination))
                return true;
            Log.warn("deleteRename renameTo failed i = " + i);
            System.gc();
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException ex) {}
        }
        // Last resort.
        // We might not have delete rights in the destination directory. Try to
        // overwrite the destination file in place.
        Log.warn("deleteRename calling overwriteFile");
        if (overwriteFile(source, destination)) {
            source.delete();
            return true;
        }
        Log.warn("deleteRename returning false");
        return false;
    }

    public static boolean copyFile(File source, File destination)
    {
        if (!source.isFile()) {
            Log.error("copyFile error - source is not a file: " + source);
            return false;
        }
        if (destination.isDirectory()) {
            Log.error("copyFile error - destination is a directory: " + destination);
            return false;
        }
        if (destination.isFile() && !destination.canWrite()) {
            Log.error("copyFile error - destination is read only: " + destination);
            return false;
        }
        boolean error = false;
        File tempFile = getTempFile(destination.getParent());
        final long length = source.length();
        long bytesCopied = 0;
        InputStream in = null;
        OutputStream out = null;
        // Copy source to tempFile.
        try {
            in = source.getInputStream();
            out = tempFile.getOutputStream();
            final int bufsize = length < 32768 ? (int) length : 32768;
            byte[] buffer = new byte[bufsize];
            while (bytesCopied < length) {
                int bytesRead = in.read(buffer, 0, bufsize);
                if (bytesRead > 0) {
                    out.write(buffer, 0, bytesRead);
                    bytesCopied += bytesRead;
                } else
                    break;
            }
        }
        catch (IOException e) {
            error = true;
        }
        try {
            if (in != null)
                in.close();
            if (out != null) {
                out.flush();
                out.close();
            }
        }
        catch (IOException e) {
            Log.error(e);
            error = true;
        }
        if (error) {
            Log.error("copyFile error");
            tempFile.delete();
            return false;
        }
        if (bytesCopied != length) {
            Log.error("copyFile error - bytesCopied != length");
            tempFile.delete();
            return false;
        }
        if (destination.exists()) {
            destination.delete();
            if (destination.exists()) {
                // Unable to delete existing destination file.
                Log.error("copyFile error - unable to delete existing destination file: " + destination);
                tempFile.delete();
                return false;
            }
        }
        if (!tempFile.renameTo(destination)) {
            Log.error("copyFile error - unable to rename temporary file");
            Log.error("tempFile = " + tempFile.netPath());
            Log.error("destination = " + destination.netPath());
            tempFile.delete();
            return false;
        }
        if (Platform.isPlatformUnix())
            destination.setPermissions(source.getPermissions());
        destination.setLastModified(source.lastModified());
        return true; // Success!
    }

    private static boolean overwriteFile(File source, File destination)
    {
        if (!source.isFile())
            return false;
        boolean error = false;
        long length = source.length();
        long totalBytes = 0;
        BufferedInputStream in = null;
        BufferedOutputStream out = null;
        try {
            in = new BufferedInputStream(source.getInputStream());
            out = new BufferedOutputStream(destination.getOutputStream());
            final int bufsize = 4096;
            byte[] buffer = new byte[bufsize];
            while (totalBytes < length) {
                int numBytes = in.read(buffer, 0, bufsize);
                if (numBytes > 0) {
                    out.write(buffer, 0, numBytes);
                    totalBytes += numBytes;
                } else
                    break;
            }
        }
        catch (IOException e) {
            Log.error(e);
            error = true;
        }

        try {
            if (in != null)
                in.close();
            if (out != null) {
                out.flush();
                out.close();
            }
        }
        catch (IOException e) {
            Log.error(e);
            error = true;
        }
        if (error)
            return false;
        if (totalBytes != length)
            return false;
        return true;
    }

    public static boolean makeBackup(File file, boolean keepOriginal)
    {
        return makeBackup(file, file.getName(), keepOriginal);
    }

    public static boolean makeBackup(File file, String name,
        boolean keepOriginal)
    {
        // No need to back it up if it doesn't exist.
        if (!file.isFile())
            return true;
        File backupDir = null;
        String backupDirectory =
            Editor.preferences().getStringProperty(Property.BACKUP_DIRECTORY);
        if (backupDirectory != null) {
            backupDir = File.getInstance(backupDirectory);
        } else {
            // Use default location.
            backupDir = File.getInstance(Directories.getUserHomeDirectory(),
                                         "backup");
        }
        if (backupDir == null)
            return false;
        if (!backupDir.isDirectory() && !backupDir.mkdirs()) {
            Log.error("can't create backup directory ".concat(backupDir.canonicalPath()));
            return false;
        }
        File backupFile = File.getInstance(backupDir, name);
        if (!keepOriginal) {
            if (backupFile.isFile() && !backupFile.delete())
                Log.error("can't delete old backupFile file ".concat(backupFile.toString()));
            if (file.renameTo(backupFile))
                return true;
        }
        if (copyFile(file, backupFile))
            return true;
        // If copyFile() failed because the existing backup file is not
        // writable, delete that file and try again.
        if (backupFile.isFile() && !backupFile.canWrite()) {
            Log.debug("deleting old backup file " + backupFile);
            backupFile.delete();
            Log.debug("retrying copyFile...");
            if (copyFile(file, backupFile))
                return true;
        }
        Log.error("makeBackup copyFile failed, returning false");
        return false;
    }

    public static Color getColor(String s)
    {
        Color color = null;
        s = s.trim();
        if (s.equals("black"))
            color = Color.black;
        else if (s.equals("white"))
            color = Color.white;
        else if (s.equals("yellow"))
            color = Color.yellow;
        else if (s.equals("blue"))
            color = Color.blue;
        else if (s.equals("red"))
            color = Color.red;
        else if (s.equals("gray"))
            color = Color.gray;
        else if (s.equals("green"))
            color = Color.green;
        else {
            try {
                // The string must consist of three numbers for the R, G, B
                // components.
                StringTokenizer st = new StringTokenizer(s);
                if (st.countTokens() == 3) {
                    int r = Integer.parseInt(st.nextToken());
                    int g = Integer.parseInt(st.nextToken());
                    int b = Integer.parseInt(st.nextToken());
                    color = new Color(r, g, b);
                }
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
        return color;
    }

    // BUG! Not really correct!
    private static final String filenameChars =
        "#-./0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ\\_abcdefghijklmnopqrstuvwxyz~";

    public static boolean isFilenameChar(char c)
    {
        return (filenameChars.indexOf(c) >= 0);
    }

    public static boolean isProcessAlive(Process process)
    {
        if (process == null)
            return false;
        try {
            process.exitValue();
            // If process is alive, we won't reach here.
            return false;
        }
        catch (IllegalThreadStateException e) {
            // If this exception is thrown, the process is still alive.
            return true;
        }
    }

    public static void kill(int pid)
    {
        if (Platform.isPlatformUnix()) {
            try {
                String[] cmdarray = { "/bin/sh", "-c", "kill -9 " + pid };
                Process p = Runtime.getRuntime().exec(cmdarray);
                p.waitFor();
            }
            catch (Throwable t) {
                Log.error(t);
            }
        }
    }

    private static int haveJpty = -1;

    public static boolean haveJpty()
    {
        if (haveJpty == -1)
            haveJpty = have("jpty") ? 1 : 0;
        return haveJpty == 1;
    }

    private static int haveLs = -1;

    public static boolean haveLs()
    {
        if (haveLs == -1)
            haveLs = have("ls") ? 1 : 0;
        return haveLs == 1;
    }

    public static boolean have(final String s)
    {
        try {
            final Process p = Runtime.getRuntime().exec(s);
            if (p != null) {
                Thread t = new Thread("Utilities.have(\"" + s + "\") destroy") {
                    public void run()
                    {
                        try {
                            final BufferedReader reader =
                                new BufferedReader(new InputStreamReader(p.getInputStream()));
                            while (reader.readLine() != null)
                                ;
                            p.getInputStream().close();
                            p.getOutputStream().close();
                            p.getErrorStream().close();
                        }
                        catch (IOException e) {
                            Log.error(e);
                        }
                        p.destroy();
                        try {
                            p.waitFor();
                        }
                        catch (InterruptedException e) {
                            Log.error(e);
                        }
                    }
                };
                t.setDaemon(true);
                t.setPriority(Thread.MIN_PRIORITY);
                t.start();
                return true;
            }
        }
        catch (Throwable t) {}
        return false;
    }

    private static String userHome;

    public static final void setUserHome(String s)
    {
        Debug.bugIfNot(userHome == null); // We only want to do this once!
        userHome = s;
    }

    public static String getUserHome()
    {
        if (userHome == null) {
            if (Platform.isPlatformWindows()) {
                String[] cmdarray = {"bash", "-c", "echo $HOME"};
                String output = exec(cmdarray);
                if (output != null) {
                    output = output.trim();
                    if (output.length() > 0) {
                        if (output.indexOf('/') >= 0)
                            userHome = uncygnify(output);
                        else
                            userHome = output;
                    }
                }
            }
            if (userHome == null) {
                userHome = System.getProperty("user.home");
                // Expand links.
                File home = File.getInstance(userHome);
                if (home != null)
                    userHome = home.canonicalPath();
            }
        }
        return userHome;
    }

    public static String cygnify(String s)
    {
        String[] cmdArray = {"cygpath", "-u", s};
        String converted = Utilities.exec(cmdArray);
        return converted != null ? converted : s;
    }

    public static String uncygnify(String s)
    {
        String[] cmdArray = {"cygpath", "-w", s};
        String converted = Utilities.exec(cmdArray);
        return converted != null ? converted : s;
    }

    public static String exec(String[] cmdarray)
    {
        try {
            Process process = Runtime.getRuntime().exec(cmdarray);
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(process.getInputStream()));
            FastStringBuffer sb = new FastStringBuffer();
            String s;
            while ((s = reader.readLine()) != null) {
                if (s.length() > 0) {
                    if (sb.length() > 0)
                        sb.append('\n');
                    sb.append(s);
                }
            }
            process.getInputStream().close();
            process.getOutputStream().close();
            process.getErrorStream().close();
            process.waitFor();
            return sb.toString();
        }
        catch (Throwable t) {
            return null;
        }
    }

    public static ImageIcon getIconFromFile(String iconFile)
    {
        URL url = Editor.class.getResource("images/".concat(iconFile));
        if (url == null)
            return null;
        return new ImageIcon(url);
    }

    // Parses integer from string. Parsing stops when we encounter a non-digit.
    public static int parseInt(String s) throws NumberFormatException
    {
        final int limit = s.length();
        int i;
        for (i = 0; i < limit; i++) {
            if (!Character.isDigit(s.charAt(i)))
                break;
        }
        if (i == 0) // No digit found.
            throw new NumberFormatException();
        return Integer.parseInt(s.substring(0, i));
    }

    public static String rightJustify(int n, int fieldWidth)
    {
        String s = String.valueOf(n);
        int pad = fieldWidth - s.length();
        if (pad <= 0)
            return s;
        FastStringBuffer sb = new FastStringBuffer(spaces(pad));
        sb.append(s);
        return sb.toString();
    }

    public static String rightJustify(String s, int fieldWidth)
    {
        int pad = fieldWidth - s.length();
        if (pad <= 0)
            return s;
        FastStringBuffer sb = new FastStringBuffer(spaces(pad));
        sb.append(s);
        return sb.toString();
    }

    public static final boolean lookingAt(String s, int i, String pattern,
        boolean ignoreCase)
    {
        return s.regionMatches(ignoreCase, i, pattern, 0, pattern.length());
    }

    public static boolean isOneOf(String s, String[] strings)
    {
        if (s != null) {
            for (int i = strings.length-1; i >= 0; i--)
                if (s.equals(strings[i]))
                    return true;
        }
        return false;
    }

    private static final String SPACES;
    private static final int SPACES_LENGTH = 256;

    static {
        char[] chars = new char[SPACES_LENGTH];
        for (int i = 0; i < SPACES_LENGTH; i++)
            chars[i] = ' ';
        SPACES = new String(chars);
    }

    public static String spaces(int count)
    {
        if (count <= 0)
            return "";
        else if (count <= SPACES_LENGTH)
            return SPACES.substring(0, count);
        else {
            FastStringBuffer sb =  new FastStringBuffer(count);
            for (int i = 0; i < count; i++)
                sb.append(' ');
            return sb.toString();
        }
    }

    public static final String getCharsetFromContentType(String contentType)
    {
        if (contentType == null)
            return null;
        int index = contentType.toLowerCase().indexOf("charset=");
        if (index < 0)
            return null;
        String s = contentType.substring(index + 8);
        FastStringBuffer sb = new FastStringBuffer();
        boolean inQuote = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (i == 0 && c == '"') {
                inQuote = true;
                continue;
            }
            if (inQuote && c == '"')
                break; // Reached end of quoted string.
            if (!inQuote && c == ';')
                break; // Reached end of parameter value.
            sb.append(c); // Normal character.
        }
        return sb.toString();
    }

    public static final String getEncodingFromCharset(String charset)
    {
        if (charset == null)
            return "iso-8859-1";
        String lower = charset.toLowerCase();
        if (lower.equals("unknown-8bit") ||
            lower.equals("x-unknown") ||
            lower.equals("us-ascii") ||
            lower.equals("default_charset") ||
            lower.equals("latin-iso8859-1"))
            return "iso-8859-1";
        return charset;
    }

    public static boolean isSupportedEncoding(String encoding)
    {
        if (encoding != null) {
            try {
                "test".getBytes(encoding);
                return true;
            }
            catch (UnsupportedEncodingException e) {}
        }
        return false;
    }

    // Extracts XML or HTML tag name (e.g. "a" or "/a") from string.
    public static String getTagName(String s)
    {
        Debug.assertTrue(s != null);
        Debug.assertTrue(s.length() > 0);
        Debug.assertTrue(s.charAt(0) == '<');
        int length = s.length();
        int start = 1;
        for (int i = start; i < length; i++) {
            switch (s.charAt(i)) {
                case ' ':
                case '>':
                case '\t':
                case '\n':
                case '\r':
                    return s.substring(start, i);
            }
        }
        return s;
    }

    // Does not handle embedded single-quoted strings.
    public static List tokenize(String s)
    {
        ArrayList list = new ArrayList();
        if (s != null) {
            FastStringBuffer sb = new FastStringBuffer();
            boolean inQuote = false;
            final int limit = s.length();
            for (int i = 0; i < limit; i++) {
                char c = s.charAt(i);
                switch (c) {
                    case ' ':
                        if (inQuote)
                            sb.append(c);
                        else if (sb.length() > 0) {
                            list.add(sb.toString());
                            sb.setLength(0);
                        }
                        break;
                    case '"':
                        if (inQuote) {
                            if (sb.length() > 0) {
                                list.add(sb.toString());
                                sb.setLength(0);
                            }
                            inQuote = false;
                        } else
                            inQuote = true;
                        break;
                    default:
                        sb.append(c);
                        break;
                }
            }
            if (sb.length() > 0)
                list.add(sb.toString());
        }
        return list;
    }

    public static String getFirstIdentifier(String s, Mode mode)
    {
        FastStringBuffer sb = new FastStringBuffer();
        int length = s.length();
        if (length > 0) {
            char c = s.charAt(0);
            if (mode.isIdentifierStart(c)) {
                sb.append(c);
                for (int i = 1; i < length; i++) {
                    c = s.charAt(i);
                    if (mode.isIdentifierPart(c))
                        sb.append(c);
                    else
                        break;
                }
            }
        }
        return sb.toString();
    }

    public static KeyStroke getKeyStroke(String keyText)
    {
        if (keyText == null)
            return null;
        keyText = keyText.trim();
        if (keyText.length() == 0)
            return null;
        if (keyText.startsWith("'")) {
            if (keyText.length() != 3)
                return null;
            if (keyText.charAt(2) != '\'')
                return null;
            return KeyStroke.getKeyStroke(keyText.charAt(1));
        }
        if (keyText.length() == 1)
            return KeyStroke.getKeyStroke(keyText.charAt(0));
        int modifiers = 0;
        while (true) {
            if (keyText.startsWith("Ctrl ") || keyText.startsWith("Ctrl\t")) {
                modifiers |= CTRL_MASK;
                keyText = keyText.substring(5).trim();
                continue;
            }
            if (keyText.startsWith("Shift ") || keyText.startsWith("Shift\t")) {
                modifiers |= SHIFT_MASK;
                keyText = keyText.substring(6).trim();
                continue;
            }
            if (keyText.startsWith("Alt ") || keyText.startsWith("Alt\t")){
                modifiers |= ALT_MASK;
                keyText = keyText.substring(4).trim();
                continue;
            }
            if (keyText.startsWith("Meta ") || keyText.startsWith("Meta\t")){
                modifiers |= META_MASK;
                keyText = keyText.substring(5).trim();
                continue;
            }
            // No more modifiers.  What's left is the key name.
            break;
        }
        if (modifiers == 0 && keyText.length() == 1) {
            char c = keyText.charAt(0);
            return KeyStroke.getKeyStroke(c);
        }
        if (modifiers == SHIFT_MASK && keyText.length() == 1) {
            char c = keyText.charAt(0);
            char lower = Character.toLowerCase(c);
            char upper = Character.toUpperCase(c);
            if (lower != upper)
                return KeyStroke.getKeyStroke(upper);
        }
        int keyCode = getKeyCode(keyText);
        if (keyCode == 0)
            return null;
        return KeyStroke.getKeyStroke(keyCode, modifiers);
    }

    public static final String getKeyText(KeyStroke keyStroke)
    {
        return getKeyText(keyStroke.getKeyChar(), keyStroke.getKeyCode(), keyStroke.getModifiers());
    }

    public static String getKeyText(char keyChar, int keyCode, int modifiers)
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (keyChar >= ' ' && keyChar != 0xffff) {
            // Mapping is defined by character.
            if (keyChar >= 'A' && keyChar <= 'Z') {
                sb.append("Shift ");
                sb.append(keyChar);
            } else {
                sb.append('\'');
                sb.append(keyChar);
                sb.append('\'');
            }
        } else {
            // Mapping is defined by key code and modifiers.
            if ((modifiers & CTRL_MASK) != 0)
                sb.append("Ctrl ");
            if ((modifiers & SHIFT_MASK) != 0)
                sb.append("Shift ");
            if ((modifiers & ALT_MASK) != 0)
                sb.append("Alt ");
            if ((modifiers & META_MASK) != 0)
                sb.append("Meta ");
            sb.append(getKeyName(keyCode));
        }
        return sb.toString();
    }

    private static String[] keyNames = {
        "Enter",
        "Backspace",
        "Tab",
        "Escape",
        "Space",
        "Page Up",
        "Page Down",
        "Home",
        "End",
        "Delete",
        "Left",
        "Right",
        "Up",
        "Down",
        "NumPad Left",
        "NumPad Right",
        "NumPad Up",
        "NumPad Down",
        "NumPad *",
        "NumPad +",
        "NumPad -",
        "NumPad Insert",
        "Mouse-1",
        "Double Mouse-1",
        "Mouse-2",
        "Double Mouse-2",
        "Mouse-3",
        "Double Mouse-3"
    };

    private static int[] keyCodes = {
        KeyEvent.VK_ENTER,
        KeyEvent.VK_BACK_SPACE,
        KeyEvent.VK_TAB,
        KeyEvent.VK_ESCAPE,
        KeyEvent.VK_SPACE,
        KeyEvent.VK_PAGE_UP,
        KeyEvent.VK_PAGE_DOWN,
        KeyEvent.VK_HOME,
        KeyEvent.VK_END,
        KeyEvent.VK_DELETE,
        KeyEvent.VK_LEFT,
        KeyEvent.VK_RIGHT,
        KeyEvent.VK_UP,
        KeyEvent.VK_DOWN,
        KeyEvent.VK_KP_LEFT,
        KeyEvent.VK_KP_RIGHT,
        KeyEvent.VK_KP_UP,
        KeyEvent.VK_KP_DOWN,
        0x6a,
        0x6b,
        0x6d,
        0x9b,
        VK_MOUSE_1,
        VK_DOUBLE_MOUSE_1,
        VK_MOUSE_2,
        VK_DOUBLE_MOUSE_2,
        VK_MOUSE_3,
        VK_DOUBLE_MOUSE_3
    };

    private static int getKeyCode(String keyName)
    {
        if (keyName.length() == 0)
            return 0;
        if (keyName.length() == 1)
            return (int) keyName.charAt(0);
        if (keyName.startsWith("0x")) {
            try {
                return Integer.parseInt(keyName.substring(2), 16);
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
            return 0;
        }
        for (int i = 0; i < keyNames.length; i++) {
            if (keyName.equals(keyNames[i]))
                return keyCodes[i];
        }
        if (keyName.charAt(0) == 'F') {
            try {
                int n = Integer.parseInt(keyName.substring(1));
                return KeyEvent.VK_F1 + n - 1;
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
        return 0;
    }

    private static String getKeyName(int keyCode)
    {
        if (keyCode >= KeyEvent.VK_0 && keyCode <= KeyEvent.VK_9 || keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z)
            return String.valueOf((char) keyCode);
        if (keyCode >= KeyEvent.VK_F1 && keyCode <= KeyEvent.VK_F12)
            return "F" + Integer.toString(keyCode - KeyEvent.VK_F1 + 1);
        if (",./;=[\\]".indexOf(keyCode) >= 0)
            return String.valueOf((char) keyCode);
        for (int i = 0; i < keyCodes.length; i++){
            if (keyCode == keyCodes[i])
                return keyNames[i];
        }
        return "0x" + Integer.toString(keyCode, 16);
    }

    public static String propertyToXml(String name, String value)
    {
        FastStringBuffer sb = new FastStringBuffer("<property name=\"");
        sb.append(name);
        sb.append("\" value=\"");
        sb.append(value);
        sb.append("\"/>");
        return sb.toString();
    }

    public static JPanel createPanel(String title)
    {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        Border border =
            new TitledBorder(BorderFactory.createEtchedBorder(), title) {
            public void paintBorder(Component c, Graphics g, int x, int y,
                                    int width, int height)
            {
                Display.setRenderingHints(g);
                super.paintBorder(c, g, x, y, width, height);
            }
        };
        panel.setBorder(border);
        return panel;
    }

    private static String defaultXMLReaderImpl;
    static {
        if (Platform.isJava14()) {
            // Sun/Blackdown 1.4.x.
            defaultXMLReaderImpl =
                "org.apache.crimson.parser.XMLReaderImpl";
        } else {
            // 1.5
            defaultXMLReaderImpl =
                "com.sun.org.apache.xerces.internal.parsers.SAXParser";
        }
    }

    public static synchronized XMLReader getDefaultXMLReader()
    {
        if (defaultXMLReaderImpl != null) {
            try {
                return XMLReaderFactory.createXMLReader(defaultXMLReaderImpl);
            }
            catch (Exception e) {
                // Not available (IBM 1.4.0/1.4.1).
                Log.debug(defaultXMLReaderImpl + " is not available");
                // Don't use this code path again!
                defaultXMLReaderImpl = null;
                // Fall through...
            }
        }
        try {
            // This should work with IBM 1.4.0/1.4.1.
            return XMLReaderFactory.createXMLReader();
        }
        catch (Throwable t) {
            // We've got a real problem...
            Log.error(t);
            return null;
        }
    }
}
