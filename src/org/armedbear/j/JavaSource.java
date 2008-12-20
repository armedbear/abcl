/*
 * JavaSource.java
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class JavaSource implements Constants
{
    private static final char SEPARATOR_CHAR = LocalFile.getSeparatorChar();

    public static File findSource(String className, String sourcePath)
    {
        final List dirNames = Utilities.getDirectoriesInPath(sourcePath);
        if (dirNames != null) {
            String fileName =
                className.replace('.', SEPARATOR_CHAR).concat(".java");
            Iterator iter = dirNames.iterator();
            while (iter.hasNext()) {
                File dir = File.getInstance((String)iter.next());
                if (dir != null) {
                    File file = File.getInstance(dir, fileName);
                    if (file != null && file.isFile())
                        return file;
                }
            }
            // Not found. Try looking for short name of file.
            int index = fileName.lastIndexOf(SEPARATOR_CHAR);
            if (index >= 0) {
                String shortName = fileName.substring(index+1);
                Log.debug("shortName = |" + shortName + "|");
                iter = dirNames.iterator();
                while (iter.hasNext()) {
                    File dir = File.getInstance((String)iter.next());
                    if (dir != null) {
                        File file = File.getInstance(dir, shortName);
                        if (file != null && file.isFile()) {
                            Log.debug("file = " + file.canonicalPath());
                            return file;
                        }
                    }
                }
            }
        }
        return null;
    }

    // Returns file containing source for class referenced in buffer.
    public static File findSource(Buffer buffer, String className, boolean exact)
    {
        String[] candidates;
        if (exact) {
            candidates = new String[1];
            candidates[0] = className;
        } else
            candidates = getCandidates(className);
        return findSource(buffer, candidates);
    }

    private static File findSource(Buffer buffer, String[] candidates)
    {
        final String[] imports = getImports(buffer);

        // Look for import in JDK source path.
        final String jdkSourcePath =
            buffer.getStringProperty(Property.JDK_SOURCE_PATH);
        if (jdkSourcePath != null) {
            final List dirNames = Utilities.getDirectoriesInPath(jdkSourcePath);
            if (dirNames != null) {
                // Tell the user if the JDK source path is bogus.
                for (Iterator it = dirNames.iterator(); it.hasNext();) {
                    String name = (String) it.next();
                    File dir = File.getInstance(name);
                    String message = null;
                    if (dir == null) {
                        message = "Invalid directory " + name;
                    } else if (!dir.isDirectory()) {
                        message = "The directory " + dir.canonicalPath() +
                            " does not exist.";
                    }
                    if (message != null)
                        MessageDialog.showMessageDialog(message, "Error");
                }
                for (int i = 0; i < candidates.length; i++) {
                    File file = findImport(candidates[i], imports, dirNames,
                        ".java");
                    if (file != null)
                        return file;
                }
            }
        }

        // Look for import relative to package root directory.
        File packageRootDir = JavaSource.getPackageRootDirectory(buffer);
        if (packageRootDir != null) {
            for (int i = 0; i < candidates.length; i++) {
                File file = findImport(candidates[i], imports, packageRootDir,
                    ".java");
                if (file != null)
                    return file;
            }
        }

        // Look in current directory (i.e. current package).
        File currentDir = buffer.getCurrentDirectory();
        for (int i = 0; i < candidates.length; i++) {
            File file =
                File.getInstance(currentDir, candidates[i].concat(".java"));
            if (file != null && file.isFile())
                return file;
        }

        return null;
    }

    private static String[] getCandidates(String s)
    {
        ArrayList list = new ArrayList();
        int index = s.indexOf('.');
        while (index >= 0) {
            list.add(s.substring(0, index));
            index = s.indexOf('.', index + 1);
        }
        list.add(s);
        String[] array = new String[list.size()];
        return (String[]) list.toArray(array);
    }

    public static String getPackageName(Buffer buffer)
    {
        String packageName = null;
        for (Line line = buffer.getFirstLine(); line != null; line = line.next()) {
            String trim = line.trim();
            if (!trim.startsWith("package"))
                continue;
            trim = trim.substring(7);
            if (trim.length() == 0)
                continue;
            char c = trim.charAt(0);
            if (c != ' ' && c != '\t')
                continue;
            trim = trim.trim();
            final int length = trim.length();
            if (length == 0)
                continue;
            FastStringBuffer sb = new FastStringBuffer();
            for (int i = 0; i < length; i++) {
                c = trim.charAt(i);
                if (c == ' ' || c == '\t' || c == ';')
                    break;
                sb.append(c);
            }
            packageName = sb.toString();
        }
        return packageName;
    }

    public static File getPackageRootDirectory(Buffer buffer)
    {
        final File file = buffer.getFile();
        if (file == null || file.isRemote())
            return null;
        final File parentDir = file.getParentFile();
        if (parentDir == null)
            return null;
        final String packageName = getPackageName(buffer);
        if (packageName == null)
            return null;
        final String packagePrefix =
            packageName.replace('.', LocalFile.getSeparatorChar());
        final String dirName = parentDir.canonicalPath();
        if (dirName.endsWith(packagePrefix)) {
            String packageRootDirName =
                dirName.substring(0, dirName.length() - packagePrefix.length());
            return File.getInstance(packageRootDirName);
        }
        return null;
    }

    public static String[] getImports(Buffer buffer)
    {
        if (buffer.getModeId() != JAVA_MODE)
            return null;
        ArrayList list = new ArrayList();
        list.add("java.lang.*");
        for (Line line = buffer.getFirstLine(); line != null; line = line.next()) {
            String trim = line.trim();
            if (!trim.startsWith("import"))
                continue;
            trim = trim.substring(6);
            if (trim.length() == 0)
                continue;
            if (trim.charAt(0) != ' ' && trim.charAt(0) != '\t')
                continue;
            trim = trim.trim();
            FastStringBuffer sb = new FastStringBuffer();
            for (int i = 0; i < trim.length(); i++) {
                char c = trim.charAt(i);
                if (c == ' ' || c == '\t' || c == ';')
                    break;
                sb.append(c);
            }
            list.add(sb.toString());
        }
        String[] array = new String[list.size()];
        return (String[]) list.toArray(array);
    }

    // Returns null if file not found.
    private static File findImport(String className, String[] imports,
        List dirNames, String extension)
    {
        // Iterate through directories.
        final int size = dirNames.size();
        for (int i = 0; i < size; i++) {
            final String dirName = (String) dirNames.get(i);
            final File dir = File.getInstance(dirName);
            if (dir != null) {
                File file = findImport(className, imports, dir, ".java");
                if (file != null)
                    return file;
            }
        }
        return null;
    }

    // Returns null if file not found.
    public static File findImport(String className, String[] imports,
        File dir, String extension)
    {
        if (dir == null)
            return null;
        if (className.indexOf('.') >= 0) {
            // Canonical class name.
            String fileName =
                className.replace('.', SEPARATOR_CHAR).concat(extension);
            File file = File.getInstance(dir, fileName);
            return (file != null && file.isFile()) ? file : null;
        }
        if (imports != null) {
            String suffix = ".".concat(className);
            for (int i = 0; i < imports.length; i++) {
                String s = imports[i];
                if (s.endsWith(suffix)) {
                    // Found it!
                    String fileName =
                        s.replace('.', SEPARATOR_CHAR).concat(extension);
                    File file = File.getInstance(dir, fileName);
                    return (file != null && file.isFile()) ? file : null;
                }
                if (s.endsWith(".*")) {
                    String prefix = s.substring(0, s.length() - 1);
                    String canonicalName = prefix.concat(className);
                    String filename =
                        canonicalName.replace('.',
                            SEPARATOR_CHAR).concat(extension);
                    File file = File.getInstance(dir, filename);
                    if (file != null && file.isFile())
                        return file;
                }
            }
        }
        return null;
    }
}
