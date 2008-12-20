/*
 * Directory.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import gnu.regexp.RESyntax;
import gnu.regexp.UncheckedRE;
import java.awt.AWTEvent;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Vector;
import javax.swing.Icon;
import javax.swing.SwingUtilities;

public final class Directory extends Buffer
{
    private static final Preferences preferences = Editor.preferences();

    // This is the flag for this particular directory buffer.
    private boolean usingNativeFormat;

    public static final int SORT_BY_NAME = 0;
    public static final int SORT_BY_DATE = 1;
    public static final int SORT_BY_SIZE = 2;

    private int sortBy = SORT_BY_NAME;

    private String limitPattern;

    private ArrayList entries = new ArrayList();

    private int numMarked = 0;

    private DirectoryHistory history = new DirectoryHistory();

    private static final RE nativeMoveToFilenameRegExp;
    private static final RE internalMoveToFilenameRegExp;

    private boolean loadError;

    static {
        // Letters.
        final String letter = "[[:alpha:]]";

        // Month is 2 or more letters, possibly padded on the right with spaces.
        final String month = letter + letter + "+";

        // Year.
        final String yyyy = "[0-9][0-9][0-9][0-9]";

        // Day of month.
        final String dd = "[ 0-3][0-9][.]?";

        // Month and day (includes following space character).
        final String monthAndDay =
            "(" + month + " *" + dd + " " + "|" + dd + " " + month + " *" + ")";

        // Time of day.
        final String HHMM = "[ 0-2][0-9]:[0-5][0-9]";

        // Time or year.
        final String timeOrYear =
            "(" + HHMM + "|" + " " + yyyy + "|" + yyyy + " " + ")";

        RESyntax syntax = new RESyntax(RESyntax.RE_SYNTAX_PERL5);
        syntax.set(RESyntax.RE_CHAR_CLASSES);

        String traditional = "[0-9]+" + " " + monthAndDay + timeOrYear + " ";

        // --time-style=long-iso
        // -rw-r--r--    1 peter    peter         147 2002-11-13 13:10 notes
        // --time-style=iso
        // -rw-r--r--    1 peter    peter       69016 11-16 18:29 Directory.java
        // -rw-r--r--    1 peter    peter       13274 2001-09-08  thinbox.tar.gz
        final String isoMaybeYear = "(" + yyyy + "-)?";
        final String isoMonthAndDay = "[01][0-9]-[0-3][0-9]";
        final String isoMaybeTime = "(" + HHMM + ")?";
        String iso = "[0-9]+" + " " + isoMaybeYear + isoMonthAndDay + " " +
            isoMaybeTime + " *";

        // Mac OS X (Pete Kirkham)
        // --time-style="+%e %b  %Y"
        // -rw-r--r--    1 pete  pete    6065 23 Oct  2003 Directories.java
        // --time-style="+%e %b %H:%M"
        // -rw-r--r--    1 pete  pete   75350 21 May 19:58 Directory.java
        String osx = "[0-9]+" + " " + dd + " " + month + " " + timeOrYear + " ";

        nativeMoveToFilenameRegExp =
            new UncheckedRE("(" + traditional + ")|(" + iso + ")|(" + osx + ")",
                            0,
                            syntax);

        internalMoveToFilenameRegExp = new UncheckedRE(":[0-5][0-9]" + " ");
    }

    public static final RE getNativeMoveToFilenameRegExp()
    {
        return nativeMoveToFilenameRegExp;
    }

    public static final RE getInternalMoveToFilenameRegExp()
    {
        return internalMoveToFilenameRegExp;
    }

    public Directory(File dir)
    {
        super();
        supportsUndo = false;
        setFile(dir);
        type = TYPE_DIRECTORY;
        readOnly = true;
        mode = DirectoryMode.getMode();
        formatter = mode.getFormatter(this);
        setInitialized(true);
    }

    public Directory(File dir, String listing)
    {
        this(dir);
        setListing(listing);
    }

    public final boolean isUsingNativeFormat()
    {
        return usingNativeFormat;
    }

    public File getCurrentDirectory()
    {
        return getFile();
    }

    public int getSortBy()
    {
        return sortBy;
    }

    public Position getInitialDotPos()
    {
        Line line = getFirstLine();
        Line upLine = null; // We'll put dot here if directory is empty.
        while (true) {
            if (line.next() == null)
                break;
            String name = getName(line);
            if (name != null) {
                if (name.equals(".."))
                    upLine = line;
                else if (!name.equals("."))
                    return new Position(line, getNameOffset(line));
            }
            line = line.next();
        }
        // Directory is empty.
        if (upLine != null)
            return new Position(upLine, getNameOffset(upLine));
        else
            return new Position(line, getNameOffset(line));
    }

    private void setLimitPattern(String s)
    {
        if (s == null || s.length() == 0)
            limitPattern = null;
        else
            limitPattern = s;
    }

    public final String getLimitPattern()
    {
        return limitPattern;
    }

    public static void dirLimit()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            Directory directory = (Directory) buffer;
            InputDialog dialog = new InputDialog(editor, "Pattern:", "Limit",
                                                 directory.getLimitPattern());
            dialog.setHistory(new History("dirLimit"));
            editor.centerDialog(dialog);
            dialog.show();
            String pattern = dialog.getInput();
            // A null pattern means the user cancelled the input dialog.
            if (pattern != null) {
                editor.repaintNow();
                directory.limit(pattern);
            }
        }
    }

    public static void dirLimit(String pattern)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory)
            ((Directory)buffer).limit(pattern);
    }

    public static void dirUnlimit()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            Directory directory = (Directory) buffer;
            if (directory.getLimitPattern() != null)
                directory.limit(null);
        }
    }

    private void limit(String pattern)
    {
        if (pattern != null) {
            pattern = pattern.trim();
            if (pattern.length() == 0)
                pattern = null;
        }
        boolean reload = false;
        if (pattern == null && limitPattern != null)
            reload = true;
        else if (pattern != null && !pattern.equals(limitPattern))
            reload = true;
        if (reload) {
            final Editor editor = Editor.currentEditor();
            editor.setWaitCursor();
            String name = null;
            Line dotLine = editor.getDotLine();
            if (dotLine instanceof DirectoryLine)
                name = getName(dotLine);
            setLimitPattern(pattern);
            if (getListing() != null)
                reloadFromListing();
            else
                reload();
            Line line = findName(name);
            Position pos;
            if (line != null)
                pos = new Position(line, getNameOffset(line));
            else
                pos = getInitialDotPos();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.setTopLine(getFirstLine());
                    ed.setDot(pos);
                    ed.setMark(null);
                    ed.moveCaretToDotCol();
                    ed.setUpdateFlag(REFRAME | REPAINT);
                }
            }
            editor.setDefaultCursor();
        }
    }

    public void rescan()
    {
        final File file = getFile();
        if (file.isRemote())
            DirectoryCache.getDirectoryCache().purge(file.getHostName());
        reload();
        if (loadError)
            return;
        boolean rescanned = false;
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                if (ed.getSidebar() != null) {
                    NavigationComponent c = ed.getSidebar().getBottomComponent();
                    if (c instanceof DirectoryTree) {
                        DirectoryTree tree = (DirectoryTree) c;
                        DirectoryTreeModel treeModel = tree.getTreeModel();
                        if (!rescanned) {
                            treeModel.rescan(file);
                            rescanned = true;
                        }
                        tree.refresh();
                    }
                }
            }
        }
    }

    public synchronized void reload()
    {
        ArrayList editors = new ArrayList();

        // Remember the name of the current file in every editor.
        ArrayList names = new ArrayList();

        // Remember the line number in every editor.
        ArrayList lineNumbers = new ArrayList();

        // Remember the top line of the display in every editor.
        ArrayList topLineNumbers = new ArrayList();

        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            String name = null;
            int lineNumber = 0;
            int topLineNumber = 0;
            if (ed.getBuffer() == this) {
                name = getName(ed.getDotLine());
                lineNumber = ed.getDotLineNumber();
                topLineNumber = ed.getDisplay().getTopLineNumber();
            }
            editors.add(ed);
            names.add(name);
            lineNumbers.add(new Integer(lineNumber));
            topLineNumbers.add(new Integer(topLineNumber));
        }

        empty();
        entries.clear();
        numMarked = 0;
        setListing(null);

        load();

        // Restore the status quo in every window.
        for (int i = 0; i < editors.size(); i++) {
            final Editor ed = (Editor) editors.get(i);
            if (ed.getBuffer() == this) {
                Line dotLine = findName((String)names.get(i));
                if (dotLine == null) {
                    dotLine =
                        getLine(((Integer)lineNumbers.get(i)).intValue());
                    if (dotLine == null) {
                        if (getFirstLine() == null) {
                            Debug.bug();
                            appendLine("");
                        }
                        dotLine = getFirstLine();
                        while (dotLine.next() != null)
                            dotLine = dotLine.next();
                    }
                }
                ed.setDot(dotLine, getNameOffset(dotLine));
                ed.setMark(null);
                final Display display = ed.getDisplay();
                display.setShift(0);
                display.moveCaretToDotCol();

                Line line =
                    getLine(((Integer)topLineNumbers.get(i)).intValue());
                if (line == null) {
                    Debug.assertTrue(getFirstLine() != null);
                    line = getFirstLine();
                    while (line.next() != null)
                        line = line.next();
                }
                display.setTopLine(line);
                display.setUpdateFlag(REPAINT);
                ed.updateLocation();
            }
        }
    }

    private synchronized void reloadFromListing()
    {
        Debug.assertTrue(getListing() != null);
        empty();
        entries.clear();
        numMarked = 0;
        load();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                final Line line = getFirstLine();
                ed.setDot(line, getNameOffset(line));
                ed.setMark(null);
                final Display display = ed.getDisplay();
                display.setShift(0);
                display.moveCaretToDotCol();
                display.setTopLine(getFirstLine());
                display.setUpdateFlag(REPAINT);
                ed.updateLocation();
            }
        }
    }

    // Called only from synchronized methods.
    private void addEntry(String name)
    {
        Debug.assertTrue(!usingNativeFormat);
        File f = File.getInstance(getFile(), name);
        if (f == null)
            return;
        DirectoryEntry de;
        if (f.isDirectory())
            de = new DirectoryEntry(name, f.lastModified(), 0, true);
        else
            de = new DirectoryEntry(name, f.lastModified(), f.length());
        if (!name.equals(".") && !name.equals("..")) {
            try {
                String cp = f.getCanonicalPath();
                String ap = f.getAbsolutePath();
                if (!cp.equals(ap)) {
                    final String canonicalPath = getFile().canonicalPath();
                    if (cp.startsWith(canonicalPath + LocalFile.getSeparator()))
                        de.setLinkedTo(cp.substring(canonicalPath.length()+1));
                    else
                        de.setLinkedTo(cp);
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        entries.add(de);
    }

    private final void appendLine(DirectoryEntry entry)
    {
        appendLine(new DirectoryLine(entry));
    }

    private synchronized DirectoryEntry findEntry(String name)
    {
        for (int i = 0; i < entries.size(); i++) {
            DirectoryEntry entry = (DirectoryEntry) entries.get(i);
            if (name.equals(entry.getName()))
                return entry;
        }
        return null;
    }

    private synchronized DirectoryEntry findNativeEntry(String string)
    {
        for (int i = 0; i < entries.size(); i++) {
            DirectoryEntry entry = (DirectoryEntry) entries.get(i);
            if (string.equals(entry.getString()))
                return entry;
        }
        return null;
    }

    private synchronized void sort()
    {
        if (usingNativeFormat) {
            Debug.bug();
            return;
        }
        if (sortBy == SORT_BY_NAME)
            sortByName();
        else if (sortBy == SORT_BY_DATE)
            sortByDate();
        else if (sortBy == SORT_BY_SIZE)
            sortBySize();
    }

    // Called only from sort().
    private void sortByName()
    {
        Debug.assertTrue(!usingNativeFormat);
        Comparator comparator = new Comparator() {
            public int compare(Object o1, Object o2)
            {
                String name1 = ((DirectoryEntry)o1).getName();
                String name2 = ((DirectoryEntry)o2).getName();
                return name1.compareToIgnoreCase(name2);
            }
        };
        Collections.sort(entries, comparator);
    }

    // Called only from sort().
    private void sortByDate() {
        Comparator comparator = new Comparator() {
            public int compare(Object o1, Object o2)
            {
                // Most recent dates first.
                long date1 = ((DirectoryEntry)o1).getDate();
                long date2 = ((DirectoryEntry)o2).getDate();
                if (date1 > date2)
                    return -1;
                if (date1 < date2)
                    return 1;
                return 0;
            }
        };
        Collections.sort(entries, comparator);
    }

    // Called only from sort().
    private void sortBySize()
    {
        Comparator comparator = new Comparator() {
            public int compare(Object o1, Object o2)
            {
                // Biggest files first.
                long size1 = ((DirectoryEntry)o1).getSize();
                long size2 = ((DirectoryEntry)o2).getSize();
                if (size1 > size2)
                    return -1;
                if (size1 < size2)
                    return 1;
                return 0;
            }
        };
        Collections.sort(entries, comparator);
    }

    // Called only from synchronized methods.
    private void addEntriesToBuffer()
    {
        final int size = entries.size();
        if (preferences.getBooleanProperty(Property.DIR_SORT_DIRECTORIES_FIRST, !usingNativeFormat)) {
            // Add lines to the buffer in two passes so directories will always be on top.
            for (int i = 0; i < size; i++) {
                DirectoryEntry entry = (DirectoryEntry) entries.get(i);
                if (entry.isDirectory())
                    appendLine(entry);
            }
            for (int i = 0; i < size; i++) {
                DirectoryEntry entry = (DirectoryEntry) entries.get(i);
                if (!entry.isDirectory())
                    appendLine(entry);
            }
        } else {
            for (int i = 0; i < size; i++) {
                DirectoryEntry entry = (DirectoryEntry) entries.get(i);
                appendLine(entry);
            }
        }
        if (getFirstLine() == null)
            appendLine("");
        setLoaded(true);
    }

    public static void dirCycleSortBy()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            editor.setWaitCursor();
            ((Directory)buffer).cycleSortBy();
            editor.setDefaultCursor();
        }
    }

    private synchronized void cycleSortBy()
    {
        if (sortBy == SORT_BY_NAME)
            sortBy = SORT_BY_DATE;
        else if (sortBy == SORT_BY_DATE)
            sortBy = SORT_BY_SIZE;
        else
            sortBy = SORT_BY_NAME;
        resort();
    }

    public void resort(int sortBy)
    {
        if (this.sortBy != sortBy) {
            this.sortBy = sortBy;
            resort();
        }
    }

    private void resort()
    {
        if (usingNativeFormat) {
            reload();
        } else {
            sort();
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return; // Shouldn't happen.
            }
            try {
                empty();
                addEntriesToBuffer();
                renumber();
            }
            finally {
                unlockWrite();
            }
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.setDot(getFirstLine(), 0);
                    ed.setMark(null);
                    final Display display = ed.getDisplay();
                    display.setTopLine(getFirstLine());
                    display.setShift(0);
                    display.setCaretCol(0);
                    display.setUpdateFlag(REPAINT);
                    ed.updateLocation();
                }
            }
        }
    }

    private synchronized void loadInternal()
    {
        // Default is true for Unix, false otherwise. User can override default.
        boolean useNativeFormat =
            preferences.getBooleanProperty(Property.DIR_USE_NATIVE_FORMAT,
                                           Platform.isPlatformUnix());
        if (useNativeFormat) {
            if (!Utilities.haveLs())
                useNativeFormat = false;
        }
        loadError = false;
        try {
            final DirectoryFilenameFilter dff;
            if (limitPattern != null)
                dff = new DirectoryFilenameFilter(limitPattern);
            else
                dff = null;
            final File file = getFile();
            if (useNativeFormat || file.isRemote()) {
                usingNativeFormat = true;
                String flags = "-la"; // Default is sort by name.
                if (sortBy == SORT_BY_DATE)
                    flags = "-lat";
                else if (sortBy == SORT_BY_SIZE)
                    flags = "-laS";
                String extraOptions =
                    getStringProperty(Property.LS_EXTRA_OPTIONS);
                if (extraOptions != null) {
                    flags += " ";
                    flags += extraOptions;
                    Log.debug("Directory.loadInternal flags = " + flags);
                }
                BufferedReader reader = null;
                if (getListing() != null) {
                    reader = new BufferedReader(new StringReader(getListing()));
                } else if (file instanceof FtpFile || file instanceof SshFile) {
                    setListing(file.getDirectoryListing());
                    if (getListing() != null)
                        reader = new BufferedReader(new StringReader(getListing()));
                    else
                        loadError = true;
                } else {
                    // Local file.
                    Process process = null;
                    if (Platform.isPlatformUnix()) {
                        String cmd =
                            "(\\cd \"" + file.canonicalPath() + "\" && \\ls " +
                            flags + ")";
                        String[] cmdarray = {"/bin/sh", "-c", cmd};
                        process = Runtime.getRuntime().exec(cmdarray);
                    } else {
                        // Windows.
                        String cp = file.canonicalPath();
                        // Convert "C:\" into "//c" for Cygwin ls.
                        if (cp.length() == 3 && cp.charAt(1) == ':' && cp.charAt(2) == '\\')
                            cp = "//" + Character.toLowerCase(cp.charAt(0));
                        String[] cmdarray = {"ls", flags, cp};
                        process = Runtime.getRuntime().exec(cmdarray);
                    }
                    reader =
                        new BufferedReader(new InputStreamReader(process.getInputStream()));
                }
                if (reader != null) {
                    String s;
                    while ((s = reader.readLine()) != null) {
                        DirectoryEntry entry =
                            DirectoryEntry.getDirectoryEntry(s, dff);
                        if (entry != null)
                            entries.add(entry);
                    }
                }
            } else {
                usingNativeFormat = false;
                boolean dirIsRoot = false;
                if (Platform.isPlatformWindows()) {
                    String cp = file.canonicalPath();
                    if (cp.length() == 3 && cp.endsWith(":\\")) // "C:\"
                        dirIsRoot = true;
                }
                if (!dirIsRoot) {
                    addEntry(".");
                    addEntry("..");
                }
                String[] names = file.list();
                if (names != null) {
                    if (dff == null) {
                        for (int i = 0; i < names.length; i++)
                            addEntry(names[i]);
                    } else {
                        for (int i = 0; i < names.length; i++) {
                            if (dff.accepts(names[i]))
                                addEntry(names[i]);
                            else {
                                File f = File.getInstance(file, names[i]);
                                if (f != null && f.isDirectory())
                                    addEntry(names[i]);
                            }
                        }
                    }
                }
                sort();
            }
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return; // Shouldn't happen.
            }
            try {
                addEntriesToBuffer();
                long totalSize = getTotalSize();
                if (totalSize > 0) {
                    int end;
                    if (usingNativeFormat) {
                        end = getFileSizeEndOffset();
                        if (end <= 0)
                            end = 45;
                    } else {
                        int nameOffset = getNameOffset();
                        end = nameOffset - 19;
                        if (end <= 0)
                            end = 13;
                    }
                    String s = String.valueOf(totalSize);
                    int begin = end - s.length();
                    if (begin < 0)
                        begin = 0;
                    FastStringBuffer sb =  new FastStringBuffer(80);
                    sb.append(Utilities.spaces(begin));
                    for (int i = s.length(); i > 0; i--)
                        sb.append('-');
                    appendLine(sb.toString());
                    sb.setLength(0);
                    sb.append(Utilities.spaces(begin));
                    sb.append(s);
                    appendLine(sb.toString());
                }
                renumber();
            }
            finally {
                unlockWrite();
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private long getTotalSize()
    {
        long totalSize = 0;
        int endOffset = -1;
        final int limit = entries.size();
        for (int i = 0; i < limit; i++) {
            DirectoryEntry entry = (DirectoryEntry) entries.get(i);
            long size = entry.getSize();
            if (size >= 0) {
                totalSize += size;
                continue;
            }
            String text = entry.getString();
            if (endOffset < 0) {
                REMatch match = nativeMoveToFilenameRegExp.getMatch(text);
                if (match != null) {
                    // The file size is followed by a single space.
                    endOffset = text.indexOf(' ', match.getStartIndex());
                }
                if (endOffset < 0)
                    endOffset = 42;
            }
            if (endOffset < text.length()) {
                int end;
                if (text.charAt(endOffset) == ' ') {
                    // Expected.
                    end = endOffset;
                } else {
                    // Encountered unexpected char at endOffset.
                    REMatch match = nativeMoveToFilenameRegExp.getMatch(text);
                    if (match == null)
                        return totalSize = -1;
                    end = text.indexOf(' ', match.getStartIndex());
                    if (end < endOffset) {
                        // Correct anomaly.
                        endOffset = end;
                        Log.debug("endOffset = " + endOffset);
                    }
                }
                int begin = text.lastIndexOf(' ', end - 1);
                if (begin < 0)
                    return -1;
                try {
                    size = Long.parseLong(text.substring(begin + 1, end));
                    entry.setSize(size);
                    totalSize += size;
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                    return -1;
                }
            } else
                return -1; // Shouldn't happen.
        }
        return totalSize;
    }

    public int load()
    {
        if (!isLoaded()) {
            final Editor editor = Editor.currentEditor();
            String reading = "Reading directory...";
            if (editor != null)
                editor.status(reading);
            loadInternal();
            if (editor != null)
                editor.status(reading + "done");
        }
        return LOAD_COMPLETED;
    }

    public void tagFileAtDot()
    {
        final Editor editor = Editor.currentEditor();
        Line line = editor.getDotLine();
        if (!(line instanceof DirectoryLine))
            return;
        String name = getName(line);
        if (name == null)
            return;
        if (name.equals("."))
            return;
        if (name.equals(".."))
            return;
        DirectoryEntry de = null;
        if (usingNativeFormat) {
            // Text of line has "T " or "  " prepended to DirectoryEntry string.
            de = findNativeEntry(line.substring(2));
        } else {
            de = findEntry(name);
        }
        if (de != null) {
            if (de.isMarked()) {
                de.setMarked(false);
                --numMarked;
            } else {
                de.setMarked(true);
                ++numMarked;
            }
            line.setText(de.toString());
            editor.update(line);
            editor.down();
            resetUndo();
        }
    }

    public void upDir()
    {
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        final File parent = getFile().getParentFile();
        if (parent == null)
            return;
        history.truncate();
        history.append(getFile(), getName(editor.getDotLine()), editor.getDotOffset());
        history.reset();
        String name = getFile().getName();
        setFile(parent);
        reload();
        Line line = findName(name);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                if (line != null)
                    ed.setDot(line, getNameOffset(line));
                else
                    ed.setDot(getFirstLine(), getNameOffset(getFirstLine()));
                ed.moveCaretToDotCol();
            }
        }
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
    }

    public String getPathAtDot()
    {
        Editor editor = Editor.currentEditor();
        if (!(editor.getDotLine() instanceof DirectoryLine))
            return null;
        String name = getName(editor.getDotLine());
        if (name == null)
            return null;
        final File dir = getFile();
        if (name.equals("."))
            return dir.netPath();
        if (name.equals("..")) {
            File parent = dir.getParentFile();
            if (parent == null)
                return null;
            else
                return parent.netPath();
        }
        File file = File.getInstance(dir, name);
        if (file == null)
            return null;
        else
            return file.netPath();
    }

    public static void dir()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory)
            return;
        File directory = editor.getCurrentDirectory();
        if (directory == null)
            return;
        if (directory.getProtocol() == File.PROTOCOL_HTTP)
            return;
        // FTP, SSH or local.
        Buffer buf = Editor.getBuffer(directory);
        // buf may be a RemoteBuffer and not specifically a Directory.
        if (buf != null) {
            File file = buffer.getFile();
            editor.makeNext(buf);
            editor.switchToBuffer(buf);
            if (buf instanceof Directory && file != null) {
                Directory dir = (Directory) buf;
                Line line = dir.findName(file.getName());
                if (line != null) {
                    editor.setDot(new Position(line, dir.getNameOffset(line)));
                    editor.setUpdateFlag(REFRAME);
                    editor.reframe();
                }
            }
        }
    }

    public static void dirOpenFile()
    {
        _dirOpenFile(false);
    }

    public static void dirOpenFileAndKillDirectory()
    {
        _dirOpenFile(true);
    }

    private static void _dirOpenFile(boolean killDirectory)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            // If this method is invoked via a mouse event mapping, move dot to
            // location of mouse click first.
            AWTEvent e = editor.getDispatcher().getLastEvent();
            if (e instanceof MouseEvent)
                editor.mouseMoveDotToPoint((MouseEvent) e);
            ((Directory)buffer).openFileAtDot(killDirectory);
        }
    }

    private synchronized void openFileAtDot(boolean killDirectory)
    {
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        if (!(editor.getDotLine() instanceof DirectoryLine))
            return;
        String name = getName(editor.getDotLine());
        if (name == null)
            return;
        if (name.equals("."))
            return;
        if (name.equals("..")) {
            upDir();
            return;
        }
        final File dir = getFile();
        if (dir.isRemote()) {
            String fullpath = dir.canonicalPath();
            if (!fullpath.endsWith("/"))
                fullpath += "/";
            fullpath += name;
            File newFile = File.getInstance(dir, fullpath);
            Buffer buf = Editor.getBufferList().findBuffer(newFile);
            if (buf != null) {
                editor.makeNext(buf);
                editor.activate(buf);
                return;
            }
            DirectoryLine line = (DirectoryLine) editor.getDotLine();
            DirectoryEntry de = line.getDirectoryEntry();
            boolean isDirectory = false;
            if (de.isDirectory())
                isDirectory = true;
            else if (de.isLink())
                isDirectory = newFile.isDirectory();
            if (isDirectory) {
                setBusy(true);
                history.truncate();
                history.append(dir, getName(editor.getDotLine()),
                    editor.getDotOffset());
                history.reset();
                empty();
                entries.clear();
                numMarked = 0;
                setListing(null);
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == this) {
                        ed.setTopLine(null);
                        ed.setDot(null);
                        ed.setMark(null);
                    }
                }
                setFile(newFile);
                Runnable reloadRunnable = new Runnable() {
                    public void run()
                    {
                        load();
                        Runnable updateRunnable = new Runnable() {
                            public void run()
                            {
                                setBusy(false);
                                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                                    Editor ed = it.nextEditor();
                                    if (ed.getBuffer() == Directory.this) {
                                        ed.setTopLine(getFirstLine());
                                        ed.setDot(getInitialDotPos());
                                        ed.setMark(null);
                                        ed.moveCaretToDotCol();
                                        ed.setUpdateFlag(REPAINT);
                                        ed.updateDisplay();
                                        ed.updateLocation();
                                    }
                                }
                                Sidebar.setUpdateFlagInAllFrames(SIDEBAR_ALL);
                            }
                        };
                        SwingUtilities.invokeLater(updateRunnable);
                    }
                };
                new Thread(reloadRunnable).start();
            } else {
                // Not a directory.
                if (newFile instanceof FtpFile)
                    buf = new RemoteBuffer((FtpFile) newFile,
                        FtpSession.getSession((FtpFile) newFile));
                else if (newFile instanceof SshFile)
                    buf = new RemoteBuffer(newFile);
                else
                    Debug.assertTrue(false);
                editor.makeNext(buf);
                editor.activate(buf);
            }
            return;
        }

        // Local file.
        File f = File.getInstance(getFile(), name);
        if (!f.exists()) {
            editor.status("File not found");
            return;
        }
        if (f.isDirectory()) {
            changeDirectory(f);
            return;
        }
        Buffer buf = editor.getBuffer(f);
        if (buf != null) {
            editor.makeNext(buf);
            editor.activate(buf);
        }
        if (killDirectory)
            kill();
    }

    public synchronized void changeDirectory(File f)
    {
        if (f.isDirectory()) {
            final Editor editor = Editor.currentEditor();
            if (f.isLocal() && !f.canRead()) {
                showMessageDialog("Directory is not readable");
                return;
            }
            final String name;
            final int offset;
            if (editor.getDot() != null) {
                name = getName(editor.getDotLine());
                offset = editor.getDotOffset();
            } else {
                name = null;
                offset = 0;
            }
            history.truncate();
            history.append(getFile(), name, offset);
            history.reset();
            empty();
            entries.clear();
            numMarked = 0;
            setListing(null);
            setFile(f);
            load();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    ed.setTopLine(getFirstLine());
                    ed.setDot(getInitialDotPos());
                    ed.setMark(null);
                    ed.moveCaretToDotCol();
                    ed.setUpdateFlag(REPAINT);
                    ed.updateLocation();
                }
            }
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
        }
    }

    // No history.
    private synchronized void changeDirectory(DirectoryHistoryEntry entry)
    {
        empty();
        entries.clear();
        numMarked = 0;
        setListing(null);
        setFile(entry.file);
        load();
        Line line = findName(entry.name);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.getDisplay().setTopLine(getFirstLine());
                ed.setUpdateFlag(REPAINT);
                if (line != null)
                    ed.setDot(line, entry.offset);
                else
                    ed.setDot(getFirstLine(), getNameOffset(getFirstLine()));
                ed.setMark(null);
                ed.moveCaretToDotCol();
                ed.updateLocation();
            }
        }
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
    }

    public static void dirBack()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            editor.setWaitCursor();
            ((Directory)buffer).back(editor);
            editor.setDefaultCursor();
        }
    }

    private void back(Editor editor)
    {
        boolean atEnd = history.atEnd();
        DirectoryHistoryEntry entry = history.getPrevious();
        if (entry != null) {
            if (atEnd) {
                String name = getName(editor.getDotLine());
                int offset = editor.getDotOffset();
                history.append(getFile(), name, offset);
            }
            changeDirectory(entry);
        } else
            editor.status("Can't go back");
    }

    public static void dirForward()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            editor.setWaitCursor();
            ((Directory)buffer).forward(editor);
            editor.setDefaultCursor();
        }
    }

    private void forward(Editor editor)
    {
        DirectoryHistoryEntry entry = history.getNext();
        if (entry != null)
            changeDirectory(entry);
        else
            editor.status("Can't go forward");
    }

    public void browseFileAtDot()
    {
        Editor editor = Editor.currentEditor();
        String name = getName(editor.getDotLine());
        if (name == null)
            return;
        File f = File.getInstance(getFile(), name);
        if (!f.exists()) {
            editor.status("File not found");
            return;
        }
        if (f.isFile()) {
            String browser = preferences.getStringProperty(Property.BROWSER);
            if (browser == null || browser.equals("j")) {
                WebBuffer.browse(editor, f, null);
                return;
            }
            // Use external browser.
            try {
                String url = "file://".concat(f.canonicalPath());
                String browserOpts =
                    preferences.getStringProperty(Property.BROWSER_OPTS);
                if (browserOpts != null) {
                    String[] cmdarray = {browser, browserOpts, url};
                    Process process = Runtime.getRuntime().exec(cmdarray);
                } else {
                    String[] cmdarray = {browser, url};
                    Process process = Runtime.getRuntime().exec(cmdarray);
                }
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    public void deleteFiles()
    {
        if (numMarked > 0)
            deleteMarkedFiles();
        else
            deleteFileAtDot();
    }

    private void showMessageDialog(String message)
    {
        MessageDialog.showMessageDialog(message, "Error");
    }

    private synchronized void deleteMarkedFiles()
    {
        if (numMarked == 0)
            return;
        Debug.assertTrue(numMarked > 0);
        String message = null;
        final Editor editor = Editor.currentEditor();
        if (numMarked > 1)
            message = "Delete " + numMarked + " tagged files?";
        else
            message = "Delete 1 tagged file?";
        boolean confirmed = editor.confirm("Delete Files", message);
        if (!confirmed)
            return;
        boolean directoryWasDeleted = false;
        FtpSession session = null;
        for (int i = 0; i < entries.size(); i++) {
            DirectoryEntry entry = (DirectoryEntry) entries.get(i);
            if (!entry.isMarked())
                continue;
            String name = entry.extractName();
            if (name == null) {
                // Shouldn't happen.
                Debug.assertTrue(false);
                continue;
            }
            File file = File.getInstance(getFile(), name);
            if (!file.isRemote() && !file.exists()) {
                showMessageDialog(file.canonicalPath() + " not found");
                return;
            }
            String displayName = file.canonicalPath();
            boolean isDirectory;
            if (file.isRemote()) {
                displayName += " on " + file.getHostName();
                isDirectory = entry.isDirectory();
            }
            else
                isDirectory = file.isDirectory();
            boolean succeeded = false;
            if (file instanceof FtpFile) {
                if (session == null)
                    session = FtpSession.getSession((FtpFile)file);
                if (session != null) {
                    if (isDirectory)
                        succeeded = session.removeDirectory(file.canonicalPath());
                    else
                        succeeded = session.deleteFile(file.canonicalPath());
                }
            } else {
                // Local file.
                // Delete the file on disk.
                file.delete();

                // Did it work?
                succeeded = !file.exists();
            }
            if (succeeded) {
                if (isDirectory)
                    directoryWasDeleted = true;
            } else {
                // Deletion failed.
                if (isDirectory) {
                    confirmed = editor.confirm("Delete Files",
                        "Unable to remove directory " + displayName + ".  Continue?");
                } else {
                    confirmed = editor.confirm("Delete Files",
                        "Unable to delete " + displayName + ".  Continue?");
                }
                if (!confirmed)
                    break;
            }
        }
        if (session != null)
            session.unlock();
        if (directoryWasDeleted) {
            rescan();
        } else {
            if (getFile().isRemote())
                DirectoryCache.getDirectoryCache().purge(getFile());
            reload();
        }
    }

    private void deleteFileAtDot()
    {
        final Editor editor = Editor.currentEditor();
        String name = getName(editor.getDotLine());
        if (name == null)
            return;
        if (name.equals("."))
            return;
        if (name.equals(".."))
            return;
        File file = File.getInstance(getFile(), name);
        if (!file.isRemote() && !file.exists()) {
            showMessageDialog("File not found");
            return;
        }
        String displayName = file.getName();
        final boolean isDirectory;
        if (file.isRemote()) {
            displayName += " on " + file.getHostName();
            isDirectory = editor.getDotLine().getText().trim().startsWith("d");
        } else
            isDirectory = file.isDirectory();
        final String title, prompt;
        if (isDirectory) {
            title = "Remove Directory";
            prompt = "Remove directory " + displayName + "?";
        } else {
            title = "Delete File";
            prompt = "Delete " + displayName + "?";
        }
        if (!editor.confirm(title, prompt))
            return;
        boolean succeeded = false;
        editor.setWaitCursor();
        if (file instanceof FtpFile) {
            FtpSession session = FtpSession.getSession((FtpFile) file);
            if (session != null) {
                if (isDirectory)
                    succeeded = session.removeDirectory(file.canonicalPath());
                else
                    succeeded = session.deleteFile(file.canonicalPath());
                session.unlock();
            }
        } else {
            // Local file.
            file.delete();
            succeeded = !file.exists();
        }
        if (succeeded) {
            if (isDirectory) {
                rescan();
            } else {
                if (getFile().isRemote())
                    DirectoryCache.getDirectoryCache().purge(getFile());
                reload();
            }
        }
        editor.setDefaultCursor();
        if (!succeeded) {
            if (isDirectory)
                showMessageDialog("Unable to remove directory " + displayName);
            else
                showMessageDialog("Unable to delete " + displayName);
        }
    }

    public static void dirDoShellCommand()
    {
        if (!Editor.checkExperimental())
            return;
        if (!Platform.isPlatformUnix())
            return;
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            if (buffer.getFile().isRemote()) {
                MessageDialog.showMessageDialog(editor,
                    "Shell commands are not supported in remote directory buffers.",
                    "Error");
                return;
            }
            ((Directory)buffer).doShellCommand(editor);
        }
    }

    private void doShellCommand(Editor editor)
    {
        List names = getMarkedNames();
        if (names == null) {
            names = new ArrayList();
            names.add(getName(editor.getDotLine()));
        }
        String prompt = null;
        if (names.size() == 1)
            prompt = "Command (on " + (String) names.get(0) + "):";
        else if (names.size() > 1)
            prompt = "Command (on " + names.size() + " tagged files):";
        else
            prompt = "Command:";
        InputDialog d = new InputDialog(editor, prompt, "Shell Command", null);
        d.setHistory(new History("dirDoShellCommand.command"));
        editor.centerDialog(d);
        d.show();
        String command = d.getInput();
        if (command == null)
            return;
        command = command.trim();
        if (command.length() == 0)
            return;
        editor.setWaitCursor();
        editor.repaintNow();
        String output = null;
        if (names != null) {
            if (command.indexOf('*') >= 0) {
                output = doCommandOnMultipleFiles(command, names);
            } else {
                FastStringBuffer sb = new FastStringBuffer();
                for (int i = 0; i < names.size(); i++) {
                    String filename = (String) names.get(i);
                    String s = doCommandOnFile(command, filename);
                    if (s != null && s.length() > 0)
                        sb.append(s);
                }
                output = sb.toString();
            }
        }
        reload();
        if (output != null && output.length() > 0) {
            OutputBuffer buf = OutputBuffer.getOutputBuffer(output);
            if (buf != null) {
                buf.setTitle(command);
                editor.makeNext(buf);
                editor.displayInOtherWindow(buf);
            }
        }
        editor.setDefaultCursor();
    }

    private String doCommandOnFile(String command, String filename)
    {
        FastStringBuffer sb = new FastStringBuffer(command);
        sb.append(' ');
        if (filename.indexOf(' ') >= 0) {
            sb.append('"');
            sb.append(filename);
            sb.append('"');
        } else
            sb.append(filename);
        ShellCommand shellCommand = new ShellCommand(sb.toString(), getFile());
        shellCommand.run();
        return shellCommand.getOutput();
    }

    private String doCommandOnMultipleFiles(String command, List files)
    {
        if (files.size() < 1)
            return null;
        String before, after;
        int index = command.indexOf('*');
        if (index >= 0) {
            before = command.substring(0, index).trim();
            after = command.substring(index + 1).trim();
        } else {
            before = command.trim();
            after = "";
        }
        if (before.length() == 0) {
            showMessageDialog("No command specified");
            return null;
        }
        FastStringBuffer sb = new FastStringBuffer(before);
        for (int i = 0; i < files.size(); i++) {
            sb.append(' ');
            String filename = (String) files.get(i);
            if (filename.indexOf(' ') >= 0) {
                sb.append('"');
                sb.append(filename);
                sb.append('"');
            } else
                sb.append(filename);
        }
        if (after.length() > 0) {
            sb.append(' ');
            sb.append(after);
        }
        ShellCommand shellCommand = new ShellCommand(sb.toString(), getFile());
        shellCommand.run();
        return shellCommand.getOutput();
    }

    private synchronized List getMarkedNames()
    {
        if (numMarked > 0) {
            ArrayList names = new ArrayList(numMarked);
            final int size = entries.size();
            for (int i = 0; i < size; i++) {
                DirectoryEntry de = (DirectoryEntry) entries.get(i);
                if (de.isMarked()) {
                    String name = de.extractName();
                    if (name != null)
                        names.add(name);
                }
            }
            return names;
        }
        return null;
    }

    public void copyFileAtDot()
    {
        copyFiles();
    }

    public void moveFileAtDot()
    {
        moveFiles();
    }

    private List getSourceFiles(Editor editor)
    {
        ArrayList sources = new ArrayList();
        if (numMarked > 0) {
            List names = getMarkedNames();
            for (int i = 0; i < names.size(); i++) {
                String name = (String) names.get(i);
                if (name != null)
                    sources.add(File.getInstance(getFile(), name));
            }
        } else if (editor.getDotLine() instanceof DirectoryLine) {
            String name = getName(editor.getDotLine());
            if (name != null)
                sources.add(File.getInstance(getFile(), name));
        }
        return sources;
    }

    private File getDestinationForCopy(Editor editor, List sources)
    {
        return getDestination(editor, sources, "Copy");
    }

    private File getDestinationForMove(Editor editor, List sources)
    {
        return getDestination(editor, sources, "Move");
    }

    private File getDestination(Editor editor, List sources, String operation)
    {
        String title = operation + " File";
        String prompt = operation + " ";
        String name = null;
        if (sources.size() == 1) {
            File source = (File) sources.get(0);
            name = source.getName();
            prompt += name;
        } else
            prompt += String.valueOf(sources.size()) + " tagged files";
        prompt += " to: ";
        CopyFileDialog d = new CopyFileDialog(editor, title, prompt, name);
        editor.centerDialog(d);
        d.show();
        editor.repaintNow();
        return d.getDestination();
    }

    private void copyFiles()
    {
        final Editor editor = Editor.currentEditor();
        List sources = getSourceFiles(editor);
        File destination = getDestinationForCopy(editor, sources);
        if (destination == null)
            return;
        final String title = "Copy Files";
        if (destination.isLocal())
            copyLocalToLocal(sources, destination, editor, title);
        else
            MessageDialog.showMessageDialog(editor, "Destination must be local!", title);
    }

    private void copyLocalToLocal(List sources, File destination, Editor editor, String title)
    {
        int numFilesCopied = 0;
        boolean mustConfirm = true;
        boolean cancelled = false;
        final int limit = sources.size();
        File destDir = destination.isDirectory() ? destination : destination.getParentFile();
        for (int i = 0; i < limit; i++) {
            File from = (File) sources.get(i);
            File to;
            if (destination.isDirectory())
                to = File.getInstance(destination, from.getName());
            else
                to = destination;
            if (mustConfirm && to.isFile()) {
                String message = "Overwrite existing file " + to.canonicalPath() + "?";
                if (i < limit-1) {
                    // More than one file is left. Provide "Yes To All" and
                    // "Cancel" buttons.
                    int response = editor.confirmAll(title, message);
                    switch (response) {
                        case RESPONSE_YES:
                            break;
                        case RESPONSE_NO:
                            continue;
                        case RESPONSE_YES_TO_ALL:
                            mustConfirm = false;
                            break;
                        case RESPONSE_CANCEL:
                            cancelled = true;
                            break;
                        default:
                            break;
                    }
                    if (cancelled)
                        break;
                } else if (!editor.confirm(title, message)) {
                    // Last file (or only file).
                    break;
                }
            }
            if (Utilities.copyFile(from, to)) {
                ++numFilesCopied;
            } else {
                String text = "Unable to copy " + from.getName() + " to " + to.canonicalPath() + ".";
                if (i < limit-1) {
                    text += " Continue?";
                    if (!editor.confirm(title, text))
                        break;
                } else
                    MessageDialog.showMessageDialog(editor, text, title);
            }
        }
        String statusText = String.valueOf(numFilesCopied) + " file";
        if (numFilesCopied > 1)
            statusText += 's';
        statusText += " copied";
        editor.status(statusText);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (destDir.equals(ed.getBuffer().getFile())) {
                ((Directory)ed.getBuffer()).reload();
                if (ed != Editor.currentEditor())
                    ed.updateDisplay();
            }
        }
    }

    private void moveFiles()
    {
        final Editor editor = Editor.currentEditor();
        List sources = getSourceFiles(editor);
        File destination = getDestinationForMove(editor, sources);
        if (destination == null)
            return;
        Log.debug("ready to move");
        if (destination.isRemote()) {
            MessageDialog.showMessageDialog(editor,
                "Destination must be local!", "Move Files");
            return;
        }
        final int count = sources.size();
        for (int i = 0; i < count; i++) {
            File source = (File) sources.get(i);

            // Move.
            boolean success = source.renameTo(destination);

            if (!success) {
                Log.warn("renameTo failed; trying copyFile...");
                success = Utilities.copyFile(source, destination);
                if (success) {
                    Log.debug("copyFile succeeded, deleting source...");
                    source.delete();
                    success = !source.exists();
                }
                else
                    Log.error("copyFile failed");
            }
            else
                Log.debug("renameTo succeeded");

            if (success) {
                // Change file information for any buffers (there should only
                // be one!) associated with moved file.
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer buf = it.nextBuffer();
                    if (source.equals(buf.getFile()))
                        buf.changeFile(destination);
                }
            } else {
                MessageDialog.showMessageDialog(editor, "Move failed", "Error");
                break;
            }
        }

        // Current directory may have changed.
        reload();
    }

    public void getFileAtDot()
    {
        final Editor editor = Editor.currentEditor();
        final Line dotLine = editor.getDotLine();
        final String name = getName(dotLine);
        if (name == null)
            return;

        final FtpFile sourceFile = (FtpFile) File.getInstance(getFile(), name);
        if (sourceFile == null)
            return;

        int status = -1;

        // We'll get in trouble here if the directory format changes.
        if (editor.getDotLine().length() >= 3) {
            char c = dotLine.charAt(2);
            if (c == 'd') {
                status = 2; // Directory.
            } else if (c == '-') {
                status = 1; // Normal file.
            }
        }

        if (status == -1) {
            // Symbolic link.
            MessageDialog.showMessageDialog("Unknown file type", "Get File");
            return;
        }
        if (status == 2) {
            MessageDialog.showMessageDialog(
                sourceFile.canonicalPath() + " is a directory",
                "Get File");
            return;
        }
        if (status != 1) {
            MessageDialog.showMessageDialog("File not found", "Get File");
            return;
        }

        CopyFileDialog d = new CopyFileDialog(editor, "Get File", "Destination:", name);
        d.setConfirmOverwrite(true);
        editor.centerDialog(d);
        d.show();
        final File destination = d.getDestination();
        editor.repaintNow();
        if (destination == null)
            return;
        if (destination.isRemote()) {
            MessageDialog.showMessageDialog(editor, "Destination must be local!", title);
            return;
        }
        final File destinationFile = destination.isDirectory() ? File.getInstance(destination, name) : destination;
        final FtpSession session = FtpSession.getSession((FtpFile)getFile());
        if (session == null)
            return;
        final long fileSize = getFileSize(dotLine.getText());
        final FtpLoadProcess loadProcess = new FtpLoadProcess(this, sourceFile, session);
        final Runnable successRunnable = new Runnable() {
            public void run()
            {
                File cache = loadProcess.getCache();
                if (cache != null)
                    Utilities.deleteRename(cache, destination);
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Directory.this)
                        ed.setDefaultCursor();
                }
            }
        };
        final ErrorRunnable errorRunnable = new ErrorRunnable("Operation failed") {
            public void run()
            {
                File cache = loadProcess.getCache();
                if (cache != null && cache.isFile())
                    cache.delete();
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Directory.this)
                        ed.setDefaultCursor();
                }
                String text = session.getErrorText();
                if (text == null || text.length() == 0)
                    text = "Unable to retrieve " + name;
                MessageDialog.showMessageDialog(text, "Error");
            }
        };
        loadProcess.setSuccessRunnable(successRunnable);
        loadProcess.setErrorRunnable(errorRunnable);
        loadProcess.setProgressNotifier(new StatusBarProgressNotifier(this));
        loadProcess.start();
    }

    public boolean isModified()
    {
        return false;
    }

    public String getTitle()
    {
        File dir = getFile();
        title = dir.canonicalPath();
        if (limitPattern != null) {
            title += LocalFile.getSeparator();
            title += limitPattern;
        }
        title += " (sorted by ";
        if (sortBy == SORT_BY_NAME)
            title += "name)";
        else if (sortBy == SORT_BY_DATE)
            title += "date)";
        else if (sortBy == SORT_BY_SIZE)
            title += "size)";
        if (dir.isRemote())
            title += "   " + dir.getHostName();
        return title;
    }

    private String getName(Line line)
    {
        return getName(line.getText());
    }

    private String getName(String s)
    {
        // Strip symbolic link (if any) from end of line.
        int end = s.indexOf(" ->");

        if (end >= 0)
            s = s.substring(0, end);

        REMatch match;

        if (usingNativeFormat)
            match = nativeMoveToFilenameRegExp.getMatch(s);
        else
            match = internalMoveToFilenameRegExp.getMatch(s);

        if (match != null)
            return s.substring(match.getEndIndex());

        return null;
    }

    private long getFileSize(String s)
    {
        if (usingNativeFormat) {
            try {
                REMatch match = nativeMoveToFilenameRegExp.getMatch(s);
                if (match != null) {
                    String toBeParsed = s.substring(match.getStartIndex());
                    int index = toBeParsed.indexOf(' ');
                    if (index >= 0) {
                        toBeParsed = toBeParsed.substring(0, index);
                        return Long.parseLong(toBeParsed);
                    }
                }
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
        return 0;
    }

    public void home()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDotOffset() == 0)
            return;
        editor.addUndo(SimpleEdit.MOVE);
        editor.beginningOfBlock();
        int offset = getNameOffset(editor.getDotLine());

        // If we're already at the first character of the name (or to the left
        // of it), go to column 0.
        if (editor.getDotOffset() <= offset)
            offset = 0;

        editor.getDot().setOffset(offset);
        editor.moveCaretToDotCol();
    }

    public static void chmod()
    {
        final Editor editor = Editor.currentEditor();
        if (!(editor.getDotLine() instanceof DirectoryLine))
            return;
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof Directory))
            return;
        final Directory directory = (Directory) buffer;
        String name = directory.getName(editor.getDotLine());
        if (name == null)
            return;
        final File file = File.getInstance(directory.getFile(), name);
        if (file == null)
            return;
        // Verify that operation is supported.
        if (file.isLocal()) {
            if (!Platform.isPlatformUnix())
                return;
        } else if (file.isRemote()) {
            int protocol = file.getProtocol();
            if (protocol != File.PROTOCOL_FTP && protocol != File.PROTOCOL_SSH)
                return;
        }
        FastStringBuffer sb = new FastStringBuffer("Change mode of ");
        sb.append(file.getName());
        sb.append(" to:");
        final String input =
            InputDialog.showInputDialog(editor, sb.toString(), "Change Mode");
        if (input == null || input.length() == 0)
            return;
        int n = 0;
        try {
            n = Integer.parseInt(input, 8);
        }
        catch (NumberFormatException e) {
            Log.error(e);
        }
        if (n == 0) {
            MessageDialog.showMessageDialog("Invalid mode string", "Change Mode");
            return;
        }
        final int permissions = n;
        if (file.isLocal()) {
            editor.setWaitCursor();
            file.setPermissions(permissions);
            editor.setDefaultCursor();
        } else if (file instanceof FtpFile) {
            final FtpSession session = FtpSession.getSession((FtpFile)file);
            if (session != null) {
                final Runnable completionRunnable = new Runnable() {
                    public void run()
                    {
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            if (ed.getBuffer() == directory)
                                ed.setDefaultCursor();
                        }
                    }
                };
                final Runnable chmodRunnable = new Runnable() {
                    public void run()
                    {
                        directory.setBusy(true);
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            if (ed.getBuffer() == directory)
                                ed.setWaitCursor();
                        }
                        if (session.verifyConnected()) {
                            session.chmod((FtpFile)file, permissions);
                            session.unlock();
                        }
                        directory.setBusy(false);
                        SwingUtilities.invokeLater(completionRunnable);
                    }
                };
                new Thread(chmodRunnable).start();
            }
        } else if (file instanceof SshFile) {
            final SshSession session = SshSession.getSession((SshFile)file);
            if (session != null) {
                final Runnable completionRunnable = new Runnable() {
                    public void run()
                    {
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            if (ed.getBuffer() == directory)
                                ed.setDefaultCursor();
                        }
                    }
                };
                final Runnable chmodRunnable = new Runnable() {
                    public void run()
                    {
                        directory.setBusy(true);
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            if (ed.getBuffer() == directory)
                                ed.setWaitCursor();
                        }
                        if (session.connect()) {
                            session.chmod((SshFile)file, permissions);
                            session.unlock();
                        }
                        directory.setBusy(false);
                        SwingUtilities.invokeLater(completionRunnable);
                    }
                };
                new Thread(chmodRunnable).start();
            }
        }
    }

    private int getNameOffset(Line line)
    {
        if (line != null) {
            REMatch match;
            if (usingNativeFormat)
                match = nativeMoveToFilenameRegExp.getMatch(line.getText());
            else
                match = internalMoveToFilenameRegExp.getMatch(line.getText());
            if (match != null)
                return match.getEndIndex();
        }
        return 0;
    }

    private int getNameOffset()
    {
        return getNameOffset(getFirstLine());
    }

    private int getFileSizeEndOffset()
    {
        Line line = getFirstLine();
        if (line != null) {
            final String text = line.getText();
            REMatch match;
            if (usingNativeFormat)
                match = nativeMoveToFilenameRegExp.getMatch(text);
            else
                match = internalMoveToFilenameRegExp.getMatch(text);
            if (match != null) {
                int start = match.getStartIndex();
                // The file size is followed by a single space.
                return text.indexOf(' ', start);
            }
        }
        return -1; // Error!
    }

    private Line findName(String name)
    {
        if (name != null) {
            if (Platform.isPlatformWindows()) {
                // Case-insensitive filesystem.
                name = name.toLowerCase();
                for (Line line = getFirstLine(); line != null; line = line.next()) {
                    String text = line.getText().toLowerCase();
                    if (text.indexOf(name) >= 0) // Performance!
                        if (name.equalsIgnoreCase(getName(line)))
                            return line;
                }
            } else {
                for (Line line = getFirstLine(); line != null; line = line.next()) {
                    if (line.getText().indexOf(name) >= 0) // Performance!
                        if (name.equals(getName(line)))
                            return line;
                }
            }
        }
        return null;
    }

    public final String toString()
    {
        File file = getFile();
        if (file.isRemote()) {
            FastStringBuffer sb = new FastStringBuffer(file.canonicalPath());
            sb.append(" [");
            sb.append(file.netPath());
            sb.append(']');
            return sb.toString();
        }
        if (Platform.isPlatformUnix()) {
            String userHome = Utilities.getUserHome();
            if (userHome != null && userHome.length() > 0) {
                String s = file.canonicalPath();
                if (s.equals(userHome))
                    return "~";
                if (s.startsWith(userHome.concat("/")))
                    return "~".concat(s.substring(userHome.length()));
            }
        }
        return file.canonicalPath();
    }

    // For the buffer list.
    public final Icon getIcon()
    {
        return Utilities.getIconFromFile("directory.png");
    }
}

class DirectoryHistory
{
    private Vector v = new Vector();
    private int index = -1;

    DirectoryHistory()
    {
    }

    boolean atEnd()
    {
        return index == -1;
    }

    void truncate()
    {
        if (index != -1)
            v.setSize(index);
    }

    void append(File file, String name, int offset)
    {
        v.add(new DirectoryHistoryEntry(file, name, offset));
    }

    DirectoryHistoryEntry getPrevious()
    {
        if (v.size() == 0)
            return null;
        if (index == -1)
            index = v.size();
        if (index > 0)
            return (DirectoryHistoryEntry) v.get(--index);
        return null;
    }

    DirectoryHistoryEntry getNext()
    {
        if (v.size() == 0)
            return null;
        if (index == -1)
            return null;
        if (index < v.size()-1)
            return (DirectoryHistoryEntry) v.get(++index);
        return null;
    }

    public void reset()
    {
        index = -1;
    }
}

class DirectoryHistoryEntry
{
    File file;
    String name;
    int offset;

    DirectoryHistoryEntry(File file, String name, int offset)
    {
        this.file = file;
        this.name = name;
        this.offset = offset;
    }
}
