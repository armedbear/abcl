/*
 * Log.java
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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;

public final class Log
{
    // Levels.
    private static final int DEBUG = 1;
    private static final int INFO  = 2;
    private static final int WARN  = 3;
    private static final int ERROR = 4;
    private static final int FATAL = 5;

    // Synchronization.
    private static final Object lock = new Object();

    private static final SimpleDateFormat dateFormat =
        new SimpleDateFormat("MMM dd HH:mm:ss.SSS ");
    private static final int lineSeparatorLength =
        System.getProperty("line.separator").length();

    // Configuration.
    private static boolean logEnabled;
    private static long maxFileSize;
    private static int maxBackupIndex;
    private static int minLevel;

    private static PrintWriter logWriter;
    private static long fileSize;
    private static boolean rollOverEnabled;

    private static final void setRollOverEnabled(boolean b)
    {
        rollOverEnabled = b;
    }

    public static final int getLevel()
    {
        return minLevel;
    }

    public static final void setLevel(int level)
    {
        minLevel = level;
    }

    public static final void debug(String s)
    {
        log(DEBUG, s);
    }

    public static final void debug(Throwable t)
    {
        log(DEBUG, t);
    }

    public static final void info(String s)
    {
        log(INFO, s);
    }

    public static final void warn(String s)
    {
        log(WARN, s);
    }

    public static final void warn(Throwable t)
    {
        log(WARN, t);
    }

    public static final void error(String s)
    {
        log(ERROR, s);
    }

    public static final void error(Throwable t)
    {
        log(ERROR, t);
    }

    public static final void fatal(String s)
    {
        log(FATAL, s);
    }

    private static final void log(int level, String s)
    {
        if (Editor.isDebugEnabled())
            System.err.println(s);
        if (logEnabled && level >= minLevel)
            writeLog(level, s);
    }

    private static final void forceLog(int level, String s)
    {
        if (Editor.isDebugEnabled())
            System.err.println(s);
        writeLog(level, s);
    }

    private static final void writeLog(int level, String s)
    {
        synchronized(lock) {
            if (logWriter != null) {
                String dt = getDateTimeString();
                logWriter.print(dt);
                logWriter.print(levelToString(level));
                logWriter.println(s);
                fileSize += dt.length() + 6 + s.length() + lineSeparatorLength;
                if (rollOverEnabled && fileSize > maxFileSize)
                    rollOver();
            }
        }
    }

    private static final void log(int level, Throwable t)
    {
        if (Editor.isDebugEnabled())
            t.printStackTrace();
        if (logEnabled && level >= minLevel) {
            synchronized(lock) {
                if (logWriter != null) {
                    String dt = getDateTimeString();
                    logWriter.print(dt);
                    logWriter.print(levelToString(level));
                    StringWriter sw = new StringWriter();
                    PrintWriter pw = new PrintWriter(sw);
                    t.printStackTrace(pw);
                    String s = sw.toString();
                    logWriter.println(s);
                    fileSize += dt.length() + 6 + s.length() + lineSeparatorLength;
                    if (rollOverEnabled && fileSize > maxFileSize)
                        rollOver();
                }
            }
        }
    }

    // Called only from synchronized methods.
    private static final void rollOver()
    {
        if (!rollOverEnabled)
            Debug.bug();
        setRollOverEnabled(false);
        long start = System.currentTimeMillis();
        Log.debug("rotating log files...");
        for (int i = maxBackupIndex-1; i >= 0; i--) {
            File source = getBackupLogFile(i);
            File destination = getBackupLogFile(i+1);
            if (destination.exists())
                destination.delete();
            if (source.isFile())
                source.renameTo(destination);
        }
        File destination = getBackupLogFile(0);
        File logFile = getLogFile();
        logWriter.flush();
        logWriter.close();
        if (destination.exists())
            destination.delete();
        if (logFile.isFile())
            logFile.renameTo(destination);
        logWriter = null;
        fileSize = 0;
        try {
            logWriter = new PrintWriter(logFile.getOutputStream(), true);
            int oldLevel = getLevel();
            setLevel(INFO);
            setRollOverEnabled(false);
            logSystemInformation();
            logUptime();
            setRollOverEnabled(true);
            setLevel(oldLevel);
        }
        catch (IOException e) {
            logEnabled = false;
            e.printStackTrace();
        }
        long elapsed = System.currentTimeMillis() - start;
        Log.debug("rollOver " + elapsed + " ms");
        setRollOverEnabled(true);
    }

    private static final File getLogFile()
    {
        return File.getInstance(Directories.getEditorDirectory(), "log");
    }

    private static final File getBackupLogFile(int index)
    {
        return File.getInstance(Directories.getEditorDirectory(),
            "log.".concat(String.valueOf(index)));
    }

    public static final void initialize()
    {
        synchronized(lock) {
            Preferences preferences = Editor.preferences();
            if (preferences != null) {
                preferences.addPreferencesChangeListener(preferencesChangeListener);
                loadPreferences();
                if (logEnabled) {
                    initializeLogWriter();
                    if (logWriter != null) {
                        setLevel(INFO);
                        info("Starting j...");
                        logSystemInformation();
                        if (Editor.isDebugEnabled())
                            setLevel(DEBUG);
                        setRollOverEnabled(true);
                    }
                }
            }
        }
    }

    private static final void initializeLogWriter()
    {
        Debug.assertTrue(logWriter == null);
        File logFile = getLogFile();
        if (logFile.isFile())
            fileSize = logFile.length();
        try {
            // Append to file, flush automatically.
            logWriter = new PrintWriter(logFile.getOutputStream(true), true);
        }
        catch (Exception e) {
            logEnabled = false;
            e.printStackTrace();
        }
    }

    private static final void logSystemInformation()
    {
        info(Version.getLongVersionString());
        String snapshotInformation = Version.getSnapshotInformation();
        if (snapshotInformation != null)
            info(snapshotInformation);
        FastStringBuffer sb = new FastStringBuffer("Java ");
        sb.append(System.getProperty("java.version"));
        sb.append(' ');
        sb.append(System.getProperty("java.vendor"));
        info(sb.toString());
        String fullversion = System.getProperty("java.fullversion");
        if (fullversion != null)
            info(fullversion);
        String vm = System.getProperty("java.vm.name");
        if (vm != null)
            info(vm);
        sb.setText(System.getProperty("os.name"));
        sb.append(' ');
        sb.append(System.getProperty("os.version"));
        info(sb.toString());
    }

    private static final void logUptime()
    {
        info("up since ".concat(getDateTimeString(Editor.getStartTimeMillis())));
    }

    private static final String getDateTimeString()
    {
        return getDateTimeString(System.currentTimeMillis());
    }

    private static final String getDateTimeString(long millis)
    {
        return dateFormat.format(new Date(millis));
    }

    // String returned is always 6 characters long and ends with a space.
    private static final String levelToString(int level)
    {
        switch (level) {
            case DEBUG:
                return "DEBUG ";
            case INFO:
                return " INFO ";
            case WARN:
                return " WARN ";
            case ERROR:
                return "ERROR ";
            case FATAL:
                return "FATAL ";
        }
        // Shouldn't happen.
        return "????? ";
    }

    private static final void loadPreferences()
    {
        synchronized(lock) {
            Preferences preferences = Editor.preferences();
            logEnabled = preferences.getBooleanProperty(Property.LOG_ENABLED);
            maxFileSize =
                preferences.getIntegerProperty(Property.LOG_MAX_FILE_SIZE);
            if (maxFileSize < 10000)
                maxFileSize = 10000; // Minimum is 10 KB.
            else if (maxFileSize > 1000000)
                maxFileSize = 1000000; // Maximum is 1 MB.
            maxBackupIndex =
                preferences.getIntegerProperty(Property.LOG_MAX_BACKUP_INDEX);
            if (maxBackupIndex < 0)
                maxBackupIndex = 0; // Minimum is one backup.
        }
    }

    private static final PreferencesChangeListener preferencesChangeListener =
        new PreferencesChangeListener() {
        public void preferencesChanged()
        {
            boolean logWasEnabled = logEnabled;
            loadPreferences();
            forceLog(DEBUG, "preferencesChanged logEnabled = " + logEnabled);
            forceLog(DEBUG, "preferencesChanged maxFileSize = " + maxFileSize);
            forceLog(DEBUG, "preferencesChanged maxBackupIndex = " + maxBackupIndex);
            if (logEnabled && !logWasEnabled) {
                // Start logging.
                initializeLogWriter();
                if (logWriter != null) {
                    setLevel(INFO);
                    info("Logging enabled");
                    logSystemInformation();
                    logUptime();
                    if (Editor.isDebugEnabled())
                        setLevel(DEBUG);
                    setRollOverEnabled(true);
                }
            } else if (logWasEnabled && !logEnabled) {
                // Stop logging.
                if (logWriter != null) {
                    forceLog(INFO, "Logging disabled");
                    logWriter.flush();
                    logWriter.close();
                    logWriter = null;
                }
            } else if (logEnabled) {
                if (Editor.preferences().getBooleanProperty(Property.DEBUG))
                    setLevel(DEBUG);
                else
                    setLevel(INFO);
            }
        }
    };
}
