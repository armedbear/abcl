/*
 * File.java
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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class File implements Comparable
{
    public static final int PROTOCOL_FILE        = 0;
    public static final int PROTOCOL_HTTP        = 1;
    public static final int PROTOCOL_HTTPS       = 2;
    public static final int PROTOCOL_FTP         = 3;
    public static final int PROTOCOL_SSH         = 4;

    public static final String PREFIX_HTTP       = "http://";
    public static final String PREFIX_HTTPS      = "https://";
    public static final String PREFIX_FTP        = "ftp://";
    public static final String PREFIX_SSH        = "ssh://";
    public static final String PREFIX_FILE       = "file://";

    public static final String PREFIX_LOCAL_HOST =
        LocalFile.getLocalHostName().concat(":");

    public static final int TYPE_UNKNOWN         = 0;
    public static final int TYPE_FILE            = 1;
    public static final int TYPE_DIRECTORY       = 2;
    public static final int TYPE_LINK            = 3;

    private static final boolean ignoreCase = Platform.isPlatformWindows();

    private java.io.File file;

    protected boolean isRemote;
    protected String hostName;
    protected String userName;
    protected String password;
    protected int port;
    protected int type; // unknown, file, directory, link
    protected String canonicalPath;
    protected int protocol = PROTOCOL_FILE;

    private String encoding;

    protected File()
    {
    }

    private File(String path)
    {
        file = new java.io.File(path);
        if (Platform.isPlatformWindows() && path.indexOf('*') >= 0) {
            // Workaround for Windows 2000, Java 1.1.8.
            canonicalPath = path;
        } else {
            try {
                canonicalPath = file.getCanonicalPath();
            }
            catch (IOException e) {
                canonicalPath = path;
            }
        }
    }

    private File(String host, String path, int protocol)
    {
        Debug.assertTrue(protocol != PROTOCOL_FTP);
        isRemote = host != null;
        hostName = host;
        canonicalPath = path;
        this.protocol = protocol;
    }

    public static File getInstance(String name)
    {
        if (name == null)
            return null;
        int length = name.length();
        if (length >= 2) {
            // Name may be enclosed in quotes.
            if (name.charAt(0) == '"' && name.charAt(length-1) == '"') {
                name = name.substring(1, length-1);
                length -= 2;
            }
        }
        if (length == 0)
            return null;

        if (name.startsWith(PREFIX_FTP))
            return FtpFile.getFtpFile(name);
        if (name.startsWith(PREFIX_HTTP) || name.startsWith(PREFIX_HTTPS))
            return HttpFile.getHttpFile(name);
        if (name.startsWith(PREFIX_SSH))
            return SshFile.getSshFile(name);

        // Local file.
        if (name.startsWith(PREFIX_FILE)) {
            name = name.substring(PREFIX_FILE.length());
            if (name.length() == 0)
                return null;
        } else if (name.startsWith(PREFIX_LOCAL_HOST)) {
            name = name.substring(PREFIX_LOCAL_HOST.length());
            if (name.length() == 0)
                return null;
        } else if (name.charAt(0) == ':') {
            name = name.substring(1);
            if (--length == 0)
                return null;
        }
        name = normalize(name);

        if (Platform.isPlatformWindows() && name.startsWith("\\")) {
            // Check for UNC path.
            String drive = getDrive(name);
            if (drive == null) {
                drive = getDrive(System.getProperty("user.dir"));
                if (drive != null)
                    name = drive.concat(name);
            }
        }

        // This might return null.
        name = canonicalize(name, LocalFile.getSeparator());

        return name != null ? new File(name) : null;
    }

    protected boolean initRemote(String name, String prefix)
    {
        if (!name.startsWith(prefix))
            return false;
        name = name.substring(prefix.length());
        int index = name.indexOf('/');

        // "ftp:///", "ssh:///" are invalid.
        if (index == 0)
            return false;

        String s;
        if (index > 0) {
            s = name.substring(0, index);
            canonicalPath = canonicalize(name.substring(index), "/");
        } else {
            s = name;
            canonicalPath = "";
        }

        // Host name may be preceded by username and password ("user:password@host") and/or
        // followed by port number ("user:password@host:port").
        index = s.indexOf('@');
        if (index >= 0) {
            // String contains '@'.
            String before = s.substring(0, index);

            // Shorten s to what's left (host and port).
            s = s.substring(index + 1);

            index = before.indexOf(':');
            if (index >= 0) {
                userName = before.substring(0, index);
                password = before.substring(index + 1);
            } else {
                // No ':', no password.
                userName = before;
            }
        } else if (protocol != PROTOCOL_FTP) // Don't break anonymous FTP!
            userName = System.getProperty("user.name");
        index = s.indexOf(':');
        if (index >= 0) {
            // String contains ':'. See if port was specified.
            hostName = s.substring(0, index);
            String portString = s.substring(index + 1);
            if (portString.length() > 0) {
                try {
                    port = Integer.parseInt(portString);
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            }
        } else {
            // No ':'. Port was not specified.
            hostName = s;
        }
        return true;
    }

    public static File getInstance(File directory, String name)
    {
        if (name == null)
            return null;
        int length = name.length();
        if (length >= 2) {
            // Name may be enclosed in quotes.
            if (name.charAt(0) == '"' && name.charAt(length-1) == '"') {
                name = name.substring(1, length-1);
                length -= 2;
            }
        }
        if (length == 0)
            return null;

        if (hasRemotePrefix(name)) {
            // Ignore directory.
            return getInstance(canonicalize(name, "/"));
        }
        if (hasLocalPrefix(name)) {
            // Fully qualified name.
            return getInstance(name);
        }

        if (directory == null)
            return null;

        if (directory instanceof HttpFile)
            return HttpFile.getHttpFile((HttpFile) directory, name);
        if (directory instanceof FtpFile)
            return FtpFile.getFtpFile((FtpFile) directory, name);
        if (directory instanceof SshFile)
            return SshFile.getSshFile((SshFile) directory, name);
        if (directory.isRemote) {
            Debug.assertTrue(false);
            File file = new File();
            file.isRemote = true;
            file.protocol = directory.protocol;
            file.hostName = directory.hostName;
            if (Utilities.isFilenameAbsolute(name))
                file.canonicalPath = canonicalize(name, "/");
            else
                file.canonicalPath = canonicalize(appendNameToPath(directory.canonicalPath(), name, '/'), "/");
            return file;
        }

        // Local file.
        name = normalize(name);
        if (Utilities.isFilenameAbsolute(name)) {
            // Name is absolute.
            if (Platform.isPlatformWindows() && name.startsWith("\\")) {
                String drive = getDrive(name);
                if (drive == null) {
                    drive = directory.getDrive();
                    if (drive != null)
                        name = drive.concat(name);
                }
            }
        } else
            name = appendNameToPath(directory.canonicalPath(), name);
        name = canonicalize(name, LocalFile.getSeparator());
        if (name == null)
            return null;
        return new File(name);
    }

    public static File getInstance(String host, String path, int protocol)
    {
        if (host == null)
            return null;
        if (path == null)
            return null;

        if (protocol != PROTOCOL_HTTP && protocol != PROTOCOL_HTTPS && protocol != PROTOCOL_FTP)
            throw new NotSupportedException();

        if (protocol == PROTOCOL_FTP)
            return FtpFile.getFtpFile(host, path);

        return new File(host, path, protocol);
    }

    public File getRoot()
    {
        if (Platform.isPlatformWindows())
            return new File(getDrive() + "\\");

        // Unix.
        return new File("/");
    }

    public static File[] listRoots()
    {
        java.io.File[] files = (java.io.File[]) java.io.File.listRoots();
        File[] list = new File[files.length];
        for (int i = 0; i < files.length; i++) {
            list[i] = new File();
            list[i].canonicalPath = files[i].getPath();
            list[i].file = new java.io.File(list[i].canonicalPath);
        }
        return list;
    }

    public String getSeparator()
    {
        return java.io.File.separator;
    }

    public char getSeparatorChar()
    {
        return java.io.File.separatorChar;
    }

    public String getDrive()
    {
        if (Platform.isPlatformWindows())
            return getDrive(canonicalPath);

        return null;
    }

    private static String getDrive(String s)
    {
        if (s != null && s.length() >= 2) {
            if (s.charAt(0) == '\\' && s.charAt(1) == '\\') {
                // UNC path.
                int index = s.indexOf('\\', 2);
                if (index >= 0) {
                    index = s.indexOf('\\', index + 1);
                    if (index >= 0)
                        return s.substring(0, index);
                }
            } else if (s.charAt(1) == ':') {
                String prefix = s.substring(0, 2).toUpperCase();
                char c = prefix.charAt(0);
                if (c >= 'A' && c <= 'Z')
                    return prefix;
            }
        }
        return null;
    }

    protected static String canonicalize(String name, String sep)
    {
        String prefix = null;
        if (name.startsWith(PREFIX_HTTP)) {
            prefix = PREFIX_HTTP;
            sep = "/";
        } else if (name.startsWith(PREFIX_HTTPS)){
            prefix = PREFIX_HTTPS;
            sep = "/";
        } else if (name.startsWith(PREFIX_FTP)) {
            prefix = PREFIX_FTP;
            sep = "/";
        } else if (name.startsWith(PREFIX_SSH)) {
            prefix = PREFIX_SSH;
            sep = "/";
        } else if (Platform.isPlatformWindows())
            prefix = getDrive(name);

        if (prefix != null)
            name = name.substring(prefix.length());

        if (sep == null)
            sep = LocalFile.getSeparator();

        StringTokenizer st = new StringTokenizer(name, sep);
        final int numTokens = st.countTokens();
        String[] array = new String[numTokens];
        int count = 0;
        for (int i = 0; i < numTokens; i++) {
            String token = st.nextToken();
            if (token.equals("..")) {
                if (count > 0)
                    --count;
                else
                    return null;
            } else if (!token.equals("."))
                array[count++] = token;
        }

        FastStringBuffer sb = new FastStringBuffer(256);
        if (prefix != null)
            sb.append(prefix);
        if (count == 0)
            sb.append(sep);
        else {
            for (int i = 0; i < count; i++) {
                if (!sb.toString().endsWith(sep))
                    sb.append(sep);
                sb.append(array[i]);
            }
        }
        return sb.toString();
    }

    public static String normalize(String name)
    {
        if (hasRemotePrefix(name))
            return name;

        if (name.equals("~") || name.equals("~/"))
            name = Utilities.getUserHome();
        else if (name.startsWith("~/"))
            name = appendNameToPath(Utilities.getUserHome(), name.substring(2));

        if (Platform.isPlatformWindows()) {
            if (name.length() >= 4) {
                if (name.charAt(0) == '/' && name.charAt(1) == '/' && name.charAt(3) == '/') {
                    // Convert Cygwin format to drive letter plus colon.
                    char c = name.charAt(2);
                    if (c >= 'a' && c <= 'z') {
                        // e.g. "//d/"
                        name = c + ":\\" + name.substring(4);
                    }
                }
            }
        }

        char toBeReplaced = LocalFile.getSeparatorChar() == '/' ? '\\' : '/';
        return name.replace(toBeReplaced, LocalFile.getSeparatorChar());
    }

    public static boolean hasRemotePrefix(String name)
    {
        if (name.startsWith(PREFIX_HTTP))
            return true;
        if (name.startsWith(PREFIX_HTTPS))
            return true;
        if (name.startsWith(PREFIX_FTP))
            return true;
        if (name.startsWith(PREFIX_SSH))
            return true;
        return false;
    }

    public static boolean hasLocalPrefix(String name)
    {
        if (name.startsWith(PREFIX_FILE))
            return true;
        if (name.startsWith(PREFIX_LOCAL_HOST))
            return true;
        if (name.length() > 0 && name.charAt(0) == ':')
            return true;
        return false;
    }

    // Uses platform-specific separator char.
    public final static String appendNameToPath(String path, String name)
    {
        return appendNameToPath(path, name, LocalFile.getSeparatorChar());
    }

    public static String appendNameToPath(String path, String name, char separator)
    {
        int pathLength = path.length();

        if (pathLength > 0 && name.length() > 0) {
            // If path does not end with a separator and name does not begin
            // with a separator, add a separator when concatenating the strings.
            if (path.charAt(pathLength - 1) != separator)
                if (name.charAt(0) != separator)
                    return path + separator + name;

            // If path ends with a separator (e.g. "/") and name begins with a
            // separator, remove one of the separators before concatenating the
            // strings.
            if (path.charAt(pathLength - 1) == separator)
                if (name.charAt(0) == separator)
                    return path + name.substring(1);
        }

        return path + name;
    }

    public String getCanonicalPath() throws IOException
    {
        if (canonicalPath == null) {
            if (!isRemote) {
                if (file != null)
                    canonicalPath = file.getCanonicalPath();
            }
        }
        return canonicalPath;
    }

    // Like getCanonicalPath(), but doesn't throw an exception.
    public final synchronized String canonicalPath()
    {
        if (canonicalPath == null) {
            if (file != null) {
                try {
                    canonicalPath = file.getCanonicalPath();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        }
        return canonicalPath;
    }

    public synchronized void setCanonicalPath(String s)
    {
        if (canonicalPath == null || canonicalPath.equals(""))
            canonicalPath = s;
    }

    public String netPath()
    {
        switch (protocol) {
            case PROTOCOL_FILE:
                return LocalFile.getLocalHostName() + ':' + canonicalPath();
            case PROTOCOL_HTTP:
                return PREFIX_HTTP + hostName + canonicalPath;
            case PROTOCOL_HTTPS:
                return PREFIX_HTTPS + hostName + canonicalPath;
            case PROTOCOL_FTP: {
                String s = PREFIX_FTP + hostName;
                if (canonicalPath != null)
                    s += canonicalPath;
                return s;
            }
            case PROTOCOL_SSH: {
                String s = PREFIX_SSH + hostName;
                if (canonicalPath != null)
                    s += canonicalPath;
                return s;
            }
        }
        Debug.assertTrue(false);
        return null;
    }

    public final String getHostName()
    {
        return hostName;
    }

    public final String getUserName()
    {
        return userName;
    }

    public final void setUserName(String userName)
    {
        this.userName = userName;
    }

    public final String getPassword()
    {
        return password;
    }

    public final void setPassword(String password)
    {
        this.password = password;
    }

    public final int getPort()
    {
        return port;
    }

    public int getProtocol()
    {
        return protocol;
    }

    public final boolean isLocal()
    {
        return !isRemote;
    }

    public final boolean isRemote()
    {
        return isRemote;
    }

    public String getName()
    {
        if (isRemote) {
            int index = canonicalPath.lastIndexOf('/');
            if (index >= 0)
                return canonicalPath.substring(index+1);
            return canonicalPath;
        }
        if (file != null)
            return file.getName();
        return null;
    }

    public String getAbsolutePath()
    {
        if (isRemote)
            return canonicalPath();
        if (file != null) {
            String absPath = file.getAbsolutePath();
            if (Platform.isPlatformUnix() && absPath.startsWith("//")) {
                absPath = absPath.substring(1); // Workaround for Java 1.1.8.
            }
            return absPath;
        }
        return null;
    }

    public String getParent()
    {
        if (isRemote) {
            if (canonicalPath.equals("/"))
                return null;
            int index = canonicalPath.lastIndexOf('/');
            if (index < 0)
                return null;
            if (index == 0) // "/usr"
                return "/";
            return canonicalPath.substring(0, index);
        }
        if (file != null)
            return file.getParent();
        return null;
    }

    public File getParentFile()
    {
        if (canonicalPath() == null || canonicalPath.equals("/")) {
            // HTTP is a special case.  We might really be looking at
            // "http://www.cnn.com/index.html", but it might appear to
            // be "http://www.cnn.com/".
            if (protocol == PROTOCOL_HTTP || protocol == PROTOCOL_HTTPS)
                return new File(hostName, "/", protocol);

            return null;
        }
        if (isRemote){
            int index = canonicalPath.lastIndexOf('/');
            if (index < 0)
                return null;
            if (index == 0) // "/usr"
                return new File(hostName, "/", protocol);
            return new File(hostName, canonicalPath.substring(0, index), protocol);
        }
        if (file != null && file.getParent() != null)
            return new File(file.getParent());
        return null;
    }

    public boolean exists()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.exists();
    }

    public boolean canWrite()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.canWrite();
    }

    public boolean canRead()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.canRead();
    }

    public boolean isFile()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.isFile();
    }

    public boolean isDirectory()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.isDirectory();
    }

    public boolean isLink()
    {
        if (file == null)
            throw new NotSupportedException();
        return !canonicalPath().equals(getAbsolutePath());
    }

    public boolean isAbsolute()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.isAbsolute();
    }

    public long lastModified()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.lastModified();
    }

    public boolean setLastModified(long time)
    {
        if (file == null)
            throw new NotSupportedException();
        return file.setLastModified(time);
    }

    public long length()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.length();
    }

    public boolean mkdir()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.mkdir();
    }

    public boolean mkdirs()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.mkdirs();
    }

    public boolean renameTo(File f)
    {
        if (file == null || f.file == null)
            throw new NotSupportedException();
        return file.renameTo(f.file);
    }

    public String[] list()
    {
        if (this instanceof FtpFile || this instanceof SshFile) {
            String listing = getDirectoryListing();
            if (listing == null)
                return null;
            FastStringReader reader = new FastStringReader(listing);
            ArrayList list = new ArrayList();
            String s;
            while ((s = reader.readLine()) != null) {
                String name = DirectoryEntry.getName(s);
                if (name == null || name.equals(".") || name.equals(".."))
                    continue;
                list.add(name);
            }
            if (list.size() == 0)
                return null;
            String[] names = new String[list.size()];
            list.toArray(names);
            return names;
        }
        if (isRemote)
            return null;
        if (file == null)
            throw new NotSupportedException();
        return file.list();
    }

    public File[] listFiles()
    {
        if (!isDirectory())
            return null;
        if (this instanceof FtpFile || this instanceof SshFile) {
            String listing = getDirectoryListing();
            if (listing == null)
                return null;
            long start = System.currentTimeMillis();
            FastStringReader reader = new FastStringReader(listing);
            ArrayList list = new ArrayList();
            int nameColumn = -1;
            String s;
            while ((s = reader.readLine()) != null) {
                if (nameColumn < 0)
                    nameColumn = DirectoryEntry.getNameColumn(s);
                DirectoryEntry entry = DirectoryEntry.getDirectoryEntry(s, null);
                if (entry != null) {
                    final String name = entry.extractName(nameColumn);
                    if (name == null || name.equals(".") || name.equals(".."))
                        continue;
                    String path = appendNameToPath(canonicalPath(), name, '/');
                    File file = null;
                    if (protocol == PROTOCOL_FTP)
                        file = new FtpFile(hostName, path, userName, password, port);
                    else if (protocol == PROTOCOL_SSH)
                        file = new SshFile(hostName, path, userName, password, port);
                    else
                        Debug.bug();
                    if (file != null) {
                        if (entry.isDirectory())
                            file.type = TYPE_DIRECTORY;
                        else if (entry.isLink())
                            file.type = TYPE_LINK;
                        else
                            file.type = TYPE_FILE;
                        list.add(file);
                    }
                }
            }
            if (list.size() == 0)
                return null;
            File[] files = new File[list.size()];
            list.toArray(files);
            long elapsed = System.currentTimeMillis() - start;
            Log.debug("listFiles " + elapsed + " ms " + this);
            return files;
        }
        if (isRemote)
            return null;
        if (file == null)
            throw new NotSupportedException();
        String[] names = file.list();
        if (names == null)
            return null;
        final FastStringBuffer sb = new FastStringBuffer();
        final String path = getAbsolutePath();
        File[] files = new File[names.length];
        for (int i = 0; i < names.length; i++) {
            sb.setText(path);
            sb.append(LocalFile.getSeparator());
            sb.append(names[i]);
            files[i] = new File(sb.toString());
        }
        return files;
    }

    public String[] list(FilenameFilter filter)
    {
        if (isRemote)
            return null;
        if (file == null)
            throw new NotSupportedException();
        return file.list(filter);
    }

    public String getDirectoryListing()
    {
        return null;
    }

    public String getDirectoryListing(boolean forceRefresh)
    {
        return null;
    }

    public boolean delete()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.delete();
    }

    public int hashCode()
    {
        if (file == null)
            throw new NotSupportedException();
        return file.hashCode();
    }

    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (!(obj instanceof File))
            return false;
        File f = (File) obj;
        // Protocol.
        if (f.protocol != protocol)
            return false;
        // Host name.
        if (f.hostName == null) {
            if (hostName != null)
                return false;
        } else if (!f.hostName.equals(hostName))
            return false;
        // Handle pathological corner case where both canonical paths are null.
        if (f.canonicalPath() == canonicalPath())
            return true;
        // At this point the canonical paths are cached for both files. If
        // either one is null, they can't be equal.
        if (f.canonicalPath == null || canonicalPath == null)
            return false;
        // Never ignore case for remote files.
        if (ignoreCase && !isRemote)
            return f.canonicalPath.equalsIgnoreCase(canonicalPath);
        return f.canonicalPath.equals(canonicalPath);
    }

    public String toString()
    {
        return netPath();
    }

    public FileInputStream getInputStream() throws FileNotFoundException
    {
        if (isRemote)
            throw new NotSupportedException();
        if (file == null)
            throw new NotSupportedException();
        return new FileInputStream(file);
    }

    public FileOutputStream getOutputStream() throws FileNotFoundException
    {
        if (isRemote)
            throw new NotSupportedException();
        if (file == null)
            throw new NotSupportedException();
        return new FileOutputStream(file);
    }

    public FileOutputStream getOutputStream(boolean append) throws FileNotFoundException
    {
        if (isRemote)
            throw new NotSupportedException();
        if (file == null)
            throw new NotSupportedException();
        return new FileOutputStream(canonicalPath, append);
    }

    public RandomAccessFile getRandomAccessFile(String mode) throws FileNotFoundException
    {
        if (isRemote)
            throw new NotSupportedException();
        if (file == null)
            throw new NotSupportedException();
        return new RandomAccessFile(file, mode);
    }

    public final String getEncoding()
    {
        return encoding;
    }

    public final void setEncoding(String encoding)
    {
        this.encoding = encoding;
    }

    public int getPermissions()
    {
        int permissions = 0;
        if (isLocal() && Platform.isPlatformUnix()) {
            String[] cmdarray = {"ls", "-ld", canonicalPath()};
            try {
                Process process = Runtime.getRuntime().exec(cmdarray);
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(process.getInputStream()));
                String output = reader.readLine();
                if (output != null) {
                    String s = output.substring(1, 10);
                    if (s.length() == 9) {
                        if (s.charAt(0) == 'r')
                            permissions += 0400;
                        if (s.charAt(1) == 'w')
                            permissions += 0200;
                        if (s.charAt(2) == 'x')
                            permissions += 0100;
                        if (s.charAt(3) == 'r')
                            permissions += 040;
                        if (s.charAt(4) == 'w')
                            permissions += 020;
                        if (s.charAt(5) == 'x')
                            permissions += 010;
                        if (s.charAt(6) == 'r')
                            permissions += 4;
                        if (s.charAt(7) == 'w')
                            permissions += 2;
                        if (s.charAt(8) == 'x')
                            permissions += 1;
                    }
                    reader.close();
                    process.getInputStream().close();
                    process.getOutputStream().close();
                    process.getErrorStream().close();
                }
            }
            // Feb 4 2000 5:30 PM
            // Catch Throwable here rather than Exception.
            // Kaffe's implementation of Runtime.exec() throws
            // java.lang.InternalError.
            catch (Throwable t) {
                Log.error(t);
            }
        }
        return permissions;
    }

    public void setPermissions(int permissions)
    {
        if (permissions != 0 && isLocal() && Platform.isPlatformUnix()) {
            String[] cmdarray = {"chmod", Integer.toString(permissions, 8), canonicalPath()};
            try {
                Process process = Runtime.getRuntime().exec(cmdarray);
                process.getInputStream().close();
                process.getOutputStream().close();
                process.getErrorStream().close();
                int exitCode = process.waitFor();
                if (exitCode != 0)
                    Log.error("setPermissions exitCode = " + exitCode);
            }
            // Feb 4 2000 5:30 PM
            // Catch Throwable here rather than Exception.
            // Kaffe's implementation of Runtime.exec() throws
            // java.lang.InternalError.
            catch (Throwable t) {
                Log.error(t);
            }
        }
    }

    public final int compareTo(Object o)
    {
        return getName().compareTo(((File)o).getName());
    }

//     private static void test(String dirname, String filename, String expected)
//     {
//         File dir = getInstance(dirname);
//         File file = getInstance(dir, filename);

//         String result;

//         if (file == null)
//             result = "null";
//         else
//             result = file.netPath();

//         if (Platform.isPlatformUnix() && result.equals(expected))
//             System.out.println("  "  + dirname + " + " + filename + " => " + result);
//         else if (Platform.isPlatformWindows() && result.equalsIgnoreCase(expected))
//             System.out.println("  "  + dirname + " + " + filename + " => " + result);
//         else
//         {
//             System.out.println("* "  + dirname + " + " + filename + " => " + result);
//             System.out.println("*** expected " + expected + " ***");
//         }
//     }

//     public static void main(String args[])
//     {
//         if (Platform.isPlatformUnix())
//         {
//             test("/home/peter", ".", "/home/peter");
//             test("/home/peter", "./", "/home/peter");
//             test("/home/peter", "..", "/home");
//             test("/home/peter", "../", "/home");
//             test("/", "~/foo", "/home/peter/foo");
//             test("/", "~/../foo", "/home/foo");
//             test("/", "~/../../foo", "/foo");
//             test("/", "~/../../../foo", "null");
//             test("/", "~peter/foo", "/~peter/foo");
//             test("/home/peter", "./foo", "/home/peter/foo");
//             test("/home/peter", "../foo", "/home/foo");
//             test("/home/peter", "../../foo", "/foo");
//             test("/home/peter", "../../../foo", "null");
//             test("/home/peter", "..foo", "/home/peter/..foo");
//             test("/home/peter", "foo", "/home/peter/foo");
//             test("/home/peter", "/foo", "/foo");
//         }
//         else
//         {
//             test("\\", ".", "C:\\");
//             test("\\home", "\\home\\peter\\j", "C:\\home\\peter\\j");
//             test("d:\\home", "\\home\\peter\\j", "D:\\home\\peter\\j");
//             test("\\home", "d:\\home\\peter\\j", "d:\\home\\peter\\j");
//             test("c:\\home\\peter", ".", "C:\\home\\peter");
//             test("c:\\home\\peter", "..", "C:\\home");
//             test("c:\\home\\peter", "..\\..", "C:\\");
//             test("c:\\home\\peter", "..\\..\\..", "null");
//             test("c:\\home\\peter", "c:\\", "C:\\");
//             test("c:\\home\\peter", "c:\\windows", "C:\\windows");
//             test("c:\\home\\peter", "c:\\windows\\", "C:\\windows");
//             test("c:\\", ".", "C:\\");
//             test("c:\\", ".\\", "C:\\");
//             test("c:\\", "..", "null");
//             test("//c/", "foo", "C:\\foo");
//         }

//         test("http://www.cnn.com", "/virtual/1998/code/cnn.css", "http://www.cnn.com/virtual/1998/code/cnn.css");
//         test("http://www.cnn.com/", "/virtual/1998/code/cnn.css", "http://www.cnn.com/virtual/1998/code/cnn.css");
//         test("http://www.cnn.com", "http://www.swatch.com", "http://www.swatch.com/");
//         test("http://www.cnn.com/", "http://www.swatch.com", "http://www.swatch.com/");
//         test("http://www.swatch.com", "specials/live_timing/index.html", "http://www.swatch.com/specials/live_timing/index.html");
//         test("http://www.swatch.com/", "specials/live_timing/index.html", "http://www.swatch.com/specials/live_timing/index.html");
//     }
}
