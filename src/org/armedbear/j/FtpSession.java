/*
 * FtpSession.java
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.swing.SwingUtilities;

public class FtpSession implements Constants
{
    private static final boolean echo = true;
    private static final Vector sessionList = new Vector();

    private static CleanupThread cleanupThread;

    private String host;
    private int port;
    private String user;
    private String password;
    private String loginDirectory;
    private String currentDirectory;
    private Socket controlSocket;
    private BufferedReader controlIn;
    private OutputStreamWriter controlOut;
    private ServerSocket serverSocket;
    private Socket dataSocket;
    private InputStream dataIn;
    private OutputStream dataOut;
    private boolean connected;
    private boolean usePassiveMode = true;
    private String errorText;
    private ProgressNotifier progressNotifier;
    private boolean locked;

    private FtpSession()
    {
        register(this);
    }

    private FtpSession(Login login, int port)
    {
        host = login.host;
        user = login.user;
        password = login.password;
        this.port = port;
        usePassiveMode =
            Editor.preferences().getBooleanProperty(Property.FTP_USE_PASSIVE_MODE);
        register(this);
    }

    private static synchronized void register(FtpSession session)
    {
        sessionList.add(session);
        if (cleanupThread == null) {
            cleanupThread = new CleanupThread(cleanupRunnable);
            cleanupThread.start();
        }
    }

    private static synchronized void unregister(FtpSession session)
    {
        if (!sessionList.contains(session))
            Debug.bug();
        sessionList.remove(session);
    }

    protected Object clone()
    {
        FtpSession session = new FtpSession();
        session.host = host;
        session.user = user;
        session.password = password;
        session.port = port;
        session.usePassiveMode = usePassiveMode;
        return session;
    }

    public final String getHost()
    {
        return host;
    }

    public final String getErrorText()
    {
        return errorText;
    }

    public final String getLoginDirectory()
    {
        return loginDirectory;
    }

    public final void setProgressNotifier(ProgressNotifier progressNotifier)
    {
        this.progressNotifier = progressNotifier;
    }

    public final boolean isLocked()
    {
        return locked;
    }

    private synchronized boolean lock()
    {
        if (locked)
            return false;
        locked = true;
        return true;
    }

    public synchronized void unlock()
    {
        if (locked) {
            progressNotifier = null;
            locked = false;
        } else
            Debug.bug("FtpSession.unlock session was not locked");
    }

    private boolean changeDirectory(String dirname)
    {
        if (dirname.equals(currentDirectory))
            return true;
        command("CWD " + dirname);
        String s= getReplyString();
        int code = getCode(s);
        if (code == 421) {
            connect();
            if (connected) {
                command("CWD " + dirname);
                s = getReplyString();
            }
            else
                return false;
        }
        if (getCode(s) == 250) {
            currentDirectory = dirname;
            return true;
        }
        return false;
    }

    public boolean isDirectory(String remotePath)
    {
        if (changeDirectory(remotePath))
            return true;

        return false;
    }

    public boolean isFile(String remotePath)
    {
        command("SIZE " + remotePath);
        String s = getReplyString();
        int code = getCode(s);
        if (code == 213)
            return true;
        if (code == 500) {
            // "SIZE" command was not recognized.
            String listing = getDirectoryListingForFile(remotePath);
            if (listing != null && listing.length() > 0) {
                char c = listing.charAt(0);
                if (c == '-' || c == 'l')
                    return true;
            }
        }
        return false;
    }

    public boolean exists(String remotePath)
    {
        if (isDirectory(remotePath))
            return true;
        else
            return isFile(remotePath);
    }

    private long getFileSize(String remotePath)
    {
        long fileSize = -1;
        command("SIZE " + remotePath);
        String s = getReplyString();
        if (getCode(s) == 213) {
            s = s.substring(3).trim();
            try {
                fileSize = Long.parseLong(s);
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
        return fileSize;
    }

    boolean deleteFile(String remotePath)
    {
        command("DELE " + remotePath);
        return getReply() == 250;
    }

    boolean removeDirectory(String remotePath)
    {
        command("RMD " + remotePath);
        return getReply() == 250;
    }

    public boolean chmod(FtpFile file, int permissions)
    {
        if (permissions != 0) {
            FastStringBuffer sb = new FastStringBuffer("SITE CHMOD ");
            sb.append(Integer.toString(permissions, 8));
            sb.append(' ');
            sb.append(file.canonicalPath());
            command(sb.toString());
            return getReply() == 200;
        }
        else
            return false;
    }

    public int getPermissions(FtpFile file)
    {
        int permissions = 0;
        String listing = getDirectoryListingForFile(file.canonicalPath());
        if (listing != null) {
            String s = listing.substring(1, 10);
            Log.debug("s = |" + s + "|");
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
        }
        return permissions;
    }

    int getFileStatus(String filename)
    {
        if (!connected) {
            connect();
            if (!connected)
                return -1;
        }
        if (changeDirectory(filename))
            return 2; // It's a directory.
        // Not a directory.
        if (!openDataSocket())
            return -1;
        int status = -1; // Not found or unknown status.
        command("LIST " + filename);
        int code = getReply();
        // 125 Data connection already open, transfer starting
        // 150 Opening ASCII mode data connection for file list
        // 226 Transfer complete
        if (code == 125 || code == 150 || code == 226) {
            String s = getData();
            closeDataSocket();
            // Look for "drwxrwxrwx"...
            if (s != null && s.length() > 10) {
                char c = s.charAt(0);
                if (c == 'd')
                    status = 2; // Shouldn't happen, since changeDirectory failed.
                else if (c == '-')
                    status = 1;
                if (status != -1) {
                    // Sanity checking.
                    boolean valid = true;
                    c = s.charAt(1);
                    if (c != 'r' && c != 'w' && c != '-')
                        valid = false;
                    else {
                        c = s.charAt(2);
                        if (c != 'r' && c != 'w' && c != '-')
                            valid = false;
                    }
                    if (!valid)
                        status = -1;
                }
            }
            // 125 Data connection already open, transfer starting
            // 150 Opening ASCII mode data connection for file list
            // 226 Transfer complete
            if (code == 125 || code == 150)
                getReply(226);
        }
        return status;
    }

    public String getDirectoryListing(File file)
    {
        if (!(file instanceof FtpFile)) {
            Debug.bug();
            return null;
        }
        Debug.assertTrue(isLocked());
        String listing = DirectoryCache.getDirectoryCache().getListing(file);
        if (listing == null) {
            listing = getDirectoryListing(file.canonicalPath());
            if (listing != null)
                DirectoryCache.getDirectoryCache().put(file, listing);
        }
        return listing;
    }

    public String getDirectoryListing(String dirname)
    {
        Debug.assertTrue(isLocked());
        String listing = null;
        if (verifyConnected()) {
            if (changeDirectory(dirname)) {
                if (openDataSocket()) {
                    command("LIST -la");
                    int code = getReply();

                    // 125 Data connection already open, transfer starting
                    // 150 Opening ASCII mode data connection for file list
                    // 226 Transfer complete
                    if (code == 125 || code == 150 || code == 226)
                        listing = getData();

                    closeDataSocket();
                    if (code == 125 || code == 150)
                        getReply(226);
                }
            }
        }
        return listing;
    }

    public String getDirectoryListingForFile(String filename)
    {
        String listing = null;
        if (connected) {
            if (openDataSocket()) {
                command("LIST -la " + filename);
                int code = getReply();

                // 125 Data connection already open, transfer starting
                // 150 Opening ASCII mode data connection for file list
                // 226 Transfer complete
                if (code == 125 || code == 150 || code == 226)
                    listing = getData();

                closeDataSocket();
                if (code == 125 || code == 150)
                    getReply(226);
            }
        }

        // With at least some FTP servers (ProFTP 1.2.0pre9) the listing
        // will be empty if any part of the filename contains any space
        // characters, although CWD works fine.

        // If that happens we CWD into the parent directory, list all the
        // files, and pick out the line of interest with a regular expression.

        // We check the listing again to make sure the file hasn't changed
        // on the server when we go to write out a modified version.

        if (listing == null || listing.length() == 0) {
            int index = filename.lastIndexOf('/');
            if (index >= 0) {
                String parent = index == 0 ? "/" : filename.substring(0, index);
                String name = filename.substring(index + 1);
                String parentListing = getDirectoryListing(parent);
                if (parentListing != null) {
                    BufferedReader reader =
                        new BufferedReader(new StringReader(parentListing));
                    String entry;
                    try {
                        while ((entry = reader.readLine()) != null) {
                            if (name.equals(DirectoryEntry.getName(entry))) {
                                // Found it!
                                listing = entry;
                                break;
                            }
                        }
                    }
                    catch (IOException e) {
                        Log.error(e);
                    }
                }
            }
        }

        Log.debug("getDirectoryListingForFile |" + listing + "|");
        return listing;
    }

    public int put(File localFile, File remoteFile, long fileSize, boolean saveInPlace)
    {
        boolean succeeded = false;
        boolean cancelled = false;
        String tempName = null;

        // Change directory to prove parent directory exists.
        if (changeDirectory(remoteFile.getParent())) {
            OutputStream out = null;
            if (saveInPlace) {
                out = getOutputStreamForFile(remoteFile.canonicalPath());
            } else {
                // It would be nice to use STOU here, but most servers don't
                // implement it.
                tempName = getUniqueName(remoteFile.getParentFile());
                Log.debug("tempName = |" + tempName + "|");
                if (tempName != null)
                    out = getOutputStreamForFile(tempName);
            }
            if (out != null) {
                try {
                    InputStream in = localFile.getInputStream();
                    byte[] bytes = new byte[16384];
                    long totalBytes = 0;
                    int bytesRead;
                    if (progressNotifier != null)
                        progressNotifier.progressStart();
                    while((bytesRead = in.read(bytes)) > 0) {
                        out.write(bytes, 0, bytesRead);
                        totalBytes += bytesRead;
                        if (progressNotifier != null) {
                            if (progressNotifier.cancelled()) {
                                cancelled = true;
                                break;
                            }
                            progressNotifier.progress("Sent ", totalBytes, fileSize);
                        }
                        // Slow things down for debugging, maybe.
                        Debug.throttle();
                    }
                    if (progressNotifier != null)
                        progressNotifier.progressStop();
                    out.flush();
                    out.close();
                    closeDataSocket();
                    in.close();
                    succeeded = getReply(226);
                    if (cancelled && tempName != null) {
                        command("DELE " + tempName);
                        getReplyString(); // Ignore reply.
                    }
                }
                catch (Exception e) {
                    Log.error(e);
                }
            }
        }

        if (succeeded && !cancelled && tempName != null) {
            do {
                if (progressNotifier != null)
                    progressNotifier.setText("Renaming temporary file");
                command("RNFR " + tempName);
                if (!getReply(350)) {
                    succeeded = false;
                    break;
                }
                command("RNTO " + remoteFile.canonicalPath());
                if (!getReply(250))
                    succeeded = false;
                break;
            } while (false);
        }

        if (progressNotifier != null) {
            if (cancelled)
                progressNotifier.setText("Transfer cancelled");
            else if (succeeded)
                progressNotifier.setText("Transfer completed");
            else
                progressNotifier.setText("Transfer failed");
        }

        if (cancelled)
            return CANCELLED;
        else if (succeeded)
            return SUCCESS;
        else
            return ERROR;
    }

    private static Random random;

    private String getUniqueName(File dir)
    {
        long now = System.currentTimeMillis();
        if (random == null) {
            // Use uptime as seed.
            random = new Random(now - Editor.getStartTimeMillis());
        }
        long n = now + Math.abs(random.nextLong() % now);
        for (int i = 0; i < 100; i++) {
            File file = File.getInstance(dir, String.valueOf(n));
            String name = file.canonicalPath();
            if (!exists(name)) {
                Log.debug("unique name = |" + name + "|");
                return name;
            }
            n += Math.abs(random.nextLong() % now);
        }
        return null;
    }

    public int get(File remoteFile, File localFile, long fileSize)
    {
        boolean succeeded = false;
        boolean cancelled = false;
        if (fileSize == 0) {
            fileSize = getFileSize(remoteFile.canonicalPath());
            if (fileSize < 0) // Error.
                fileSize = 0;
        }
        InputStream  in = getInputStreamForFile(remoteFile.canonicalPath());
        if (in == null)
            return ERROR;
        try {
            OutputStream out = localFile.getOutputStream();
            byte[] bytes = new byte[16384];
            long totalBytes = 0;

            if (progressNotifier != null)
                progressNotifier.progressStart();

            while (true) {
                int bytesRead = 0;
                try {
                    // We may get an exception here if user cancels.
                    bytesRead = in.read(bytes);
                }
                catch (Exception e) {
                    if (progressNotifier == null || !progressNotifier.cancelled())
                        Log.error(e);
                    else
                        // Exception is expected.
                        Log.debug("FtpSession.get cancelled");
                }
                if (bytesRead <= 0)
                    break;
                out.write(bytes, 0, bytesRead);
                totalBytes += bytesRead;
                if (progressNotifier != null) {
                    if (progressNotifier.cancelled()) {
                        cancelled = true;
                        break;
                    }
                    progressNotifier.progress("Received ",  totalBytes, fileSize);
                }
                // Slow things down for debugging, maybe.
                Debug.throttle();
            }
            if (progressNotifier != null)
                progressNotifier.progressStop();
            // If the user cancels, just close the data connection.
            // A bit rude, but it seems to work.
            out.flush();
            out.close();
            in.close();
            closeDataSocket();
            if (cancelled)
                localFile.delete();
            succeeded = getReply(226);
        }
        catch (SocketException e) {
            if (cancelled)
                ; // Exception is expected in this case.
            else
                Log.error(e);
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (progressNotifier != null) {
            if (cancelled)
                progressNotifier.setText("Transfer cancelled");
            else if (succeeded)
                progressNotifier.setText("Transfer completed");
            else
                progressNotifier.setText("Transfer failed");
        }
        if (cancelled)
            return CANCELLED;
        else if (succeeded)
            return SUCCESS;
        else
            return ERROR;
    }

    public synchronized boolean verifyConnected()
    {
        if (connected) {
            command("NOOP");
            if (getReply() == 421) {
                Log.debug("verifyConnected calling connect");
                connect();
            }
        } else
            connect();
        if (connected && progressNotifier != null)
            progressNotifier.setText("Connected to " + host);
        return connected;
    }

    private synchronized void connect()
    {
        if (progressNotifier != null)
            progressNotifier.setText("Connecting to " + host);
        Log.debug("connecting to " + host);

        connected = false;
        loginDirectory = null;
        currentDirectory = null;
        errorText = null;

        SocketConnection sc = new SocketConnection(host, port, 30000, 200, progressNotifier);
        controlSocket = sc.connect();
        if (controlSocket == null) {
            errorText = sc.getErrorText();
            return;
        }

        try {
            controlIn =
                new BufferedReader(new InputStreamReader(controlSocket.getInputStream()));
            controlOut = new OutputStreamWriter(controlSocket.getOutputStream());
        }
        catch (IOException e) {
            Log.error(e);
            disconnect();
            return;
        }
        getReplyString();
        command("USER " + user);
        int code = getReply();
        if (code == 331) {
            // Password required.
            if (password != null) {
                command("PASS " + password);
                code = getReply();
                if (code == 530) {
                    errorText = "Login incorrect";
                    user = null;
                    password = null;
                }
            }
        }
        if (code != 230) {
            if (errorText == null || errorText.length() == 0) {
                if (lastReply != null)
                    errorText = lastReply.substring(3).trim();
                if (errorText == null || errorText.length() == 0)
                    errorText = "Unable to connect to " + host;
            }
            disconnect();
            return;
        }

        // Determine login directory.
        command("PWD");
        String s = getReplyString();
        int begin = s.indexOf('"');
        if (begin >= 0) {
            int end = s.indexOf('"', ++begin);
            if (end >= 0)
                loginDirectory = currentDirectory = s.substring(begin, end);
        }

        command("TYPE I");
        code = getReply();
        if (code != 200) {
            Log.error("connect didn't get 200");
            disconnect();
            return;
        }

        connected = true;
        Log.debug("connected!");
    }

    private void command(String s)
    {
        boolean reconnect = false;
        if (echo)
            Log.debug("==> " + (s.startsWith("PASS ") ? "PASS" : s));
        try {
            controlOut.write(s + "\r\n");
            controlOut.flush();
            return;
        }
        catch (Exception e) {
            Log.error("exception command " + s);
            reconnect = true;
        }
        if (reconnect) {
            // Exception may mean we were disconnected by remote host because
            // of inactivity. Try to reconnect.
            Log.debug("trying to reconnect...");
            connect();
            if (connected) {
                if (echo)
                    Log.debug("==> " + (s.startsWith("PASS ") ? "PASS" : s));
                try {
                    controlOut.write(s + "\r\n");
                    controlOut.flush();
                    return;
                }
                catch (Exception e) {
                    Log.error("2nd exception command " + s);
                    reconnect = true;
                }
            }
        }
    }

    private String lastReply;

    // Returns final line of reply.
    private String getReplyString()
    {
        String s = null;
        try {
            do {
                s = controlIn.readLine();
                if (echo && s != null)
                    Log.debug("<== " + s);
            } while (s != null && !isEndOfReply(s));
        }
        catch (Exception e) {}
        lastReply = s;
        return s;
    }

    private static boolean isEndOfReply(String s)
    {
        // If it's not the last line of the reply, the 4th char will be '-'.
        if (s != null &&
            s.length() >= 4 &&
            s.charAt(3) == ' ' &&
            Character.isDigit(s.charAt(0)) &&
            Character.isDigit(s.charAt(1)) &&
            Character.isDigit(s.charAt(2)))
        {
            return true;
        }

        return false;
    }

    private boolean getReply(int required)
    {
        String s = getReplyString();
        if (s == null)
            return false;
        return getCode(s) == required;
    }

    private int getReply()
    {
        String s = getReplyString();
        if (s ==  null)
            return 421;
        return getCode(s);
    }

    private static int getCode(String reply)
    {
        int code = -1;
        if (reply != null) {
            // We do sanity checking when we get the reply string, so we don't
            // need it here.
            try {
                code = Integer.parseInt(reply.substring(0, 3));
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
        return code;
    }

    private String getData()
    {
        if (!usePassiveMode)
            acceptConnectionFromServer();
        byte[] buf = new byte[16384];
        FastStringBuffer sb = new FastStringBuffer();
        try {
            while (true) {
                int bytesRead = dataIn.read(buf); // Blocks.
                if (bytesRead < 0)
                    break;
                sb.append(new String(buf, 0, bytesRead));
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        return sb.toString();
    }

    private void closeDataSocket()
    {
        if (dataSocket != null) {
            if (dataIn != null) {
                try {
                    dataIn.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            if (dataOut != null) {
                try {
                    dataOut.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            try {
                dataSocket.close();
            }
            catch (IOException e){
                Log.error(e);
            }
            dataSocket = null;
            dataIn = null;
            dataOut = null;
        }
    }

    private boolean openDataSocket()
    {
        if (!connected) {
            connect();
            if (!connected)
                return false;
        }
        closeDataSocket();
        if (usePassiveMode) {
            command("PASV");
            String reply;
            while (true){
                reply = getReplyString();
                if (reply == null) {
                    Log.error("openDataSocket null reply to PASV");
                    return false;
                }
                int code = getCode(reply);
                if (code >= 400)
                    return false; // Error.
                if (code == 227)
                    break; // The one we want.
                // Otherwise the reply we received is not a reply to the PASV command.
                // Loop back and try again.
            }
            int begin = reply.indexOf('(');
            if (begin < 0)
                return false;
            int end = reply.indexOf(')');
            if (end < 0)
                return false;
            String s = reply.substring(begin+1, end);
            StringTokenizer st = new StringTokenizer(s, ",", false);
            if (st.countTokens() != 6)
                return false;
            String[] addr = new String[4];
            for (int i = 0; i < 4; i++)
                addr[i] = st.nextToken();
            String address = addr[0] + "." + addr[1] + "." + addr[2] + "." + addr[3];
            try {
                int hibyte = Integer.parseInt(st.nextToken());
                int lobyte = Integer.parseInt(st.nextToken());
                int dataPort = hibyte * 256 + lobyte;
                Log.debug("opening data socket");
                dataSocket = new Socket(address, dataPort);
                dataIn = dataSocket.getInputStream();
                dataOut = dataSocket.getOutputStream();
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
            catch (IOException ex) {
                Log.error(ex);
            }
            return dataSocket != null;
        } else {
            // We're not using passive mode.
            try {
                InetAddress localHost = controlSocket.getLocalAddress();
                serverSocket = new ServerSocket(0);
                int dataPort = serverSocket.getLocalPort();
                int lobyte = (dataPort & 0x00ff);
                int hibyte = ((dataPort & 0xff00) >> 8);
                byte[] addrBytes = localHost.getAddress();
                int[] addr = new int[4];
                for (int i = 0; i < 4; i++) {
                    addr[i] = addrBytes[i];
                    if (addr[i] < 0)
                        addr[i] += 256;
                }
                String s = "PORT " + addr[0] + "," + addr[1] + "," + addr[2] + "," + addr[3] + "," + hibyte + "," + lobyte;
                command(s);
                return getReply(200);
            }
            catch (Exception e) {
                Log.error(e);
            }
            return false;
        }
    }

    private void acceptConnectionFromServer()
    {
        try {
            dataSocket = serverSocket.accept();
            if (dataSocket != null) {
                dataIn = dataSocket.getInputStream();
                dataOut = dataSocket.getOutputStream();
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private InputStream getInputStreamForFile(String filename)
    {
        if (openDataSocket()) {
            command("RETR " + filename);
            int code = getCode(getReplyString());
            if (code > -1 && code < 400) {
                if (!usePassiveMode)
                    acceptConnectionFromServer();
                return dataIn;
            }
        }
        return null;
    }

    private OutputStream getOutputStreamForFile(String filename)
    {
        if (filename == null)
            return null;
        if (openDataSocket()) {
            command("STOR " + filename);
            int code = getCode(getReplyString());
            if (code > -1 && code < 400) {
                if (!usePassiveMode)
                    acceptConnectionFromServer();
                return dataOut;
            }
            if (code == 550 || code == 553)
                errorText = "Access denied";
        }
        return null;
    }

    private static synchronized void cleanup()
    {
        // Walk buffer list in event dispatch thread.
        if (!SwingUtilities.isEventDispatchThread()) {
            Debug.bug();
            return;
        }
        if (sessionList.size() == 0)
            return; // Nothing to do.
        for (int i = sessionList.size(); i-- > 0;) {
            FtpSession session = (FtpSession) sessionList.get(i);
            String host = session.getHost();
            boolean inUse = false;
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf.getFile() instanceof FtpFile) {
                    if (host.equals(buf.getFile().getHostName())) {
                         inUse = true;
                         break;
                    }
                }
            }
            if (!inUse) {
                session.close();
                unregister(session);
            }
        }
        if (sessionList.size() == 0) {
            if (cleanupThread != null) {
                cleanupThread.cancel();
                cleanupThread = null;
            }
        }
    }

    private static final Runnable cleanupRunnable = new Runnable() {
        public void run()
        {
            cleanup();
        }
    };

    public synchronized void disconnect()
    {
        Log.debug("disconnect");
        if (controlSocket != null) {
            Log.debug("closing control socket...");
            try {
                controlSocket.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
            controlSocket = null;
            controlIn = null;
            controlOut = null;
        }
        if (dataSocket != null) {
            Log.debug("closing data socket...");
            try {
                dataSocket.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
            dataSocket = null;
            dataIn = null;
            dataOut = null;
        }
        connected = false;
    }

    private void close()
    {
        Log.debug("FtpSession.close");
        if (connected) {
            final Editor editor = Editor.currentEditor();
            editor.setWaitCursor();
            Runnable r = new Runnable() {
                public void run()
                {
                    try {
                        if (echo)
                            Log.debug("==> QUIT");
                        controlOut.write("QUIT\r\n");
                        controlOut.flush();
                        getReply();
                    }
                    catch (IOException e) {}
                }
            };
            Thread t = new Thread(r);
            t.start();
            try {
                t.join(3000);
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
            if (t.isAlive()) {
                Log.debug("stopping QUIT thread");
                t.stop();
            }
            disconnect();
            editor.setDefaultCursor();
        }
        Log.debug("leaving close");
    }

    public static synchronized FtpSession getSession(FtpFile file)
    {
        if (file == null)
            return null;
        Login login = new Login(file.getHostName(), file.getUserName(), file.getPassword());
        if (login == null)
            return null; // Invalid URL.
        // Try to find an idle session for the required host.
        FtpSession session = lockSession(login.host, file.getPort());
        if (session != null) {
            if (session.checkLogin())
                return session;

            session.unlock();
            return null;
        }
        // No idle session for this host. Try to find a session to clone.
        session = findSession(login.host, file.getPort());
        if (session != null) {
            session = (FtpSession) session.clone();
            if (session.lock()) {
                if (session.checkLogin())
                    return session;
                session.unlock();
            }
            return null;
        }
        if (login.user == null || login.password == null) {
            // The URL lacked either a user name or a password or both.
            final Editor editor = Editor.currentEditor();
            if (login.user == null) {
                // No user name in URL. We'll take the first entry in ~/.netrc
                // for the host in question.
                Login maybe = Netrc.getLogin(login.host);
                if (maybe != null) {
                    login.user = maybe.user;
                    login.password = maybe.password;
                }
                if (login.user == null) {
                    // Nothing relevant in ~/.netrc.  Ask the user.
                    login.user = InputDialog.showInputDialog(editor, "Login:", "Login on " + login.host);
                    editor.repaintNow();
                }
            }
            if (login.user == null)
                return null; // User cancelled.
            if (login.user.length() == 0)
                login.user = "anonymous";
            if (login.password == null) {
                // We have host and user name but no password.
                login.password = Netrc.getPassword(login.host, login.user);
                if (login.password == null) {
                    if (login.user.equals("anonymous"))
                        login.password = Editor.preferences().getStringProperty(Property.FTP_ANONYMOUS_PASSWORD);
                    if (login.password == null) {
                        // Ask the user.
                        login.password = PasswordDialog.showPasswordDialog(editor, "Password:", "Password");
                        editor.repaintNow();
                    }
                }
            }
            // If we don't have a password, give up (it can be blank, but not
            // null).
            if (login.password == null)
                return null;
        }
        // Final sanity check.
        if (login.user.length() == 0)
            return null;
        // At this point we have non-empty strings for host and user name, and
        // a non-null password.
        session = new FtpSession(login, file.getPort());
        if (session.lock())
            return session;
        return null;
    }

    // Make sure the login is comnplete. Get the user to enter the username
    // and/or password if missing. Don't look in .netrc or preferences; we may
    // be here because the information in .netrc or preferences didn't work.
    private boolean checkLogin()
    {
        final Editor editor = Editor.currentEditor();
        if (user == null) {
            // Nothing relevant in ~/.netrc.  Ask the user.
            user = InputDialog.showInputDialog(editor, "Login:", "Login on " + host);
            editor.repaintNow();
            if (user == null)
                return false; // User cancelled.
        }
        if (user.length() == 0)
            user = "anonymous";
        if (password == null) {
            // Ask the user.
            password = PasswordDialog.showPasswordDialog(editor, "Password:", "Password");
            editor.repaintNow();
            // If we don't have a password, give up.
            if (password == null)
                return false;
        }
        // Final sanity check.
        if (user.length() == 0)
            return false;
        return true;
    }

    private static FtpSession lockSession(String host, int port)
    {
        for (int i = 0; i < sessionList.size(); i++) {
            FtpSession session = (FtpSession) sessionList.get(i);
            if (session.host.equals(host) && session.port == port) {
                if (session.lock())
                    return session;
            }
        }
        return null;
    }

    private static FtpSession findSession(String host, int port)
    {
        for (int i = 0; i < sessionList.size(); i++) {
            FtpSession session = (FtpSession) sessionList.get(i);
            if (session.host.equals(host) && session.port == port)
                return session;
        }
        return null;
    }
}
