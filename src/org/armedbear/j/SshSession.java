/*
 * SshSession.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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
import gnu.regexp.REException;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import javax.swing.SwingUtilities;

public final class SshSession implements Constants
{
    public static final int DEFAULT_PORT = 22;

    private static final int TRY_AGAIN     = 0;
    private static final int AUTHENTICATED = 1;
    private static final int PASSWORD      = 2;
    private static final int PASSPHRASE    = 3;
    private static final int YES           = 4;
    private static final int NO            = 5;

    private static final String PROMPT = "$ ";

    private static ArrayList sessionList;

    private static CleanupThread cleanupThread;

    private String hostName;
    private String userName;
    private String password;
    private String passphrase;
    private int port;

    private boolean locked;

    private Process process;

    private OutputStreamWriter stdin;
    private StdoutThread stdoutThread;
    private StderrThread stderrThread;

    private FastStringBuffer output = new FastStringBuffer();

    private boolean connected;

    private String loginDirectory;

    private boolean echo;

    // tcsh doesn't like "\cd", so we may change it to "cd" later.
    private String cd = "\\cd";

    private Buffer outputBuffer;

    private String passwordTitle;
    private String passwordPrompt;

    private SshSession(SshFile file, boolean locked)
    {
        hostName = file.getHostName();
        Debug.assertTrue(hostName != null);
        userName = file.getUserName();
        password = file.getPassword();
        port = file.getPort();
        this.locked = locked;
        register(this);
    }

    private SshSession(SshSession other, boolean locked)
    {
        hostName = other.getHostName();
        Debug.assertTrue(hostName != null);
        userName = other.getUserName();
        password = other.getPassword();
        passphrase = other.getPassphrase();
        port = other.getPort();
        this.locked = locked;
        register(this);
    }

    private static synchronized void register(SshSession session)
    {
        if (sessionList == null)
            sessionList = new ArrayList();
        sessionList.add(session);
        if (cleanupThread == null) {
            cleanupThread = new CleanupThread(cleanupRunnable);
            cleanupThread.start();
        }
        Log.debug("leaving register() session count = " + sessionList.size());
    }

    private static synchronized void unregister(SshSession session)
    {
        if (sessionList == null) {
            Debug.bug();
            return;
        }
        if (!sessionList.contains(session))
            Debug.bug();
        sessionList.remove(session);
        Log.debug("leaving unregister() session count = "
            + sessionList.size());
    }

    public static synchronized SshSession getSession(SshFile file)
    {
        if (file == null) {
            Debug.bug();
            return null;
        }
        if (file.getHostName() == null) {
            Debug.bug();
            return null;
        }
        if (file.getUserName() == null) {
            Debug.bug();
            file.setUserName(System.getProperty("user.name"));
        }
        SshSession session = lockSession(file);
        if (session != null)
            return session;
        // No idle session found for this file. Try to find a session to clone.
        session = findSession(file);
        if (session != null)
            return new SshSession(session, true);
        // No session to clone.
        return new SshSession(file, true);
    }

    // Called only from synchronized methods.
    private static SshSession lockSession(SshFile file)
    {
        if (sessionList != null) {
            for (int i = sessionList.size(); i-- > 0;) {
                SshSession session = (SshSession) sessionList.get(i);
                if (session.getUserName().equals(file.getUserName())) {
                    if (session.getHostName().equals(file.getHostName())) {
                        if (session.getPort() == file.getPort()) {
                            if (session.lock()) {
                                return session;
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    // Called only from synchronized methods.
    private static SshSession findSession(SshFile file)
    {
        if (sessionList != null) {
            for (int i = sessionList.size(); i-- > 0;) {
                SshSession session = (SshSession) sessionList.get(i);
                if (session.getUserName().equals(file.getUserName())) {
                    if (session.getHostName().equals(file.getHostName())) {
                        if (session.getPort() == file.getPort()) {
                            return session;
                        }
                    }
                }
            }
        }
        return null;
    }

    public final synchronized boolean isLocked()
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
        if (locked)
            locked = false;
        else
            Debug.bug("SshSession.unlock session was not locked");
        synchronized (SshSession.class) {
            if (sessionList != null)
                Log.debug("unlock session count = " + sessionList.size());
            else
                Debug.bug("SshSession.unlock no session list");
        }
    }

    public final String getHostName()
    {
        return hostName;
    }

    public final String getUserName()
    {
        return userName;
    }

    public final String getPassword()
    {
        return password;
    }

    public final void setPassword(String password)
    {
        this.password = password;
    }

    public final String getPassphrase()
    {
        return passphrase;
    }

    public final int getPort()
    {
        return port;
    }

    public final String getLoginDirectory()
    {
        return loginDirectory;
    }

    public void setOutputBuffer(Buffer buf)
    {
        outputBuffer = buf;
    }

    public boolean isDirectory(String canonicalPath)
    {
        SshFile file =
            new SshFile(hostName, canonicalPath, userName, null, port);
        if (DirectoryCache.getDirectoryCache().getListing(file) != null)
            return true;
        if (!connect())
            return false;
        try {
            return changeDirectory(canonicalPath);
        }
        catch (Exception e) {
            Log.error(e);
        }
        // An exception occurred. We're confused. Wait a moment.
        try {
            Thread.sleep(1000);
        }
        catch (InterruptedException e) {
            Log.error(e);
        }
        //  Try again.
        try {
            Log.warn("isDirectory() retrying after exception ...");
            // Send an empty command to flush the channel.
            command("");
            return changeDirectory(canonicalPath);
        }
        catch (Exception e) {
            // Another exception. Give up.
            Log.error(e);
            return false;
        }
    }

    private String stat(String canonicalPath)
    {
        FastStringBuffer sb = new FastStringBuffer("stat -t \"");
        sb.append(canonicalPath);
        sb.append('"');
        String cmd = sb.toString();
        String response = command(cmd);
        Log.debug(cmd + " " + response);
        if (response != null && response.startsWith(canonicalPath))
            return response.substring(canonicalPath.length()).trim();
        return null;
    }

    // Determines file type from string returned by stat.
    private static int getType(String s)
    {
        if (s != null) {
            List tokens = Utilities.tokenize(s);
            if (tokens.size() == 14) {
                String token = (String) tokens.get(2);
                Log.debug("token = |" + token + "|");
                try {
                    int n = Integer.parseInt(token, 16);
                    Log.debug("n = " + Integer.toString(n, 8));
                    if ((n & 0040000) == 0040000)
                        return File.TYPE_DIRECTORY;
                    if ((n & 0120000) == 0120000)
                        return File.TYPE_LINK;
                    if ((n & 0100000) == 0100000)
                        return File.TYPE_FILE;
                }
                catch (NumberFormatException e) {}
            }
        }
        return File.TYPE_UNKNOWN;
    }

    // Throws an exception if we don't recognize the response.
    private boolean changeDirectory(String canonicalPath) throws Exception
    {
        FastStringBuffer sb = new FastStringBuffer(cd);
        sb.append(" \"");
        sb.append(canonicalPath);
        sb.append('"');
        String cmd = sb.toString();
        Log.debug("changeDirectory cmd = |" + cmd + "|");
        String response = command(cmd);
        Log.debug("changeDirectory response = |" + response + "|");
        if (response == null)
            throw new Exception(); // Lost connection.
        if (response.indexOf("Command not found") >= 0) {
            if (cd.equals("\\cd")) {
                // tcsh doesn't like "\cd". Try again with "cd".
                cd = "cd";
                response = command(cd + " \"" + canonicalPath + '"');
                if (response == null)
                    throw new Exception(); // Lost connection.
            } else
                return false;
        }
        if (response.equals(PROMPT)) {
            // No news is good news.
            Log.debug("changeDirectory succeeded");
            return true;
        }
        String lower = response.toLowerCase();
        if (lower.indexOf("not a directory") >= 0)
            return false;
        else if (lower.indexOf("no such file or directory") >= 0)
            return false;
        // If the directory name is very long (> 80 chars or so), bash will
        // wrap (i.e. mangle) the response. Try to detect that situation.
        int index = response.indexOf('\r');
        if (index < 0)
            index = response.indexOf('\n');
        if (index >= 0) {
            String beginning = response.substring(0, index);
            Log.debug("beginning = |" + beginning + "|");
            if (cmd.startsWith(beginning)) {
                Log.debug("cmd starts with beginning!");
                index = response.lastIndexOf((char)8);
                if (index >= 0) {
                    Log.debug("backspace found!");
                    String end = response.substring(index+1);
                    Log.debug("end = |" + end + "|");
                    if (cmd.endsWith(end)) {
                        Log.debug("cmd ends with end!");
                        return true;
                    }
                }
            }
        }
        // Unknown response. We must be confused.
        throw new Exception();
    }

    public boolean exists(String canonicalPath)
    {
        if (connect()) {
            String response = lsld(canonicalPath);
            if (response != null && response.length() > 0) {
                char c = response.charAt(0);
                if (c == 'd' || c == '-')
                    return true;
            }
        }
        return false;
    }

    public static String getDirectoryListing(File file)
    {
        if (!(file instanceof SshFile)) {
            Debug.assertTrue(false);
            return null;
        }
        String listing = DirectoryCache.getDirectoryCache().getListing(file);
        if (listing == null) {
            SshSession session = getSession((SshFile)file);
            if (session != null) {
                listing = session.retrieveDirectoryListing(file);
                session.unlock();
                if (listing != null)
                    DirectoryCache.getDirectoryCache().put(file, listing);
            }
        }
        return listing;
    }

    public String retrieveDirectoryListing(File file)
    {
        if (!(file instanceof SshFile)) {
            Debug.bug();
            return null;
        }
        if (!isLocked()) {
            Debug.bug();
            return null;
        }
        if (connect()) {
            try {
                // Do it this way to support symlinks.
                if (changeDirectory(file.canonicalPath()))
                    // Change directory succeeded. Do ls -la here.
                    return lsla();
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
        return null;
    }

    public synchronized boolean chmod(SshFile file, int permissions)
    {
        if (permissions != 0 && connect()) {
            FastStringBuffer sb = new FastStringBuffer("chmod ");
            sb.append(Integer.toString(permissions, 8));
            sb.append(' ');
            sb.append(file.canonicalPath());
            final String response = command(sb.toString());
            // Look for error message in response.
            if (response != null && response.indexOf("chmod:") < 0) {
                // Success! Remove old entry (if any) from directory cache.
                DirectoryCache.getDirectoryCache().purge(file);
                return true;
            }
        }
        return false;
    }

    public synchronized boolean isConnected()
    {
        return connected;
    }

    public synchronized boolean connect()
    {
        if (connected) {
            Log.debug("SshSession.connect(): already connected");
            return true;
        }
        FastStringBuffer sb = new FastStringBuffer("jpty ssh ");
        if (userName != null && userName.length() > 0) {
            sb.append("-l ");
            sb.append(userName);
            sb.append(' ');
        }
        if (port != DEFAULT_PORT) {
            sb.append("-p ");
            sb.append(port);
            sb.append(' ');
        }
        sb.append(hostName);
        try {
            process = Runtime.getRuntime().exec(sb.toString());
        }
        catch (Throwable t) {
            Log.error(t);
            return false;
        }
        try {
            stdin = new OutputStreamWriter(process.getOutputStream());
            int timeout =
                Editor.preferences().getIntegerProperty(Property.SSH_TIMEOUT);
            Log.debug("ssh timeout is " + timeout + " ms");
            stdoutThread = new StdoutThread();
            stdoutThread.setTimeOut(timeout);
            stderrThread = new StderrThread();
            stderrThread.setTimeOut(timeout);
            stdoutThread.start();
            stderrThread.start();
        }
        catch (Throwable t) {
            Log.error(t);
            return false;
        }
        if (!authenticate()) {
            killProcess();
            String message = output.toString().trim();
            if (message.length() == 0)
                message = "Authentication failed";
            MessageDialog.showMessageDialog(message, "Error");
            return false;
        }
        if (!connected) {
            killProcess();
            MessageDialog.showMessageDialog("Lost connection", "Error");
        }
        return connected;
    }

    private void initializeConnection()
    {
        boolean oldEcho = echo;
        echo = true;
        String response = command("exec /bin/sh");
        Log.debug("response = |" + response + "|");
        FastStringBuffer sb = new FastStringBuffer("PS1='");
        sb.append(PROMPT);
        sb.append('\'');
        response = command(sb.toString());
        Log.debug("response = |" + response + "|");
        Log.debug("PROMPT   = |" + PROMPT + "|");
        command("unset MAILCHECK");
        loginDirectory = getCurrentDirectory();
        connected = (loginDirectory != null);
        echo = oldEcho;
    }

    private boolean authenticate()
    {
        output.setLength(0);
        int response;
        while ((response = checkInitialResponse()) == TRY_AGAIN) {
            Log.debug("authenticate() TRY_AGAIN");
            try {
                final long TIMEOUT = 30000; // 30 seconds
                long start = System.currentTimeMillis();
                wait(TIMEOUT);
                if (System.currentTimeMillis() - start > TIMEOUT)
                    return false;
            }
            catch (InterruptedException e) {
                Log.debug("SshSession.connect interrupted!");
                return false;
            }
        }
        if (response == AUTHENTICATED) {
            initializeConnection();
            return connected;
        }
        if (response == PASSWORD)
            return authenticateWithPassword();
        if (response == PASSPHRASE)
            return authenticateWithPassphrase();
        return false;
    }

    private boolean authenticateWithPassword()
    {
        if (password == null) {
            password = Netrc.getPassword(hostName, userName);
            if (password == null) {
                if (SwingUtilities.isEventDispatchThread())
                    getPasswordRunnable.run();
                else {
                    try {
                        SwingUtilities.invokeAndWait(getPasswordRunnable);
                    }
                    catch (Exception e) {
                        Log.error(e);
                    }
                }
                if (password == null) {
                    killProcess();
                    return false;
                }
            }
        }
        return _authenticate(password);
    }

    private boolean authenticateWithPassphrase()
    {
        if (passphrase == null) {
            if (SwingUtilities.isEventDispatchThread())
                getPassphraseRunnable.run();
            else {
                try {
                    SwingUtilities.invokeAndWait(getPassphraseRunnable);
                }
                catch (Exception e) {
                    Log.error(e);
                }
            }
            if (passphrase == null) {
                killProcess();
                return false;
            }
        }
        return _authenticate(passphrase);
    }

    private boolean _authenticate(String pass)
    {
        output.setLength(0);
        boolean oldEcho = echo;
        echo = true;
        sendPass(pass);
        if (checkAuthenticationResponse()) {
            Log.debug("authenticate SUCCEEDED!");
            initializeConnection();
            echo = oldEcho;
            return connected;
        } else {
            Log.debug("authenticate FAILED!");
            echo = oldEcho;
            return false;
        }
    }

    private int checkInitialResponse()
    {
        final String s = output.toString().trim();
        String check;
        int index = s.lastIndexOf("\r\n");
        if (index >= 0) {
            check = s.substring(index+2);
        } else {
            index = s.lastIndexOf('\n');
            if (index >=0 )
                check = s.substring(index+1);
            else
                check = s;
        }
        Log.debug("check = |" + check + "|");
        String lower = check.toLowerCase();
        if (lower.indexOf("connection refused") >= 0)
            return NO;
        if (lower.endsWith("password:")) {
            passwordTitle = "Password";
            passwordPrompt = check;
            return PASSWORD;
        }
        if (s.startsWith("Password:") && lower.endsWith("response:")) {
            passwordTitle = "Password";
            passwordPrompt = check;
            return PASSWORD;
        }
        if (lower.startsWith("enter passphrase ") && lower.endsWith(":")) {
            // We don't want to use the password from .netrc in this situation.
            password = null;
            passwordTitle = "Passphrase";
            passwordPrompt = check;
            return PASSPHRASE;
        }
        RE promptRE = getPromptRE();
        if (promptRE.getMatch(lower) != null)
            return AUTHENTICATED;
        return TRY_AGAIN;
    }

    private boolean checkAuthenticationResponse()
    {
        String s = output.toString();
        Log.debug("checkAuthenticationResponse output = |" + output + "|");
        int result = checkResponse(s);
        Log.debug("checkAuthenticationResponse result = " + result);
        while (result == TRY_AGAIN) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return false;
            }
            s = output.toString();
            Log.debug("checkAuthenticationResponse output = |" + output + "|");
            result = checkResponse(s);
            Log.debug("checkAuthenticationResponse result = " + result);
        }
        Log.debug("checkAuthenticationResponse returning " + (result == YES));
        return result == YES;
    }

    // Helper for checkAuthenticationResponse().
    private int checkResponse(String s)
    {
        if (s.toLowerCase().indexOf("denied") >= 0)
            return NO;
        String prompt = null;
        for (int i = s.length(); i-- > 0;) {
            char c = s.charAt(i);
            if (c == '\r' || c == '\n') {
                prompt = s.substring(i+1);
                break;
            }
        }
        if (prompt == null)
            prompt = s;
        Log.debug("prompt = |" + reveal(prompt) + "|");
        RE promptRE = getPromptRE();
        Debug.assertTrue(promptRE != null);
        REMatch match = promptRE.getMatch(prompt);
        if (match != null)
            return YES;
        return TRY_AGAIN;
    }

    private RE _promptRE;

    public RE getPromptRE()
    {
        if (_promptRE == null) {
            try {
                _promptRE = new RE(Editor.preferences().
                    getStringProperty(Property.SSH_PROMPT_PATTERN));
            }
            catch (REException e) {
                Log.error(e);
                _promptRE = new UncheckedRE(DEFAULT_SHELL_PROMPT_PATTERN);
            }
        }
        Debug.assertTrue(_promptRE != null);
        return _promptRE;
    }

    private Runnable getPasswordRunnable = new Runnable() {
        public void run()
        {
            final Editor editor = Editor.currentEditor();
            editor.setDefaultCursor();
            password = PasswordDialog.showPasswordDialog(editor, passwordPrompt,
                passwordTitle);
            editor.setWaitCursor();
        }
    };

    private Runnable getPassphraseRunnable = new Runnable() {
        public void run()
        {
            final Editor editor = Editor.currentEditor();
            editor.setDefaultCursor();
            passphrase = PasswordDialog.showPasswordDialog(editor, passwordPrompt,
                passwordTitle);
            editor.setWaitCursor();
        }
    };

    private String getCurrentDirectory()
    {
        final String s = command("pwd");
        if (s == null)
            return null; // Lost connection.
        int index = s.indexOf("\r\n");
        if (index < 0)
            index = s.indexOf('\n');
        final String dir = index >= 0 ? s.substring(0, index) : s;
        Log.debug("getCurrentDirectory() returning |" + dir + "|");
        return dir;
    }

    private void disconnect()
    {
        if (connected) {
            try {
                stdin.write("exit\n");
            }
            catch (Exception e) {
                Log.error(e);
            }
            killProcess();
        }
    }

    private void killProcess()
    {
        Process p = process; // Avoid races.
        if (p != null) {
            try {
                Log.debug("calling Process.destroy()");
                p.destroy();
                Log.debug("calling Process.waitFor()");
                p.waitFor();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
            process = null;
            synchronized (this) {
                if (stdin != null) {
                    try {
                        stdin.close();
                    }
                    catch (IOException e) {
                        Log.error(e);
                    }
                    stdin = null;
                }
                if (stdoutThread != null) {
                    stdoutThread.cancel();
                    stdoutThread = null;
                }
                if (stderrThread != null) {
                    stderrThread.cancel();
                    stderrThread = null;
                }
            }
        }
        connected = false;
    }

    public synchronized final void dispose()
    {
        Log.debug("SshSession.dispose");
        if (connected)
            disconnect();
        unregister(this);
    }

    private synchronized String command(String cmd)
    {
        if (!write(cmd.concat("\n")))
            return null;
        output.setLength(0);
        while (output.length() == 0) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
        String s = output.toString();
        // Strip echo of original command.
        if (s.startsWith(cmd)) {
            int i = cmd.length();
            while (i < s.length()) {
                char c = s.charAt(i);
                if (c == '\r' || c == '\n')
                    ++i;
                else
                    break;
            }
            if (i > cmd.length())
                s = s.substring(i);
        }
        // Strip prompt.
        int index = s.lastIndexOf("\r\n");
        if (index >= 0) {
            s = s.substring(0, index);
        } else {
            index = s.lastIndexOf('\n');
            if (index >= 0)
                s = s.substring(0, index);
        }
        return s;
    }

    private static final RE totalRE = new UncheckedRE("\\n?[^0-9]+ [0-9]+");

    private synchronized String lsla()
    {
        boolean valid = false;
        if (!write("\\ls -la\n"))
            return null;
        output.setLength(0);
        String s = null;
        for (int i = 0; i < 2; i++) {
            if (i > 0)
                Log.debug("lsla retry " + i);
            try {
                wait();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return null;
            }
            s = output.toString();
            REMatch match = totalRE.getMatch(s);
            Log.debug("match = |" + match + "|");
            if (match == null) {
                Log.error("lsla no \"total\" line");
                continue;
            }
            s = s.substring(match.getEndIndex());
            int index = s.indexOf('\n');
            if (index < 0) {
                // Shouldn't happen.
                Log.error("lsla no '\\n'");
                continue;
            }
            s = s.substring(index + 1);
            valid = true;
            break;
        }
        if (!valid) {
            Log.error("lsla output not valid - returning null");
            return null;
        }
        // Strip prompt.
        int index = s.lastIndexOf("\r\n");
        if (index >= 0)
            s = s.substring(0, index);
        else {
            index = s.lastIndexOf('\n');
            if (index >= 0)
                s = s.substring(0, index);
        }
        return s;
    }

    // Do ls -ld on one file or directory.
    private synchronized String lsld(String path)
    {
        Debug.assertTrue(path != null);
        Debug.assertTrue(path.length() != 0);
        FastStringBuffer sb = new FastStringBuffer("\\ls -ld \"");
        sb.append(path);
        sb.append('"');
        sb.append('\n');
        if (!write(sb.toString()))
            return null;
        output.setLength(0);
        while (output.length() == 0){
            try {
                wait();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
        String s = output.toString();

        // Strip echo of original command.
        if (s.startsWith("ls -ld ")) {
            // Strip through end of line.
            int index = s.indexOf('\n');
            if (index < 0)
                return null; // Shouldn't happen.
            s = s.substring(index + 1);
        }

        // Skip lines starting with '<'.
        while (s.length() > 0 && s.charAt(0) == '<') {
            int index = s.indexOf('\n');
            if (index < 0)
                return null; // Shouldn't happen.
            s = s.substring(index + 1);
        }

        // Now we've arrived at the line we want. Strip "\r\n" or '\n'.
        int index = s.indexOf("\r\n");
        if (index >= 0) {
            s = s.substring(0, index);
        } else {
            index = s.lastIndexOf('\n');
            if (index >= 0)
                s = s.substring(0, index);
        }
        return s;
    }

    // Password or passphrase.
    private void sendPass(String pass)
    {
        Debug.assertTrue(pass != null);
        try {
            stdin.write(pass);
            stdin.write("\n");
            stdin.flush();
            if (outputBuffer != null) {
                FastStringBuffer sb = new FastStringBuffer("==> ");
                for (int i = pass.length(); i-- > 0;)
                    sb.append('*');
                sb.append('\n');
                writeToOutputBuffer(sb.toString());
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private boolean write(String s)
    {
        try {
            if (echo || Editor.preferences().getBooleanProperty(Property.SSH_ECHO))
                Log.debug("==> |" + s + "|");
            if (outputBuffer != null)
                writeToOutputBuffer("==> " + s);
            stdin.write(s);
            stdin.flush();
            return true;
        }
        catch (IOException e) {
            Log.error(e);
            killProcess();
            return false;
        }
    }

    private void writeToOutputBuffer(final String s)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                // Avoid race (and NPE) if setOutputBuffer(null) gets called in
                // another thread.
                final Buffer buf = outputBuffer;
                if (buf == null)
                    return;
                try {
                    buf.lockWrite();
                }
                catch (InterruptedException e) {
                    Log.debug(e);
                    return;
                }
                try {
                    buf.append(s);
                    buf.renumber();
                }
                finally {
                    buf.unlockWrite();
                }
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == buf) {
                        ed.setDot(buf.getEnd());
                        ed.moveCaretToDotCol();
                        ed.setUpdateFlag(REPAINT);
                        ed.updateDisplay();
                    }
                }
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void checkLogin()
    {
        if (userName == null)
            userName = System.getProperty("user.name");
        if (password == null) {
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf.getFile() instanceof SshFile) {
                    SshFile f = (SshFile) buf.getFile();
                    if (f.hostName != null && f.hostName.equals(hostName)) {
                        if (f.getUserName() != null && f.getUserName().equals(userName)) {
                            password = f.getPassword();
                            break;
                        }
                    }
                }
            }
        }
    }

    private static synchronized void cleanup()
    {
        // Walk buffer list in event dispatch thread.
        if (!SwingUtilities.isEventDispatchThread()) {
            Debug.bug();
            return;
        }
        if (sessionList != null) {
            for (int i = sessionList.size(); i-- > 0;) {
                SshSession session = (SshSession) sessionList.get(i);
                if (session.isLocked())
                    continue;
                String hostName = session.getHostName();
                boolean inUse = false;
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer buf = it.nextBuffer();
                    if (buf.getFile() instanceof SshFile) {
                        if (hostName.equals(buf.getFile().getHostName())) {
                            inUse = true;
                            break;
                        }
                    }
                }
                if (!inUse)
                    session.dispose();
            }
            if (sessionList.size() == 0) {
                sessionList = null;
                if (cleanupThread != null) {
                    cleanupThread.cancel();
                    cleanupThread = null;
                }
            }
        }
    }

    private static final Runnable cleanupRunnable = new Runnable() {
        public void run()
        {
            cleanup();
        }
    };

    private String stdOutFilter(String s)
    {
        return s;
    }

    private synchronized void stdOutUpdate(final String s)
    {
        if (echo || Editor.preferences().getBooleanProperty(Property.SSH_ECHO))
            Log.debug("<== |" + s + "|");
        if (outputBuffer != null)
            writeToOutputBuffer(s);
        output.append(s);
        notify();
    }

    private String stdErrFilter(String s)
    {
        return s;
    }

    private void stdErrUpdate(final String s)
    {
        Log.debug("stderr: |" + s + "|");
    }

    private static String reveal(String s)
    {
        FastStringBuffer sb = new FastStringBuffer();
        final int length = s.length();
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            switch (c) {
                case '\r':
                    sb.append("\\r");
                    break;
                case '\n':
                    sb.append("\\n");
                    break;
                case '\t':
                    sb.append("\\t");
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        return sb.toString();
    }

    class StdoutThread extends ReaderThread
    {
        // If this constructor is private, we run into jikes 1.15 bug #2256.
        StdoutThread()
        {
            super(process.getInputStream());
        }

        public String filter(String s)
        {
            return stdOutFilter(s);
        }

        public void update(String s)
        {
            stdOutUpdate(s);
        }
    }

    class StderrThread extends ReaderThread
    {
        // If this constructor is private, we run into jikes 1.15 bug #2256.
        StderrThread()
        {
            super(process.getErrorStream());
        }

        public String filter(String s)
        {
            return stdErrFilter(s);
        }

        public void update(String s)
        {
            stdErrUpdate(s);
        }
    }
}
