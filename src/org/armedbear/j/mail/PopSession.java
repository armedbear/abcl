/*
 * PopSession.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

package org.armedbear.j.mail;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.SocketException;
import org.armedbear.j.Log;
import org.armedbear.j.Netrc;
import org.armedbear.j.SocketConnection;

public final class PopSession
{
    private static final int DEFAULT_PORT = 110;

    // States.
    private static final int DISCONNECTED  = 0;
    private static final int AUTHORIZATION = 1;
    private static final int TRANSACTION   = 2;

    // Responses.
    public static final int OK  = 0;
    public static final int ERR = 1;

    private int state;
    private boolean echo = false;
    private String host;
    private final String user;
    private String password;
    private int port;
    private Socket socket;
    private MailReader reader;
    private OutputStreamWriter writer;
    private String errorText;

    private PopSession(PopURL url, String user, String password)
    {
        this.host = url.getHost();
        this.port = url.getPort();
        this.user = user;
        this.password = password;
    }

    public final String getHost()
    {
        return host;
    }

    public final int getPort()
    {
        return port;
    }

    public final String getUser()
    {
        return user;
    }

    public final String getPassword()
    {
        return password;
    }

    public final void setPassword(String password)
    {
        this.password = password;
    }

    public final void setEcho(boolean b)
    {
        echo = b;
    }

    public final boolean getEcho()
    {
        return echo;
    }

    public final String getErrorText()
    {
        return errorText;
    }

    public static PopSession getSession(PopURL url)
    {
        if (url.getHost() == null)
            return null;
        String user = url.getUser();
        if (user == null)
            user = System.getProperty("user.name");
        String password = Netrc.getPassword(url.getHost(), user);
        if (password == null)
            return null;
        return new PopSession(url, user, password);
    }

    public static PopSession getSession(PopURL url, String user)
    {
        String password = Netrc.getPassword(url.getHost(), user);
        if (password == null)
            return null;
        return new PopSession(url, user, password);
    }

    public static PopSession getSession(PopURL url, String user, String password)
    {
        return new PopSession(url, user, password);
    }

    public boolean connect()
    {
        setEcho(true);
        socket = null;
        errorText = null;
        SocketConnection sc =
            new SocketConnection(host, port, 30000, 200, null);
        socket = sc.connect();
        if (socket == null) {
            errorText = sc.getErrorText();
            return false;
        }
        setTimeout(30000); // 30-second timeout
        state = AUTHORIZATION;
        try {
            reader = new MailReader(socket.getInputStream());
            writer = new OutputStreamWriter(socket.getOutputStream());
            if (readLine() != null) {
                if (write("user " + user)) {
                    if (getResponse() == OK) {
                        if (write("pass " + password)) {
                            if (getResponse() == OK) {
                                state = TRANSACTION;
                                setEcho(false);
                                return true;
                            }
                        }
                    }
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        // Something went astray.
        disconnect();
        errorText = "Login failed";
        setEcho(false);
        return false;
    }

    // Set specified timeout (in milliseconds).
    private synchronized void setTimeout(int ms)
    {
        if (socket != null) {
            try {
                socket.setSoTimeout(ms);
            }
            catch (SocketException e) {
                Log.error(e);
            }
        } else
            Log.debug("PopSession.setTimeout socket is null");
    }

    public synchronized boolean logout()
    {
        Log.debug("PopSession.logout");
        boolean succeeded = false;
        if (state > DISCONNECTED) {
            setEcho(true);
            write("quit");
            if (getResponse() == OK)
                succeeded = true;
        }
        disconnect();
        return succeeded;
    }

    public void disconnect()
    {
        if (socket != null) {
            try {
                socket.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        state = DISCONNECTED;
    }

    protected void finalize()
    {
        Log.debug("PopSession.finalize");
    }

    public synchronized String readLine()
    {
        try {
            String s = reader.readLine();
            if (echo && s != null)
                Log.debug("<== " + s);
            return s;
        }
        catch (IOException e) {
            Log.error(e);
            disconnect();
            return null;
        }
    }

    public synchronized boolean write(String s)
    {
        if (writer == null)
            return false;
        if (echo)
            Log.debug("==> " + (s.startsWith("pass ") ? "pass" : s));
        s += "\r\n";
        try {
            writer.write(s);
            writer.flush();
            return true;
        }
        catch (IOException e) {
            Log.error(e);
            disconnect();
            return false;
        }
    }

    public synchronized int getResponse()
    {
        while (true) {
            String s = readLine();
            if (s == null) {
                state = DISCONNECTED;
                return ERR;
            }
            if (s.startsWith("+OK"))
                return OK;
            if (s.startsWith("-ERR"))
                return ERR;
        }
    }
}
