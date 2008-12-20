/*
 * SmtpSession.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.NoRouteToHostException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.List;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.Property;
import org.armedbear.j.Utilities;

public final class SmtpSession extends Writer
{
    private static final int DEFAULT_PORT = 25;

    private boolean echo = false;
    private String hostName;
    private int port;
    private Socket socket;
    private BufferedReader reader;
    private BufferedWriter writer;
    private boolean connected;
    private String errorText;

    private SmtpSession(String hostName)
    {
        this.hostName = hostName;
        this.port = DEFAULT_PORT;
    }

    private SmtpSession(String hostName, int port)
    {
        this.hostName = hostName;
        this.port = port;
    }

    public final void setEcho(boolean b)
    {
        echo = b;
    }

    // Returns session with connection already established (or null).
    public static SmtpSession getDefaultSession()
    {
        return getSession(Editor.preferences().getStringProperty(Property.SMTP));
    }

    // Returns session with connection already established (or null).
    public static SmtpSession getSession(String server)
    {
        if (server == null)
            return null;
        SmtpSession session = null;
        // Port may be specified.
        int index = server.indexOf(':');
        if (index < 0) {
            session = new SmtpSession(server);
        } else {
            try {
                int port = Integer.parseInt(server.substring(index + 1));
                String hostName = server.substring(0, index);
                session = new SmtpSession(hostName, port);
            }
            catch (NumberFormatException e) {
                Log.error(e);
                FastStringBuffer sb = new FastStringBuffer();
                sb.append("Unable to parse SMTP server name \"");
                sb.append(server);
                sb.append('"');
                MessageDialog.showMessageDialog(sb.toString(), "Error");
                return null;
            }
        }
        Debug.assertTrue(session != null);
        session.setEcho(true);
        if (!session.connect())
            return null;
        session.setEcho(false);
        return session;
    }

    public final String getErrorText()
    {
        return errorText;
    }

    public boolean sendMessage(SendMail sm, File messageFile)
    {
        List addressees = sm.getAddressees();
        if (addressees == null || addressees.size() == 0)
            return false;
        if (!connect())
            return false;
        try {
            setEcho(true);
            FastStringBuffer sb = new FastStringBuffer("mail from:<");
            sb.append(sm.getFromAddress());
            sb.append('>');
            writeLine(sb.toString());
            if (getResponse() != 250)
                return false;
            for (int i = 0; i < addressees.size(); i++) {
                String addressee = (String) addressees.get(i);
                String addr = sm.getAddress(addressee);
                if (addr == null) {
                    errorText = "Invalid addressee \"" + addressee + "\"";
                    return false;
                }
                sb.setText("rcpt to:<");
                sb.append(addr);
                sb.append('>');
                writeLine(sb.toString());
                if (getResponse() != 250) {
                    errorText = "Address not accepted \"" + addr + "\"";
                    return false;
                }
            }
            writeLine("data");
            if (getResponse() != 354)
                return false;
            setEcho(false);
            BufferedReader messageFileReader = new BufferedReader(new InputStreamReader(messageFile.getInputStream()));
            String s;
            while ((s = messageFileReader.readLine()) != null)
                writeLine(s);
            setEcho(true);
            writeLine(".");
            if (getResponse() != 250)
                return false;
            quit();
        }
        catch (Throwable t) {
            Log.error(t);
        }
        finally {
            setEcho(false);
            disconnect();
        }
        // Add addressees to address book.
        AddressBook addressBook = AddressBook.getGlobalAddressBook();
        for (int i = 0; i < addressees.size(); i++) {
            String addressee = (String) addressees.get(i);
            MailAddress a = MailAddress.parseAddress(addressee);
            if (a != null) {
                addressBook.maybeAddMailAddress(a);
                addressBook.promote(a);
            }
        }
        AddressBook.saveGlobalAddressBook();
        return true;
    }

    public boolean connect()
    {
        if (connected)
            return true;
        Log.debug("connecting to port " + port + " on " + hostName + " ...");
        try {
            socket = new Socket(hostName, port);
        }
        catch (UnknownHostException e) {
            errorText = "Unknown SMTP server " + hostName;
            return false;
        }
        catch (NoRouteToHostException e) {
            errorText = "No route to SMTP server " + hostName;
            return false;
        }
        catch (ConnectException e) {
            errorText = "Connection refused by SMTP server " + hostName;
            return false;
        }
        catch (IOException e) {
            Log.error(e);
            errorText = e.toString();
            return false;
        }
        try {
            reader =
                new BufferedReader(new InputStreamReader(socket.getInputStream()));
            writer =
                new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
            getResponse();
            writeLine("HELO " + InetAddress.getLocalHost().getHostAddress());
            if (getResponse() == 250) {
                connected = true;
                return true;
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        return false;
    }

    public void quit()
    {
        setEcho(true);
        writeLine("QUIT");
        getResponse();
        setEcho(false);
        disconnect();
    }

    public synchronized void disconnect()
    {
        if (connected) {
            try {
                socket.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
            socket = null;
            reader = null;
            writer = null;
            connected = false;
        }
    }

    public int getResponse()
    {
        while (true) {
            String s = readLine();
            if (s == null)
                break;
            if (s.length() < 4)
                break;
            if (s.charAt(3) == ' ') {
                try {
                    return Utilities.parseInt(s);
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
                break;
            }
        }
        return 0;
    }

    private String readLine()
    {
        try {
            String s = reader.readLine();
            if (echo && s != null)
                Log.debug("<== " + s);
            return s;
        }
        catch (IOException e) {
            Log.error(e);
            return null;
        }
    }

    public void write(int c) throws IOException
    {
        writer.write(c);
    }

    public void write(char[] chars) throws IOException
    {
        writer.write(chars);
    }

    public void write(char[] chars, int offset, int length) throws IOException
    {
        writer.write(chars, offset, length);
    }

    public void write(String s) throws IOException
    {
        writer.write(s);
    }

    public void write(String s, int offset, int length) throws IOException
    {
        writer.write(s, offset, length);
    }

    public void flush() throws IOException
    {
        writer.flush();
    }

    public void close() throws IOException
    {
        writer.close();
    }

    public boolean writeLine(String s)
    {
        if (echo)
            Log.debug("==> " + s);
        try {
            writer.write(s);
            writer.write("\r\n");
            writer.flush();
            return true;
        }
        catch (IOException e) {
            Log.error(e);
            return false;
        }
    }
}
