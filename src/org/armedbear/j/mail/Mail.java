/*
 * Mail.java
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

import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Headers;
import org.armedbear.j.Log;
import org.armedbear.j.Property;
import org.armedbear.j.Utilities;

public final class Mail
{
    private static File sentMessagesFile;

    public static final File getSentMessagesFile()
    {
        final String fcc = Editor.preferences().getStringProperty(Property.FCC);
        if (fcc == null)
            return null;

        // For now, we just support a hard-coded scheme.
        if (!fcc.equals("sent"))
            return null;

        if (sentMessagesFile == null) {
            File local =
                File.getInstance(Directories.getMailDirectory(), "local");
            File sent = File.getInstance(local, "sent");
            if (!sent.isDirectory())
                sent.mkdirs();
            if (sent.isDirectory())
                sentMessagesFile = File.getInstance(sent, "mbox");
        }
        return sentMessagesFile;
    }

    public static final MailAddress getUserMailAddress()
    {
        String address = Editor.preferences().getStringProperty(Property.USER_MAIL_ADDRESS);
        if (address == null)
            return null;
        return new MailAddress(Editor.preferences().getStringProperty(Property.USER_FULL_NAME), address);
    }

    public static boolean bounceMessage(Message message, MailAddress[] to)
    {
        if (message == null)
            return false;
        if (to == null)
            return false;
        SmtpSession session = SmtpSession.getDefaultSession();
        if (session == null)
            return false;
        boolean succeeded = bounceMessage(message, to, session);
        session.quit();
        return succeeded;
    }

    public static boolean bounceMessage(Message message, MailAddress[] to,
        SmtpSession session)
    {
        if (message == null)
            return false;
        if (to == null)
            return false;
        if (session == null)
            return false;
        session.setEcho(true);
        session.writeLine("rset");
        int response = session.getResponse();
        if (response == 500) {
            // "command unrecognized"
            Log.warn("bounceMessage calling session.quit()...");
            session.quit();
            Log.warn("bounceMessage calling session.reconnect()...");
            if (!session.connect())
                return false;
            session.writeLine("rset");
            response = session.getResponse();
        }
        if (response != 250)
            return false;
        session.writeLine("mail from:<" + getUserMailAddress().getAddress() + ">");
        if (session.getResponse() != 250)
            return false;
        for (int i = 0; i < to.length; i++) {
            String address = to[i].getAddress();
            session.writeLine("rcpt to: " + address);
            if (session.getResponse() != 250)
                return false;
        }
        session.writeLine("data");
        if (session.getResponse() != 354)
            return false;
        session.setEcho(false);
        if (!writeMessageText(message, to, session))
            return false;
        session.setEcho(true);
        session.writeLine(".");
        boolean succeeded = session.getResponse() == 250;
        session.setEcho(false);
        return succeeded;
    }

    private static boolean writeMessageText(Message message, MailAddress[] to,
        Writer writer)
    {
        try {
            FastStringReader reader =
                new FastStringReader(message.getRawHeaders());
            String s;
            while((s = reader.readLine()) != null) {
                if (s.startsWith("X-J-Status"))
                    continue;
                if (s.startsWith("X-UIDL"))
                    continue;
                writer.write(s);
                writer.write("\r\n");
            }

            // Add "Resent" headers.
            writer.write("Resent-From: ");
            writer.write(getUserMailAddress().toString());
            writer.write("\r\n");

            writer.write("Resent-Date: ");
            writer.write(RFC822Date.getDateTimeString());
            writer.write("\r\n");

            writer.write("Resent-To: ");
            int length = 11;
            for (int i = 0; i < to.length; i++) {
                String address = to[i].toString();
                if (length + address.length() > 76) {
                    writer.write("\r\n\t");
                    length = 8; // Assuming tab width is 8.
                }
                writer.write(address);
                length += address.length();
                if (i < to.length-1) {
                    writer.write(", ");
                    length += 2;
                }
            }
            writer.write("\r\n");

            writer.write("Resent-Message-Id: ");
            writer.write(generateMessageId());
            writer.write("\r\n");

            // Terminate headers.
            writer.write("\r\n");

            // Body.
            String body = message.getRawBody();
            String contentType = message.getHeaderValue(Headers.CONTENT_TYPE);
            if (contentType != null) {
                String charset =
                    Utilities.getCharsetFromContentType(contentType);
                String encoding = Utilities.getEncodingFromCharset(charset);
                if (!encoding.equalsIgnoreCase("iso-8859-1")) {
                    try {
                        byte[] bytes = body.getBytes(encoding);
                        body = new String(bytes, 0);
                    }
                    catch (UnsupportedEncodingException e) {
                        Log.error(e);
                    }
                }
            }
            writer.write(body);
            if (!body.endsWith("\r\n"))
                writer.write("\r\n");
            return true;
        }
        catch (Exception e) {
            Log.error(e);
            return false;
        }
    }

    private static long messageIdentifier = System.currentTimeMillis() % 10000;

    public static String generateMessageId()
    {
        String hostName = null;
        try {
            InetAddress addr = InetAddress.getLocalHost();
            hostName = addr.getHostName();
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (hostName == null)
            hostName = "unknown"; // Avoid NPE below.
        SimpleDateFormat df = new SimpleDateFormat ("yyyyMMddHHmmss");
        Calendar cal = Calendar.getInstance();
        FastStringBuffer sb = new FastStringBuffer(128);
        sb.append('<');
        sb.append(df.format(cal.getTime()));
        sb.append('.');
        sb.append(messageIdentifier++);
        sb.append('@');
        sb.append(hostName);
        sb.append('>');
        return sb.toString();
    }

    public static boolean writeFcc(Message message, String destination, int flags)
    {
        if (destination.startsWith("mailbox:"))
            destination = destination.substring(8);
        File file = File.getInstance(destination);
        if (file == null)
            return false;
        if (!file.isLocal())
            return false;
        Mbox mbox = Mbox.getInstance(file);
        if (!mbox.lock())
            return false;
        boolean result = mbox.appendMessage(message, flags);
        mbox.unlock();
        mbox.updateViews();
        return result;
    }
}
