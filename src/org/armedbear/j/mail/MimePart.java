/*
 * MimePart.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Vector;
import org.armedbear.j.Directories;
import org.armedbear.j.File;
import org.armedbear.j.Headers;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public class MimePart
{
    protected String raw;
    protected Headers headers;

    private Vector parts;

    public MimePart(String raw)
    {
        this.raw = raw;
    }

    public MimePart(String raw, Headers headers)
    {
        this.raw = raw;
        this.headers = headers;
    }

    public final String getRawText()
    {
        return raw;
    }

    public String getAllHeaders()
    {
        if (raw.startsWith("\r\n"))
            return "\r\n";
        if (raw.startsWith("\n"))
            return "\n";
        int index = raw.indexOf("\r\n\r\n");
        if (index >= 0)
            return RFC2047.decode(raw.substring(0, index + 2));
        index = raw.indexOf("\n\n");
        if (index >= 0)
            return RFC2047.decode(raw.substring(0, index + 1));
        return raw;
    }

    public String getRawHeaders()
    {
        if (raw.startsWith("\r\n"))
            return "\r\n";
        if (raw.startsWith("\n"))
            return "\n";
        int index = raw.indexOf("\r\n\r\n");
        if (index >= 0)
            return raw.substring(0, index + 2);
        index = raw.indexOf("\n\n");
        if (index >= 0)
            return raw.substring(0, index + 1);
        return raw;
    }

    public String getRawBody()
    {
        if (raw.startsWith("\r\n"))
            return raw.substring(2);
        else if (raw.startsWith("\n"))
            return raw.substring(1);
        else {
            int index = raw.indexOf("\r\n\r\n");
            if (index >= 0)
                return raw.substring(index + 4);
            else {
                index = raw.indexOf("\n\n");
                if (index >= 0)
                    return raw.substring(index + 2);
                else
                    return raw;
            }
        }
    }

    public String getDecodedBody()
    {
        final String rawBody = getRawBody();
        final String contentType = getContentType();
        final String transferEncoding = getTransferEncoding();
        final String charset =
            Utilities.getCharsetFromContentType(getHeaderValue(Headers.CONTENT_TYPE));
        final String characterEncoding = Utilities.getEncodingFromCharset(charset);
        if (contentType == null || contentType.toLowerCase().startsWith("text/")) {
            if (transferEncoding == null ||
                transferEncoding.equals("7bit") ||
                transferEncoding.equals("8bit") ||
                transferEncoding.equals("binary"))
                return rawBody;
            else if (transferEncoding.equals("quoted-printable")) {
                byte[] bytes = QuotedPrintableDecoder.decode(rawBody);
                try {
                    return new String(bytes, characterEncoding);
                }
                catch (UnsupportedEncodingException e) {
                    Log.error(e);
                    return new String(bytes);
                }
            } else if (transferEncoding.equals("base64")) {
                try {
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    Base64Decoder.decode(rawBody, out); // Ignore errors.
                    byte[] bytes = out.toByteArray();
                    if (bytes != null) {
                        try {
                            return new String(bytes, 0, bytes.length,
                                characterEncoding);
                        }
                        catch (UnsupportedEncodingException e) {
                            Log.error(e);
                            return new String(bytes, 0, bytes.length);
                        }
                    }
                }
                catch (IOException e) {
                    Log.error(e);
                }
                return null;
            } else
                return null;
        } else if (contentType.startsWith("multipart/"))
            return rawBody;
        else if (transferEncoding == null)
            return rawBody;
        else
            return null;
    }

    private byte[] getDecodedBodyAsByteArray()
    {
        final String rawBody = getRawBody();
        final String encoding = getTransferEncoding();
        if (encoding == null || encoding.equals("7bit") ||
            encoding.equals("8bit") || encoding.equals("binary")) {
            byte[] bytes = null;
            try {
                 bytes = rawBody.getBytes("ISO8859_1");
            }
            catch (UnsupportedEncodingException e) {
                Log.error(e);
            }
            return bytes;
        }
        if (encoding.equals("quoted-printable"))
            return QuotedPrintableDecoder.decode(rawBody);
        if (encoding.equals("base64")) {
            try {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                if (Base64Decoder.decode(rawBody, out))
                    return out.toByteArray();
            }
            catch (IOException e) {
                Log.error(e);
            }
            // Fall through, return null...
        }
        return null;
    }

    public Vector getParts()
    {
        return parts;
    }

    public MimePart getPart(int i)
    {
        if (parts == null)
            return null;
        if (i < 0)
            return null;
        if (i >= parts.size())
            return null;
        return (MimePart) parts.get(i);
    }

    protected void addParts(Vector v)
    {
        v.add(this);
        // Recurse.
        if (parts != null) {
            for (int i = 0; i < parts.size(); i++) {
                MimePart part = (MimePart) parts.get(i);
                String contentType = part.getContentType();
                if (contentType != null && contentType.startsWith("multipart/"))
                    part.addParts(v);
                else
                    v.add(part);
            }
        }
    }

    public final Headers getHeaders()
    {
        if (headers == null)
            headers = Headers.parse(raw);
        return headers;
    }

    public final String getHeaderValue(int index)
    {
        if (headers == null)
            headers = Headers.parse(raw);
        return headers.getValue(index);
    }

    public final String getContentType()
    {
        String s = getHeaderValue(Headers.CONTENT_TYPE);
        if (s == null )
            return null;
        s = s.trim();
        if (s.length() == 0)
            return null;
        int index = s.indexOf(';');
        if (index >= 0)
            s = s.substring(0, index);
        return s.toLowerCase();
    }

    public final String getTransferEncoding()
    {
        String s = getHeaderValue(Headers.CONTENT_TRANSFER_ENCODING);
        if (s == null )
            return null;
        return s.toLowerCase();
    }

    public final String getDisposition()
    {
        String s = getHeaderValue(Headers.CONTENT_DISPOSITION);
        if (s == null)
            return null;
        s = s.trim();
        int index = s.indexOf(';');
        if (index >= 0 )
            s = s.substring(0, index);
        return s;
    }

    public final int getSize()
    {
        String s = getRawBody();
        return s == null ? 0 : s.length();
    }

    public final boolean isAttachment()
    {
        String s = getHeaderValue(Headers.CONTENT_DISPOSITION);
        if (s != null && s.trim().startsWith("attachment"))
            return true;
        else
            return false;
    }

    public final boolean isInline()
    {
        String s = getHeaderValue(Headers.CONTENT_DISPOSITION);
        if (s != null && s.trim().startsWith("inline"))
            return true;
        return false;
    }

    public String getAttachmentFileName()
    {
        String filename =
            getHeaderParameter(Headers.CONTENT_DISPOSITION, "filename");
        if (filename == null)
            filename = getHeaderParameter(Headers.CONTENT_TYPE, "name");
        if (filename != null) {
            filename = filename.trim();
            int length = filename.length();
            // Strip quotes.
            if (length >= 2 && filename.charAt(0) == '"' &&
                filename.charAt(length-1) == '"')
                filename = filename.substring(1, length-1);
            // Filename might be RFC2047-encoded.
            filename = RFC2047.decode(filename);
            // Remove path prefix, if any.
            int index = filename.lastIndexOf('/');
            if (index < 0)
                index = filename.lastIndexOf('\\');
            if (index >= 0)
                filename = filename.substring(index+1);
        }
        return filename;
    }

    private String getHeaderParameter(int header, String parameterName)
    {
        String s = getHeaderValue(header);
        if (s != null) {
            s = s.trim();
            String lower = s.toLowerCase();
            int index = lower.indexOf(parameterName.concat("="));
            if (index >= 0) {
                int begin = index + parameterName.length() + 1;
                int end = s.indexOf(';', begin);
                if (end >= 0)
                    return s.substring(begin, end);
                else
                    return s.substring(begin);
            }
        }
        return null;
    }

    public File cacheDecoded()
    {
        String filename = getAttachmentFileName();
        String extension = null;
        if (filename != null && filename.length() > 0) {
            extension = Utilities.getExtension(filename);
        } else {
            // No filename specified.
            String contentType = getContentType();
            if (contentType != null) {
                if (contentType.equals("image/jpeg"))
                    extension = ".jpg";
                else if (contentType.equals("image/gif"))
                    extension = ".gif";
                else if (contentType.equals("text/html"))
                    extension = ".html";
            }
        }
        File cache =
            Utilities.getTempFile(Directories.getTempDirectory(), extension);
        if (cache != null && saveDecoded(cache))
            return cache;
        else
            return null;
    }

    public boolean saveDecoded(File file)
    {
        String encoding = getTransferEncoding();
        if (encoding == null || encoding.equals("7bit") ||
            encoding.equals("8bit") || encoding.equals("binary") ||
            encoding.equals("quoted-printable")) {
            try {
                byte[] bytes = getDecodedBodyAsByteArray();
                if (bytes == null)
                    return false;
                FileOutputStream out = file.getOutputStream();
                out.write(bytes);
                out.flush();
                out.close();
                return true;
            }
            catch (IOException e) {
                Log.error(e);
                return false;
            }
        } else if (encoding.equals("base64")) {
            return saveDecodedBase64(file);
        } else if (encoding.equals("x-uuencode")) {
            Log.error("saveDecoded x-uuencode not supported");
            return false;
        } else {
            Log.error("saveDecoded unrecognized encoding " + encoding);
            return false;
        }
    }

    private boolean saveDecodedBase64(File file)
    {
        boolean success = false;
        try {
            BufferedOutputStream out =
                new BufferedOutputStream(file.getOutputStream());
            String rawBody = getRawBody();
            success = Base64Decoder.decode(rawBody, out);
            out.flush();
            out.close();
        }
        catch (IOException e) {
            Log.error(e);
            success = false;
        }
        if (!success)
            file.delete();
        return success;
    }

    private static final String BOUNDARY_START = "boundary=";

    public void parse()
    {
        final String contentType = getHeaderValue(Headers.CONTENT_TYPE);
        if (contentType == null)
            return;
        if (contentType.toLowerCase().startsWith("multipart/")) {
            int index = contentType.toLowerCase().indexOf(BOUNDARY_START);
            if (index < 0) {
                Log.error("can't find boundary parameter");
                return;
            }
            String boundary =
                contentType.substring(index + BOUNDARY_START.length()).trim();
            if (boundary.length() >= 2 && boundary.charAt(0) == '"') {
                int end = boundary.indexOf('"', 1);
                if (end >= 0)
                    boundary = boundary.substring(1, end);
            }
            parts = parseParts(boundary);
            if (parts != null) {
                for (int i = 0; i < parts.size(); i++) {
                    MimePart part = (MimePart) parts.get(i);
                    part.parse();
                }
            }
            return;
        }
        final String disposition = getDisposition();
        if (disposition != null && disposition.equalsIgnoreCase("attachment")) {
            parts = new Vector();
            MimePart part = new MimePart(raw);
            parts.add(part);
        }
    }

    private Vector parseParts(String boundary)
    {
        final String marker = "--" + boundary;
        final String endMarker = marker + "--";
        final int veryEnd = raw.indexOf(endMarker);
        int start = raw.indexOf(marker);
        if (start < 0)
            return null;
        Vector v = new Vector();
        while (true) {
            start += marker.length();
            if (raw.charAt(start) == '\r')
                ++start;
            if (raw.charAt(start) == '\n')
                ++start;
            int end = raw.indexOf(marker, start);
            if (end < 0)
                end = raw.length();
            MimePart part = new MimePart(raw.substring(start, end));
            v.add(part);
            if (end == veryEnd || end == raw.length())
                break;
            start = end;
        }
        return v;
    }
}
