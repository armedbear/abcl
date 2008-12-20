/*
 * HttpLoadProcess.java
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

package org.armedbear.j;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.URL;
import java.security.Provider;
import java.security.Security;
import java.util.List;
import javax.swing.SwingUtilities;

public final class HttpLoadProcess extends LoadProcess implements BackgroundProcess,
    Runnable, Cancellable
{
    private Socket socket;
    private boolean render = true;

    private String request;
    private String responseHeaders;
    private String contentType;

    private int redirectionCount;

    private FastStringBuffer sbHeaders = new FastStringBuffer();

    public HttpLoadProcess(Buffer buffer, HttpFile file)
    {
        super(buffer, file);
    }

    public final String getRequest()
    {
        return request;
    }

    public final String getResponseHeaders()
    {
        return responseHeaders;
    }

    public final String getContentType()
    {
        return contentType;
    }

    private final void setContentType(String s)
    {
        contentType = s;
    }

    public void run()
    {
        if (buffer != null) {
            buffer.setBusy(true);
            buffer.setBackgroundProcess(this);
        }
        load();
        if (buffer != null && buffer.getBackgroundProcess() == this) {
            Log.debug("calling setBackgroundProcess(null)");
            buffer.setBackgroundProcess(null);
            buffer.setBusy(false);
        }
    }

    private void load()
    {
        boolean usingProxy = false;
        if (file.getProtocol() == File.PROTOCOL_HTTPS) {
            if (!findProvider()) {
                error("No SSL provider found");
                return;
            }
        }
        cache = Utilities.getTempFile();
        if (cache == null) {
            Log.error("HttpLoadProcess.load cache is null");
            return; // Report error!
        }
        Debug.assertTrue(socket == null);
        String hostName = file.getHostName();
        int port = file.getPort();
        if (file.getProtocol() == File.PROTOCOL_HTTPS) {
            socket = createSSLSocket(hostName, port);
            if (socket == null) {
                if (!cancelled)
                    error("Can't create SSL socket");
                return;
            }
        } else {
            String httpProxy = Editor.preferences().getStringProperty("httpProxy");
            if (httpProxy != null) {
                if (httpProxy.startsWith("http://"))
                    httpProxy = httpProxy.substring(7);
                int index = httpProxy.indexOf(':');
                if (index >= 0) {
                    try {
                        port = Integer.parseInt(httpProxy.substring(index + 1));
                        hostName = httpProxy.substring(0, index);
                        usingProxy = true;
                    }
                    catch (NumberFormatException e) {
                        Log.error(e);
                    }
                }
            }
            connect(hostName, port);
        }
        if (cancelled) {
            Log.debug("cancelled!!");
            if (cancelRunnable != null)
                SwingUtilities.invokeLater(cancelRunnable);
            return;
        }
        if (socket == null) {
            if (errorRunnable != null)
                SwingUtilities.invokeLater(errorRunnable);
            return;
        }
        String location = null;
        boolean redirected = false;
        String encoding = null;
        try {
            InputStream in = socket.getInputStream();
            OutputStreamWriter writer = new OutputStreamWriter(socket.getOutputStream());
            FastStringBuffer sb = new FastStringBuffer(1024);
            sb.append("GET ");
            sb.append(usingProxy ? file.netPath() : file.canonicalPath());
            sb.append(" HTTP/1.0\r\n");
            sb.append("Host: ");
            sb.append(file.getHostName());
            sb.append("\r\n");
            String userAgent = Editor.preferences().getStringProperty(Property.HTTP_USER_AGENT);
            if (userAgent != null && userAgent.length() > 0) {
                sb.append("User-Agent: ");
                sb.append(userAgent);
                sb.append("\r\n");
            }
            if (Editor.preferences().getBooleanProperty(Property.HTTP_ENABLE_COOKIES)) {
                String cookie = Cookie.getCookie(new URL(file.netPath()));
                if (cookie != null) {
                    sb.append("Cookie: ");
                    sb.append(cookie);
                    sb.append("\r\n");
                }
            }
            sb.append("\r\n");
            request = sb.toString();
            writer.write(request);
            writer.flush();
            sb.setLength(0);
            sbHeaders.append(request);
            OutputStream out = cache.getOutputStream();
            byte[] buf = new byte[16384];
            long totalBytes = 0;
            int totalLength = 0; // Includes length of response headers.
            if (progressNotifier != null)
                progressNotifier.progressStart();
            while (!cancelled) {
                int bytesRead = 0;
                try {
                    // We may get an exception here if the user cancels.
                    bytesRead = in.read(buf);
                }
                catch (Exception e) {
                    if (!cancelled)
                        Log.error(e);
                }
                if (bytesRead <= 0)
                    break;
                if (sb != null) {
                    int oldLength = sb.length();
                    sb.append(new String(buf, 0, bytesRead, "ISO8859_1"));
                    String s = sb.toString();
                    int skip = 4; // "\r\n\r\n"
                    int index = s.indexOf("\r\n\r\n");
                    if (index < 0) {
                        index = s.indexOf("\n\n");
                        skip = 2; // "\n\n"
                    }
                    if (index >= 0) {
                        // We've got the headers.
                        sb = null;
                        responseHeaders = s.substring(0, index + skip);
                        sbHeaders.append(responseHeaders);
                        int statusCode = getStatusCode(responseHeaders);
                        Log.debug("statusCode = " + statusCode);
                        if (statusCode == 301 || statusCode == 302) {
                            // "Moved Permanently", "Moved Temporarily"
                            location = getLocation(responseHeaders);
                            redirected = true;
                            Log.debug("redirected to |" + location + "|");
                            Log.debug(request);
                            Log.debug(responseHeaders);
                        }
                        // Remove status line.
                        int end = responseHeaders.indexOf('\n');
                        if (end >= 0)
                            responseHeaders = responseHeaders.substring(end + 1);
                        int contentLength = getContentLength(responseHeaders);
                        if (contentLength != 0)
                            totalLength = responseHeaders.length() + contentLength;
                        Log.debug("responseHeaders = |" + responseHeaders + "|");
                        Headers headers = Headers.parse(responseHeaders);
                        setContentType(headers.getValue(Headers.CONTENT_TYPE));
                        Log.debug("content-type = |" + contentType + "|");
                        String charset =
                            Utilities.getCharsetFromContentType(contentType);
                        Log.debug("charset = |" + charset + "|");
                        if (charset != null)
                            encoding = Utilities.getEncodingFromCharset(charset);
                        Log.debug("encoding = |" + encoding + "|");
                        if (Editor.preferences().getBooleanProperty(Property.HTTP_ENABLE_COOKIES)) {
                            String cookie = headers.getValue(Headers.SET_COOKIE);
                            if (cookie != null)
                                Cookie.setCookie(new URL(file.netPath()), cookie);
                        }
                        int offset = index - oldLength + skip;
                        int length = bytesRead - offset;
                        out.write(buf, offset, length);
                    }
                } else
                    out.write(buf, 0, bytesRead);
                totalBytes += bytesRead;
                if (progressNotifier != null)
                    progressNotifier.progress("Received ", totalBytes, totalLength);
            }
            if (progressNotifier != null)
                progressNotifier.progressStop();
            out.close();
            in.close();
            socket.close();
            socket = null;
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (cancelled)
            cache.delete();
        if (!cache.isFile())
            cache = null;
        if (!cancelled && render && redirected && redirectionCount < 5) {
            if (cache != null) {
                if (cache.isFile())
                    cache.delete();
                cache = null;
            }
            File parent = file.getParentFile();
            if (parent != null) {
                // Recurse.
                file = HttpFile.getHttpFile((HttpFile)parent, location);
                ++redirectionCount;
                load();
                return;
            }
        }
        if (cache != null) {
            // Success!
            final HttpFile httpFile = (HttpFile) file;
            httpFile.setCache(cache);
            httpFile.setHeaders(sbHeaders.toString());
            httpFile.setContentType(contentType);
            httpFile.setEncoding(encoding);
            cache.setEncoding(encoding);
            if (successRunnable != null)
                SwingUtilities.invokeLater(successRunnable);
        } else if (cancelled) {
            if (cancelRunnable != null)
                SwingUtilities.invokeLater(cancelRunnable);
        } else if (errorRunnable != null)
            SwingUtilities.invokeLater(errorRunnable);
    }

    private void connect(final String hostName, final int port)
    {
        Debug.assertTrue(socket == null);
        Log.debug("Connecting to " + hostName + " on port " + port + "...");
        if (progressNotifier != null)
            progressNotifier.setText("Connecting to " + hostName + " on port " + port + "...");
        SocketConnection sc = new SocketConnection(hostName, port, 30000, 200, this);
        socket = sc.connect();
        if (socket != null) {
            if (progressNotifier != null)
                progressNotifier.setText("Connected to " + hostName);
        } else
            setErrorText(sc.getErrorText());
    }

    private boolean findProvider()
    {
        Provider provider = null;
        try {
            provider =
                (Provider) Class.forName("com.sun.net.ssl.internal.ssl.Provider").newInstance();
        }
        catch (Exception e) {}
        if (provider != null) {
            Security.addProvider(provider);
            System.setProperty("java.protocol.handler.pkgs",
                "com.sun.net.ssl.internal.www.protocol");
            return true;
        }
        return false;
    }

    private Socket createSSLSocket(String hostName, int port)
    {
        try {
            // SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
            Class SSLSocketFactory = Class.forName("javax.net.ssl.SSLSocketFactory");
            java.lang.reflect.Method getDefault = SSLSocketFactory.getMethod("getDefault", new Class[0]);
            Object factory = getDefault.invoke(null, new Object[0]);

            // Socket socket = factory.createSocket(hostName, port);
            Class[] parameterTypes = new Class[2];
            parameterTypes[0] = String.class;
            parameterTypes[1] = Integer.TYPE;
            java.lang.reflect.Method createSocket = factory.getClass().getMethod("createSocket", parameterTypes);
            Object[] args = new Object[2];
            args[0] = hostName;
            args[1] = new Integer(port);
            Socket socket = (Socket) createSocket.invoke(factory, args);
            return socket;
        }
        catch (Throwable t) {
            Log.error(t);
            return null;
        }
    }

    private static int getStatusCode(String responseHeaders)
    {
        int begin = responseHeaders.indexOf(' ') + 1;
        if (begin == 0)
            return -1;
        int end = responseHeaders.indexOf(' ', begin);
        if (end < 0)
            return -1;
        try {
            return Utilities.parseInt(responseHeaders.substring(begin, end));
        }
        catch (NumberFormatException e) {
            return -1;
        }
    }

    private static String getLocation(String responseHeaders)
    {
        final String lookFor = "\nlocation:";
        final int index = responseHeaders.toLowerCase().indexOf(lookFor);
        if (index < 0)
            return null;
        final int begin = index + lookFor.length();
        final int end = responseHeaders.indexOf('\n', begin);
        if (end < 0)
            return null;
        String location = responseHeaders.substring(begin, end).trim();
        if (location.startsWith("http:/") && !location.startsWith("http://")) {
            // Be permissive in what we accept.
            location = "http://".concat(location.substring(6));
        }
        return location;
    }

    private static int getContentLength(String responseHeaders)
    {
        final String lookFor = "\r\ncontent-length:";
        final int index = responseHeaders.toLowerCase().indexOf(lookFor);
        if (index < 0)
            return 0; // No content length header.
        final String value = responseHeaders.substring(index + lookFor.length()).trim();
        try {
            return Utilities.parseInt(value);
        }
        catch (NumberFormatException e) {
            return 0;
        }
    }

    private void error(String errorText)
    {
        Log.error(errorText);
        if (errorRunnable != null) {
            errorRunnable.setMessage(errorText);
            SwingUtilities.invokeLater(errorRunnable);
        }
    }

    public static void httpShowHeaders()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        final File file = buffer.getFile();
        if (file instanceof HttpFile) {
            editor.setWaitCursor();
            final String title = "httpShowHeaders ".concat(file.netPath());
            Buffer buf = null;
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer b = it.nextBuffer();
                if (b instanceof OutputBuffer && b.getParentBuffer() == buffer) {
                    if (title.equals(b.getTitle())) {
                        buf = b;
                        break;
                    }
                }
            }
            if (buf == null) {
                buf = OutputBuffer.getOutputBuffer(((HttpFile)file).getHeaders());
                buf.setParentBuffer(buffer);
                buf.setTitle(title);
            }
            editor.makeNext(buf);
            editor.activateInOtherWindow(buf);
            editor.setDefaultCursor();
        }
    }
}
