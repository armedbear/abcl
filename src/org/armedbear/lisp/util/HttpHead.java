/*
 * HttpHead.java
 *
 * Copyright (C) 2010 Mark Evenson
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */
package org.armedbear.lisp.util;

import org.armedbear.lisp.Debug;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.Socket;
import java.net.URL;

/** 
 * Use HTTP/1.1 HEAD to retrieve the specified header field.
 */
public class HttpHead {
    static private String get(String urlString, String key) {
        URL url = null;
        try {
            url = new URL(urlString);
        } catch (MalformedURLException e) {
            log("Failed to form url from " + "'" + urlString + "'" + ": " + e);
        }
        return get(url, key);
    }

    static public String get(URL url, String key) {
        Socket socket = null;
        String result = null;
        try {
            String protocol = url.getProtocol();
            if (!protocol.equals("http")) {
                log("The protocol " + "'" + protocol + "'" + " is not http.");
                return result;
            }

            socket = new Socket(Proxy.NO_PROXY); // XXX add Proxy

            int port = url.getPort();
            if (port == -1) {
                port = 80;
            }
            InetSocketAddress address = new InetSocketAddress(url.getHost(), port);
            try {
                socket.connect(address, 5000); // ??? too long?  too short?
            } catch (IOException ex) {
                log("Connection failed: " + ex);
                return result;
            }

            PrintWriter out = null;
            BufferedReader in = null;
            try {
                out = new PrintWriter(socket.getOutputStream());
                in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            } catch (IOException e) {
                log("Failed to establish socket io: " + e);
                return result;
            }

            String head = "HEAD " + url.getPath() + " HTTP/1.1";
            out.println(head);
            out.println("Host: " + url.getAuthority());
            out.println("Connection: close");
            out.println("");
            out.flush();

            String line = null;
            try {
                line = in.readLine();
            } catch (IOException e) {
                log("Failed to read HTTP response: " + e);
            }
            String status[] = line.split("\\s");
            if (status[1].equals("200")) {
                result = findHeader(in, key);
            } else if (status[1].startsWith("3")) {
                // Follow redirects ad nauseum
                String location = findHeader(in, "Location");
                if (location != null) {
                    return get(location, key);
                }
            } else {
                log("Unexpected response: " + line);
            }
        } finally {
            try {
                socket.close();
            } catch (IOException e) {
            }
        }
        return result;
    }

    static private String findHeader(BufferedReader in, String key) {
        String result = null;
        String line;
        try {
            while ((line = in.readLine()) != null) {
                int i = line.indexOf(":");
                if (i == -1) {
                    continue; // XXX parse multi-line HTTP headers
                }
                String k = line.substring(0, i);
                String v = line.substring(i + 1).trim();
                if (k.equals(key)) {
                    result = v;
                    break;
                }
            }
        } catch (IOException e) {
            log("Failed to read headers: " + e);
        }
        return result;
    }

    static private void log(String message) {
        Debug.warn(message);
    }

    public static void main(String argv[]) {
        if (argv.length != 1) {
            System.out.println("Usage: <cmd> URL");
            return;
        }
        String modified = get(argv[0], "Last-Modified");
        if (modified != null) {
            System.out.println("Last-Modified: " + modified);
        } else {
            System.out.println("No result returned.");
        }
    }
}
