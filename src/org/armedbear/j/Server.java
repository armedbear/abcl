/*
 * Server.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.Vector;
import javax.swing.SwingUtilities;

public class Server implements Runnable
{
    private static Server server;

    private ServerSocket socket;
    private Thread thread;

    public static void startServer()
    {
        try {
            server = new Server();
            server.socket = new ServerSocket(0);
            int port = server.socket.getLocalPort();
            OutputStream out = Editor.portfile.getOutputStream();
            out.write(String.valueOf(port).getBytes());
            out.close();
            server.thread = new Thread(server);
            server.thread.setName("server");
            server.thread.setPriority(Thread.MIN_PRIORITY);
            server.thread.setDaemon(true);
            server.thread.start();
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    public static void stopServer()
    {
        Editor.portfile.delete();
    }

    public void run()
    {
        while (true) {
            try {
                Socket sock = socket.accept(); // Blocks.
                // Process request.
                BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
                Vector v = null;
                while (true) {
                    String s = in.readLine();
                    if (s == null)
                        break;
                    if (v == null)
                        v = new Vector();
                    v.add(s);
                }
                in.close();
                sock.close();
                SwingUtilities.invokeLater(new Messenger(v));
            }
            catch (SocketException e) {
                return;
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    class Messenger implements Runnable
    {
        Vector v = null;

        // If this constructor is private, we run into jikes 1.15 bug #2256.
        Messenger(Vector v)
        {
            this.v = v;
        }

        public void run()
        {
            Editor editor = Editor.currentEditor();
            if (v != null && v.size() > 0) {
                Editor other = editor.getOtherEditor();
                if (other != null && editor.getBuffer().isSecondary())
                    editor = other;
                if (!editor.getBuffer().isPrimary())
                    Debug.bug();
                Buffer toBeActivated = editor.openFiles(v);
                if (toBeActivated != null) {
                    editor.makeNext(toBeActivated);
                    editor.switchToBuffer(toBeActivated);
                    if (!Editor.getEditorList().contains(editor))
                        Debug.bug();
                    editor.updateDisplay();
                }
            }
            editor.getFrame().toFront();
            editor.requestFocus();
            Editor.restoreFocus();
        }
    }
}
