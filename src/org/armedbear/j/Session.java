/*
 * Session.java
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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public final class Session extends DefaultHandler implements Constants
{
    private static File sessionDirectory;

    private final File file;

    private List bufferEntries;
    private SessionBufferEntry currentBufferEntry;

    private Session()
    {
        file = File.getInstance(Directories.getEditorDirectory(), "session.xml");
    }

    private Session(File file)
    {
        this.file = file;
    }

    public static File getSessionDirectory()
    {
        if (sessionDirectory == null) {
            sessionDirectory =
                File.getInstance(Directories.getEditorDirectory(), "sessions");
            if (!sessionDirectory.isDirectory()) {
                sessionDirectory.mkdirs();
                if (!sessionDirectory.isDirectory())
                    Log.error("unable to create session directory!");
            }
        }
        return sessionDirectory;
    }

    private static File getSessionFile(String name)
    {
        return File.getInstance(getSessionDirectory(), name);
    }

    public static Session getSession(String name)
    {
        File file = getSessionFile(name);
        if (file != null && file.isFile())
            return new Session(file);
        return null;
    }

    public static Session getDefaultSession()
    {
        return new Session();
    }

    public static void saveDefaultSession()
    {
        getDefaultSession().save();
    }

    public static void saveCurrentSession()
    {
        String name = Editor.getSessionName();
        if (name != null) {
            File file = getSessionFile(name);
            if (file != null) {
                Session session = new Session(file);
                session.save();
            }
        }
    }

    public static void saveSession()
    {
        String name = Editor.getSessionName();
        if (name == null) {
            ChooseSessionDialog d = new ChooseSessionDialog("Save Session");
            d.show();
            name = d.getInput();
        }
        if (name != null)
            saveSession(name);
    }

    public static void saveSession(String name)
    {
        File file = getSessionFile(name);
        if (file != null) {
            Session session = new Session(file);
            session.save();
            Editor.setSessionName(name);
            Editor.currentEditor().status("Session saved");
        }
    }

    public static void loadSession()
    {
        ChooseSessionDialog d = new ChooseSessionDialog("Load Session");
        d.show();
        String name = d.getInput();
        if (name != null)
            loadSession(name);
    }

    public static void loadSession(String name)
    {
        File file = getSessionFile(name);
        if (file == null)
            return;
        if (!file.isFile()) {
            String message = "File \"" + file.canonicalPath() + "\" not found";
            MessageDialog.showMessageDialog(message, "Load Session");
            return;
        }
        final Editor editor = Editor.currentEditor();
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            if (!editor.okToClose(it.nextBuffer()))
                return;
        }
        // Load new session.
        Session session = new Session(file);
        if (!session.load()) {
            Log.error("unable to load session from " + file);
            MessageDialog.showMessageDialog(
                "Unable to load session from " + file,
                "Load Session");
            return;
        }

        if (Editor.getSessionName() != null)
            if (Editor.preferences().getBooleanProperty(Property.AUTOSAVE_NAMED_SESSIONS))
                saveCurrentSession();

        Marker.invalidateAllMarkers();

        editor.setWaitCursor();
        // Close all the existing buffers.
        for (BufferIterator iter = new BufferIterator(); iter.hasNext();) {
            Buffer buf = iter.nextBuffer();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                ed.views.remove(buf);
            }
            buf.deleteAutosaveFile();
            iter.remove();
            buf.dispose();
        }
        editor.unsplitWindow();
        Buffer toBeActivated = session.createBuffers();
        // Make sure read-only status is correct for each buffer.
        for (BufferIterator it = new BufferIterator(); it.hasNext();)
            editor.reactivate(it.nextBuffer());
        for (EditorIterator it = new EditorIterator(); it.hasNext();)
            it.nextEditor().activate(toBeActivated);
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
        Sidebar.refreshSidebarInAllFrames();
        Editor.setSessionName(name);
        editor.setDefaultCursor();
    }

    public Buffer restore()
    {
        if (file == null) {
            Debug.bug();
            return null;
        }
        if (!file.isFile())
            return null;
        if (!load()) {
            Log.error("Session.restore unable to load " + file);
            return null;
        }
        return createBuffers();
    }

    private Buffer createBuffers()
    {
        long start = System.currentTimeMillis();
        Buffer toBeActivated = null;
        long lastActivated = 0;
        Iterator iter = bufferEntries.iterator();
        while (iter.hasNext()) {
            SessionBufferEntry entry = (SessionBufferEntry) iter.next();
            if (entry != null) {
                File file = File.getInstance(entry.getPath());
                if (file != null && file.isLocal()) {
                    // See if a buffer already exists.  (The buffer would have
                    //  been created by autosave recovering a local file.)
                    Buffer buf = Editor.getBufferList().findBuffer(file);
                    if (buf != null) {
                        // do nothing, we already have a buffer
                    } else if (file.isDirectory()) {
                        buf = new Directory(file);
                    } else if (file.isFile() && file.canRead()) {
                        if (entry.getModeId() == WEB_MODE)
                            buf = WebBuffer.createWebBuffer(file, null, null);
                        else
                            buf = Buffer.precreateBuffer(file);
                    }
                    if (buf != null) {
                        buf.setLastView(new View(entry));
                        if (toBeActivated == null ||
                            entry.getLastActivated() > lastActivated) {
                            toBeActivated = buf;
                            lastActivated = entry.getLastActivated();
                        }
                    }
                } else {
                    Log.error("Session.createBuffers file = " + file);
                    Debug.bug();
                }
            }
        }
        if (toBeActivated == null)
            toBeActivated = Editor.getBufferList().getFirstBuffer();
        long elapsed = System.currentTimeMillis() - start;
        Log.debug("createBuffers " + Editor.getBufferList().size() +
            " buffers " + elapsed + " ms");
        return toBeActivated;
    }

    public void save()
    {
        try {
            File tempFile = Utilities.getTempFile();
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(
                    tempFile.getOutputStream()));
            writer.write("<?xml version=\"1.0\"?>");
            writer.newLine();
            writer.write("<session version=\"" + getVersion() + "\">");
            writer.newLine();
            writer.write("  <buffers>");
            writer.newLine();
            int index = 0;
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                // Skip shell, compilation, HTTP buffers etc.
                if (!buf.canBeRestored())
                    continue;
                // Skip untitled buffers.
                if (buf.getFile() == null)
                    continue;
                SessionBufferEntry entry = new SessionBufferEntry(buf, index++);
                writer.write(entry.toXml());
                writer.newLine();
            }
            writer.write("  </buffers>");
            writer.newLine();
            writer.write("</session>");
            writer.newLine();
            writer.flush();
            writer.close();
            Utilities.deleteRename(tempFile, file);
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private boolean load()
    {
        InputStream inputStream = null;
        try {
            if (file != null && file.isFile())
                inputStream = file.getInputStream();
        }
        catch (IOException e) {
            Log.error(e);
            return false;
        }
        if (inputStream == null)
            return false;
        XMLReader xmlReader = Utilities.getDefaultXMLReader();
        if (xmlReader == null)
            return false;
        xmlReader.setContentHandler(this);
        try {
            InputSource inputSource = new InputSource(inputStream);
            xmlReader.parse(inputSource);
        }
        catch (Exception e) {
            Log.error(e);
            return false;
        }
        if (bufferEntries == null)
            return false;
        // Make sure we can open at least one buffer.
        Iterator iter = bufferEntries.iterator();
        while (iter.hasNext()) {
            SessionBufferEntry entry = (SessionBufferEntry) iter.next();
            if (entry != null) {
                File file = File.getInstance(entry.getPath());
                Debug.assertTrue(file.isLocal());
                if (file.exists())
                    return true;
            }
        }
        return false;
    }

    public void startElement(String uri, String localName, String qName,
        Attributes attributes) throws SAXException
    {
        if (localName.equals("buffer") || qName.equals("buffer")) {
            currentBufferEntry = new SessionBufferEntry();
            String path = attributes.getValue("", "path");
            currentBufferEntry.setPath(path);
            String mode = attributes.getValue("", "mode");
            currentBufferEntry.setMode(mode);
            String dot = attributes.getValue("", "dot");
            int index = dot.indexOf(',');
            if (index >= 0) {
                String s1 = dot.substring(0, index);
                String s2 = dot.substring(index + 1);
                try {
                    currentBufferEntry.setDotLineNumber(Integer.parseInt(s1));
                    currentBufferEntry.setDotOffset(Integer.parseInt(s2));
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            }
            String when = attributes.getValue("", "when");
            try {
                currentBufferEntry.setLastActivated(Long.parseLong(when));
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
    }

    public void endElement(String uri, String localName, String qName)
    {
        if (localName.equals("buffer") || qName.equals("buffer")) {
            if (bufferEntries == null)
                bufferEntries = new ArrayList();
            bufferEntries.add(currentBufferEntry);
        }
    }

    private final int getVersion()
    {
        return 1;
    }
}
