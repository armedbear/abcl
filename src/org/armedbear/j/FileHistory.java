/*
 * FileHistory.java
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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

public final class FileHistory extends DefaultHandler implements ContentHandler
{
    private static final int MAX_ENTRIES = 100;

    // Singleton.
    private static FileHistory fileHistory;

    private ArrayList list = new ArrayList();

    private File file;

    private FileHistory()
    {
        file = File.getInstance(Directories.getEditorDirectory(), "files.xml");
        if (file.isFile())
            load();
    }

    public static synchronized FileHistory getFileHistory()
    {
        if (fileHistory == null) {
            fileHistory = new FileHistory();
            Editor.protect(fileHistory);
        }
        return fileHistory;
    }

    public synchronized FileHistoryEntry findEntry(String canonicalPath)
    {
        final int limit = list.size();
        for (int i = 0; i < limit; i++) {
            FileHistoryEntry entry = (FileHistoryEntry) list.get(i);
            if (entry.getName().equals(canonicalPath))
                return entry;
        }
        return null;
    }

    public synchronized void store(FileHistoryEntry newEntry)
    {
        Debug.assertTrue(newEntry != null);
        Debug.assertTrue(newEntry.getName() != null);
        final int limit = list.size();
        for (int i = 0; i < limit; i++) {
            FileHistoryEntry entry = (FileHistoryEntry) list.get(i);
            if (entry.getName().equals(newEntry.getName())) {
                if (i == 0) {
                    list.set(0, newEntry);
                    return;
                }
                list.remove(i);
                break;
            }
        }
        // Add new entry.
        list.add(0, newEntry);
    }

    private void load()
    {
        XMLReader xmlReader = Utilities.getDefaultXMLReader();
        if (xmlReader != null) {
            xmlReader.setContentHandler(this);
            try {
                InputSource inputSource = new InputSource(file.getInputStream());
                xmlReader.parse(inputSource);
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    private FileHistoryEntry currentEntry = null;

    public void startElement(String uri, String localName, String qName,
        Attributes attributes) throws SAXException
    {
        if (localName.equals("files") || qName.equals("files")) {
            String version = attributes.getValue("version");
            if (!version.equals(getVersion()))
                throw new SAXException("Unknown file history format");
        } else if (localName.equals("file") || qName.equals("file")) {
            // Start a new entry.
            currentEntry = new FileHistoryEntry();
            currentEntry.setName(attributes.getValue("", "name"));
            currentEntry.setEncoding(attributes.getValue("", "encoding"));
            currentEntry.setMode(attributes.getValue("", "mode"));
            try {
                currentEntry.setWhen(Long.parseLong(attributes.getValue("when")));
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        } else if (localName.equals("property") || qName.equals("property")) {
            Debug.assertTrue(currentEntry != null);
            String key = attributes.getValue("", "name");
            String value = attributes.getValue("", "value");
            Property property = Property.findProperty(key);
            if (property != null)
                currentEntry.setPropertyFromString(property, value);
        }
    }

    public void endElement(String uri, String localName, String qName)
    {
        if (localName.equals("file") || qName.equals("file")) {
            list.add(currentEntry);
            currentEntry = null;
        }
    }

    public synchronized void save()
    {
        try {
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
            writer.write("<?xml version=\"1.0\"?>");
            writer.newLine();
            writer.write("<files version=\"" + getVersion() + "\">");
            writer.newLine();
            final int limit = Math.min(list.size(), MAX_ENTRIES);
            for (int i = 0; i < limit; i++) {
                FileHistoryEntry entry = (FileHistoryEntry) list.get(i);
                writer.write(entry.toXml());
                writer.newLine();
            }
            writer.write("</files>");
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private static final String getVersion()
    {
        return "2";
    }
}
