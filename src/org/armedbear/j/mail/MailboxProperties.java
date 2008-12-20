/*
 * MailboxProperties.java
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

package org.armedbear.j.mail;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Iterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.File;
import org.armedbear.j.Log;
import org.armedbear.j.Property;
import org.armedbear.j.PropertyList;
import org.armedbear.j.Utilities;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

public final class MailboxProperties
{
    private static final int MAX_ENTRIES = 100;

    private static final String lineSeparator =
        System.getProperty("line.separator");

    private static ArrayList list;

    private MailboxProperties()
    {
    }

    public static synchronized PropertyList getProperties(MailboxURL url)
    {
        if (list == null)
            initialize();
        String name = url.getCanonicalName();
        for (int i = list.size()-1; i >= 0; i--) {
            Entry entry = (Entry) list.get(i);
            if (entry.name.equals(name))
                return entry.properties;
        }
        return null;
    }

    public static synchronized void saveProperties(Mailbox mb)
    {
        MailboxURL url = mb.getUrl();
        if (url == null) {
            Debug.bug();
            return;
        }
        String name = url.getCanonicalName();
        if (name == null) {
            Debug.bug();
            return;
        }
        if (name.length() == 0) {
            Debug.bug();
            return;
        }
        PropertyList properties = mb.getProperties();
        if (properties == null || properties.size() == 0) {
            Log.debug("MailboxProperties.saveProperties no properties set");
            return;
        }
        if (list == null)
            initialize();
        Entry newEntry = new Entry(name, properties);
        final int limit = list.size();
        for (int i = 0; i < limit; i++) {
            Entry entry = (Entry) list.get(i);
            if (entry.name.equals(name)) {
                if (i == 0) {
                    list.set(0, newEntry);
                    save();
                    return;
                }
                list.remove(i);
                break;
            }
        }
        // Add new entry.
        list.add(0, newEntry);
        save();
    }

    private static synchronized void initialize()
    {
        if (list == null) {
            Editor.protect(MailboxProperties.class);
            list = new ArrayList();
            File file = getFile();
            if (file.isFile()) {
                XMLReader xmlReader = Utilities.getDefaultXMLReader();
                if (xmlReader != null) {
                    try {
                        xmlReader.setContentHandler(new Handler());
                        InputSource inputSource =
                            new InputSource(file.getInputStream());
                        xmlReader.parse(inputSource);
                    }
                    catch (Exception e) {
                        Log.error(e);
                    }
                }
            }
            // Delete old mailboxes.xml in ~/.j (if any).
            File oldFile =
                File.getInstance(Directories.getEditorDirectory(),
                    "mailboxes.xml");
            if (oldFile != null && oldFile.isFile())
                oldFile.delete();
        }
    }

    private static void add(Entry entry)
    {
        list.add(entry);
    }

    private static synchronized void save()
    {
        try {
            File file = getFile();
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
            writer.write("<?xml version=\"1.0\"?>");
            writer.newLine();
            writer.write("<mailboxes version=\"" + getVersion() + "\">");
            writer.newLine();
            final int limit = Math.min(list.size(), MAX_ENTRIES);
            for (int i = 0; i < limit; i++) {
                Entry entry = (Entry) list.get(i);
                writer.write(entry.toXml());
                writer.newLine();
            }
            writer.write("</mailboxes>");
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private static final File getFile()
    {
        return File.getInstance(Directories.getMailDirectory(),
            "mailboxes.xml");
    }

    private static final String getVersion()
    {
        return "1";
    }

    private static class Entry
    {
        final String name;
        final PropertyList properties;
        long when;

        Entry(String name, PropertyList properties)
        {
            this.name = name;
            this.properties = properties;
            when = System.currentTimeMillis();
        }

        Entry(String name)
        {
            this.name = name;
            properties = new PropertyList();
        }

        String toXml()
        {
            FastStringBuffer sb = new FastStringBuffer("  <mailbox name=\"");
            sb.append(name);
            sb.append("\"");
            sb.append(" when=\"");
            sb.append(String.valueOf(when));
            sb.append("\"");
            sb.append(">");
            sb.append(lineSeparator);
            if (properties != null) {
                Iterator it = properties.keyIterator();
                if (it != null) {
                    while (it.hasNext()) {
                        Property property = (Property) it.next();
                        Object value = properties.getProperty(property);
                        if (value != null) {
                            sb.append(propertyToXml(property.getDisplayName(),
                                value.toString()));
                        }
                    }
                }
            }
            sb.append("  </mailbox>");
            return sb.toString();
        }

        private static String propertyToXml(String name, String value)
        {
            FastStringBuffer sb = new FastStringBuffer("    <property name=\"");
            sb.append(name);
            sb.append("\" value=\"");
            sb.append(value);
            sb.append("\"/>");
            sb.append(lineSeparator);
            return new String(sb.toString());
        }
    }

    private static class Handler extends DefaultHandler implements ContentHandler
    {
        private Entry currentEntry = null;

        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException
        {
            if (localName.equals("mailboxes") || qName.equals("mailboxes")) {
                String version = attributes.getValue("version");
                if (!version.equals(MailboxProperties.getVersion()))
                    throw new SAXException("Unknown mailbox history format");
            } else if (localName.equals("mailbox") || qName.equals("mailbox")) {
                // Start a new entry.
                String mailboxName = attributes.getValue("name");
                currentEntry = new Entry(mailboxName);
                try {
                    currentEntry.when =
                        Long.parseLong(attributes.getValue("when"));
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            } else if (localName.equals("property") || qName.equals("property")) {
                Debug.assertTrue(currentEntry != null);
                String key = attributes.getValue("name");
                if (key != null) {
                    String value = attributes.getValue("value");
                    if (value != null) {
                        Property property = Property.findProperty(key);
                        if (property != null)
                            currentEntry.properties.setPropertyFromString(property,
                                value);
                    }
                }
            }
        }

        public void endElement(String uri, String localName, String qName)
        {
            if (localName.equals("mailbox") || qName.equals("mailbox")) {
                MailboxProperties.add(currentEntry);
                currentEntry = null;
            }
        }
    }
}
