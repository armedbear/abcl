/*
 * JdbSession.java
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

package org.armedbear.j.jdb;

import java.io.BufferedWriter;
import java.io.EOFException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

public final class JdbSession extends Properties
{
    private List breakpointSpecifications;
    private List breakpoints;

    public JdbSession()
    {
    }

    public String getName()
    {
        return getProperty("name", "");
    }

    private static File jdbDir;

    private static File getSettingsDirectory()
    {
        if (jdbDir == null) {
            jdbDir = File.getInstance(Directories.getEditorDirectory(), "jdb");
            if (!jdbDir.isDirectory())
                jdbDir.mkdirs();
        }
        return jdbDir;
    }

    private static File sessionDir;

    private File getDefaultSessionFile()
    {
        return File.getInstance(getSettingsDirectory(), "defaults.xml");
    }

    public static String[] getSessionNames()
    {
        return sessionDir.list();
    }

    public static void deleteSession(String name)
    {
        File file = File.getInstance(sessionDir, name);
        if (file.isFile())
            file.delete();
    }

    public String getMainClass()
    {
        return getProperty("mainClass", "");
    }

    public void setMainClass(String s)
    {
        put("mainClass", s);
    }

    public String getMainClassArgs()
    {
        return getProperty("mainClassArgs", "");
    }

    public void setMainClassArgs(String s)
    {
        put("mainClassArgs", s);
    }

    public String getClassPath()
    {
        return getProperty("classPath", "");
    }

    public void setClassPath(String s)
    {
        put("classPath", s);
    }

    public String getJavaHome()
    {
        return getProperty("javaHome", "");
    }

    public void setJavaHome(String s)
    {
        put("javaHome", s);
    }

    public String getJavaExecutable()
    {
        return getProperty("javaExecutable", "");
    }

    public void setJavaExecutable(String s)
    {
        put("javaExecutable", s);
    }

    public String getVMArgs()
    {
        return getProperty("vmArgs", "");
    }

    public void setVMArgs(String s)
    {
        put("vmArgs", s);
    }

    public boolean getStartSuspended()
    {
        String s = getProperty("startSuspended");
        return s != null && s.equals("true");
    }

    public void setStartSuspended(boolean b)
    {
        put("startSuspended", b ? "true" : "false");
    }

    public String getSourcePath()
    {
        return getProperty("sourcePath", "");
    }

    public void setSourcePath(String s)
    {
        put("sourcePath", s);
    }

    public void saveDefaults()
    {
        save(getDefaultSessionFile());
    }

    public void loadDefaults()
    {
        clear();
        File file = getDefaultSessionFile();
        if (file != null && file.isFile())
            load(file);
    }

    private void save(File file)
    {
        try {
            OutputStream out = file.getOutputStream();
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(out));
            writer.write("<?xml version=\"1.0\"?>");
            writer.newLine();
            writer.write("<session version=\"" + getVersion() + "\">");
            writer.newLine();
            Enumeration propertyNames = propertyNames();
            while (propertyNames.hasMoreElements()) {
                String name = (String) propertyNames.nextElement();
                String value = getProperty(name);
                writer.write("  ");
                writer.write(Utilities.propertyToXml(name, value));
                writer.newLine();
            }
            saveBreakpoints(writer);
            writer.write("</session>");
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private void saveBreakpoints(BufferedWriter writer)
    {
        if (breakpoints != null && breakpoints.size() > 0) {
            try {
                writer.write("  <breakpoints>");
                writer.newLine();
                Iterator iter = breakpoints.iterator();
                while (iter.hasNext()) {
                    Object obj = iter.next();
                    if (obj instanceof MethodBreakpoint) {
                        MethodBreakpoint bp = (MethodBreakpoint) obj;
                        if (!bp.isTemporary())
                            writer.write(bp.toXml());
                    } else if (obj instanceof LineNumberBreakpoint) {
                        LineNumberBreakpoint bp = (LineNumberBreakpoint) obj;
                        if (!bp.isTemporary())
                            writer.write(bp.toXml());
                    }
                }
                writer.write("  </breakpoints>");
                writer.newLine();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
    }

    private void load(File file)
    {
        XMLReader xmlReader = Utilities.getDefaultXMLReader();
        if (xmlReader != null) {
            xmlReader.setContentHandler(new Handler());
            try {
                InputSource inputSource = new InputSource(file.getInputStream());
                xmlReader.parse(inputSource);
            }
            catch (EOFException ignored) {}
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    public List getBreakpointSpecifications()
    {
        return breakpointSpecifications;
    }

    public void setBreakpoints(List breakpoints)
    {
        this.breakpoints = breakpoints;
    }

    private static final String getVersion()
    {
        return "1";
    }

    private class Handler extends DefaultHandler implements ContentHandler
    {
        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException
        {
            if (localName.equals("session") || qName.equals("session")) {
                String version = attributes.getValue("version");
                if (!version.equals(getVersion()))
                    throw new SAXException("Unknown session format");
            } else if (localName.equals("property") || qName.equals("property")) {
                // Session property.
                String propertyName = attributes.getValue("name");
                String value = attributes.getValue("value");
                setProperty(propertyName, value);
            } else if (localName.equals("breakpoints") || qName.equals("breakpoints")) {
                breakpointSpecifications = new ArrayList();
            } else if (localName.equals("breakpoint") || qName.equals("breakpoint")) {
                BreakpointSpecification spec =
                    new BreakpointSpecification(attributes);
                breakpointSpecifications.add(spec);
            }
        }
    }
}
