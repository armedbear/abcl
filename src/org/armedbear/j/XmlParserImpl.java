/*
 * XmlParserImpl.java
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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Stack;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public final class XmlParserImpl extends DefaultHandler implements Runnable,
    ContentHandler, EntityResolver
{
    private static final String VALIDATION =
        "http://xml.org/sax/features/validation";

    private String parserClassName;
    private boolean aelfred;
    private final Buffer buffer;
    private Reader reader;
    private XMLReader xmlReader;
    private TreeModel treeModel;
    private Stack stack;
    private Exception exception;
    private DefaultMutableTreeNode current;
    private Locator locator;
    private FastStringBuffer output;

    public XmlParserImpl(Buffer buffer)
    {
        Debug.assertTrue(buffer != null);
        this.buffer = buffer;
    }

    public boolean initialize()
    {
        String className =
            Editor.preferences().getStringProperty("org.xml.sax.driver");
        if (className == null)
            className = System.getProperty("org.xml.sax.driver");
        if (className != null) {
            try {
                xmlReader = XMLReaderFactory.createXMLReader(className);
            }
            catch (Exception e) {
                Log.debug(e);
            }
        }
        if (xmlReader == null)
            xmlReader = Utilities.getDefaultXMLReader();
        if (xmlReader == null) {
            parserClassName = null;
            aelfred = false;
            Log.error("no parser found");
        } else {
            parserClassName = xmlReader.getClass().getName();
            if (parserClassName.equals("org.armedbear.j.aelfred.SAXDriver"))
                aelfred = true;
            else
                aelfred = false;
        }
        return xmlReader != null;
    }

    public String getParserClassName()
    {
        return parserClassName;
    }

    public void setReader(Reader reader)
    {
        this.reader = reader;
    }

    public boolean enableValidation(boolean enable)
    {
        if (xmlReader == null) {
            Debug.bug();
            return false;
        }
        try {
            xmlReader.setFeature(VALIDATION, enable);
        }
        catch (SAXNotRecognizedException e) {
            Log.error(e);
            return false;
        }
        catch (SAXNotSupportedException e) {
            Log.error(e);
            return false;
        }
        return true;
    }

    private boolean isValidating()
    {
        if (xmlReader == null) {
            Debug.bug();
            return false;
        }
        try {
            return xmlReader.getFeature(VALIDATION);
        }
        catch (SAXNotRecognizedException e) {}
        catch (SAXNotSupportedException e) {}
        return false;
    }

    public Exception getException()
    {
        return exception;
    }

    public String getOutput()
    {
        return output != null ? output.toString() : "";
    }

    public void run()
    {
        if (xmlReader == null) {
            Debug.bug();
            initialize();
        }

        if (xmlReader == null) {
            Log.error("no XML reader available");
            return;
        }

        exception = null;
        output = new FastStringBuffer();

        final boolean validating = isValidating();

        output.append("Using ");
        output.append(xmlReader.getClass().getName());
        output.append(" (");
        if (!validating)
            output.append("not ");
        output.append("validating)\n");

        // Parser must be associated with a buffer.
        if (buffer == null) {
            Debug.bug();
            return;
        }

        InputSource inputSource = null;
        final File file = buffer.getFile();
        if (reader != null) {
            inputSource = new InputSource(reader);
        } else if (file != null) {
            try {
                InputStream inputStream = file.getInputStream();
                if (inputStream != null) {
                    inputSource = new InputSource(inputStream);
                    String encoding = file.getEncoding();
                    if (encoding != null) {
                        inputSource.setEncoding(encoding);
                        Log.debug("parser encoding is " + encoding);
                    }
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        if (inputSource == null)
            return;
        if (file != null) {
            if (file.isRemote())
                inputSource.setSystemId(file.netPath());
            else
                inputSource.setSystemId("file://".concat(file.canonicalPath()));
        }
        treeModel = null;
        stack = new Stack();

        if (xmlReader != null) {
            xmlReader.setContentHandler(this);
            xmlReader.setErrorHandler(this);
            xmlReader.setEntityResolver(this);
            long start = System.currentTimeMillis();
            try {
                xmlReader.parse(inputSource);
            }
            catch (Exception e) {
                exception = e;
            }
            long elapsed = System.currentTimeMillis() - start;
            output.append('\n');
            output.append(validating ? "Validation" : "Parsing");
            output.append(" finished (");
            output.append(elapsed);
            output.append(" ms)");
        }
    }

    public InputSource resolveEntity(String publicId, String systemId)
    {
        if (systemId == null)
            return null;
        if (Platform.isPlatformWindows() && systemId.startsWith("file://")) {
            // Strip "file://" prefix.
            String filename = systemId.substring(7);
            // Make sure there's a colon after the drive letter.
            if (filename.length() > 2 && filename.charAt(1) == '/')
                filename = filename.substring(0, 1) + ':' + filename.substring(1);
            // Make sure the slashes are pointing the right way.
            filename = File.normalize(filename);
            try {
                return new InputSource(new FileInputStream(filename));
            }
            catch (FileNotFoundException e) {}
            // FileNotFoundException was thrown.
            if (filename.length() > 3 && filename.charAt(1) == ':'
                && filename.charAt(2) == '\\') {
                // Try relative to buffer's directory.
                File file = File.getInstance(buffer.getFile().getParentFile(),
                    filename.substring(3));
                try {
                    return new InputSource(new FileInputStream(file.canonicalPath()));
                }
                catch (Exception e) {}
            }
        }
        if (aelfred) {
            // There's no way to tell aelfred about new system identifiers, so
            // URLs relative to the new entity won't be resolved correctly if
            // we return an input stream here. Don't use caching if we're
            // using aelfred!
            Log.debug("using aelfred - cache not supported");
            return null;
        }
        if (!buffer.getBooleanProperty(Property.ENABLE_CACHE)) {
            Log.debug("cache disabled");
            return null;
        }
        if (systemId.startsWith("http://")) {
            Cache cache = Cache.getCache();
            if (cache != null) {
                Log.debug("checking cache for ".concat(systemId));
                File file = cache.get(systemId);
                if (file == null) {
                    Log.debug("caching ".concat(systemId));
                    file = cache.put(systemId);
                }
                if (file != null) {
                    try {
                        Log.debug("returning input stream from cache");
                        InputSource inputSource =
                            new InputSource(file.getInputStream());
                        inputSource.setSystemId(systemId);
                        return inputSource;
                    }
                    catch (Exception e) {
                        Log.error(e);
                    }
                }
            }
        }
        return null;
    }

    public void setDocumentLocator(Locator locator)
    {
        this.locator = locator;
    }

    public void startElement(String uri, String localName, String qName,
        Attributes attributes) throws SAXException
    {
        int lineNumber = 0;
        int columnNumber = 0;
        if (locator != null) {
            lineNumber = locator.getLineNumber();
            columnNumber = locator.getColumnNumber();
        }
        DefaultMutableTreeNode node =
            new DefaultMutableTreeNode(new XmlTreeElement(localName,
                attributes, lineNumber, columnNumber));
        if (treeModel == null) {
            treeModel = new DefaultTreeModel(node);
        } else {
            Debug.assertTrue(current != null);
            current.insert(node, current.getChildCount());
            stack.push(current);
        }
        current = node;
    }

    public void endElement(String uri, String localName, String qName)
    {
        if (stack.empty())
            current = null;
        else
            current = (DefaultMutableTreeNode) stack.pop();
    }

    public void warning(SAXParseException e)
	throws SAXException
    {
        appendMessage("Warning", e);
    }

    public void error(SAXParseException e)
	throws SAXException
    {
        appendMessage("Error", e);
    }

    public void fatalError(SAXParseException e)
	throws SAXException
    {
        appendMessage("Fatal error", e);
    }

    private void appendMessage(String what, SAXParseException e)
    {
        FastStringBuffer sb = new FastStringBuffer();
        final String systemId = e.getSystemId();
        final int lineNumber = e.getLineNumber();
        if (systemId.startsWith("file://")) {
            sb.append(systemId.substring(7));
            sb.append(':');
            sb.append(lineNumber);
        } else if (systemId.startsWith("file:")) {
            sb.append(systemId.substring(5));
            sb.append(':');
            sb.append(lineNumber);
        } else {
            sb.append(systemId);
            sb.append(" line ");
            sb.append(lineNumber);
        }
        sb.append(": ");
        sb.append(what);
        sb.append(": ");
        sb.append(e.getMessage());
        sb.append('\n');
        output.append(sb.toString());
    }

    public TreeModel getTreeModel()
    {
        return treeModel;
    }
}
