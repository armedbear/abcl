/*
 * WebLoader.java
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PushbackReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Stack;

public final class WebLoader implements WebConstants
{
    private PushbackReader reader;
    private final FastStringBuffer textBuffer = new FastStringBuffer();
    private final Stack indentStack = new Stack();
    private final Stack tableStack = new Stack();
    private Table currentTable;
    private int sourceOffset;
    private int offset;
    private final int maxChars = 80;
    private LineSegmentList segments;
    private LineSequence lines;
    private final Hashtable refs = new Hashtable();
    private int indentLevel;
    private File file;

    public WebLoader(File file)
    {
        this.file = file;
        if (file.getEncoding() == null)
            file.setEncoding("iso-8859-1");
        Debug.assertTrue(file.isLocal());
    }

    public WebLoader(Reader reader)
    {
        this.reader = new PushbackReader(new BufferedReader(reader));
    }

    public final Hashtable getRefs()
    {
        return refs;
    }

    public LineSequence load()
    {
        try {
            loadInternal();
        }
        catch (EncodingChangeException e) {
            Log.debug("encoding change!");
            Log.debug("new encoding = |" + e.getNewEncoding() + "|");
            file.setEncoding(e.getNewEncoding());
            reader = null;
            try {
                loadInternal();
            }
            catch (EncodingChangeException ex) {
                Log.error(ex);
            }
        }
        // Handle zero length files.
        if (lines.getFirstLine() == null)
            lines.appendLine(new WebLine(sourceOffset));
        return lines;
    }

    private void loadInternal() throws EncodingChangeException
    {
        if (reader == null) {
            Debug.assertTrue(file != null);
            String encoding = file.getEncoding();
            if (encoding == null)
                encoding = Editor.preferences().getStringProperty(Property.DEFAULT_ENCODING);
            try {
                InputStream inputStream = file.getInputStream();
                reader = new PushbackReader(new BufferedReader(new InputStreamReader(inputStream, encoding)));
            }
            catch (IOException e) {
                Log.error(e);
                return;
            }
        }
        lines = new LineSequence();
        sourceOffset = 0;
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                // Line separator always counts as 1 char, so count '\n' but
                // not '\r'.
                if (c != '\r')
                    ++sourceOffset;
                switch (c) {
                    case '<':
                        processMarkup();
                        break;
                    case '&':
                        processEntity();
                        break;
                    default:
                        doChar((char)c);
                        break;
                }
            }
            flushLine();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private boolean bold;
    private boolean strong;
    private boolean italic;
    private boolean emphasis;
    private boolean heading;
    private boolean h1;
    private boolean center;
    private boolean preformatted;
    private boolean whitespace;
    private Link link;

    private final boolean centered()
    {
        return center || h1;
    }

    private void processMarkup() throws EncodingChangeException
    {
        final String tag = gatherTag();
        if (tag.length() < 3) {
            doText(tag);
            return;
        }
        char c = tag.charAt(1);
        if (c == '/') {
            if (!Character.isLetter(tag.charAt(2))) {
                doText(tag);
                return;
            }
        } else {
            if (c == '!') {
                // We only care about comments.
                if (tag.equals("<!--"))
                    skipComment();
                return;
            }
            if (c == '?') {
                // Ignore XML declaration, processing instructions.
                return;
            }
            if (!Character.isLetter(c)) {
                doText(tag);
                return;
            }
        }
        final String tagName = Utilities.getTagName(tag).toLowerCase().intern();

        // Unsupported tags.
        if (tagName == "applet") {
            skipTag("/applet");
            return;
        }
        if (tagName == "form") {
            flushLine();
            textBuffer.append("[Form]");
            flushSegment(null, FORMAT_DISABLED);
            flushLine();
            return;
        }
        if (tagName == "/form") {
            flushLine();
            textBuffer.append("[End Form]");
            flushSegment(null, FORMAT_DISABLED);
            newLine();
            return;
        }
        if (tagName == "input") {
            List attributes = getAttributes(tag);
            String type = getAttribute(attributes, "type");
            if (type != null) {
                if (type.equalsIgnoreCase("submit")) {
                    flushSegment();
                    String value = getAttribute(attributes, "value");
                    if (value == null)
                        value = "Submit"; // Default label.
                    textBuffer.append('[');
                    textBuffer.append(value);
                    textBuffer.append(']');
                    flushSegment(null, FORMAT_DISABLED);
                } else if (type.equalsIgnoreCase("image")) {
                    flushSegment();
                    textBuffer.append("[Image]");
                    flushSegment(null, FORMAT_DISABLED);
                }
            }
            return;
        }
        if (tagName == "object") {
            skipTag("/object");
            return;
        }
        if (tagName == "xml") {
            skipTag("/xml");
            return;
        }
        if (tagName == "script") {
            skipScript();
            return;
        }

        if (tagName == "title") {
            processTitle();
            return;
        }
        if (tagName == "b") {
            flushSegment();
            if (bold) {
                // Two <b>'s in a row. This one is probably a typo for </b>.
                bold = false;
            } else {
                bold = true;
            }
            return;
        }
        if (tagName == "/b") {
            flushSegment();
            bold = false;
            return;
        }
        if (tagName == "strong") {
            flushSegment();
            strong = true;
            return;
        }
        if (tagName == "/strong") {
            flushSegment();
            strong = false;
            return;
        }
        if (tagName == "i") {
            flushSegment();
            italic = true;
            return;
        }
        if (tagName == "/i") {
            flushSegment();
            italic = false;
            return;
        }
        if (tagName == "em") {
            flushSegment();
            emphasis = true;
            return;
        }
        if (tagName == "/em") {
            flushSegment();
            emphasis = false;
            return;
        }
        if (tagName == "q" || tagName == "/q") {
            // Indent if we're at the beginning of the line.
            maybeIndent();
            textBuffer.append('"');
            return;
        }
        if (tagName == "a") {
            if (link != null)
                // The last <a> tag was never terminated. This is probably a typo for </a>.
                processEndAnchor();
            else
                processAnchor(tag);
            return;
        }
        if (tagName == "/a") {
            processEndAnchor();
            return;
        }
        if (tagName == "h1") {
            newLine();
            heading = true;
            h1 = true;
            return;
        }
        if (tagName == "/h1") {
            newLine();
            heading = false;
            h1 = false;
            return;
        }
        if (tagName == "h2" ||
            tagName == "h3" ||
            tagName == "h4" ||
            tagName == "h5" ||
            tagName == "h6") {
            newLine();
            heading = true;
            return;
        }
        if (tagName == "/h2" ||
            tagName == "/h3" ||
            tagName == "/h4" ||
            tagName == "/h5" ||
            tagName == "/h6") {
            newLine();
            heading = false;
            return;
        }
        if (tagName == "br") {
            // Forced line break. If there's no text to flush, append a blank
            // line.
            if (!flushLine()) {
                lines.appendLine(new WebLine(sourceOffset));
                ++offset;
            }
            return;
        }
        if (tagName == "div") {
            flushLine();
            return;
        }
        if (tagName == "/div") {
            flushLine();
            return;
        }
        if (tagName == "p") {
            newLine();
            return;
        }
        if (tagName == "pre") {
            flushLine();
            preformatted = true;
            return;
        }
        if (tagName == "/pre") {
            newLine();
            preformatted = false;
            return;
        }
        if (tagName == "blockquote") {
            newLine();
            indentStack.push("blockquote");
            ++indentLevel;
            return;
        }
        if (tagName == "/blockquote") {
            newLine();
            if (!indentStack.empty()) {
                String s = (String) indentStack.pop();
                --indentLevel;
                if (!s.equals("blockquote"))
                    Log.error("**** /blockquote: stack imbalance");
            }
            return;
        }
        // Definition list.
        if (tagName == "dl") {
            newLine();
            indentStack.push("dl");
            return;
        }
        // Never omitted.
        if (tagName == "/dl") {
            newLine();
            // Handle unbalanced <dt> and/or <dd> tags.
            while (!indentStack.empty()) {
                String s = (String) indentStack.peek();
                if (s.equals("dd")) {
                    indentStack.pop();
                    --indentLevel;
                } else if (s.equals("dl")) {
                    indentStack.pop();
                    break;
                } else {
                    // Shouldn't happen.
                    break;
                }
            }
            return;
        }
        // Definition.
        if (tagName == "dd") {
            flushLine();
            if (!indentStack.empty()) {
                String s = (String) indentStack.peek();
                if (s.equals("dl"))
                    ;
                else if (s.equals("dd")) {
                    // Keep same indentation.
                    return;
                } else
                    Log.error("**** dd: top of stack is " + s);
            } else
                Log.error("**** dd: indentStack unexpectedly empty");
            indentStack.push("dd");
            ++indentLevel;
            return;
        }
        // Term to be defined.
        if (tagName == "dt") {
            flushLine();
            if (!indentStack.empty()) {
                String s = (String) indentStack.peek();
                if (s.equals("dd")) {
                    indentStack.pop(); // <dt> terminating <dd> (javadoc)
                    --indentLevel;
                } else if (s.equals("dl"))
                    ;
                else
                    Log.error("**** dt: top of stack is " + s);
            } else
                Log.error("**** dt: indentStack unexpectedly empty");
            return;
        }
        if (tagName == "img") {
            processImg(tag);
            return;
        }
        if (tagName == "center") {
            flushLine();
            center = true;
            return;
        }
        if (tagName == "/center") {
            flushLine();
            center = false;
            return;
        }
        if (tagName == "hr") {
            flushLine();
            link = null;
            for (int i = 0; i < maxChars(); i++)
                textBuffer.append('-');
            flushLine();
            return;
        }
        if (tagName == "ul") {
            newLine();
            indentStack.push("ul");
            ++indentLevel;
        }
        // Never omitted.
        if (tagName == "/ul") {
            newLine();
            if (!indentStack.empty()) {
                indentStack.pop();
                --indentLevel;
            }
        }
        // End tag is usually omitted.
        if (tagName == "li") {
            flushLine();
            if (indentStack.size() > 0) {
                textBuffer.append(Utilities.spaces(getIndent()));
            } else {
                textBuffer.append(Utilities.spaces(4));
            }
            if (textBuffer.length() >= 2)
                textBuffer.setCharAt(textBuffer.length() - 2, '\u2022');
            flushSegment(null, 0);
            return;
        }
        if (tagName == "style") {
            skipTag("/style");
            return;
        }
        if (tagName == "table") {
            newLine();
            tableStack.push(currentTable);
            currentTable = new Table();
            return;
        }
        if (tagName == "/table") {
            flushLine();
            if (!tableStack.empty())
                currentTable = (Table) tableStack.pop();
            else
                Log.error("**** /table: table stack imbalance source offset = " + sourceOffset);
            return;
        }
        // </tr> tag may be omittted.
        if (tagName == "tr") {
            flushLine();
            if (currentTable != null)
                currentTable.nextRow();
            else
                Log.error("**** tr: currentTable is null source offset = " + sourceOffset);
            return;
        }
        // </td> tag may be omitted.
        if (tagName == "td" || tagName == "th") {
            flushSegment();
            if (currentTable != null) {
                currentTable.nextColumn();
                int currentOffset = getCurrentOffset();
                // Leave at least one space between columns (but no space
                // before the first column).
                int numSpaces = 1;
                if (currentTable.getColumnIndex() == 0 || currentOffset == 0)
                    numSpaces = 0;
                int minimumOffset = currentTable.getMinimumOffset();
                if (minimumOffset > 0) {
                    if (currentOffset < minimumOffset)
                        numSpaces = minimumOffset - currentOffset;
                }
                textBuffer.append(Utilities.spaces(numSpaces));
                flushSegment(null, FORMAT_WHITESPACE);
                String s = getAttribute(tag, "width");
                if (s != null) {
                    if (s.endsWith("%")) {
                        s = s.substring(0, s.length()-1).trim();
                        if (s.length() > 0) {
                            try {
                                int percent = Integer.parseInt(s);
                                int width = maxChars() * percent / 100;
                                currentTable.setColumnWidth(width);
                            }
                            catch (NumberFormatException e) {
                                Log.error(e);
                            }
                        }
                    } else
                        ; // Ignore widths specified in pixels.
                }
            } else
                Log.error("**** td: currentTable is null");
            return;
        }
        if (tagName == "meta") {
            // Ignore change of encoding if we're not loading a file. This can
            // happen when load() is called from MessageBuffer.setText() to
            // process an HTML message.
            if (file == null)
                return;
            String encoding = file.getEncoding();
            // Ignore the specified encoding if we have already determined the
            // encoding from the byte order mark.
            if (encoding != null) {
                if (encoding.equals("UnicodeBig") || encoding.equals("UnicodeLittle"))
                    return;
            }
            List attributes = getAttributes(tag);
            String httpEquiv = getAttribute(attributes, "http-equiv");
            if (httpEquiv != null) {
                if (httpEquiv.toLowerCase().equals("content-type")) {
                    String contentType = getAttribute(attributes, "content");
                    if (contentType != null) {
                        String charset =
                            Utilities.getCharsetFromContentType(contentType);
                        Log.debug("charset = |" + charset + "|");
                        if (charset != null && charset.length() > 0) {
                            String newEncoding =
                                Utilities.getEncodingFromCharset(charset);
                            Log.debug("new encoding = " + newEncoding);
                            if (!newEncoding.equalsIgnoreCase(encoding))
                                throw new EncodingChangeException(newEncoding);
                            Log.debug("no encoding change");
                        }
                    }
                }
            }
            return;
        }
    }

    private void processTitle()
    {
        FastStringBuffer sb = new FastStringBuffer();
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c != '\r')
                    ++sourceOffset;
                if (c == '<') {
                    String tag = gatherTag();
                    if (!isTag(tag, "/title"))
                        Log.error("processTitle unexpected tag " + tag);
                    break;
                } else if (c == '&') {
                    String entity = gatherEntity();
                    sb.append(substituteEntity(entity));
                } else
                    sb.append((char)c);
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        String title = sb.toString().trim();
        if (lines.getFirstLine() == null) {
            if (textBuffer.length() == 0) {
                if (title.length() < maxChars())
                    textBuffer.append(Utilities.spaces(maxChars() - title.length()));
                textBuffer.append(title);
                flushLine();
            }
        }
    }

    private void processAnchor(String tag)
    {
        flushSegment();
        List attributes = getAttributes(tag);
        if (attributes != null) {
            for (int i = 0; i < attributes.size(); i++) {
                StringPair pair = (StringPair) attributes.get(i);
                if (pair.first.equals("href"))
                    link = new Link(pair.second.trim());
                else if (pair.first.equals("name"))
                    addRef(pair.second, offset);
            }
        }
    }

    private void processEndAnchor()
    {
        boolean appendSpace = false;
        while (textBuffer.toString().endsWith(" ")) {
            appendSpace = true;
            textBuffer.setLength(textBuffer.length() - 1);
        }
        flushSegment();
        link = null;
        if (appendSpace) {
            textBuffer.append(' ');
            flushSegment();
        }
    }

    private void processImg(String tag)
    {
        flushSegment();
        List attributes = getAttributes(tag);
        String alt = getAttribute(attributes, "alt");
        String src = getAttribute(attributes, "src");
        String width = getAttribute(attributes, "width");
        String height = getAttribute(attributes, "height");
        int w = 0;
        int h = 0;
        if (width != null) {
            try {
                w = Integer.parseInt(width);
            }
            catch (NumberFormatException e) {}
        }
        if (height != null) {
            try {
                h = Integer.parseInt(height);
            }
            catch (NumberFormatException e) {}
        }
        // Create image link if appropriate.
        ImageLink imageLink = null;
        if (src != null && src.length() > 0) {
            String lower = src.toLowerCase();
            if (lower.endsWith(".jpg") || lower.endsWith(".gif") || lower.endsWith(".png")) {
                // Only provide image link if image is big enough.
                if (w >= 100 && h >= 100)
                    imageLink = new ImageLink(src);
            }
        }
        if (imageLink != null) {
            FastStringBuffer sb = new FastStringBuffer("[IMAGE");
            if (width != null && height != null) {
                sb.append(' ');
                sb.append(width);
                sb.append('x');
                sb.append(height);
            }
            sb.append(']');
            if (alt != null && (alt = alt.trim()).length() > 0) {
                sb.append(' ');
                sb.append(alt);
            }
            imageLink.setText(sb.toString());
            textBuffer.append(imageLink.getText());
            flushSegment(imageLink, FORMAT_LINK);
        }
        // Add a space if the last character on the line so far is not
        // already a space.
        if (segments == null || segments.size() == 0) {
            // We don't need to add a space at the beginning of the line.
            return;
        }
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 0; i < segments.size(); i++) {
            HtmlLineSegment segment = (HtmlLineSegment) segments.getSegment(i);
            sb.append(segment.getText());
        }
        if (sb.length() == 0 || sb.charAt(sb.length()-1) == ' ')
            return;
        // The last character is not a space, so we need to add one.
        textBuffer.append(' ');
        flushSegment(null, FORMAT_WHITESPACE);
    }

    private final void addRef(String ref, int offset)
    {
        refs.put(ref, new Integer(offset));
    }

    private static final String getAttribute(String tag, String attributeName)
    {
        return getAttribute(getAttributes(tag), attributeName);
    }

    private static String getAttribute(List attributes, String attributeName)
    {
        if (attributes != null) {
            for (int i = attributes.size()-1; i >= 0; i--) {
                StringPair pair = (StringPair) attributes.get(i);
                if (pair.first.equals(attributeName))
                    return pair.second;
            }
        }
        return null;
    }

    private static List getAttributes(String tag)
    {
        final int NEUTRAL         = 0;
        final int ATTRIBUTE_NAME  = 1;
        final int SPACE_BEFORE_EQ = 2;
        final int SPACE_AFTER_EQ  = 3;
        final int ATTRIBUTE_VALUE = 4;

        int state = NEUTRAL;
        FastStringBuffer sb = new FastStringBuffer();
        String name = null;
        String value = null;
        ArrayList attributes = null;
        char delim = 0;

        final int limit = tag.length();
        int i;
        // Skip past tag name.
        for (i = 0; i < limit; i++) {
            char c = tag.charAt(i);
            if (c == '>')
                return null;
            if (Character.isWhitespace(c)) {
                ++i;
                break;
            }
        }

        for (; i < limit; i++) {
            char c = tag.charAt(i);
            switch (state) {
                case NEUTRAL:
                    if (Character.isWhitespace(c))
                        ;
                    else {
                        sb.setLength(0);
                        sb.append(c);
                        state = ATTRIBUTE_NAME;
                    }
                    break;
                case ATTRIBUTE_NAME:
                    if (c == '=') {
                        name = sb.toString().toLowerCase();
                        sb.setLength(0);
                        state = SPACE_AFTER_EQ;
                    } else if (Character.isWhitespace(c)) {
                        name = sb.toString().toLowerCase();
                        sb.setLength(0);
                        state = SPACE_BEFORE_EQ;
                    } else
                        sb.append(c);
                    break;
                case SPACE_BEFORE_EQ:
                    if (Character.isWhitespace(c))
                        ;
                    else if (c == '=')
                        state = SPACE_AFTER_EQ;
                    else {
                        // An attribute with no value.
                        sb.setLength(0);
                        state = NEUTRAL;
                        if (attributes == null)
                            attributes = new ArrayList();
                        attributes.add(new StringPair(name, ""));
                        name = value = null;
                    }
                    break;
                case SPACE_AFTER_EQ:
                    if (Character.isWhitespace(c))
                        ;
                    else if ( c == '"' || c == '\'') {
                        delim = c;
                        sb.setLength(0);
                        state = ATTRIBUTE_VALUE;
                    } else {
                        delim = 0;
                        sb.setLength(0);
                        sb.append(c);
                        state = ATTRIBUTE_VALUE;
                    }
                    break;
                case ATTRIBUTE_VALUE:
                    if (delim != 0) {
                        if (c == delim) {
                            value = sb.toString();
                            sb.setLength(0);
                            state = NEUTRAL;
                            if (attributes == null)
                                attributes = new ArrayList();
                            attributes.add(new StringPair(name, value));
                            name = value = null;
                        } else if (c == '&') {
                            FastStringBuffer sbEntity = new FastStringBuffer();
                            sbEntity.append('&');
                            for (++i; i < limit; i++) {
                                c = tag.charAt(i);
                                if (c == delim) {
                                    // Not really an entity.
                                    sb.append(sbEntity.toString());
                                    // Let outer loop handle the delimiter.
                                    --i;
                                    break;
                                }
                                sbEntity.append(c);
                                if (c == ';') {
                                    sb.append(substituteEntity(sbEntity.toString()));
                                    break;
                                }
                            }
                        } else
                            sb.append(c);
                    } else {
                        // Attribute value is not enclosed in quotes.
                        if (c == '>' || Character.isWhitespace(c)) {
                            value = sb.toString();
                            sb.setLength(0);
                            state = NEUTRAL;
                            if (attributes == null)
                                attributes = new ArrayList();
                            attributes.add(new StringPair(name, value));
                            name = value = null;
                        } else if (c == '&') {
                            FastStringBuffer sbEntity = new FastStringBuffer();
                            sbEntity.append('&');
                            for (++i; i < limit; i++) {
                                c = tag.charAt(i);
                                if (c == ' ' || c == '>') {
                                    // Reached end of attribute. Back up one char.
                                    --i;
                                    // We've already got the whole entity (if it is one).
                                    break;
                                }
                                sbEntity.append(c);
                                if (c == ';')
                                    break;
                            }
                            sb.append(substituteEntity(sbEntity.toString()));
                        } else
                            sb.append(c);
                    }
                    break;
            }
        }

        return attributes;
    }

    // tagName can be e.g. "table" or "/table".
    private static boolean isTag(String s, String tagName)
    {
        Debug.assertTrue(tagName.indexOf('<') < 0);
        Debug.assertTrue(tagName.indexOf('>') < 0);
        Debug.assertTrue(tagName.indexOf(' ') < 0);

        // Shortest possible tag is "<a>".
        if (s == null || s.length() < 3)
            return false;
        if (s.charAt(0) != '<')
            return false;
        int length = tagName.length();
        if (s.length() < length + 2)
            return false;
        if (!s.regionMatches(true, 1, tagName, 0, length))
            return false;
        // Char after tag name must be whitespace or '>'.
        char c = s.charAt(length + 1);
        return c == '>' || Character.isWhitespace(c);
    }

    private String gatherTag()
    {
        final int TAG_NAME        = 0;
        final int NEUTRAL         = 1;
        final int ATTRIBUTE_NAME  = 2;
        final int SPACE_BEFORE_EQ = 3;
        final int SPACE_AFTER_EQ  = 4;
        final int ATTRIBUTE_VALUE = 5;
        final int MARKED_SECTION  = 6;
        final int BANG            = 7;
        final int INVALID         = 8;

        FastStringBuffer sb = new FastStringBuffer(256);
        sb.append('<');
        int length = 1;
        int state = TAG_NAME;
        char delim = 0;

        int ch;

        try {
            while ((ch = reader.read()) >= 0) {
                char c = (char) ch;
                if (c == '<') {
                    // We only expect to see a '<' inside a quoted attribute value.
                    // An actual example from msnbc.com: <a href="<!--none-->">
                    if (state != ATTRIBUTE_VALUE || delim == 0) {
                        Log.error("unexpected '<' sourceOffset = " + sourceOffset);
                        reader.unread(c);
                        return sb.toString();
                    }
                }
                if (c != '\r')
                    ++sourceOffset;
                // Ignore whitespace after initial "<" or "</".
                if (c <= ' ') {
                    if (length == 1)
                        continue;
                    if (length == 2 && sb.charAt(1) == '/')
                        continue;
                }
                sb.append(c);
                ++length;
                switch (state) {
                    case TAG_NAME:
                        if (c == '>') {
                            // End of tag, no attributes.
                            return sb.toString();
                        } else if (Character.isWhitespace(c)) {
                            // Reached end of tag name.
                            state = NEUTRAL;
                        } else if (length == 2 && c == '!') {
                            state = BANG;
                        } else if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == ':') {
                            ; // OK at any time
                        } else if (length == 2 && (c == '/' || c == '!')) {
                            ; // OK as second char only
                        } else if (length > 2 && ((c >= '0' && c <= '9') || c == '-' || c == '.')) {
                            ; // OK second char or later
                        } else {
                            // Not really a tag.
                            Log.error("invalid tag sourceOffset = " + sourceOffset);
                            state = INVALID;
                        }
                        break;
                    case BANG:
                        if (c == '>') {
                            return sb.toString();
                        } else if (length == 4 && sb.toString().equals("<!--")) {
                            // Start of comment.
                            return sb.toString();
                        } else if (length == 3 && sb.toString().equals("<![")) {
                            state = MARKED_SECTION;
                        }
                        break;
                    case NEUTRAL:
                        if (c == '>')
                            return sb.toString();
                        else if (!Character.isWhitespace(c))
                            state = ATTRIBUTE_NAME;
                        break;
                    case ATTRIBUTE_NAME:
                        if (c == '>')
                            return sb.toString();
                        else if (c == '=')
                            state = SPACE_AFTER_EQ;
                        else if (Character.isWhitespace(c))
                            state = SPACE_BEFORE_EQ;
                        break;
                    case SPACE_BEFORE_EQ:
                        if (c == '>')
                            return sb.toString();
                        else if (Character.isWhitespace(c))
                            ;
                        else if (c == '=')
                            state = SPACE_AFTER_EQ;
                        else {
                            // An attribute with no value.
                            state = NEUTRAL;
                        }
                        break;
                    case SPACE_AFTER_EQ:
                        if (c == '>')
                            return sb.toString();
                        else if (Character.isWhitespace(c))
                            ;
                        else if ( c == '"' || c == '\'') {
                            delim = c;
                            state = ATTRIBUTE_VALUE;
                        } else {
                            delim = 0;
                            state = ATTRIBUTE_VALUE;
                        }
                        break;
                    case ATTRIBUTE_VALUE:
                        if (delim != 0) {
                            if (c == delim)
                                state = NEUTRAL;
                        } else {
                            // Attribute value is not enclosed in quotes.
                            if (c == '>')
                                return sb.toString();
                            else if (Character.isWhitespace(c))
                                state = NEUTRAL;
                        }
                        break;
                    case MARKED_SECTION:
                        if (c == '>') {
                            if (sb.toString().endsWith("]>"))
                                return sb.toString();
                        }
                        break;
                    case INVALID:
                        if (c == '>') {
                            Log.error("invalid tag |" + sb.toString() +
                                "| sourceOffset = " + sourceOffset);
                            return sb.toString();
                        }
                        break;
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }

        return sb.toString();
    }

    private void processEntity()
    {
        String entity = gatherEntity();
        doText(substituteEntity(entity));
    }

    private String gatherEntity()
    {
        FastStringBuffer sb = new FastStringBuffer('&');
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c == '<' || c == '&') {
                    reader.unread(c);
                    break;
                }
                if (c != '\r')
                    ++sourceOffset;
                sb.append((char) c);
                if (c == ';')
                    break;
                if (c == ' ')
                    break;
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        return sb.toString();
    }

    private static String substituteEntity(String entity)
    {
        final int length = entity.length();
        if (length < 2)
            return entity;
        if (entity.equals("& "))
            return entity; // Not really an entity.
        if (entity.charAt(1) == '#') {
            // Remove leading "&#" and trailing ';' if present.
            String s;
            if (entity.charAt(length - 1) == ';')
                s = entity.substring(2, length - 1);
            else
                s = entity.substring(2);

            int n = -1;
            try {
                n = Integer.parseInt(s);
            }
            catch (NumberFormatException e) {}

            if (n >= 0) {
                switch (n) {
                    case 145: // Left single quote.
                    case 146: // Right single quote.
                        return "'";
                    case 147: // Left double quote.
                    case 148: // Right double quote.
                        return "\"";
                    case 149: // Bullet.
                        return String.valueOf((char)8226);
                    case 150: // En dash.
                        return "-";
                    case 151: // Em dash.
                        return "--";
                    case 153:
                        return "(TM)";
                    case 174:
                        return "(R)";
                    default:
                        return String.valueOf((char)n);
                }
            }
        }

        // Remove leading '&' and trailing ';' if present.
        String s;
        if (entity.charAt(length - 1) == ';')
            s = entity.substring(1, length-1).intern();
        else
            s = entity.substring(1).intern();

        if (s == "quot")
            return "\"";
        else if (s == "trade") // 153
            return "(TM)";
        else if (s == "nbsp")
            return String.valueOf((char)160);
        else if (s == "copy")
            return String.valueOf((char)169);
        else if (s == "laquo")
            return String.valueOf((char)171);
        else if (s == "reg") // 174
            return "(R)";
        else if (s == "acute")
            return String.valueOf((char)180);
        else if (s == "auml")
            return String.valueOf((char)228);
        else if (s == "middot")
            return String.valueOf((char)183);
        else if (s == "raquo")
            return String.valueOf((char)187);
        else if (s == "eacute")
            return String.valueOf((char)233);
        else if (s == "iuml")
            return String.valueOf((char)239);
        else if (s == "mdash")
            return String.valueOf((char)8212);
        else if (s == "ldquo")
            return String.valueOf((char)8220);
        else if (s == "rdquo")
            return String.valueOf((char)8221);
        else if (s == "bull")
            return String.valueOf((char)8226);
        else if (s == "AElig")
            return "AE";
        else if (s == "amp")
            return "&";
        else if (s == "lt")
            return "<";
        else if (s == "gt")
            return ">";
        else
            return entity;
    }

    private void skipComment()
    {
        FastStringBuffer sb = new FastStringBuffer();
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c != '\r')
                    ++sourceOffset;
                sb.append((char) c);
                if (c == '>' && sb.toString().endsWith("-->"))
                    return;
            }
        }
        catch (IOException e){
            Log.error(e);
        }
    }

    private void skipTag(String tagName)
    {
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c != '\r')
                    ++sourceOffset;
                if (c == '<') {
                    String tag = gatherTag();
                    if (isTag(tag, tagName))
                        return;
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private void skipScript()
    {
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c != '\r')
                    ++sourceOffset;
                if (c == '<') {
                    if (readEndScriptTag())
                        return;
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private boolean readEndScriptTag()
    {
        final String s = "</script>";
        final int length = s.length();
        FastStringBuffer sb = new FastStringBuffer('<');
        try {
            int c;
            while ((c = reader.read()) >= 0) {
                if (c != '\r')
                    ++sourceOffset;
                sb.append(Character.toLowerCase((char)c));
                if (sb.length() < length) {
                    if (!s.startsWith(sb.toString()))
                        return false;
                } else
                    return s.equals(sb.toString());
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        return false;
    }

    private void doText(String s)
    {
        final int length = s.length();
        for (int i = 0; i < length; i++)
            doChar(s.charAt(i));
    }

    private void doChar(char c)
    {
        if (preformatted) {
            switch (c) {
                case '\t':
                    final int spaces = 8 - getCurrentOffset() % 8;
                    for (int i = spaces-1; i >= 0; i--)
                        textBuffer.append(' ');
                    break;
                case '\r':
                    break;
                case '\n':
                    flushSegment();
                    if (segments != null) {
                        lines.appendLine(new WebLine(segments, sourceOffset));
                        segments = null;
                    } else
                        lines.appendLine(new WebLine(sourceOffset));
                    ++offset; // Line separator always counts as 1.
                    break;
                default:
                    textBuffer.append(c);
                    break;
            }
            return;
        }

        switch (c) {
            case 133: // Ellipsis.
                textBuffer.append("...");
                break;
            case 145: // Left single quote.
            case 146: // Right single quote.
                textBuffer.append('\'');
                break;
            case 147: // Left double quote.
            case 148: // Right double quote.
                textBuffer.append('"');
                break;
            case 149: // Bullet.
                textBuffer.append((char)8226);
                break;
            case 150:
                // En dash.
                textBuffer.append('-');
                break;
            case 151:
                // Em dash.
                textBuffer.append("--");
                break;
            case 153:
                textBuffer.append("(TM)");
                break;
            case '\n':
            case '\t':
            case ' ':
                // Append a space unless the preceding character was a space
                // or non-breaking space.
                if (textBuffer.length() > 0) {
                    char preceding = textBuffer.charAt(textBuffer.length() - 1);
                    if (preceding != ' ' && preceding != 160)
                        textBuffer.append(' ');
                } else if (segments != null && segments.size() > 0) {
                    // Check the last character in the previous segment.
                    HtmlLineSegment seg = (HtmlLineSegment) segments.getLastSegment();
                    String s = seg.getText();
                    if (s.length() == 0)
                        textBuffer.append(' ');
                    else {
                        char preceding = s.charAt(s.length() - 1);
                        if (preceding != ' ' && preceding != 160)
                            textBuffer.append(' ');
                    }
                }
                break;
            case '\r':
                break;
            default:
                // A non-whitespace character.
                // Indent if we're at the beginning of the line.
                maybeIndent();
                textBuffer.append(c);
                break;
        }

        if (Character.isWhitespace(c))
            maybeWrap();
    }

    private void maybeIndent()
    {
        if (indentLevel > 0) {
            if (segments == null && textBuffer.length() == 0) {
                textBuffer.append(Utilities.spaces(getIndent()));
                flushSegment(null, FORMAT_WHITESPACE);
            }
        }
    }

    private final int getIndent()
    {
        return indentLevel * 4;
    }

    private int getCurrentOffset()
    {
        int currentOffset = 0;
        if (segments != null) {
            for (int i = segments.size()-1; i >= 0; i--)
                currentOffset += segments.getSegment(i).length();
        }
        currentOffset += textBuffer.length();
        return currentOffset;
    }

    private final void flushSegment()
    {
        flushSegment(true);
    }

    private void flushSegment(boolean wrap)
    {
        if (textBuffer.length() > 0) {
            if (wrap)
                maybeWrap();
            int format = 0;
            if (link != null)
                format |= FORMAT_LINK;
            if (bold || strong || heading)
                format |= FORMAT_BOLD;
            if (italic || emphasis)
                format |= FORMAT_ITALIC;
            if (whitespace)
                format |= FORMAT_WHITESPACE;
            if (segments == null)
                segments = new LineSegmentList();
            segments.addSegment(new HtmlLineSegment(textBuffer.toString(), format, link));
            offset += textBuffer.length();
            textBuffer.setLength(0);
        }
    }

    private void flushSegment(Link link, int format)
    {
        if (textBuffer.length() > 0) {
            if (segments == null)
                segments = new LineSegmentList();
            segments.addSegment(new HtmlLineSegment(textBuffer.toString(), format, link));
            offset += textBuffer.length();
            textBuffer.setLength(0);
        }
    }

    private void maybeWrap()
    {
        if (preformatted)
            return;
        int currentOffset = getCurrentOffset();
        if (currentOffset > maxChars()) {
            int length = textBuffer.length();

            // Cumulative length of preceding segments.
            int preceding = currentOffset - length;

            final String text = textBuffer.toString();
            int index = text.lastIndexOf(' ');
            while (index >= 0 && preceding + index > maxChars())
                index = text.lastIndexOf(' ', index - 1);

            if (index >= 0) {
                // Found a suitable break.
                String remainder = text.substring(index + 1);
                textBuffer.setLength(index); // Trims trailing space.
                flushSegment(false); // No wrap!
                if (segments != null) {
                    lines.appendLine(new WebLine(segments, sourceOffset));
                    ++offset; // Line separator always counts as 1.
                    segments = null;
                }
                maybeIndent();
                textBuffer.append(remainder);
            } else {
                // No suitable break in text buffer.
                textBuffer.setLength(0);
                if (segments != null) {
                    final int last = segments.size() - 1;
                    if (last >= 0) {
                        final HtmlLineSegment lastSegment = (HtmlLineSegment) segments.getSegment(last);
                        final String segmentText = lastSegment.getText();
                        index = segmentText.lastIndexOf(' ');
                        if (index >= 0) {
                            // Found a break.
                            final String head = segmentText.substring(0, index);
                            final String tail = segmentText.substring(index + 1);

                            // We're removing a trailing space. Adjust offset
                            // accordingly.
                            --offset;

                            final int format = lastSegment.getFormat();
                            final Link link = lastSegment.getLink();

                            segments.setSegment(last, new HtmlLineSegment(head, format, link));
                            lines.appendLine(new WebLine(segments, sourceOffset));

                            // Line separator always counts as 1.
                            ++offset;

                            segments = null;
                            if (tail.length() > 0) {
                                maybeIndent();
                                if (segments == null)
                                    segments = new LineSegmentList();
                                segments.addSegment(new HtmlLineSegment(tail, format, link));
                            }
                        } else {
                            // No break. Move last segment to current line.
                            segments.removeSegment(lastSegment);
                            lines.appendLine(new WebLine(segments, sourceOffset));

                            // Line separator always counts as 1.
                            ++offset;

                            segments = null;
                            maybeIndent();
                            if (segments == null)
                                segments = new LineSegmentList();
                            segments.addSegment(lastSegment);
                        }
                    }
                }

                maybeIndent();
                textBuffer.append(text);
                flushSegment(false); // No wrap!
            }
        }
    }

    // Returns true if it does anything.
    private boolean flushLine()
    {
        flushSegment();
        if (centered() && currentTable == null && segments != null) {
            int length = getCurrentOffset();
            if (maxChars() > length) {
                int numSpaces = (maxChars() - length) / 2;
                if (numSpaces > 0) {
                    segments.addSegment(0, new HtmlLineSegment(Utilities.spaces(numSpaces),
                                                               FORMAT_WHITESPACE, null));
                    offset += numSpaces;
                }
            }
        }
        if (segments != null) {
            lines.appendLine(new WebLine(segments, sourceOffset));
            ++offset; // Line separator always counts as 1.
            segments = null;
            return true;
        } else
            return false;
    }

    private void newLine()
    {
        flushLine();
        Line lastLine = lines.getLastLine();
        if (lastLine != null && lastLine.length() > 0 && !lastLine.isBlank()) {
            lines.appendLine(new WebLine(sourceOffset));
            ++offset;
        }
    }

    private final int maxChars()
    {
//         if (maxChars == 0) {
//             // We have to be careful here because this might get called before
//             // the display is initialized if we're opening a file on the
//             // command line.
//             Display display = Editor.currentEditor().getDisplay();
//             int displayWidth = display.getWidth();
//             if (displayWidth > 0) {
//                 int charWidth = display.getCharWidth();
//                 if (charWidth > 0)
//                     maxChars = display.getWidth() / charWidth - 2;
//             }
//             if (maxChars <= 0)
//                 maxChars = 80;
//         }
        Debug.assertTrue(maxChars == 80);
        return maxChars;
    }

    private static class EncodingChangeException extends Exception
    {
        private String newEncoding;

        EncodingChangeException(String newEncoding)
        {
            this.newEncoding = newEncoding;
        }

        String getNewEncoding()
        {
            return newEncoding;
        }
    }
}
