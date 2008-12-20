/*
 * HtmlFormatter.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

public final class HtmlFormatter extends Formatter implements Constants
{
    // HTML formats must not overlap with Java formats!
    private static final int HTML_FORMAT_FIRST = JavaFormatter.JAVA_FORMAT_LAST + 1;

    private static final int HTML_FORMAT_TEXT           = HTML_FORMAT_FIRST;
    private static final int HTML_FORMAT_COMMENT        = HTML_FORMAT_FIRST + 1;
    private static final int HTML_FORMAT_TAG            = HTML_FORMAT_FIRST + 2;
    private static final int HTML_FORMAT_TAG_IMAGE      = HTML_FORMAT_FIRST + 3;
    private static final int HTML_FORMAT_TAG_ANCHOR     = HTML_FORMAT_FIRST + 4;
    private static final int HTML_FORMAT_TAG_TABLE      = HTML_FORMAT_FIRST + 5;
    private static final int HTML_FORMAT_TAG_TABLE_ROW  = HTML_FORMAT_FIRST + 6;
    private static final int HTML_FORMAT_TAG_TABLE_DATA = HTML_FORMAT_FIRST + 7;
    private static final int HTML_FORMAT_SCRIPT         = HTML_FORMAT_FIRST + 8;

    private StringBuffer sb = new StringBuffer();

    private JavaFormatter javaFormatter;

    public HtmlFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        javaFormatter = new JavaFormatter(buffer, LANGUAGE_JAVASCRIPT);
    }

    private void endToken(int state)
    {
        if (sb.length() > 0) {
            int format = HTML_FORMAT_TEXT;
            switch (state) {
                case STATE_NEUTRAL:
                    break;
                case STATE_TAG:
                    format = HTML_FORMAT_TAG;
                    break;
                case STATE_HTML_COMMENT:
                    format = HTML_FORMAT_COMMENT;
                    break;
                case STATE_SCRIPT:
                    format = HTML_FORMAT_SCRIPT;
                    break;
                default:
                    break;
            }
            addSegment(sb.toString(), format);
            sb.setLength(0);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        if (line == null) {
            clearSegmentList();
            addSegment("", HTML_FORMAT_TEXT);
            return segmentList;
        }
        final int flags = line.flags();
        if (flags == STATE_SCRIPT || flags == STATE_COMMENT) {
            final String trim = line.trim();
            if (trim.startsWith("<!--")) {
                clearSegmentList();
                addSegment(line.getText(), HTML_FORMAT_COMMENT);
                return segmentList;
            }
            if (!trim.regionMatches(true, 0, "</script>", 0, 9))
                return javaFormatter.formatLine(line);
        }
        parseLine(line.getText(), flags);
        for (int i = 0; i < segmentList.size(); i++) {
            LineSegment segment = segmentList.getSegment(i);
            if (segment.getFormat() != HTML_FORMAT_TAG)
                continue;
            String token = segment.getText().toLowerCase();
            if (token.startsWith("<a ") || token.equals("<a>")|| token.equals("</a>"))
                segment.setFormat(HTML_FORMAT_TAG_ANCHOR);
            else if (token.startsWith("<img ") || token.equals("<img>"))
                segment.setFormat(HTML_FORMAT_TAG_IMAGE);
            else if (token.startsWith("<table ") || token.equals("<table>") || token.equals("</table>"))
                segment.setFormat(HTML_FORMAT_TAG_TABLE);
            else if (token.startsWith("<tr ") || token.equals("<tr>") || token.equals("</tr>"))
                segment.setFormat(HTML_FORMAT_TAG_TABLE_ROW);
            else if (token.startsWith("<td ") || token.equals("<td>") || token.equals("</td>"))
                segment.setFormat(HTML_FORMAT_TAG_TABLE_DATA);
        }
        return segmentList;
    }

    private void parseLine(String text, int state)
    {
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(text, buffer.getTabWidth());
        else
            text = Utilities.detab(text, buffer.getTabWidth());
        clearSegmentList();
        sb.setLength(0);
        int i = 0;
        final int limit = text.length();
        while (i < limit) {
            char c = text.charAt(i);
            if (state == STATE_HTML_COMMENT) {
                if (i < limit-2 && text.substring(i, i+3).equals("-->")) {
                    sb.append("-->");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 3;
                } else {
                    sb.append(c);
                    ++i;
                }
                continue;
            }
            if (state == STATE_TAG) {
                if (c == '>') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_SCRIPT) {
                if (c == '<') {
                    if (text.regionMatches(true, i, "</script>", 0, 4)) {
                        endToken(state);
                        state = STATE_TAG;
                        sb.append(text.substring(i, i+9));
                        endToken(state);
                        state = STATE_NEUTRAL;
                        i += 9;
                        continue;
                    }
                }
                sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_SCRIPT_TAG) {
                if (c == '>') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_SCRIPT;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            // Not in comment or tag.
            if (c == '<') {
                endToken(state);
                if (text.regionMatches(i, "<!--", 0, 4)) {
                    state = STATE_HTML_COMMENT;
                    sb.append("<!--");
                    i += 4;
                    continue;
                }
                sb.append(c);
                state = STATE_TAG;
            } else
                sb.append(c);
            ++i;
        }
        endToken(state);
    }

    public boolean parseBuffer()
    {
        Line line = buffer.getFirstLine();
        if (line == null)
            return false;
        Position pos = new Position(line, 0);
        boolean changed = false;
        int state = STATE_NEUTRAL;
        while (line != null) {
            int oldflags = line.flags();
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (state == STATE_HTML_COMMENT) {
                    if (c == '-') {
                        pos.moveTo(line, i);
                        if (pos.lookingAt("-->")) {
                            state = STATE_NEUTRAL;
                            i += 2;
                            continue;
                        }
                    }
                    continue;
                }
                if (state == STATE_SCRIPT_TAG) {
                    if (c == '>') {
                        state = STATE_SCRIPT;
                        continue;
                    }
                }
                if (state == STATE_TAG) {
                    if (c == '>') {
                        state = STATE_NEUTRAL;
                        continue;
                    }
                }
                if (state == STATE_SCRIPT) {
                    if (c == '<') {
                        pos.moveTo(line, i);
                        if (pos.lookingAtIgnoreCase("</script>")) {
                            state = STATE_NEUTRAL;
                            i += 8;
                        }
                    } else if (c == '/' && i < limit-1) {
                        c = line.charAt(i+1);
                        if (c == '*') {
                            pos.moveTo(line, i);
                            state = STATE_COMMENT;
                            ++i;
                        }
                    }
                    continue;
                }
                if (state == STATE_COMMENT) {
                    if (c == '*' && i < limit-1) {
                        c = line.charAt(i+1);
                        if (c == '/') {
                            pos.moveTo(line, i);
                            state = STATE_SCRIPT;
                            ++i;
                        }
                    }
                    continue;
                }
                // Neutral state.
                if (c == '<') {
                    pos.moveTo(line, i);
                    if (pos.lookingAt("<!--")) {
                        state = STATE_HTML_COMMENT;
                        i += 3;
                        continue;
                    }
                    if (pos.lookingAtIgnoreCase("<script>")) {
                        state = STATE_SCRIPT;
                        i += 7;
                        continue;
                    }
                    if (pos.lookingAtIgnoreCase("<script ")) {
                        state = STATE_SCRIPT_TAG;
                        i += 7;
                        continue;
                    }
                    state = STATE_TAG;
                    continue;
                }
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = javaFormatter.getFormatTable();
            formatTable.setModeName("HtmlMode");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(HTML_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG, "tag");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG_IMAGE, "image");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG_ANCHOR, "anchor");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG_TABLE, "table");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG_TABLE_ROW, "tableRow");
            formatTable.addEntryFromPrefs(HTML_FORMAT_TAG_TABLE_DATA, "tableData");
            formatTable.addEntryFromPrefs(HTML_FORMAT_SCRIPT, "script");
        }
        return formatTable;
    }

    public void reset()
    {
        javaFormatter.reset();
        super.reset();
    }
}
