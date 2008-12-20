/*
 * MakefileFormatter.java
 *
 * Copyright (C) 2000-2006 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;

public final class MakefileFormatter extends Formatter
{
  private static final int MAKEFILE_FORMAT_TEXT    = 0;
  private static final int MAKEFILE_FORMAT_COMMENT = 1;
  private static final int MAKEFILE_FORMAT_STRING  = 2;
  private static final int MAKEFILE_FORMAT_KEYWORD = 3;
  private static final int MAKEFILE_FORMAT_TARGET  = 4;

  private static final int  STATE_BACKQUOTE = STATE_LAST + 1;

  private FastStringBuffer sb = new FastStringBuffer();
  private int tokStart;

  private static final RE targetRE = new UncheckedRE("^\\S+.*:");
  private static final RE assignmentRE = new UncheckedRE("^\\S+\\s*:?=");
  private static final RE conditionalRE =
    new UncheckedRE("^( *ifn?(eq|def)\\s)|^( *else\\s+(ifn?(eq|def))?)|^( *else\\s*)|^( *endif\\s*)");

  public MakefileFormatter(Buffer buffer)
  {
    this.buffer = buffer;
  }

  private void endToken(int state)
  {
    if (sb.length() > 0) 
      {
        int format;
        switch (state) 
          {
          case STATE_QUOTE:
          case STATE_SINGLEQUOTE:
          case STATE_BACKQUOTE:
            format = MAKEFILE_FORMAT_STRING;
            break;
          case STATE_COMMENT:
            format = MAKEFILE_FORMAT_COMMENT;
            break;
          default:
            format = MAKEFILE_FORMAT_TEXT;
            break;
          }
        addSegment(sb.toString(), format);
        tokStart += sb.length();
        sb.setLength(0);
      }
  }

  private void addToken(String s, int format)
  {
    int length = s.length();
    if (length > 0) 
      {
        addSegment(s, format);
        tokStart += length;
      }
  }

  private void parseLine(String text, int state)
  {
    if (Editor.tabsAreVisible())
      text = Utilities.makeTabsVisible(text, buffer.getTabWidth());
    else
      text = Utilities.detab(text, buffer.getTabWidth());
    clearSegmentList();
    int braceCount = 0;
    sb.setLength(0);
    int i = 0;
    tokStart = 0;
    if (text.trim().startsWith("#")) 
      {
        addToken(text, MAKEFILE_FORMAT_COMMENT);
        return;
      }
    // Try some regexps at the beginning of the line.
    REMatch match = conditionalRE.getMatch(text);
    if (match != null) 
      {
        addToken(match.toString(), MAKEFILE_FORMAT_KEYWORD);
        i += match.toString().length();
      } 
    else 
      {
        match = assignmentRE.getMatch(text);
        if (match != null) 
          {
            addToken(match.toString(), MAKEFILE_FORMAT_TEXT);
            i += match.toString().length();
          } 
        else 
          {
            match = targetRE.getMatch(text);
            if (match != null) 
              {
                addToken(match.toString(), MAKEFILE_FORMAT_TARGET);
                i += match.toString().length();
              }
          }
      }
    final int limit = text.length();
    // Skip whitespace at start of line.
    while (i < limit) 
      {
        char c = text.charAt(i);
        if (Character.isWhitespace(c)) 
          {
            sb.append(c);
            ++i;
          } 
        else 
          {
            endToken(state);
            break;
          }
      }
    while (i < limit) 
      {
        char c = text.charAt(i);
        if (state == STATE_QUOTE) 
          {
            sb.append(c);
            if (c == '"') 
              {
                endToken(state);
                state = STATE_NEUTRAL;
              } 
            else if (c == '\\' && i < limit-1) 
              {
                // Escape char.
                sb.append(text.charAt(++i));
              }
            ++i;
            continue;
          }
        if (state == STATE_SINGLEQUOTE) 
          {
            sb.append(c);
            if (c == '\'') 
              {
                endToken(state);
                state = STATE_NEUTRAL;
              }
            ++i;
            continue;
          }
        if (state == STATE_BACKQUOTE) 
          {
            sb.append(c);
            if (c == '`') 
              {
                endToken(state);
                state = STATE_NEUTRAL;
              }
            ++i;
            continue;
          }
        // Reaching here, we're not in a quoted string.
        if (c == '"') 
          {
            endToken(state);
            sb.append(c);
            state = STATE_QUOTE;
            ++i;
            continue;
          }
        if (c == '\'') 
          {
            endToken(state);
            sb.append(c);
            state = STATE_SINGLEQUOTE;
            ++i;
            continue;
          }
        if (c == '`') 
          {
            endToken(state);
            sb.append(c);
            state = STATE_BACKQUOTE;
            ++i;
            continue;
          }
        if (c == '#') 
          {
            endToken(state);
            state = STATE_COMMENT;
            sb.append(text.substring(i));
            endToken(state);
            return;
          }
        if (state == STATE_IDENTIFIER) 
          {
            if (buffer.mode.isIdentifierPart(c))
              sb.append(c);
            else 
              {
                endToken(state);
                sb.append(c);
                state = STATE_NEUTRAL;
              }
            ++i;
            continue;
          }
        if (state == STATE_NUMBER) 
          {
            if (Character.isDigit(c))
              sb.append(c);
            else 
              {
                endToken(state);
                sb.append(c);
                if (buffer.mode.isIdentifierStart(c))
                  state = STATE_IDENTIFIER;
                else
                  state = STATE_NEUTRAL;
              }
            ++i;
            continue;
          }
        if (state == STATE_NEUTRAL) 
          {
            if (buffer.mode.isIdentifierStart(c)) 
              {
                endToken(state);
                sb.append(c);
                state = STATE_IDENTIFIER;
              } 
            else if (Character.isDigit(c)) 
              {
                endToken(state);
                sb.append(c);
                state = STATE_NUMBER;
              } 
            else // Still neutral...
              sb.append(c);
          }
        ++i;
      }
    endToken(state);
  }

  public LineSegmentList formatLine(Line line)
  {
    if (line == null) 
      {
        clearSegmentList();
        addSegment("", MAKEFILE_FORMAT_TEXT);
        return segmentList;
      }
    parseLine(line.getText(), line.flags());
    for (int i = 0; i < segmentList.size(); i++) 
      {
        LineSegment segment = segmentList.getSegment(i);
        if (segment.getFormat() > 0)
          continue;
        String token = segment.getText();
        if (isKeyword(token))
          segment.setFormat(MAKEFILE_FORMAT_KEYWORD);
        else
          segment.setFormat(MAKEFILE_FORMAT_TEXT);
      }
    return segmentList;
  }

  public FormatTable getFormatTable()
  {
    if (formatTable == null) 
      {
        formatTable = new FormatTable("MakefileMode");
        formatTable.addEntryFromPrefs(MAKEFILE_FORMAT_TEXT, "text");
        formatTable.addEntryFromPrefs(MAKEFILE_FORMAT_COMMENT, "comment");
        formatTable.addEntryFromPrefs(MAKEFILE_FORMAT_STRING, "string");
        formatTable.addEntryFromPrefs(MAKEFILE_FORMAT_KEYWORD, "keyword");
        formatTable.addEntryFromPrefs(MAKEFILE_FORMAT_TARGET, "target");
      }
    return formatTable;
  }
}
