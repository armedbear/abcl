/*
 * Search.java
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

import gnu.regexp.RE;
import gnu.regexp.REException;
import gnu.regexp.REMatch;

public class Search implements Cloneable
{
    private String pattern;
    private String lowerCasePattern;
    private int patternLength;

    private RE re;
    private REMatch match;

    private boolean ignoreCase;
    private boolean wholeWordsOnly;
    private boolean regularExpression;
    private boolean isMultilinePattern;
    private boolean restrictToSelection;

    private Region region;

    public Search()
    {
        setPattern(new String());
    }

    public Search(String pattern, boolean ignoreCase, boolean wholeWordsOnly)
    {
        setPattern(pattern);
        this.ignoreCase = ignoreCase;
        this.wholeWordsOnly = wholeWordsOnly;
    }

    public final String getPattern()
    {
        return pattern;
    }

    public final void setPattern(String s)
    {
        if (s != null) {
            pattern = s;
            lowerCasePattern = s.toLowerCase();
            patternLength = s.length();
        } else {
            pattern = lowerCasePattern = "";
            patternLength = 0;
        }
    }

    public final void appendCharToPattern(char c)
    {
        pattern += c;
        lowerCasePattern = pattern.toLowerCase();
        ++patternLength;
    }

    public final int getPatternLength()
    {
        return patternLength;
    }

    public final String getLowerCasePattern()
    {
        return lowerCasePattern;
    }

    public final RE getRE()
    {
        return re;
    }

    public final void setRE(RE re)
    {
        this.re = re;
    }

    public final REMatch getMatch()
    {
        return match;
    }

    public final boolean ignoreCase()
    {
        return ignoreCase;
    }

    public final void setIgnoreCase(boolean b)
    {
        ignoreCase = b;
    }

    public final boolean wholeWordsOnly()
    {
        return wholeWordsOnly;
    }

    public final void setWholeWordsOnly(boolean b)
    {
        wholeWordsOnly = b;
    }

    public final boolean isRegularExpression()
    {
        return regularExpression;
    }

    public final void setRegularExpression(boolean b)
    {
        regularExpression = b;
    }

    public final boolean isMultilinePattern()
    {
        return isMultilinePattern;
    }

    public final void setMultiline(boolean b)
    {
        isMultilinePattern = b;
    }

    public final boolean restrictToSelection()
    {
        return restrictToSelection;
    }

    public final void setRestrictToSelection(boolean b)
    {
        restrictToSelection = b;
    }

    public final Region getRegion()
    {
        return region;
    }

    public final void setRegion(Region r)
    {
        region = r;
    }

    protected Position findInBuffer(Buffer buffer)
    {
        return find(buffer, new Position(buffer.getFirstLine(), 0));
    }

    public final Position find(Buffer buffer, Position start)
    {
        return regularExpression ? findRegExp(buffer, start) : findString(buffer, start);
    }

    public final Position reverseFind(Buffer buffer, Position start)
    {
        if (regularExpression) {
            if (isMultilinePattern)
                return reverseFindMultilineRegExp(buffer, start);
            else
                return reverseFindRegExp(buffer, start);
        } else
            return reverseFindString(buffer, start);
    }

    public final Position find(Mode mode, Position start)
    {
        return regularExpression ? findRegExp(mode, start) : findString(mode, start);
    }

    public final Position findInLine(Mode mode, Position start)
    {
        if (regularExpression)
            return findRegExpInLine(mode, start.getLine(), start.getOffset(),
                start.getLineLength());
        else
            return findStringInLine(mode, start.getLine(), start.getOffset(),
                start.getLineLength());
    }

    public final boolean find(String s)
    {
        return regularExpression ? findRegExp(s) : findString(s);
    }

    public final boolean findDelimited(String s, Mode mode)
    {
        return regularExpression ? findRegExpDelimited(s, mode) : findStringDelimited(s, mode);
    }

    // Search is restricted to region if restrictToSelection is true and
    // region is not null.
    public final Position findString(Buffer buffer, Position start)
    {
        return findString(buffer.getMode(), start);
    }

    // Search is restricted to region if restrictToSelection is true and
    // region is not null.
    private Position findString(Mode mode, Position start)
    {
        Debug.assertTrue(lowerCasePattern.equals(pattern.toLowerCase()));
        Debug.assertTrue(patternLength == pattern.length());
        Line line = start.getLine();
        int begin = start.getOffset();
        Line endLine;
        if (restrictToSelection && region != null)
            endLine = region.getEndLine();
        else
            endLine = null;
        Position pos = null;
        while (line != endLine) {
            pos = findStringInLine(mode, line, begin, line.length());
            if (pos != null)
                return pos;
            line = line.next();
            begin = 0;
        }
        if (line != null) {
            // End line of region.
            pos = findStringInLine(mode, line, begin, region.getEnd().getOffset());
        }
        return pos;
    }

    // Region is ignored.
    public Position findString(Buffer buffer, Position start, boolean wrapBuffer)
    {
        Debug.assertTrue(lowerCasePattern.equals(pattern.toLowerCase()));
        Debug.assertTrue(patternLength == pattern.length());
        Mode mode = buffer.getMode();
        Line line = start.getLine();
        int begin = start.getOffset();
        while (line != null) {
            Position pos = findStringInLine(mode, line, begin, line.length());
            if (pos != null)
                return pos;
            line = line.next();
            begin = 0;
        }
        if (wrapBuffer) {
            line = buffer.getFirstLine();
            Line endLine = start.getNextLine();
            while (line != endLine) {
                Position pos = findStringInLine(mode, line, 0, line.length());
                if (pos != null)
                    return pos;
                line = line.next();
            }
        }
        return null;
    }

    private Position findStringInLine(Mode mode, Line line, int begin, int end)
    {
        String toBeSearched;
        if (end < line.length())
            toBeSearched = line.substring(0, end);
        else
            toBeSearched = line.getText();
        int index = begin;
        int limit = end - patternLength;
        while (index <= limit) {
            if (ignoreCase)
                index = toBeSearched.toLowerCase().indexOf(lowerCasePattern, index);
            else
                index = toBeSearched.indexOf(pattern, index);
            if (index < 0)
                break;
            Position pos = new Position(line, index);
            if (!wholeWordsOnly || Utilities.isDelimited(mode, pos, patternLength))
                return pos;
            ++index;
        }
        return null;
    }

    private boolean findString(String s)
    {
        Debug.assertTrue(patternLength == pattern.length());
        int index = 0;
        int limit = s.length() - patternLength;
        while (index <= limit) {
            if (ignoreCase)
                index = s.toLowerCase().indexOf(lowerCasePattern, index);
            else
                index = s.indexOf(pattern, index);
            if (index < 0)
                break;
            if (!wholeWordsOnly || Utilities.isDelimited(s, index, patternLength))
                return true;
            ++index;
        }
        return false;
    }

    private boolean findStringDelimited(String s, Mode mode)
    {
        Debug.assertTrue(wholeWordsOnly);
        Debug.assertTrue(patternLength == pattern.length());
        int index = 0;
        int limit = s.length() - patternLength;
        while (index <= limit) {
            if (ignoreCase)
                index = s.toLowerCase().indexOf(lowerCasePattern, index);
            else
                index = s.indexOf(pattern, index);
            if (index < 0)
                break;
            if (Utilities.isDelimited(s, index, patternLength, mode))
                return true;
            ++index;
        }
        return false;
    }

    // Region is ignored.
    public Position reverseFindString(Buffer buffer, Position start)
    {
        Debug.assertTrue(lowerCasePattern.equals(pattern.toLowerCase()));
        Debug.assertTrue(patternLength == pattern.length());
        Line line = start.getLine();
        Position pos = reverseFindStringInLine(buffer, line, 0, start.getOffset());
        if (pos != null)
            return pos;
        line = line.previous();
        while (line != null) {
            pos = reverseFindStringInLine(buffer, line, 0, line.length());
            if (pos != null)
                return pos;
            line = line.previous();
        }
        return null;
    }

    private Position reverseFindStringInLine(Buffer buffer, Line line, int begin, int end)
    {
        int index = end;
        while (index >= begin) {
            if (ignoreCase)
                index = line.getText().toLowerCase().lastIndexOf(lowerCasePattern, index);
            else
                index = line.getText().lastIndexOf(pattern, index);
            if (index < 0)
                break;
            Position pos = new Position(line, index);
            if (!wholeWordsOnly || Utilities.isDelimited(buffer, pos, patternLength))
                return pos;
            --index;
        }
        return null;
    }

    // Search is restricted to region if restrictToSelection is true and
    // region is not null.
    public final Position findRegExp(Buffer buffer, Position start)
    {
        if (isMultilinePattern)
            return findMultilineRegExp(buffer, start);
        else
            return findRegExp(buffer.getMode(), start);
    }

    public void setREFromPattern() throws REException
    {
        int cflags = 0;
        if (isMultilinePattern)
            cflags |= RE.REG_MULTILINE;
        if (ignoreCase)
            cflags |= RE.REG_ICASE;
        re = new RE(pattern, cflags);
    }

    // Search is restricted to region if restrictToSelection is true and
    // region is not null.
    private Position findMultilineRegExp(Buffer buffer, Position start)
    {
        if (re == null) {
            try {
                setREFromPattern();
            }
            catch (Throwable t) {
                Log.error(t);
                return null;
            }
        }
        final String s = buffer.getText();
        int startIndex = buffer.getAbsoluteOffset(start);
        int endIndex = -1;
        if (restrictToSelection && region != null)
            endIndex = buffer.getAbsoluteOffset(region.getEnd());
        while (true) {
            match = findMatch(s, startIndex, endIndex);
            if (match == null)
                return null;
            if (!wholeWordsOnly)
                break;
            if (Utilities.isDelimited(buffer.getMode(), s,
                match.getStartIndex(), match.getEndIndex()))
                break;
            startIndex = match.getStartIndex() + 1;
        }
        return buffer.getPosition(match.getStartIndex());
    }

    private Position reverseFindMultilineRegExp(Buffer buffer, Position start)
    {
        if (re == null) {
            try {
                setREFromPattern();
            }
            catch (Throwable t) {
                Log.error(t);
                return null;
            }
        }
        int startIndex = 0;
        int endIndex = buffer.getAbsoluteOffset(start);
        final String s = buffer.getText().substring(0, endIndex);
        REMatch lastMatch = null;
        while (true) {
            match = findMatch(s, startIndex, -1);
            if (match == null)
                break;
            if (!wholeWordsOnly)
                lastMatch = match;
            else if (Utilities.isDelimited(buffer.getMode(), s,
                                             match.getStartIndex(),
                                             match.getEndIndex()))
                lastMatch = match;
            startIndex = match.getStartIndex() + 1;
        }
        if (lastMatch == null)
            return null;
        match = lastMatch;
        return buffer.getPosition(match.getStartIndex());
    }

    // Search is restricted to region if endIndex >= 0.
    private REMatch findMatch(String s, int startIndex, int endIndex)
    {
        REMatch m = re.getMatch(s, startIndex);
        if (m == null)
            return null; // Not found at all.
        if (endIndex >= 0 && m.getEndIndex() > endIndex)
            return null; // Match extends past end of selected region.
        return m;
    }

    // Search is restricted to region if restrictToSelection is true and
    // region is not null.
    private Position findRegExp(Mode mode, Position start)
    {
        if (re == null) {
            try {
                setREFromPattern();
            }
            catch (Throwable t) {
                Log.error(t);
                return null;
            }
        }
        match = null;
        Line line = start.getLine();
        int begin = start.getOffset();
        Line endLine;
        if (restrictToSelection && region != null)
            endLine = region.getEndLine();
        else
            endLine = null;
        Position pos = null;
        while (line != endLine) {
            pos = findRegExpInLine(mode, line, begin, line.length());
            if (pos != null)
                return pos;
            line = line.next();
            begin = 0;
        }
        if (line != null) {
            // End line of region.
            pos = findRegExpInLine(mode, line, begin, region.getEndOffset());
        }
        return pos;
    }

    private Position findRegExpInLine(Mode mode, Line line, int begin, int end)
    {
        String toBeSearched;
        if (end < line.length())
            toBeSearched = line.substring(0, end);
        else
            toBeSearched = line.getText();
        int index = begin;
        int limit = toBeSearched.length();
        while (index <= limit) {
            match = re.getMatch(toBeSearched, index);
            if (match == null)
                break;
            Position pos = new Position(line, match.getStartIndex());
            if (!wholeWordsOnly || Utilities.isDelimited(mode, pos, match.toString().length()))
                return pos;
            index = match.getStartIndex() + 1;
        }
        return null;
    }

    private boolean findRegExp(String toBeSearched)
    {
        int index = 0;
        int limit = toBeSearched.length();
        while (index <= limit) {
            match = re.getMatch(toBeSearched, index);
            if (match == null)
                break;
            if (!wholeWordsOnly || Utilities.isDelimited(toBeSearched, match.getStartIndex(), match.toString().length()))
                return true;
            index = match.getStartIndex() + 1;
        }
        return false;
    }

    private boolean findRegExpDelimited(String s, Mode mode)
    {
        Debug.assertTrue(wholeWordsOnly);
        int index = 0;
        int limit = s.length();
        while (index <= limit) {
            match = re.getMatch(s, index);
            if (match == null)
                break;
            if (Utilities.isDelimited(s, match.getStartIndex(), match.toString().length(), mode))
                return true;
            index = match.getStartIndex() + 1;
        }
        return false;
    }

    // Region is ignored.
    public Position reverseFindRegExp(Buffer buffer, Position start)
    {
        if (re == null) {
            try {
                re = new RE(pattern, ignoreCase ? RE.REG_ICASE : 0);
            }
            catch (Throwable t) {
                Log.error(t);
                return null;
            }
        }
        Line line = start.getLine();
        match = null;
        Position pos = reverseFindRegExpInLine(buffer, line, 0, start.getOffset());
        if (pos != null)
            return pos;
        line = line.previous();
        while (line != null) {
            pos = reverseFindRegExpInLine(buffer, line, 0, line.length());
            if (pos != null)
                return pos;
            line = line.previous();
        }
        return null;
    }

    private Position reverseFindRegExpInLine(Buffer buffer, Line line, int begin, int end)
    {
        int index = end;
        while (index >= begin) {
            match = re.getMatch(line.getText(), index);
            if (match != null && match.getStartIndex() <= end) {
                Position pos = new Position(line, match.getStartIndex());
                if (!wholeWordsOnly || Utilities.isDelimited(buffer, pos, match.toString().length()))
                    return pos;
            }
            --index;
        }
        return null;
    }

    public void notFound(Editor editor)
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (regularExpression)
            sb.append("Regular expression ");
        sb.append('"');
        sb.append(pattern);
        sb.append("\" not found");
        editor.status(sb.toString());
    }

    public boolean equals(Object object)
    {
        if (this == object)
            return true;
        if (object instanceof Search) {
            Search search = (Search) object;
            if (pattern == null) {
                if (search.pattern != null)
                    return false;
            } else {
                // pattern != null
                if (!pattern.equals(search.pattern))
                    return false;
            }
            if (ignoreCase != search.ignoreCase)
                return false;
            if (wholeWordsOnly != search.wholeWordsOnly)
                return false;
            if (regularExpression != search.regularExpression)
                return false;
            if (restrictToSelection != search.restrictToSelection)
                return false;
            // Passed all tests.
            return true;
        }
        return false;
    }

    public Object clone()
    {
        try {
            return super.clone();
        }
        catch (CloneNotSupportedException e) {
            throw new InternalError(e.toString());
        }
    }
}
