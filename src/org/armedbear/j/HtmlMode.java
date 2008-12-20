/*
 * HtmlMode.java
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

import gnu.regexp.RE;
import gnu.regexp.REException;
import gnu.regexp.REMatch;
import java.awt.event.KeyEvent;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import javax.swing.undo.CompoundEdit;

public final class HtmlMode extends AbstractMode implements Constants, Mode
{
    private static final Mode mode = new HtmlMode();
    private static List elements;
    private static RE tagNameRE;
    private static RE attributeNameRE;
    private static RE quotedValueRE;
    private static RE unquotedValueRE;

    private HtmlMode()
    {
        super(HTML_MODE, HTML_MODE_NAME);
        // Support embedded JavaScript.
        keywords = new Keywords(JavaScriptMode.getMode());
    }

    public static final Mode getMode()
    {
        return mode;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new HtmlFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_TAB, 0, "tab");
        km.mapKey(KeyEvent.VK_TAB, CTRL_MASK, "insertTab");
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "newline");
        km.mapKey(KeyEvent.VK_M, CTRL_MASK, "htmlFindMatch");
        km.mapKey(KeyEvent.VK_E, CTRL_MASK, "htmlInsertMatchingEndTag");
        km.mapKey(KeyEvent.VK_B, CTRL_MASK, "htmlBold");
        km.mapKey('=', "htmlElectricEquals");
        km.mapKey('>', "electricCloseAngleBracket");
        km.mapKey(KeyEvent.VK_V, CTRL_MASK | ALT_MASK, "viewPage");
        km.mapKey(KeyEvent.VK_I, ALT_MASK, "cycleIndentSize");

        // These are the "normal" mappings.
        km.mapKey(KeyEvent.VK_COMMA, CTRL_MASK | SHIFT_MASK, "htmlInsertTag");
        km.mapKey(KeyEvent.VK_PERIOD, CTRL_MASK | SHIFT_MASK, "htmlEndTag");

        // The "normal" mappings don't work for Linux, but these do.
        km.mapKey(0x7c, CTRL_MASK | SHIFT_MASK, "htmlInsertTag");
        km.mapKey(0x7e, CTRL_MASK | SHIFT_MASK, "htmlEndTag");
    }

    public boolean canIndent()
    {
        return true;
    }

    public boolean canIndentPaste()
    {
        return false;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        if (line.flags() == STATE_SCRIPT)
            return JavaScriptMode.getMode().getCorrectIndentation(line, buffer);
        // Ignore comments.
        if (line.flags() == STATE_HTML_COMMENT)
            return buffer.getIndentation(line); // Unchanged.
        if (line.trim().startsWith("<!--"))
            return buffer.getIndentation(line); // Unchanged.
        Line model = getModel(line);
        if (model == null)
            return 0;
        int indent = buffer.getIndentation(model);
        if (line.trim().startsWith("</")) {
            Position pos = findMatchingStartTag(line);
            if (pos != null)
                return buffer.getIndentation(pos.getLine());
            indent -= buffer.getIndentSize();
            if (indent < 0)
                indent = 0;
            return indent;
        }
        final String trim = model.trim();
        if (trim.startsWith("<") && !trim.startsWith("</")) {
            if (trim.startsWith("<!")) // Document type declaration.
                return indent;
            // Model starts with start tag.
            String name = Utilities.getTagName(trim).toLowerCase();
            if (name.equals("html") || name.equals("head") || name.equals("body") || name.equals("form"))
                return indent;
            boolean wantsEndTag = wantsEndTag(name);
            if (wantsEndTag) {
                String startTag = "<" + name;
                String endTag = "</" + name;
                int count = 1;
                int limit = trim.length();
                for (int i = startTag.length(); i < limit; i++) {
		    // Handle empty tags (e.g., <div />).
		    if (trim.charAt(i) == '/' &&
			i + 1 < limit && trim.charAt(i+1) == '>') {
			--count;
		    } else if (trim.charAt(i) == '<') {
			if (lookingAtIgnoreCase(trim, i, endTag))
			    --count;
			else if (lookingAtIgnoreCase(trim, i, startTag))
			    ++count;
		    }

                }
                if (count > 0)
                    indent += buffer.getIndentSize();
            }
        }
        return indent;
    }

    // Line must start with an end tag.
    private Position findMatchingStartTag(Line line)
    {
        String s = line.trim();
        if (!s.startsWith("</"))
            return null;
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 2; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c <= ' ')
                break;
            if (c == '>')
                break;
            sb.append(c);
        }
        String name = sb.toString();
        String toBeMatched = "</" + name + ">";
        String match = "<" + name;
        int count = 1;
        boolean foundIt = false;
        Position pos = new Position(line, 0);
        final String commentStart = "<!--";
        final String commentEnd = "-->";
        final String emptyTagStart = "<";
        final String emptyTagEnd = "/>";
        // Search backward.
        while (!pos.atStart()) {
            pos.prev();
            if (pos.lookingAt(commentEnd)) {
                do {
                    pos.prev();
                } while (!pos.atStart() && !pos.lookingAt(commentStart));
	    } else if (pos.lookingAt(emptyTagEnd)) {
		do {
		    pos.prev();
		} while (!pos.atStart() && !pos.lookingAt(emptyTagStart));
            } else if (pos.lookingAtIgnoreCase(toBeMatched)) {
                ++count;
            } else if (pos.lookingAtIgnoreCase(match)) {
                if (pos.lookingAtIgnoreCase(match + ">"))
                    --count;
                else if (pos.lookingAtIgnoreCase(match + " "))
                    --count;
                else if (pos.lookingAtIgnoreCase(match + "\t"))
                    --count;
                if (count == 0) {
                    foundIt = true;
                    break;
                }
            }
        }
        if (foundIt)
            return pos;
        // Not found.
        return null;
    }

    private static final boolean lookingAtIgnoreCase(String s, int i, String pattern)
    {
        return s.regionMatches(true, i, pattern, 0, pattern.length());
    }

    private static Line getModel(Line line)
    {
        Line model = line;
        while ((model = model.previous()) != null) {
            if (model.flags() == STATE_HTML_COMMENT)
                continue;
            if (model.trim().startsWith("<!--"))
                continue;
            if (model.isBlank())
                continue;
            break;
        }
        return model;
    }

    public char fixCase(Editor editor, char c)
    {
        if (!editor.getBuffer().getBooleanProperty(Property.FIX_CASE))
            return c;
        if (!initRegExps())
            return c;
        Position pos = findStartOfTag(editor.getDot());
        if (pos != null) {
            int index = pos.getOffset();
            String text = pos.getLine().getText();
            REMatch match = tagNameRE.getMatch(text, index);
            if (match == null)
                return c;
            if (match.getEndIndex() >= editor.getDotOffset()) {
                // Tag name.
                if (editor.getBuffer().getBooleanProperty(Property.UPPER_CASE_TAG_NAMES))
                    return Character.toUpperCase(c);
                else
                    return Character.toLowerCase(c);
            }
            while (true) {
                index = match.getEndIndex();
                match = attributeNameRE.getMatch(text, index);
                if (match == null)
                    return c;
                if (match.getEndIndex() >= editor.getDotOffset()) {
                    // Attribute name.
                    if (editor.getBuffer().getBooleanProperty(Property.UPPER_CASE_ATTRIBUTE_NAMES))
                        return Character.toUpperCase(c);
                    else
                        return Character.toLowerCase(c);
                }
                index = match.getEndIndex();
                match = quotedValueRE.getMatch(text, index);
                if (match == null) {
                    match = unquotedValueRE.getMatch(text, index);
                    if (match == null)
                        return c;
                }
                if (match.getEndIndex() >= editor.getDotOffset()) {
                    // Attribute value.
                    return c;
                }
            }
        }
        return c;
    }

    private static boolean checkElectricEquals(Editor editor)
    {
        Position pos = findStartOfTag(editor.getDot());
        if (pos == null)
            return false;
        char c = editor.getDotChar();
        if (c == '>' || c == '/' || Character.isWhitespace(c))
            return true;
        return false;
    }

    private static Position findStartOfTag(Position pos)
    {
        int offset = pos.getOffset();
        String text = pos.getLine().getText();
        while (--offset >= 0) {
            char c = text.charAt(offset);
            if (c == '>')
                return null;
            if (c == '<')
                return new Position(pos.getLine(), offset);
        }
        return null;
    }

    public static List elements()
    {
        if (elements == null)
            loadElementList();
        return elements;
    }

    private static boolean wantsEndTag(String elementName)
    {
        elementName = elementName.trim().toLowerCase();
        if (elements == null)
            loadElementList();
        if (elements != null) {
            final int limit = elements.size();
            for (int i = 0; i < limit; i++) {
                HtmlElement element = (HtmlElement) elements.get(i);
                if (element.getName().equals(elementName))
                    return element.wantsEndTag();
            }
        }
        return true; // Default.
    }

    private static void loadElementList()
    {
        elements = HtmlElement.getDefaultElements();
        String filename = Editor.preferences().getStringProperty(Property.HTML_MODE_TAGS);
        if (filename != null && filename.length() > 0) {
            try {
                FileInputStream istream = new FileInputStream(filename);
                loadElementsFromStream(istream);
            }
            catch (FileNotFoundException e) {
                Log.error(e);
            }
        }
    }

    private static void loadElementsFromStream(InputStream istream)
    {
        Debug.assertTrue(elements != null);
        if (istream != null) {
            try {
                BufferedReader in =
                    new BufferedReader(new InputStreamReader(istream));
                while (true) {
                    String s = in.readLine();
                    if (s == null)
                        break; // Reached end of file.
                    s = s.trim();
                    // Ignore blank lines.
                    if (s.trim().length() == 0)
                        continue;
                    // Ignore comment lines.
                    if (s.charAt(0) == '#')
                        continue;
                    int index = s.indexOf('=');
                    if (index >= 0) {
                        // Element names are always stored in lower case.
                        String name = s.substring(0, index).trim().toLowerCase();
                        String value = s.substring(index + 1).trim();
                        boolean wantsEndTag = value.equals("1") || value.equals("true");
                        boolean found = false;
                        for (int i = 0; i < elements.size(); i++) {
                            HtmlElement element = (HtmlElement) elements.get(i);
                            if (element.getName().equals(name)) {
                                element.setWantsEndTag(wantsEndTag);
                                found = true;
                                break;
                            }
                        }
                        if (!found)
                            elements.add(new HtmlElement(name, wantsEndTag));
                    }
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
    }

    private static boolean initRegExps()
    {
        if (tagNameRE == null) {
            try {
                tagNameRE = new RE("</?[A-Za-z0-9]*");
                attributeNameRE = new RE("\\s+[A-Za-z0-9]*");
                quotedValueRE = new RE("\\s*=\\s*\"[^\"]*");
                unquotedValueRE = new RE("\\s*=\\s*\\S*");
            }
            catch (REException e) {
                tagNameRE = null;
                return false;
            }
        }
        return true;
    }

    public static void htmlStartTag()
    {
        htmlTag(Editor.currentEditor(), false);
    }

    public static void htmlEndTag()
    {
        htmlTag(Editor.currentEditor(), true);
    }

    private static void htmlTag(Editor editor, boolean isEndTag)
    {
        if (!editor.checkReadOnly())
            return;
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        editor.insertChar('<');
        if (isEndTag)
            editor.insertChar('/');
        editor.insertChar('>');
        editor.addUndo(SimpleEdit.MOVE);
        editor.getDot().moveLeft();
        editor.moveCaretToDotCol();
        editor.endCompoundEdit(compoundEdit);
    }

    public static void htmlInsertTag()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        InsertTagDialog d = new InsertTagDialog(editor);
        editor.centerDialog(d);
        d.show();
        _htmlInsertTag(editor, d.getInput());
    }

    public static void htmlInsertTag(String input)
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        _htmlInsertTag(editor, input);
    }

    private static void _htmlInsertTag(Editor editor, String input)
    {
        if (input != null && input.length() > 0) {
            final String tagName, extra;
            int index = input.indexOf(' ');
            if (index >= 0) {
                tagName = input.substring(0, index);
                extra = input.substring(index);
            } else {
                tagName = input;
                extra = "";
            }
            InsertTagDialog.insertTag(editor, tagName, extra, wantsEndTag(tagName));
        }
    }

    public static void htmlInsertMatchingEndTag()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        Position pos = editor.getDotCopy();
        while (pos.prev()) {
            // If we find an end tag, we've got nothing to match.
            if (pos.lookingAt("</"))
                return;

            if (pos.getChar() == '<') {
                if (pos.next()) {
                    FastStringBuffer sb = new FastStringBuffer();
                    char c;
                    while (!Character.isWhitespace(c = pos.getChar()) && c != '>') {
                        sb.append(c);
                        if (!pos.next())
                            return;
                    }
                    if (sb.length() == 0)
                        return;
                    final String endTag = "</" + sb.toString() + ">";
                    final Buffer buffer = editor.getBuffer();
                    try {
                        buffer.lockWrite();
                    }
                    catch (InterruptedException e) {
                        Log.error(e);
                        return;
                    }
                    try {
                        CompoundEdit compoundEdit = editor.beginCompoundEdit();
                        editor.fillToCaret();
                        editor.addUndo(SimpleEdit.INSERT_STRING);
                        editor.insertStringInternal(endTag);
                        buffer.modified();
                        editor.addUndo(SimpleEdit.MOVE);
                        editor.moveCaretToDotCol();
                        if (buffer.getBooleanProperty(Property.AUTO_INDENT))
                            editor.indentLine();
                        editor.endCompoundEdit(compoundEdit);
                    }
                    finally {
                        buffer.unlockWrite();
                    }
                }
                return;
            }
        }
    }

    public static void htmlBold()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (!editor.checkReadOnly())
            return;
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        if (editor.getMark() == null)
            editor.fillToCaret();
        boolean upper = buffer.getBooleanProperty(Property.UPPER_CASE_TAG_NAMES);
        InsertTagDialog.insertTag(editor,  upper ? "B" : "b", "", true);
        editor.endCompoundEdit(compoundEdit);
    }

    public static void htmlElectricEquals()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        boolean ok = false;
        if (editor.getModeId() == HTML_MODE) {
            if (editor.getBuffer().getBooleanProperty(Property.ATTRIBUTES_REQUIRE_QUOTES))
                if (checkElectricEquals(editor))
                    ok = true;
        }
        if (ok) {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.fillToCaret();
            editor.addUndo(SimpleEdit.INSERT_STRING);
            editor.insertStringInternal("=\"\"");
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().moveLeft();
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
        } else
            editor.insertNormalChar('=');
    }

    public static void htmlFindMatch()
    {
        final Editor editor = Editor.currentEditor();
        final String special = "{([})]";
        final String commentStart = "<!--";
        final String commentEnd = "-->";
        final String emptyTagStart = "<";
        final String emptyTagEnd = "/>";
        Position dot = editor.getDot();
        char c = dot.getChar();
        if (special.indexOf(c) >= 0) {
            editor.findMatchingChar();
            return;
        }
        Position saved = dot.copy();
        while ((c = dot.getChar()) > ' ' && c != '<' && dot.getOffset() > 0) {
            if (dot.lookingAt(commentEnd))
                break;
            dot.prev();
        }
        if (c <= ' ')
            dot.next();
        Position start = dot.copy();
        FastStringBuffer sb = new FastStringBuffer(dot.getChar());
        dot.next();
        while ((c = dot.getChar()) > ' ' && c != '>') {
            sb.append(c);
            if (!dot.next()) {
                editor.status("Nothing to match");
                dot.moveTo(saved);
                return;
            }
        }
        if (c == '>')
            sb.append(c);
	else {
	    Position probe = dot.copy();
	    while (probe.next() && probe.getChar() != '>') {
		if (probe.lookingAt(emptyTagEnd)) {
		    editor.status("Nothing to match");
		    dot.moveTo(saved);
		    return;
		}
	    }
	}
        String toBeMatched = sb.toString();
        String match = null;
        boolean searchForward = true;
        if (toBeMatched.equals(commentStart))
            match = commentEnd;
        else if (toBeMatched.equals(commentEnd)) {
            match = commentStart;
            searchForward = false;
        } else if (toBeMatched.startsWith("</")) {
            match = "<".concat(toBeMatched.substring(2));
            if (match.endsWith(">"))
                match = match.substring(0, match.length()-1);
            searchForward = false;
        } else if (toBeMatched.startsWith("<") && !toBeMatched.endsWith(emptyTagEnd)) {
            if (toBeMatched.endsWith(">"))
                toBeMatched = toBeMatched.substring(0, toBeMatched.length()-1);
            match = "</" + toBeMatched.substring(1);
            if (!match.endsWith(">"))
                match += '>';
        } else {
            editor.status("Nothing to match");
            dot.moveTo(saved);
            return;
        }
        editor.setWaitCursor();
        int count = 1;
        boolean succeeded = false;
        dot.moveTo(start);
        if (searchForward) {
            dot.skip(toBeMatched.length());
            if (toBeMatched.equals(commentStart)) {
                while (!dot.atEnd()) {
                    if (dot.lookingAt(commentEnd)) {
                        succeeded = true;
                        break;
                    }
                    dot.next();
                }
            } else {
                // Find matching end tag.
                while (!dot.atEnd()) {
                    if (dot.lookingAt(commentStart)) {
                        dot.skip(commentStart.length());
                        while (!dot.atEnd()) {
                            if (dot.lookingAt(commentEnd)) {
                                dot.skip(commentEnd.length());
                                break;
                            }
                            dot.next();
                        }
                    } else if (dot.lookingAtIgnoreCase(toBeMatched)) {
                        dot.skip(toBeMatched.length());
                        while (!dot.atEnd()) {
                            if (dot.lookingAt(emptyTagEnd)) {
                                dot.skip(emptyTagEnd.length());
                                break;
                            }

			    c = dot.getChar();
			    if (c == '>') {
				++count;
				dot.next();
				break;
			    }

                            dot.next();
                        }
                    } else if (dot.lookingAtIgnoreCase(match)) {
                        --count;
                        if (count == 0) {
                            succeeded = true;
                            break;
                        }
                        dot.skip(match.length());
                    } else
                        dot.next();
                }
            }
        } else {
            // Search backward.
            while (!dot.atStart()) {
                dot.prev();
                if (dot.lookingAt(commentEnd)) {
                    do {
                        dot.prev();
                    }
                    while (!dot.atStart() && !dot.lookingAt(commentStart));
		} else if (dot.lookingAt(emptyTagEnd)) {
		    do {
			dot.prev();
		    } while (!dot.atStart() && !dot.lookingAt(emptyTagStart));
                } else if (dot.lookingAtIgnoreCase(toBeMatched)) {
                    ++count;
                } else if (dot.lookingAtIgnoreCase(match)) {
                    if (dot.lookingAtIgnoreCase(match + ">"))
                        --count;
                    else if (dot.lookingAtIgnoreCase(match + " "))
                        --count;
                    else if (dot.lookingAtIgnoreCase(match + "\t"))
                        --count;
                    if (count == 0) {
                        succeeded = true;
                        break;
                    }
                }
            }
        }
        if (succeeded) {
            Position matchPos = dot.copy();
            dot.moveTo(saved);
            editor.updateDotLine();
            editor.addUndo(SimpleEdit.MOVE);
            dot.moveTo(matchPos);
            editor.updateDotLine();
            editor.moveCaretToDotCol();
        } else {
            dot.moveTo(saved);
            editor.status("No match");
        }
        editor.setDefaultCursor();
    }
}
