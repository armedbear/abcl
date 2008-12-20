/*
 * AbstractMode.java
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

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.util.List;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;

public abstract class AbstractMode implements Constants, Mode
{
    private static final Preferences preferences = Editor.preferences();

    protected KeyMap keyMap;
    protected File keyMapFile;
    protected PropertyList properties;
    protected Keywords keywords;

    private final int id;
    private final String displayName;

    protected AbstractMode(int id, String displayName)
    {
        this.id = id;
        this.displayName = displayName;
        if (Editor.isLispInitialized()) {
            String hook =
                displayName.toLowerCase().replace(' ', '-') + "-mode-hook";
            Editor.invokeHook(hook);
        }
    }

    public final int getId()
    {
        return id;
    }

    public final String getDisplayName()
    {
        return displayName;
    }

    public Buffer createBuffer(File file)
    {
        return null;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new PlainTextFormatter(buffer);
    }

    public final String toString()
    {
        return displayName;
    }

    // Should never return null.
    public synchronized final KeyMap getKeyMap()
    {
        if (keyMap == null) {
            if (Editor.isLispInitialized()) {
                String functionName =
                    displayName.toLowerCase().replace(' ', '-').concat("-mode-map");
                FastStringBuffer sb = new FastStringBuffer("(ignore-errors (");
                sb.append("j::");
                sb.append(functionName);
                sb.append("))");
                try {
                    LispObject result =
                        Interpreter.evaluate(sb.toString());
                    if (result instanceof JavaObject) {
                        Object obj = ((JavaObject)result).getObject();
                        if (obj instanceof KeyMap) {
                            keyMap = (KeyMap) obj;
                            return keyMap;
                        }
                    }
                }
                catch (Throwable t) {
                    Log.debug(t);
                }
            }
            if (!loadKeyMapForMode()) {
                keyMap = new KeyMap();
                setKeyMapDefaults(keyMap);
            }
        }
        return keyMap;
    }

    private boolean loadKeyMapForMode()
    {
        keyMap = null;
        String filename = preferences.getStringProperty(getFullKey("keyMap"));
        if (filename != null) {
            keyMapFile = File.getInstance(filename);
            if (keyMapFile != null) {
                keyMap = new KeyMap();
                if (keyMap.load(keyMapFile))
                    return true;
            }
        }
        keyMap = null;
        keyMapFile = null;
        return false;
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        // Default implementation just leaves keymap empty.
    }

    public File getKeyMapFile()
    {
        return keyMapFile;
    }

    public synchronized final void useDefaultKeyMap()
    {
        keyMap = new KeyMap();
        setKeyMapDefaults(keyMap);
    }

    public synchronized final void deleteKeyMap()
    {
        keyMap = null;
    }

    public String getMenuName()
    {
        return "Default";
    }

    public MenuBar createMenuBar(Frame frame)
    {
        MenuBar menuBar = new MenuBar("Default");
        menuBar.add(new Menu("File", 'F'));
        menuBar.add(new Menu("Edit", 'E'));
        menuBar.add(new Menu("View", 'V'));
        menuBar.add(new Menu("Search", 'S'));
        menuBar.add(new Menu("Go", 'G'));
        menuBar.add(new Menu("Mode", 'M'));
        menuBar.add(new Menu("Lisp", 'L'));
        menuBar.add(new Menu("Help", 'H'));
        return menuBar;
    }

    public void populateMenu(Editor editor, Menu menu)
    {
        final String text = menu.getText();
        if (text == "File")
            populateFileMenu(editor, menu);
        else if (text == "Edit")
            populateEditMenu(editor, menu);
        else if (text == "View")
            populateViewMenu(editor, menu);
        else if (text == "Search")
            populateSearchMenu(editor, menu);
        else if (text == "Go")
            populateGoMenu(editor, menu);
        else if (text == "Mode") {
            populateModeMenu(editor, menu);
            if (menu.getMenuComponentCount() == 0)
                menu.add(new JMenuItem("This menu isn't here yet!")).setEnabled(false);
        } else if (text == "Lisp")
            populateLispMenu(editor, menu);
        else if (text == "Help")
            populateHelpMenu(editor, menu);
    }

    private static void populateFileMenu(Editor editor, Menu menu)
    {
        final boolean isNotReadOnly = !editor.getBuffer().isReadOnly();
        menu.add(editor, "New", 'N', "newBuffer");
        menu.add(editor, "Open...", 'O', "openFile");
        menu.add(editor, "Recent Files...", 'R', "recentFiles");
        menu.addSeparator();
        menu.add(editor, "Save", 'S', "save", isNotReadOnly);
        menu.add(editor, "Save As...", 'E', "saveAs");
        menu.add(editor, "Save a Copy...", 'Y', "saveCopy");
        menu.add(editor, "Save All", 'A', "saveAll");
        menu.add(editor, "Close", 'C', "killBuffer");
        menu.add(editor, "Close All", 'L', "closeAll");
        menu.add(editor, "Close Others", 'H', "closeOthers");
        menu.add(editor, "Revert", 'V', "revertBuffer");
        menu.add(editor, "Set Encoding", 'G', "setEncoding");
        menu.addSeparator();
        menu.add(editor, "Properties", 'I', "properties");
        menu.addSeparator();
        menu.add(editor, "Next Buffer", 'T', "nextBuffer");
        menu.add(editor, "Previous Buffer", 'R', "prevBuffer");
        menu.addSeparator();
        menu.add(editor, "New Frame", 'M', "newFrame");
        menu.add(editor, "Execute Command...", 'D', "executeCommand");
        menu.addSeparator();
        menu.add(editor, "Print...", 'P', "print");
        menu.addSeparator();
        menu.add(editor, "Save Session", 'S', "saveSession");
        menu.add(editor, "Load Session...", 'L', "loadSession");
        menu.addSeparator();
        menu.add(editor, "Save All/Exit", '/', "saveAllExit");
        menu.add(editor, "Exit", 'X', "quit");
    }

    private static void populateEditMenu(Editor editor, Menu menu)
    {
        final boolean isNotReadOnly = !editor.getBuffer().isReadOnly();
        menu.add(editor, "Undo", 'U', "undo");
        menu.add(editor, "Redo", 'O', "redo");
        menu.addSeparator();
        menu.add(editor, "Cut", 'T', "killRegion", isNotReadOnly);
        menu.add(editor, "Cut Append", 'D', "killAppend", isNotReadOnly);
        menu.add(editor, "Copy", 'C', "copyRegion");
        menu.add(editor, "Copy Append", 'A', "copyAppend");
        menu.add(editor, "Paste", 'P', "paste", isNotReadOnly);
        menu.add(editor, "Cycle Paste", 'Y', "cyclePaste", isNotReadOnly);
        menu.addSeparator();
        menu.add(editor, "Cycle Tab Width", 'B', "cycleTabWidth");
        menu.add(editor, "Cycle Indent Size", 'N', "cycleIndentSize");
        menu.add(editor, "Indent", 'I', "indentLineOrRegion", isNotReadOnly);
        menu.addSeparator();
        menu.add(editor, "Upper Case", 'R', "upperCaseRegion", isNotReadOnly);
        menu.add(editor, "Lower Case", 'L', "lowerCaseRegion", isNotReadOnly);
    }

    private static void populateViewMenu(Editor editor, Menu menu)
    {
        JCheckBoxMenuItem toolbarMenuItem = new JCheckBoxMenuItem("Toolbar");
        toolbarMenuItem.setMnemonic('T');
        toolbarMenuItem.setActionCommand("toggleToolbar");
        toolbarMenuItem.addActionListener(editor.getDispatcher());
        toolbarMenuItem.setSelected(editor.getFrame().getShowToolbar());
        menu.add(toolbarMenuItem);
        JCheckBoxMenuItem sidebarMenuItem = new JCheckBoxMenuItem("Sidebar");
        sidebarMenuItem.setMnemonic('S');
        sidebarMenuItem.setActionCommand("toggleSidebar");
        sidebarMenuItem.addActionListener(editor.getDispatcher());
        sidebarMenuItem.setSelected(editor.getSidebar() != null);
        menu.add(sidebarMenuItem);
        menu.addSeparator();
        menu.add(editor, "Split Window", 'W', "splitWindow");
        menu.add(editor, "Unsplit Window", 'U', "unsplitWindow");
        menu.add(editor, "Close Window", 'C', "killWindow");
    }

    protected void populateSearchMenu(Editor editor, Menu menu)
    {
        final File dir = editor.getCurrentDirectory();
        final boolean local = (dir != null && dir.isLocal());
        if (Editor.preferences().getBooleanProperty(Property.USE_INCREMENTAL_FIND))
            menu.add(editor, "Incremental Find...", 'I', "incrementalFind");
        menu.add(editor, "Find...", 'F', "find");
        menu.add(editor, "Find Next", 'T', "findNext");
        menu.add(editor, "Find Previous", 'R', "findPrev");
        menu.add(editor, "Find in Files...", 'S', "findInFiles", local);
        menu.addSeparator();
        menu.add(editor, "List Occurrences of Last Pattern", 'L', "listOccurrences",
                 editor.getLastSearch() != null);
        menu.add(editor, "List Occurrences of Pattern in Files", 'O', "listFiles",
                 FindInFiles.getFindInFiles() != null);
        menu.addSeparator();
        final boolean isNotReadOnly = !editor.getBuffer().isReadOnly();
        if (!(editor.getBuffer() instanceof Directory))
            menu.add(editor, "Replace...", 'P', "replace", isNotReadOnly);
        menu.add(editor, "Replace in Files...", 'E', "replaceInFiles", local);
        if (!(editor.getBuffer() instanceof Directory)) {
            menu.addSeparator();
            menu.add(editor, "Find Tag...", 'A', "findTag");
        }
    }

    private static void populateGoMenu(Editor editor, Menu menu)
    {
        menu.add(editor, "Go to Line...", 'L', "jumpToLine");
        menu.add(editor, "Go to Column...", 'C', "jumpToColumn");
        menu.add(editor, "Go to Offset...", 'O', "jumpToOffset");
        if (editor.getModeId() == HTML_MODE)
            menu.add(editor, "Go to Matching HTML", 'M', "htmlFindMatch");
        else
            menu.add(editor, "Go to Matching Character", 'M', "findMatchingChar");
        menu.add(editor, "Go to Tag", 'A', "findTagAtDot");
        menu.add(editor, "Go to Next Occurrence of Word", 'T', "findNextWord");
        menu.add(editor, "Go to Previous Occurrence of Word", 'R', "findPrevWord");
        if (editor.getBuffer().getBooleanProperty(Property.SHOW_CHANGE_MARKS)) {
            menu.add(editor, "Go to Next Change", 'H', "nextChange");
            menu.add(editor, "Go to Previous Change", 'G', "previousChange");
        }
        menu.addSeparator();
        menu.add(editor, "Push Position", 'U', "pushPosition");
        menu.add(editor, "Pop Position", 'P', "popPosition");
    }

    public void populateModeMenu(Editor editor, Menu menu)
    {
    }

    public void populateLispMenu(Editor editor, Menu menu)
    {
        menu.add(editor, "Run Lisp as Separate Process", 'L', "lisp");
        menu.add(editor, "Run Embedded Lisp", 'E', "jlisp");
    }

    private static void populateHelpMenu(Editor editor, Menu menu)
    {
        menu.add(editor, "Help", 'P', "help");
        menu.add(editor, "Apropos...", 'A', "apropos");
        menu.add(editor, "Key Bindings", 'B', "describeBindings");
        menu.add(editor, "Describe Key...", 'K', "describeKey");
        menu.add(editor, "Where is...", 'W', "whereIs");
        menu.add(editor, "About J", 'O', "about");
    }

    public JPopupMenu getContextMenu(Editor editor)
    {
        final JPopupMenu popup = new JPopupMenu();
        addDefaultContextMenuItems(editor, popup);
        popup.pack();
        return popup;
    }

    protected void addDefaultContextMenuItems(Editor editor, JPopupMenu popup)
    {
        final Buffer buffer = editor.getBuffer();
        final Dispatcher dispatcher = editor.getDispatcher();

        JMenuItem menuItem;

        if (buffer.supportsUndo()) {
            menuItem = new JMenuItem();
            menuItem.setText("Undo");
            menuItem.setActionCommand("undo");
            menuItem.addActionListener(dispatcher);
            if (!buffer.canUndo())
                menuItem.setEnabled(false);
            popup.add(menuItem);

            menuItem = new JMenuItem();
            menuItem.setText("Redo");
            menuItem.setActionCommand("redo");
            menuItem.addActionListener(dispatcher);
            if (!buffer.canRedo())
                menuItem.setEnabled(false);
            popup.add(menuItem);

            popup.addSeparator();
        }

        menuItem = new JMenuItem();
        menuItem.setText(editor.getMark() == null ? "Cut line" : "Cut");
        menuItem.setActionCommand("killRegion");
        menuItem.addActionListener(dispatcher);
        if (buffer.isReadOnly())
            menuItem.setEnabled(false);
        popup.add(menuItem);

        menuItem = new JMenuItem();
        menuItem.setText(editor.getMark() == null ? "Copy line" : "Copy");
        menuItem.setActionCommand("copyRegion");
        menuItem.addActionListener(dispatcher);
        if (editor.getMark() == null && editor.getDotLine().isBlank())
            menuItem.setEnabled(false);
        popup.add(menuItem);

        menuItem = new JMenuItem("Paste");
        menuItem.setActionCommand("paste");
        menuItem.addActionListener(dispatcher);
        if (!editor.canPaste())
            menuItem.setEnabled(false);
        popup.add(menuItem);

        // List occurrences.
        menuItem = new JMenuItem();
        menuItem.setActionCommand("listOccurrencesOfPatternAtDot");
        menuItem.addActionListener(dispatcher);
        Search search = editor.getSearchAtDot();
        if (search != null) {
            if (editor.getMark() != null)
                menuItem.setText("List occurrences of selected text");
            else
                menuItem.setText("List occurrences of \"" + search.getPattern() + "\"");
        } else {
            menuItem.setText("List occurences of pattern under cursor");
            menuItem.setEnabled(false);
        }
        popup.addSeparator();
        popup.add(menuItem);

        // Find tag.
        if (buffer.isTaggable()) {
            menuItem = new JMenuItem();
            menuItem.setActionCommand("findTagAtDot");
            menuItem.addActionListener(dispatcher);
            if (editor.getMark() == null) {
                Expression expr = getExpressionAtDot(editor, false);
                if (expr != null)
                    menuItem.setText("Find tag \"" + expr.getName() + "\"");
            } else {
                menuItem.setText("Find tag under cursor");
                menuItem.setEnabled(false);
            }
            popup.add(menuItem);
        }

        // Folding.
        popup.addSeparator();
        addContextMenuItem("Fold", "fold", popup, dispatcher);
        addContextMenuItem("Unfold", "unfold", popup, dispatcher);
        addContextMenuItem("Unfold all", "unfoldAll", popup, dispatcher);

        // Properties.
        if (buffer.getFile() != null && !(buffer instanceof Directory)) {
            popup.addSeparator();
            addContextMenuItem("Properties", "properties", popup, dispatcher);
        }
    }

    protected JMenuItem addContextMenuItem(String text, String command,
        JPopupMenu popup, Dispatcher dispatcher)
    {
        JMenuItem menuItem = new JMenuItem(text);
        menuItem.setActionCommand(command);
        menuItem.addActionListener(dispatcher);
        popup.add(menuItem);
        return menuItem;
    }

    public ToolBar getToolBar(Frame frame)
    {
        ToolBar tb = getCustomToolBar(frame);
        if (tb != null)
            return tb;
        return getDefaultToolBar(frame);
    }

    protected ToolBar getCustomToolBar(Frame frame)
    {
        String filename =
            Editor.preferences().getStringProperty(getFullKey("toolbar"));
        if (filename != null) {
            File file = File.getInstance(filename);
            if (file != null && file.isFile()) {
                ToolBar tb = ToolBar.createToolBar(frame, file);
                if (tb != null)
                    return tb;
            }
        }
        return null;
    }

    protected ToolBar getDefaultToolBar(Frame frame)
    {
        return frame.getDefaultToolBar();
    }

    public NavigationComponent getSidebarComponent(Editor editor)
    {
        if (isTaggable())
            return new SidebarTagList(editor.getSidebar(), editor);
        else
            return null;
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return null;
    }

    public boolean isTaggable()
    {
        return false;
    }

    public boolean hasQualifiedNames()
    {
        return false;
    }

    public boolean isQualifiedName(String s)
    {
        return s.indexOf('.') >= 0 || s.indexOf("::") >= 0;
    }

    public boolean canIndent()
    {
        return false;
    }

    public boolean canIndentPaste()
    {
        return canIndent();
    }

    public boolean acceptsLinePaste(Editor editor)
    {
        return true;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        return 0;
    }

    public SyntaxIterator getSyntaxIterator(Position pos)
    {
        return new DefaultSyntaxIterator(pos);
    }

    public String getCommentStart()
    {
        return null;
    }

    public String getCommentEnd()
    {
        return null;
    }

    public boolean getBooleanProperty(Property property)
    {
        String key = property.key();

        // Look for mode-specific setting in preferences.
        String s = preferences.getStringProperty(getFullKey(key));

        if (s == null) {
            // No mode-specific setting in preferences.
            // Look in property list for mode.
            // (Property list for mode overrides global preference!)
            if (properties != null) {
                Object value = properties.getProperty(property);
                if (value instanceof Boolean)
                    return ((Boolean) value).booleanValue();
            }

            // Not in property list for mode.
            // Look for global setting in preferences.
            s = preferences.getStringProperty(key);
        }

        if (s != null) {
            s = s.trim();
            if (s.equals("true") || s.equals("1"))
                return true;
            if (s.equals("false") || s.equals("0"))
                return false;
        }

        // Not in preferences or property list for mode.
        // Use hard-coded default.
        return ((Boolean)getDefaultValue(property)).booleanValue();
    }

    public int getIntegerProperty(Property property)
    {
        String key = property.key();

        // Look for mode-specific setting in preferences.
        String s = preferences.getStringProperty(getFullKey(key));

        if (s == null) {
            // No mode-specific setting in preferences.
            // Look in property list for mode.
            // (Property list for mode overrides global preference!)
            if (properties != null) {
                Object value = properties.getProperty(property);
                if (value instanceof Integer)
                    return ((Integer) value).intValue();
            }

            // Not in property list for mode.
            // Look for global setting in preferences.
            s = preferences.getStringProperty(key);
        }

        if (s != null) {
            try {
                return Integer.parseInt(s.trim());
            }
            catch (NumberFormatException e) {}
        }

        // Not in preferences or property list for mode.
        // Use hard-coded default.
        return ((Integer)getDefaultValue(property)).intValue();
    }

    public String getStringProperty(Property property)
    {
        String key = property.key();

        // Look for mode-specific setting in preferences.
        String s = preferences.getStringProperty(getFullKey(key));

        if (s == null) {
            // No mode-specific setting in preferences.
            // Look in property list for mode.
            // (Property list for mode overrides global preference!)
            if (properties != null) {
                Object value = properties.getProperty(property);
                if (value instanceof String)
                    return (String) value;
            }

            // Not in property list for mode.
            // Look for global setting in preferences.
            s = preferences.getStringProperty(key);
        }

        if (s != null)
            return s;

        // Not in preferences or property list for mode.
        // Use hard-coded default.
        return (String) getDefaultValue(property); // May be null.
    }

    public Color getColorProperty(Property property)
    {
        String key = property.key();

        // Look for mode-specific setting in preferences.
        String value = preferences.getStringProperty(getFullKey(key));

        if (value == null) {
            // We don't check the mode-specific properties list here. Is that
            // correct?

            // Look for global setting in preferences.
            value = preferences.getStringProperty(key);
        }

        if (value != null)
            return Utilities.getColor(value);
        else
            return null;
    }

    public void setProperty(Property property, String value)
    {
        if (properties == null)
            properties = new PropertyList();
        properties.setProperty(property, value);
    }

    public void setProperty(Property property, boolean value)
    {
        if (properties == null)
            properties = new PropertyList();
        properties.setProperty(property, value);
    }

    public void setProperty(Property property, int value)
    {
        if (properties == null)
            properties = new PropertyList();
        properties.setProperty(property, value);
    }

    protected Object getDefaultValue(Property property)
    {
        return property.getDefaultValue();
    }

    public final boolean accepts(String filename)
    {
        return Editor.getModeList().modeAccepts(id, filename);
    }

    protected String getFullKey(String key)
    {
        FastStringBuffer sb = new FastStringBuffer(this.getClass().getName());
        sb.append('.');
        sb.append(key);
        final String fullKey = sb.toString().toLowerCase();
        if (fullKey.startsWith("org.armedbear.j.mail."))
            return fullKey.substring(21);
        else if (fullKey.startsWith("org.armedbear.j."))
            return fullKey.substring(16);
        else
            return fullKey;
    }

    public boolean isIdentifierStart(char c)
    {
        return Character.isJavaIdentifierStart(c);
    }

    public boolean isIdentifierPart(char c)
    {
        return Character.isJavaIdentifierPart(c);
    }

    public boolean isDelimited(Position pos, int length)
    {
        final Line line = pos.getLine();
        final int offset = pos.getOffset();
        if (offset > 0) {
            if (isIdentifierPart(line.charAt(offset - 1)))
                return false;
        }
        final int after = offset + length;
        if (after < pos.getLineLength() && isIdentifierPart(line.charAt(after)))
            return false;
        return true;
    }

    public boolean isInQuote(Buffer buffer, Position pos)
    {
        // The default implementation considers both single and double quotes
        // (which is wrong for Lisp) and only looks at the current line (which
        // is wrong for C and C++).
        Line line = pos.getLine();
        int offset = pos.getOffset();
        boolean inQuote = false;
        char quoteChar = '\0';
        for (int i = 0; i < offset; i++) {
            char c = line.charAt(i);
            if (c == '\\') {
                // Escape.
                ++i;
            } else if (inQuote) {
                if (c == quoteChar)
                    inQuote = false;
            } else {
                if (c == '"' || c == '\'') {
                    inQuote = true;
                    quoteChar = c;
                }
            }
        }
        return inQuote;
    }

    public boolean isInComment(Buffer buffer, Position pos)
    {
        return false;
    }

    public boolean isCommentLine(Line line)
    {
        return false;
    }

    public char fixCase(Editor editor, char c)
    {
        return c;
    }

    public String getContextString(Editor editor, boolean verbose)
    {
        final List tags = editor.getBuffer().getTags();
        if (tags != null) {
            Position pos = editor.getDot();
            if (pos != null) {
                LocalTag tag = null;
                // Find the tag before the cursor position.
                final int target = pos.lineNumber();
                final int limit = tags.size();
                for (int i = 0; i < limit; i++) {
                    LocalTag nextTag = (LocalTag) tags.get(i);
                    if (nextTag.lineNumber() > target)
                        break;
                    else
                        tag = nextTag;
                }
                if (tag != null)
                    return verbose ? tag.getLongName() : tag.getMethodName();
            }
        }
        return null;
    }

    public String getMouseMovedContextString(Editor editor, Position pos)
    {
        return null;
    }

    public String getToolTipText(Editor editor, MouseEvent e)
    {
        return null;
    }

    public void loadFile(Buffer buffer, File file)
    {
    }

    public boolean confirmClose(Editor editor, Buffer buffer)
    {
        if (!buffer.isModified())
            return true;
        if (buffer.getFile() == null)
            return true;
        return CloseBufferConfirmationDialog.confirmClose(editor, buffer);
    }

    public boolean isKeyword(String s)
    {
        if (keywords != null)
            return keywords.isKeyword(s);
        return false;
    }

    public Expression getExpressionAtDot(Editor editor, boolean exact)
    {
        if (editor.getDot() == null)
            return null;
        final Position begin;
        if (editor.getMark() != null) {
            // Start at beginning of marked block.
            Region r = new Region(editor);
            begin = r.getBegin();
        } else
            begin = editor.getDot();
        Line line = begin.getLine();
        int offset = begin.getOffset();
        if (offset < line.length() && isIdentifierStart(line.charAt(offset))) {
            String identifier = getIdentifier(line, offset);
            if (identifier != null)
                return new Expression(identifier);
        }
        // Try moving to the left.
        while (--offset >= 0) {
            char c = line.charAt(offset);
            if (isIdentifierStart(c)) {
                String identifier = getIdentifier(line, offset);
                if (identifier != null)
                    return new Expression(identifier);
            }
        }
        // Nothing there.  Go back to starting point and try moving right.
        offset = begin.getOffset();
        while (++offset < line.length()) {
            char c = line.charAt(offset);
            if (isIdentifierStart(c)) {
                String identifier = getIdentifier(line, offset);
                if (identifier != null)
                    return new Expression(identifier);
            }
        }
        return null;
    }

    public final String getIdentifier(Position pos)
    {
        return getIdentifier(pos.getLine(), pos.getOffset());
    }

    public String getIdentifier(Line line, int offset)
    {
        final int limit = line.length();
        if (offset < limit) {
            char c = line.charAt(offset);
            if (isIdentifierPart(c)) {
                while (offset > 0) {
                    --offset;
                    c = line.charAt(offset);
                    if (!isIdentifierPart(c)) {
                        ++offset;
                        break;
                    }
                }
                // Now we're looking at the first character of the identifier.
                c = line.charAt(offset);
                if (isIdentifierStart(c)) {
                    FastStringBuffer sb = new FastStringBuffer(c);
                    while (++offset < limit) {
                        c = line.charAt(offset);
                        if (isIdentifierPart(c))
                            sb.append(c);
                        else
                            break;
                    }
                    return sb.toString();
                }
            }
        }
        return null;
    }

    public Position findIdentifierStart(Line line, int offset)
    {
        if (!isIdentifierPart(line.charAt(offset)))
            return null;
        int start = offset;
        while (--offset >= 0) {
            if (!isIdentifierPart(line.charAt(offset)))
                break;
            start = offset;
        }
        return new Position(line, start);
    }
}
