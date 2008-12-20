/*
 * JavaMode.java
 *
 * Copyright (C) 1998-2006 Peter Graves
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
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.undo.CompoundEdit;

public class JavaMode extends AbstractMode implements Constants, Mode
{
  private static final String[] javaConditionals =
    {
      "if",
      "else",
      "do",
      "while",
      "for",
      "switch",
      "try",
      "catch",
      "finally",
      "synchronized"
    };

  private static Mode mode;
  private static Object jdb;

  protected String[] conditionals;

  private JavaMode()
  {
    super(JAVA_MODE, JAVA_MODE_NAME);
    keywords = new Keywords(this);
    conditionals = javaConditionals;
  }

  protected JavaMode(int id, String displayName)
  {
    super(id, displayName);
  }

  // Don't construct the singleton class instance until we actually need it,
  // to avoid unnecessary overhead for the derived classes.
  public static Mode getMode()
  {
    if (mode == null)
      mode = new JavaMode();
    return mode;
  }

  public static final Object getJdb()
  {
    return jdb;
  }

  public static final void setJdb(Object obj)
  {
    jdb = obj;
  }

  public boolean canIndent()
  {
    return true;
  }

  public SyntaxIterator getSyntaxIterator(Position pos)
  {
    return new JavaSyntaxIterator(pos);
  }

  public String getCommentStart()
  {
    return "// ";
  }

  public Formatter getFormatter(Buffer buffer)
  {
    return new JavaFormatter(buffer);
  }

  protected void setKeyMapDefaults(KeyMap km)
  {
    km.mapKey('{', "electricOpenBrace");
    km.mapKey('}', "electricCloseBrace");
    km.mapKey(KeyEvent.VK_TAB, CTRL_MASK, "insertTab");
    km.mapKey(KeyEvent.VK_TAB, 0, "tab");
    km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
    km.mapKey(';', "electricSemi");
    km.mapKey(':', "electricColon");
    km.mapKey('*', "electricStar");
    km.mapKey(KeyEvent.VK_T, CTRL_MASK, "findTag");
    km.mapKey(KeyEvent.VK_PERIOD, ALT_MASK, "findTagAtDot");
    km.mapKey(KeyEvent.VK_COMMA, ALT_MASK, "listMatchingTagsAtDot");
    km.mapKey(KeyEvent.VK_PERIOD, CTRL_MASK | ALT_MASK, "findTagAtDotOtherWindow");
    km.mapKey(')', "closeParen");
    km.mapKey(KeyEvent.VK_I, ALT_MASK, "cycleIndentSize");

    km.mapKey(KeyEvent.VK_9, CTRL_MASK | SHIFT_MASK, "insertParentheses");
    km.mapKey(KeyEvent.VK_0, CTRL_MASK | SHIFT_MASK, "movePastCloseAndReindent");

    km.mapKey(KeyEvent.VK_OPEN_BRACKET, CTRL_MASK | SHIFT_MASK, "insertBraces");
    // Duplicate mapping for 1.4.
    km.mapKey(KeyEvent.VK_BRACELEFT, CTRL_MASK | SHIFT_MASK, "insertBraces");

    km.mapKey(KeyEvent.VK_F12, 0, "wrapComment");

    // Duplicate mapping to support IBM 1.3 for Linux.
    km.mapKey(0xffc9, 0, "wrapComment"); // F12

    km.mapKey(KeyEvent.VK_OPEN_BRACKET, CTRL_MASK, "fold");
    km.mapKey(KeyEvent.VK_CLOSE_BRACKET, CTRL_MASK, "unfold");

    km.mapKey(KeyEvent.VK_F9, 0, "compile");
    km.mapKey(KeyEvent.VK_F9, CTRL_MASK, "recompile");
    km.mapKey(KeyEvent.VK_F1, ALT_MASK, "jdkHelp");
    km.mapKey(KeyEvent.VK_F1, CTRL_MASK, "source");

    // This is the "normal" mapping.
    km.mapKey(KeyEvent.VK_COMMA,  CTRL_MASK | SHIFT_MASK, "htmlInsertTag");
    // The "normal" mapping doesn't work for Linux, but this does.
    km.mapKey(0x7c, CTRL_MASK | SHIFT_MASK, "htmlInsertTag");

    if (Editor.checkExperimental())
      {
        km.mapKey(KeyEvent.VK_SEMICOLON, ALT_MASK, "JavaMode.insertComment");
        km.mapKey(KeyEvent.VK_ENTER, ALT_MASK, "JavaMode.newlineAndIndentForComment");
      }

    if (Platform.isPlatformLinux())
      {
        // Blackdown 1.1.7v3, 1.2pre2, IBM 1.1.8.
        // Duplicate mappings needed for VK_9, VK_0 and VK_OPEN_BRACKET.
        km.mapKey(0x68, CTRL_MASK | SHIFT_MASK, "insertParentheses");
        km.mapKey(0x69, CTRL_MASK | SHIFT_MASK, "movePastCloseAndReindent");
        km.mapKey(0xbb, CTRL_MASK | SHIFT_MASK, "insertBraces");
      }
  }

  public void populateModeMenu(Editor editor, Menu menu)
  {
    menu.add(editor, "Compile...", 'C', "compile");
    menu.add(editor, "Recompile", 'R', "recompile");
    boolean enabled = CompilationCommands.getCompilationBuffer() != null;
    menu.addSeparator();
    menu.add(editor, "Next Error", 'N', "nextError", enabled);
    menu.add(editor, "Previous Error", 'P', "previousError", enabled);
    menu.add(editor, "Show Error Message", 'M', "showMessage", enabled);
    menu.addSeparator();
    MenuItem jdbMenuItem = menu.add(editor, "Debug...", 'D', "jdb");
    if (jdb != null)
      jdbMenuItem.setEnabled(false);
    else
      {
        try
          {
            Class.forName("com.sun.jdi.Bootstrap");
          }
        catch (ClassNotFoundException e)
          {
            jdbMenuItem.setEnabled(false);
          }
      }
  }

  public JPopupMenu getContextMenu(Editor editor)
  {
    final JPopupMenu popup = new JPopupMenu();
    if (jdb != null)
      {
        final Line line = editor.getDotLine();
        if (line != null)
          {
            final Dispatcher dispatcher = editor.getDispatcher();
            JMenuItem menuItem =
              addContextMenuItem("Set breakpoint",
                                 "jdbSetBreakpoint", popup, dispatcher);
            if (line.isBlank() || line.getAnnotation() != null)
              menuItem.setEnabled(false);
            menuItem =
              addContextMenuItem("Delete breakpoint",
                                 "jdbDeleteBreakpoint", popup, dispatcher);
            if (line.getAnnotation() == null)
              menuItem.setEnabled(false);
            menuItem =
              addContextMenuItem("Run to current line",
                                 "jdbRunToCurrentLine", popup, dispatcher);
            if (line.isBlank())
              menuItem.setEnabled(false);
            popup.addSeparator();
          }
      }
    addDefaultContextMenuItems(editor, popup);
    popup.pack();
    return popup;
  }

  public NavigationComponent getSidebarComponent(Editor editor)
  {
    if (getId() == JAVA_MODE)
      {
        View view = editor.getCurrentView();
        if (view == null)
          return null; // Shouldn't happen.
        if (view.getSidebarComponent() == null)
          view.setSidebarComponent(new JavaTree(editor));
        return view.getSidebarComponent();
      }
    // For subclasses...
    return super.getSidebarComponent(editor);
  }

  public Tagger getTagger(SystemBuffer buffer)
  {
    return new JavaTagger(buffer);
  }

  public boolean isTaggable()
  {
    return true;
  }

  public boolean hasQualifiedNames()
  {
    return true;
  }

  public boolean isQualifiedName(String s)
  {
    return s.indexOf('.') >= 0;
  }

  public int getCorrectIndentation(final Line line, final Buffer buffer)
  {
    if (line.flags() == STATE_COMMENT)
      return indentComment(line, buffer);
    final String text = line.trim();
    final char textFirstChar = text.length() > 0 ? text.charAt(0) : 0;
    if (textFirstChar == '}')
      return indentClosingBrace(line, buffer);

    if (textFirstChar == 'c' || textFirstChar == 'd')
      {
        // Does line begin with "case" or "default"?
        final String firstIdentifier = getFirstIdentifier(text);
        if (firstIdentifier.equals("case") || firstIdentifier.equals("default"))
          return indentSwitchLabel(line, buffer);
        // Otherwise fall through...
      }
    else if (textFirstChar == 'e')
      {
        // Does line begin with "else" or "elseif"?
        final String firstIdentifier = getFirstIdentifier(text);
        if (firstIdentifier.equals("else") || firstIdentifier.equals("elseif"))
          {
            Position match = matchElse(new Position(line, 0));
            if (match != null)
              return buffer.getIndentation(match.getLine());
          }
        // Otherwise fall through...
      }
    else if (textFirstChar == '"')
      {
        // If quoted text starts in column 0, leave it alone.
        if (line.charAt(0) == '"')
          return 0;
      }

    Position paren = findEnclosingParen(new Position(line, 0));
    if (paren != null)
      return indentInParen(paren, buffer);

    final Line model = findModel(line);
    if (model == null)
      return 0;

    final int indentSize = buffer.getIndentSize();

    final String firstIdentifier = getFirstIdentifier(text);
    if (firstIdentifier.equals("throws") ||
        firstIdentifier.equals("implements"))
      {
        Position pos = findBeginningOfStatement(new Position(model, 0));
        return buffer.getIndentation(pos.getLine()) + indentSize;
      }

    final String modelText = trimSyntacticWhitespace(model.getText());

    // Model line can't be blank, so this is safe.
    final char modelLastChar = modelText.charAt(modelText.length() - 1);

    if (modelLastChar == '{')
      return indentAfterOpeningBrace(model, modelText, buffer);

    if (modelLastChar == ')')
      return indentAfterCloseParen(model, text, textFirstChar, buffer);

    final String lastIdentifier = getLastIdentifier(modelText);
    if (lastIdentifier != null && lastIdentifier.equals("else"))
      return indentAfterElse(model, text, textFirstChar, buffer);

    final char modelFirstChar = modelText.charAt(0);
    if (modelFirstChar == 'c' || modelFirstChar == 'd')
      {
        final String modelFirstIdentifier = getFirstIdentifier(modelText);
        if (modelFirstIdentifier.equals("case") ||
            modelFirstIdentifier.equals("default"))
          return indentAfterSwitchLabel(model, text, textFirstChar, buffer);
        // Otherwise fall through...
      }

    final int indent = getIndentationOfEnclosingScope(line, buffer);

    if (textFirstChar == '{')
      {
        if (buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE))
          {
            // Never indent before the opening brace of a class or method.
            if (!isOpeningBraceOfClassOrMethod(line))
              return indent + indentSize;
          }
        return indent;
      }

    if (textFirstChar == '=')
      return indent + indentSize;

    if (modelLastChar == ',')
      {
        if (buffer.getModeId() == CPP_MODE && modelFirstChar == ':')
          {
            // Model line is start of member initialization list, current
            // line is continuation.
            return indent + 2;
          }
        if (isInArrayInitializer(line))
          return indent;
        // Otherwise it's a continuation line.
        return indent + indentSize;
      }

    // Check for continuation line.
    if (isContinued(modelText, modelLastChar))
      return indent + indentSize;

    return indent;
  }

  private final int indentComment(Line line, Buffer buffer)
  {
    final Line model = findModel(line);
    if (model == null)
      return 0;
    int indent = buffer.getIndentation(model);
    if (model.trim().startsWith("/*"))
      if (line.trim().startsWith("*"))
        return indent+1;
    return indent;
  }

  protected int indentClosingBrace(Line line, Buffer buffer)
  {
    Position pos = matchClosingBrace(new Position(line, 0));
    if (isOpeningBraceOfClassOrMethod(pos.getLine()))
      pos = findBeginningOfStatement(pos);
    else if (!pos.getLine().trim().startsWith("{"))
      pos = findPreviousConditional(pos);
    return buffer.getIndentation(pos.getLine());
  }

  private final int indentSwitchLabel(Line line, Buffer buffer)
  {
    Line switchLine = findSwitch(line);
    if (switchLine != null)
      return buffer.getIndentation(switchLine) + buffer.getIndentSize();
    return 0;
  }

  private final int indentInParen(Position posParen, Buffer buffer)
  {
    final Line line = posParen.getLine();
    if (line.trim().endsWith("(") || !buffer.getBooleanProperty(Property.LINEUP_ARGLIST))
      return buffer.getIndentation(line) + buffer.getIndentSize();
    final int limit = line.length();
    int offset = posParen.getOffset();
    do
      {
        ++offset;
      }
    while (offset < limit && line.charAt(offset) <= ' ');
    if (offset <= limit)
      return buffer.getCol(line, offset);
    return 0;
  }

  private final int indentAfterOpeningBrace(Line model, String modelText,
                                            Buffer buffer)
  {
    final int indentSize = buffer.getIndentSize();
    if (isOpeningBraceOfClassOrMethod(model))
      {
        Position pos = findBeginningOfStatement(new Position(model, 0));
        int indent = buffer.getIndentation(pos.getLine());
        if (buffer.getBooleanProperty(Property.INDENT_AFTER_OPENING_BRACE))
          indent += indentSize;
        return indent;
      }
    Position pos = new Position(model, model.length()-1);
    if (modelText.charAt(0) != '{')
      pos = findPreviousConditional(pos);
    int indent = buffer.getIndentation(pos.getLine());
    if (buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE))
      indent += indentSize;
    final boolean indentBeforeBrace =
      buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE);
    if (indentBeforeBrace && pos.getLine() != model)
      indent += indentSize;
    return indent;
  }

  private final int indentAfterElse(Line model, String text,
                                    char textFirstChar, Buffer buffer)
  {
    int indent = buffer.getIndentation(model);
    final boolean indentBeforeBrace =
      buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE);
    if (indentBeforeBrace || textFirstChar != '{')
      return indent + buffer.getIndentSize();
    else
      return indent;
  }

  private final int indentAfterSwitchLabel(Line model, String text,
                                           char textFirstChar, Buffer buffer)
  {
    int indent = buffer.getIndentation(model);
    final boolean indentBeforeBrace =
      buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE);
    if (indentBeforeBrace || textFirstChar != '{')
      return indent + buffer.getIndentSize();
    else
      return indent;
  }

  private final int indentAfterCloseParen(Line model, String text,
                                          char textFirstChar, Buffer buffer)
  {
    // Find matching '('.
    SyntaxIterator it = getSyntaxIterator(new Position(model, model.length()));
    char c;
    do
      {
        c = it.prevChar();
      }
    while (c != SyntaxIterator.DONE && c != ')');
    Position pos = it.getPosition();
    pos = matchClosingParen(pos);
    boolean indent = false;
    final String s = getIdentifierBefore(pos);
    final String[] indentAfter = {"if", "while", "for", "switch", "catch"};
    if (Utilities.isOneOf(s, indentAfter))
      {
        indent = true;
      }
    else if (buffer.getModeId() == JAVA_MODE)
      {
        if (s.equals("synchronized"))
          indent = true;
      }
    else if (buffer.getModeId() == PHP_MODE)
      {
        if (s.equals("elseif") || s.equals("foreach"))
          indent = true;
      }
    if (indent)
      {
        int modelIndent = buffer.getIndentation(pos.getLine());
        if (textFirstChar != '{' ||
            buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE))
          return modelIndent + buffer.getIndentSize();
        else
          return modelIndent;
      }
    if (buffer.getModeId() == JAVA_MODE)
      {
        RE re = new UncheckedRE("\\s+new\\s+");
        if (re.getMatch(pos.getLine().getText().substring(0, pos.getOffset())) != null)
          indent = true;
      }
    int modelIndent =
      buffer.getIndentation(findBeginningOfStatement(pos).getLine());
    if (indent && (textFirstChar != '{' || buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE)))
      return modelIndent + buffer.getIndentSize();
    else
      return modelIndent;
  }

  private final int getIndentationOfEnclosingScope(Line line, Buffer buffer)
  {
    SyntaxIterator it = getSyntaxIterator(new Position(line, 0));
loop:
    while (true)
      {
        switch (it.prevChar())
          {
          case ')':
            {
              Position pos = matchClosingParen(it.getPosition());
              it = getSyntaxIterator(pos);
              break;
            }
          case '}':
            {
              Position pos = matchClosingBrace(it.getPosition());
              if (pos.getOffset() == 0)
                return 0;
              pos = findBeginningOfStatement(pos);
              return buffer.getIndentation(pos.getLine());
            }
          case '{':
            {
              Line model = it.getLine();
              String modelText = trimSyntacticWhitespace(model.getText());
              if (modelText.equals("{"))
                return buffer.getIndentation(model) + buffer.getIndentSize();
              return indentAfterOpeningBrace(model, modelText, buffer);
            }
          case ':':
            {
              String firstIdentifier = getFirstIdentifier(it.getLine());
              if (firstIdentifier.equals("case") || firstIdentifier.equals("default"))
                return buffer.getIndentation(it.getLine()) + buffer.getIndentSize();
              String trim = it.getLine().trim();
              if (trim.startsWith(": ") || trim.startsWith(":\t"))
                continue;
              if (trim.startsWith("public:") || trim.startsWith("private:") || trim.startsWith("protected:"))
                return buffer.getIndentation(it.getLine()) + buffer.getIndentSize();
              break;
            }
          case SyntaxIterator.DONE:
            return 0;
          }
      }
  }

  private boolean isInArrayInitializer(Line line)
  {
    // Find matching opening brace.
    Position match = matchClosingBrace(new Position(line, 0));
    SyntaxIterator it = getSyntaxIterator(match);
    char c;
    do
      {
        c = it.prevChar();
      }
    while (c != SyntaxIterator.DONE && Character.isWhitespace(c));
    if (c == '=' || c == ']')
      return true;
    return false;
  }

  protected static Line findModel(Line line)
  {
    Line model = line.previous();
    if (line.flags() == STATE_COMMENT)
      {
        // Any non-blank line is an acceptable model.
        while (model != null && model.isBlank())
          model = model.previous();
      }
    else
      {
        while (model != null)
          {
            if (isAcceptableModel(model))
              break; // Found an acceptable model.
            else
              model = model.previous();
          }
      }
    return model;
  }

  private static final boolean isAcceptableModel(Line line)
  {
    int flags = line.flags();
    if (flags == STATE_COMMENT || flags == STATE_QUOTE)
      return false;
    if (line.isBlank())
      return false;
    String trim = line.trim();
    char firstChar = trim.charAt(0);
    if (firstChar == '/')
      {
        if (trim.length() > 1 && trim.charAt(1) =='/')
          return false;
      }
    else if (firstChar == '#')
      return false;
    else if (firstChar == '=')
      return false;
    else if (firstChar == ':')
      {
        if (trim.startsWith(": ") || trim.startsWith(":\t"))
          return false;
      }
    String s = trimSyntacticWhitespace(line.getText());
    if (s.length() == 0)
      return false;
    return true;
  }

  // Returns true if line contains opening brace of class or method.
  private boolean isOpeningBraceOfClassOrMethod(Line line)
  {
    if (line.length() == 0)
      return false;
    String text = trimSyntacticWhitespace(line.getText());
    if (!text.endsWith("{"))
      return false;
    if (text.equals("{"))
      {
        Line modelLine = findModel(line);
        if (modelLine == null)
          return true;
        Position beginningOfStatement =
          findBeginningOfStatement(new Position(modelLine, 0));
        text = beginningOfStatement.getLine().trim();
      }
    else
      text = text.substring(0, text.length()-1).trim();
    if (text.indexOf('=') >= 0)
      return false;
    final String firstIdentifier = getFirstIdentifier(text);
    if (Utilities.isOneOf(firstIdentifier, conditionals))
      return false;
    if (firstIdentifier.equals("case") || firstIdentifier.equals("default"))
      return false;
    return true;
  }

  protected String getFirstIdentifier(String s)
  {
    return Utilities.getFirstIdentifier(s, this);
  }

  protected final String getFirstIdentifier(Line line)
  {
    return getFirstIdentifier(line.trim());
  }

  protected final String getLastIdentifier(String s)
  {
    int i = s.length()-1;
    while (i >= 0)
      {
        if (isIdentifierPart(s.charAt(i)))
          {
            if (i > 0)
              --i;
            else
              break;
          }
        else
          {
            ++i;
            break;
          }
      }
    if (i >= 0 && i < s.length())
      return s.substring(i);
    return null;
  }

  private final String getIdentifierBefore(Position pos)
  {
    while (pos.prev())
      if (!Character.isWhitespace(pos.getChar()))
        break;
    while (isIdentifierPart(pos.getChar()) && pos.prev())
      ;
    while (!isIdentifierStart(pos.getChar()) && pos.next())
      ;
    return pos.getIdentifier(this);
  }

  protected final Position matchClosingBrace(Position start)
  {
    SyntaxIterator it = getSyntaxIterator(start);
    int count = 1;
    while (true)
      {
        switch (it.prevChar())
          {
          case '}':
            ++count;
            break;
          case '{':
            --count;
            if (count == 0)
              return it.getPosition();
            break;
          case SyntaxIterator.DONE:
            return it.getPosition();
          default:
            break;
          }
      }
  }

  protected final Position matchClosingParen(Position start)
  {
    SyntaxIterator it = getSyntaxIterator(start);
    int count = 1;
    while (true)
      {
        switch (it.prevChar())
          {
          case ')':
            ++count;
            break;
          case '(':
            --count;
            if (count == 0)
              return it.getPosition();
            break;
          case SyntaxIterator.DONE:
            return it.getPosition();
          default:
            break;
          }
      }
  }

  // Scan backwards from starting position, looking for unmatched opening
  // parenthesis.
  protected Position findEnclosingParen(Position start)
  {
    SyntaxIterator it = getSyntaxIterator(start);
    int parenCount = 0;
    int braceCount = 0;
    boolean seenBrace = false;
    while (true)
      {
        switch (it.prevChar())
          {
          case '{':
            if (braceCount == 0)
              return null; // Found unmatched '{'.
            --braceCount;
            seenBrace = true;
            break;
          case '}':
            ++braceCount;
            seenBrace = true;
            break;
          case ';':
            if (seenBrace)
              return null;
            break;
          case ')':
            ++parenCount;
            break;
          case '(':
            if (parenCount == 0)
              return it.getPosition(); // Found unmatched '('.
            --parenCount;
            break;
          case SyntaxIterator.DONE:
            return null;
          default:
            break;
          }
      }
  }

  private final Position findEnclosingBrace(Position start)
  {
    SyntaxIterator it = getSyntaxIterator(start);
    int count = 0;
    while (true)
      {
        switch (it.prevChar())
          {
          case '}':
            ++count;
            break;
          case '{':
            if (count == 0)
              return it.getPosition(); // Found unmatched '{'.
            --count;
            break;
          case SyntaxIterator.DONE:
            return null;
          default:
            break;
          }
      }
  }

  // Scan backwards from line, looking for the start of a switch statement.
  protected final Line findSwitch(Line line)
  {
    Position pos = findEnclosingBrace(new Position(line, 0));
    if (pos != null)
      {
        line = pos.getLine();
        do
          {
            String s = getFirstIdentifier(line);
            if (s.equals("switch"))
              return line;
          }
        while ((line = line.previous()) != null);
      }
    return null;
  }

  private Position matchElse(Position start)
  {
    SyntaxIterator it = getSyntaxIterator(start);
    int count = 1;
    char c;
    while ((c = it.prevChar()) != SyntaxIterator.DONE)
      {
        if (c == '}')
          {
            Position match = matchClosingBrace(it.getPosition());
            it = getSyntaxIterator(match);
            continue;
          }
        if (c == 'e')
          {
            Position pos = it.getPosition();
            if (pos.getIdentifier(this).equals("else"))
              {
                ++count;
                continue;
              }
          }
        if (c == 'i')
          {
            Position pos = it.getPosition();
            if (pos.getIdentifier(this).equals("if"))
              {
                --count;
                if (count == 0)
                  return pos;
                continue;
              }
          }
      }
    return null;
  }

  public Position findBeginningOfStatement(Position start)
  {
    Position pos = new Position(start);

    final Position posParen = findEnclosingParen(pos);

    if (posParen != null)
      pos = posParen;

    final String trim = trimSyntacticWhitespace(pos.getLine().getText());
    final String lastIdentifier = getLastIdentifier(trim);
    if (lastIdentifier != null && lastIdentifier.equals("else"))
      return new Position(pos.getLine(), 0); // BUG!! This is clearly wrong!
    final String firstIdentifier = getFirstIdentifier(trim);
    if (firstIdentifier != null &&
        (firstIdentifier.equals("case") || firstIdentifier.equals("default")))
      return new Position(pos.getLine(), 0);

    while (pos.getLine().trim().startsWith("}") && pos.getPreviousLine() != null)
      {
        pos.moveTo(pos.getPreviousLine(), pos.getPreviousLine().length());
        pos = matchClosingBrace(pos);
      }

    SyntaxIterator it = getSyntaxIterator(pos);
    boolean inParen = false;
    int count = 0;
    while (true)
      {
        char c = it.prevChar();
        if (c == SyntaxIterator.DONE)
          return it.getPosition();
        if (inParen)
          {
            if (c == ')')
              ++count;
            else if (c == '(')
              {
                --count;
                if (count == 0) // Found it!
                  inParen = false;
              }
            continue;
          }
        if (c == ')')
          {
            inParen = true;
            count = 1;
            continue;
          }
        if (c == '{')
          {
            // If previous non-whitespace char is '=' then this is an array
            // initializer.
            pos = it.getPosition(); // Save position.
            char ch;
            do
              {
                ch = it.prevChar();
              }
            while (ch != SyntaxIterator.DONE && Character.isWhitespace(ch));
            if (ch == '=' || ch == ']')
              {
                // It is an array initializer.
                pos = it.getPosition();
                pos.moveTo(pos.getLine(), 0);
                return pos;
              }
            // Not an array initializer.
            it = getSyntaxIterator(pos); // Restore position.
            // Fall through...
          }
        if (";{}:".indexOf(c) >= 0)
          {
            if (c == ':')
              {
                String s = it.getLine().trim();
                if (s.startsWith(": ") || s.startsWith(":\t"))
                  continue;
              }
            do
              {
                c = it.nextChar();
              }
            while (c != SyntaxIterator.DONE && Character.isWhitespace(c));
            pos = it.getPosition();
            pos.setOffset(0);
            return pos;
          }
      }
  }

  public Position findPreviousConditional(Position start)
  {
    Position pos = start.copy();
    Position posParen = findEnclosingParen(pos);
    if (posParen != null)
      pos = posParen;
    while (pos.getLine().trim().startsWith("}") && pos.getPreviousLine() != null)
      {
        pos.moveTo(pos.getPreviousLine(), pos.getPreviousLine().length());
        pos = matchClosingBrace(pos);
      }
    while (true)
      {
        if (pos.getLine().flags() != STATE_COMMENT)
          {
            String text = pos.getLine().trim();
            // Handle "} else".
            if (text.startsWith("}"))
              text = text.substring(1).trim();
            String firstIdentifier = getFirstIdentifier(text);
            if (Utilities.isOneOf(firstIdentifier, conditionals))
              {
                pos.setOffset(pos.getLine().getText().indexOf(firstIdentifier));
                return pos;
              }
          }
        Line previousLine = pos.getPreviousLine();
        if (previousLine == null)
          return new Position(pos.getLine(), 0);
        pos.moveTo(previousLine, previousLine.length());
        if (pos.getLine().flags() == STATE_COMMENT)
          continue;
        posParen = findEnclosingParen(pos);
        if (posParen != null)
          {
            pos = posParen;
            continue;
          }
        String s = trimSyntacticWhitespace(pos.getLine().getText());
        if (s.length() > 0)
          {
            if (s.charAt(0) == '#') // C preprocessor.
              break;
            char lastChar = s.charAt(s.length()-1);
            if (lastChar == ';' || lastChar == '{' || lastChar == '}' ||
                lastChar == ':')
              break;
          }
      }
    // No conditional found.
    return start;
  }

  protected final boolean isContinued(String text, char lastChar)
  {
    switch (lastChar)
      {
      case '+':
        return !text.endsWith("++");
      case '/':
        return !text.endsWith("//");
      case '=':
        return (!text.endsWith("==") && !text.endsWith("!="));
      case ',':
        return true;
      case '.':
        return true;
      case '|':
        return text.endsWith("||");
      case '&':
        return text.endsWith("&&");
      default:
        return false;
      }
  }

  // Replaces syntactic whitespace (quotes and comments) with actual space
  // characters, then returns trimmed string.
  protected static String trimSyntacticWhitespace(String s)
  {
    JavaSyntaxIterator it = new JavaSyntaxIterator(null);
    return new String(it.hideSyntacticWhitespace(s)).trim();
  }

  public boolean isIdentifierStart(char c)
  {
    return Character.isJavaIdentifierStart(c);
  }

  public boolean isIdentifierPart(char c)
  {
    return Character.isJavaIdentifierPart(c);
  }

  public boolean isInComment(Buffer buffer, Position pos)
  {
    if (buffer == null || pos == null)
      {
        Debug.bug();
        return false;
      }
    final Line line = pos.getLine();
    final String text = line.getText();
    if (text == null)
      return false;
    final char[] chars = text.toCharArray();
    final int offset = pos.getOffset();
    if (buffer.needsParsing())
      buffer.getFormatter().parseBuffer();
    int state = line.flags();
    final int length = chars.length;
    for (int i = 0; i < length; i++)
      {
        if (i == offset)
          return state == STATE_COMMENT;
        char c = chars[i];
        if (c == '\\' && i < length-1)
          {
            // Escape character.
            continue;
          }
        if (state == STATE_QUOTE)
          {
            if (c == '"')
              state = STATE_NEUTRAL;
            continue;
          }
        if (state == STATE_SINGLEQUOTE)
          {
            if (c == '\'')
              state = STATE_NEUTRAL;
            continue;
          }
        if (state == STATE_COMMENT)
          {
            if (c == '*' && i < length-1 && chars[i+1] == '/')
              {
                // /* */ comment ending
                state = STATE_NEUTRAL;
              }
            continue;
          }
        // Reaching here, STATE_NEUTRAL...
        if (c == '"')
          {
            state = STATE_QUOTE;
            continue;
          }
        if (c == '\'')
          {
            state = STATE_SINGLEQUOTE;
            continue;
          }
        if (c == '/')
          {
            if (i < length-1)
              {
                if (chars[i+1] == '*')
                  {
                    // /* */ comment starting
                    state = STATE_COMMENT;
                    continue;
                  }
                if (chars[i+1] == '/')
                  {
                    // "//" comment starting
                    return true;
                  }
              }
          }
      }
    return state == STATE_COMMENT;
  }

  public boolean isCommentLine(Line line)
  {
    return line.trim().startsWith("//");
  }

  public static void insertComment()
  {
    if (!Editor.checkExperimental())
      return;
    final Editor editor = Editor.currentEditor();
    String toBeInserted =
      Editor.preferences().getStringProperty(Property.JAVA_MODE_INSERT_COMMENT_TEXT);
    if (toBeInserted == null)
      toBeInserted = "/**\\n * |\\n */";
    Position caretPos = null;
    CompoundEdit compoundEdit = editor.beginCompoundEdit();
    final int limit = toBeInserted.length();
    for (int i = 0; i < limit; i++)
      {
        char c = toBeInserted.charAt(i);
        if (c == '|')
          {
            caretPos = new Position(editor.getDot());
            continue;
          }
        if (c == '\\' && i < limit-1)
          {
            c = toBeInserted.charAt(++i);
            if (c == 'n')
              {
                editor.newlineAndIndent();
                continue;
              }
            // Otherwise fall through...
          }
        editor.insertChar(c);
      }
    if (caretPos != null)
      editor.moveDotTo(caretPos);
    editor.moveCaretToDotCol();
    editor.endCompoundEdit(compoundEdit);
    editor.getFormatter().parseBuffer();
  }

  public static void newlineAndIndentForComment()
  {
    final Editor editor = Editor.currentEditor();
    if (!editor.checkReadOnly())
      return;
    final Buffer buffer  = editor.getBuffer();
    final Display display = editor.getDisplay();
    String commentPrefix = null;
    String s = editor.getDotLine().getText().trim();
    int flags = editor.getDotLine().flags();
    if (flags == STATE_COMMENT)
      {
        if (s.startsWith("*") && !s.endsWith("*/"))
          commentPrefix = "* ";
      }
    else
      {
        // Look for start of comment on current line.
        if (s.startsWith("/*") && !s.endsWith("*/"))
          commentPrefix = "* ";
        else if (s.startsWith("//"))
          commentPrefix = "// ";
      }
    if (commentPrefix == null)
      {
        // No special handling necessary.
        editor.newlineAndIndent();
        return;
      }
    CompoundEdit compoundEdit = buffer.beginCompoundEdit();
    if (editor.getMark() != null)
      editor.deleteRegion();
    editor.addUndo(SimpleEdit.INSERT_LINE_SEP);
    editor.insertLineSeparator();
    // Trim leading whitespace. (This code actually trims trailing
    // whitespace too.)
    editor.addUndo(SimpleEdit.LINE_EDIT);
    editor.getDotLine().setText(editor.getDotLine().getText().trim());
    // Insert the comment prefix at the start of the new line.
    editor.addUndo(SimpleEdit.INSERT_STRING);
    editor.insertStringInternal(commentPrefix);
    // Make the indentation code think we're still in a multi-line
    // comment, if we were in one before.
    editor.getDotLine().setFlags(flags);
    editor.indentLine();
    // We want dot to end up right after the comment prefix.
    editor.moveDotToIndentation();
    editor.getDot().skip(commentPrefix.length());
    display.moveCaretToDotCol();
    buffer.endCompoundEdit(compoundEdit);
  }

  public String getToolTipText(Editor editor, MouseEvent e)
  {
    if (editor.getModeId() == JAVA_MODE)
      {
        if (editor.getBuffer().getBooleanProperty(Property.ENABLE_TOOL_TIPS))
          {
            Position pos =
              editor.getDisplay().positionFromPoint(e.getPoint());
            if (pos != null)
              {
                final String name = getQualifiedName(pos);
                if (name != null)
                  {
                    JavaContext context = new JavaContext(editor);
                    context.parseContext(pos);
                    JavaVariable var = context.findDeclaration(name);
                    if (var != null)
                      return var.toString();
                  }
              }
          }
      }
    return null;
  }

  private String getQualifiedName(Position pos)
  {
    Line line = pos.getLine();
    int offset = pos.getOffset();
    final int limit = line.length();
    if (offset < limit)
      {
        char c = line.charAt(offset);
        if (isIdentifierPart(c))
          {
            while (offset > 0)
              {
                --offset;
                c = line.charAt(offset);
                if (!isIdentifierPart(c) && c != '.')
                  {
                    ++offset;
                    break;
                  }
              }
            // Now we're looking at the first character of the identifier.
            c = line.charAt(offset);
            if (isIdentifierStart(c))
              {
                FastStringBuffer sb = new FastStringBuffer();
                sb.append(c);
                while (++offset < limit)
                  {
                    c = line.charAt(offset);
                    if (isIdentifierPart(c))
                      {
                        sb.append(c);
                      }
                    else if (c == '.' && offset < pos.getOffset())
                      {
                        // We don't want to go beyond the end of the
                        // simple name at pos.
                        sb.append(c);
                      }
                    else
                      break;
                  }
                return sb.toString();
              }
          }
      }
    return null;
  }

  public Expression getExpressionAtDot(final Editor editor, final boolean exact)
  {
    if (editor.getModeId() == OBJC_MODE)
      return super.getExpressionAtDot(editor, exact);
    if (editor.getDot() == null)
      return null;
    Position begin;
    if (editor.getMark() != null)
      {
        // Start at beginning of marked block.
        Region r = new Region(editor);
        begin = r.getBegin();
      }
    else
      begin = editor.getDot();
    final Line line = begin.getLine();
    final int offset = begin.getOffset();
    Position posExpr = null;
    if (exact)
      {
        if (offset < line.length() && isIdentifierPart(line.charAt(offset)))
          posExpr = findIdentifierStart(line, offset);
        if (posExpr == null)
          return null;
      }
    if (posExpr == null)
      {
        // Not exact, or no identifier there.  Try to be smart.
        RE re = new UncheckedRE("([A-Za-z_$]+[A-Za-z_$0-9]*)\\s*\\(");
        final String text = editor.getDotLine().getText();
        int index = 0;
        REMatch  match;
        while ((match = re.getMatch(text, index)) != null)
          {
            String identifier = match.toString(1);
            if (!isKeyword(identifier))
              {
                posExpr = new Position(line, match.getStartIndex());
                // If we've found a match to the right of dot, we're done.
                if (match.getEndIndex() > offset)
                  break;
              }
            index = match.getEndIndex();
          }
      }
    if (posExpr == null)
      {
        // Smart didn't help.  Go back to exact.
        if (offset < line.length() && isIdentifierStart(line.charAt(offset)))
          posExpr = findIdentifierStart(line, offset);
      }
    if (posExpr == null)
      return null;
    Position pos = posExpr.copy();
    // Gather up method name.
    FastStringBuffer sb = new FastStringBuffer();
    while (true)
      {
        char c = pos.getChar();
        if (!isIdentifierPart(c))
          break;
        sb.append(c);
        if (!pos.next())
          break;
      }
    String name = sb.toString().trim();
    // Skip whitespace (if any) between identifier and '('.
    while (true)
      {
        char c = pos.getChar();
        if (!Character.isWhitespace(c))
          break;
        if (!pos.next())
          break;
      }
    final int arity;
    if (editor.getModeId() == JAVASCRIPT_MODE)
      arity = -1; // Can't trust arity in JavaScript.
    else
      arity = getArity(editor, pos);
    if (arity >= 0)
      return new JavaExpression(name, arity);
    else
      return new JavaExpression(name, arity, TAG_UNKNOWN);
  }

  private int getArity(Editor editor, Position pos)
  {
    if (pos.getChar() != '(')
      return -1;
    if (!pos.next())
      return -1;
    final Position start = pos.copy();
    int parenCount = 0;
    int arity = 0;
    char quoteChar = '\0';
    boolean inQuote = false;
    while (!pos.atEnd())
      {
        char c = pos.getChar();
        if (inQuote)
          {
            if (c == quoteChar)
              inQuote = false;
            pos.next();
            continue;
          }
        // Not in a quoted string.
        if (c == '"' || c == '\'')
          {
            inQuote = true;
            quoteChar = c;
            pos.next();
            continue;
          }
        if (c == ',')
          {
            if (parenCount == 0) // Top level.
              ++arity;
            pos.next();
            continue;
          }
        if (c == '(')
          {
            ++parenCount;
            pos.next();
            continue;
          }
        if (c == ')')
          {
            --parenCount;
            if (parenCount < 0)
              {
                // Closing paren, done.
                if (arity == 0)
                  {
                    // We haven't seen a comma.
                    Region r = new Region(editor.getBuffer(), start, pos);
                    if (r.toString().trim().length() > 0)
                      arity = 1;
                  }
                else
                  ++arity;
                return arity;
              }
            pos.next();
            continue;
          }
        pos.next();
      }
    return -1;
  }
}
