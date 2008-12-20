/*
 * JavaTagger.java
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

import java.util.Stack;
import java.util.ArrayList;

public class JavaTagger extends Tagger implements Constants
{
  // States.
  private static final int NEUTRAL            =  0;
  private static final int INTERFACE_NAME     =  1;
  private static final int CLASS_NAME         =  2;
  private static final int CLASS_PROLOG       =  3; // After name, before opening brace.
  private static final int EXTENDS            =  4;
  private static final int IMPLEMENTS         =  5;
  private static final int METHOD_NAME        =  6;
  private static final int METHOD_PROLOG      =  7;
  private static final int STATIC_INITIALIZER =  8;
  private static final int NEW                =  9;
  private static final int FIELD_INITIALIZER  = 10; // After '=' in a variable declaration.

  protected Position pos;
  protected String token;
  protected Position tokenStart;

  private ArrayList tags;
  private JavaClass currentClass;
  private int visibility; // TAG_PUBLIC, TAG_PRIVATE, TAG_PROTECTED

  public JavaTagger(SystemBuffer buffer)
  {
    super(buffer);
  }

  public synchronized void run()
  {
    pos = new Position(buffer.getFirstLine(), 0);
    token = null;
    tokenStart = null;
    tags = new ArrayList();
    currentClass = null;
    visibility = 0;
    final boolean beanShell = buffer.getModeId() == BEANSHELL_MODE;
    final boolean javaScript = buffer.getModeId() == JAVASCRIPT_MODE;
    final Stack stack = new Stack();
    int state = NEUTRAL;
    while (!pos.atEnd())
      {
        char c = pos.getChar();
        if (Character.isWhitespace(c))
          {
            pos.skipWhitespace();
            continue;
          }
        if (c == '\'' || c == '"')
          {
            pos.skipQuote();
            continue;
          }
        if (pos.lookingAt("/*"))
          {
            skipComment(pos);
            continue;
          }
        if (pos.lookingAt("//"))
          {
            LocalTag tag = checkForExplicitTag(pos, JAVA_MODE);
            if (tag instanceof JavaTag)
              tags.add(tag);
            skipSingleLineComment(pos);
            continue;
          }
        if (state == STATIC_INITIALIZER)
          {
            if (c == '{')
              {
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            // Anything else...
            state = NEUTRAL;
            // Fall through...
          }
        if (state == METHOD_NAME)
          {
            if (c == '{')
              {
                addTag(TAG_METHOD);
                visibility = 0;
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            if (c == ';')
              {
                if (beanShell)
                  {
                    ; // It's just a function call.
                  }
                else
                  {
                    // Abstract or native method.
                    addTag(TAG_METHOD);
                    visibility = 0;
                  }
                state = NEUTRAL;
                pos.next();
                continue;
              }
            if (pos.lookingAt("throws"))
              {
                addTag(TAG_METHOD);
                // Set token to null here so we don't try to add this method again.
                token = null;
                visibility = 0;
                state = METHOD_PROLOG;
                pos.skip(6); // Skip over "throws".
                continue;
              }
            state = NEUTRAL;
            // Fall through...
          }
        if (state == CLASS_PROLOG)
          {
            if (pos.lookingAt("extends"))
              {
                state = EXTENDS;
                pos.skip(7); // Skip over "extends".
                continue;
              }
            if (pos.lookingAt("implements"))
              {
                state = IMPLEMENTS;
                pos.skip(10);
                continue;
              }
            if (c == '{')
              state = NEUTRAL;
            pos.next();
            continue;
          }
        if (state == IMPLEMENTS)
          {
            if (c == '{')
              {
                state = NEUTRAL;
                pos.next();
                continue;
              }
            // else fall through...
          }
        if (state == METHOD_PROLOG)
          {
            // Wait for a semicolon or the opening brace of the method body.
            if (c == '{')
              {
                skipBrace();
                state = NEUTRAL;
              }
            else if (c == ';')
              {
                // Abstract or native method. If token is null, we've
                // added it already.
                if (token != null)
                  {
                    addTag(TAG_METHOD);
                    visibility = 0;
                  }
                state = NEUTRAL;
                pos.next();
              }
            else
              pos.next();
            continue;
          }
        if (state == NEW)
          {
            if (c == '(')
              {
                skipParen();
                continue;
              }
            else if (c == '{')
              {
                skipBrace();
                continue;
              }
            else if (c == ';')
              state = NEUTRAL;
            pos.next();
            continue;
          }
        if (state == FIELD_INITIALIZER)
          {
            if (c == '(')
              {
                skipParen();
                continue;
              }
            if (c == '{')
              {
                skipBrace();
                if (javaScript)
                  state = NEUTRAL;
                continue;
              }
            if (c == ',')
              state = NEUTRAL;
            else if (c == ';')
              {
                state = NEUTRAL;
                visibility = 0;
              }
            pos.next();
            continue;
          }
        if (state == IMPLEMENTS)
          {
            if (c == ',')
              {
                pos.next();
                continue;
              }
          }
        if (c == '}')
          {
            if (!stack.empty())
              currentClass = (JavaClass) stack.pop();
            else
              currentClass = null;
            pos.next();
            continue;
          }
        if (Character.isJavaIdentifierStart(c))
          {
            // If we're in an "extends" or "implements" clause, the token
            // may be a canonical class name (like "java.lang.String").
            // Otherwise the token is a simple identifier.
            gatherToken(state == EXTENDS || state == IMPLEMENTS);
            if (state == INTERFACE_NAME)
              {
                if (currentClass != null)
                  stack.push(currentClass);
                final JavaClass parentClass = currentClass;
                currentClass = new JavaClass(token, TAG_INTERFACE);
                state = CLASS_PROLOG;
                // Add a tag for the class itself.
                tags.add(new JavaTag("interface ".concat(token),
                                     tokenStart,TAG_INTERFACE, visibility, parentClass));
                visibility = 0;
              }
            else if (state == CLASS_NAME)
              {
                if (currentClass != null)
                  stack.push(currentClass);
                final JavaClass parentClass = currentClass;
                currentClass = new JavaClass(token, TAG_CLASS);
                state = CLASS_PROLOG;
                // Add a tag for the class itself.
                tags.add(new JavaTag("class ".concat(token), tokenStart,
                                     TAG_CLASS, visibility, parentClass));
                visibility = 0;
              }
            else if (state == EXTENDS)
              {
                tags.add(new JavaTag(token, tokenStart, TAG_EXTENDS,
                                     visibility, currentClass));
                state = CLASS_PROLOG;
              }
            else if (state == IMPLEMENTS)
              tags.add(new JavaTag(token, tokenStart, TAG_IMPLEMENTS,
                                   visibility, currentClass));
            else if (token.equals("package") || token.equals("import"))
              skipSemi();
            else if (token.equals("interface"))
              state = INTERFACE_NAME;
            else if (token.equals("class"))
              state = CLASS_NAME;
            else if (token.equals("static"))
              state = STATIC_INITIALIZER;
            else if (token.equals("new"))
              // Don't be confused by lines like "Runnable r = new Runnable() { ... };"
              state = NEW;
            else if (token.equals("public"))
              visibility |= TAG_PUBLIC;
            else if (token.equals("protected"))
              visibility |= TAG_PROTECTED;
            else if (token.equals("private"))
              visibility |= TAG_PRIVATE;
            continue;
          }
        if (c == '(')
          {
            skipParen();
            state = METHOD_NAME;
            continue;
          }
        if (c == '=')
          {
            addTag(TAG_FIELD);
            state = FIELD_INITIALIZER;
            pos.next();
            continue;
          }
        // "int x, y;"
        if (c == ';' || c == ',')
          {
            if (token != null)
              addTag(TAG_FIELD);
            // Don't reset the visibility until we see the semicolon.
            if (c == ';')
              visibility = 0;
          }
        pos.next();
      }
    buffer.setTags(tags);
  }

  private void addTag(int type)
  {
    if (currentClass != null)
      {
        FastStringBuffer sb = new FastStringBuffer(currentClass.getName());
        sb.append('.');
        sb.append(token);
        tags.add(new JavaTag(sb.toString(), tokenStart, type, visibility, currentClass));
      }
    else
      tags.add(new JavaTag(token, tokenStart, type, visibility));
  }

  protected static final void skipComment(Position pos)
  {
    while (true)
      {
        if (pos.lookingAt("*/"))
          {
            pos.skip(2);
            return;
          }
        if (!pos.next())
          return;
      }
  }

  protected final void skipSingleLineComment(Position pos)
  {
    Line next = pos.getNextLine();
    if (next != null)
      pos.moveTo(next, 0);
    else
      pos.setOffset(pos.getLineLength());
  }

  protected final LocalTag checkForExplicitTag(Position pos, int modeId)
  {
    final String explicitTag =
      Editor.preferences().getStringProperty(Property.EXPLICIT_TAG);
    if (explicitTag != null && explicitTag.length() > 0)
      {
        pos = pos.copy();
        String s = pos.getString(); // Substring to end of line.
        int index = s.indexOf(explicitTag);
        if (index >= 0)
          {
            pos.skip(index + explicitTag.length());
            pos.skipWhitespace();
            // Now we're looking at the first character of the tag.
            FastStringBuffer sb = new FastStringBuffer();
            char c = pos.getChar();
            if (c == '"')
              {
                while (pos.next())
                  {
                    c = pos.getChar();
                    if (c == '"')
                      break;
                    sb.append(c);
                  }
              }
            else
              {
                // By default, explicit tags are whitespace-delimited, so
                // they can contain characters that are not legal in Java
                // identifiers ("symbol-value").
                sb.append(c);
                while (pos.next())
                  {
                    c = pos.getChar();
                    if (Character.isWhitespace(c))
                      break;
                    sb.append(c);
                  }
              }
            final String tag = sb.toString();
            // Exact location of tag is beginning of text on line
            // containing tag.
            pos.setOffset(0);
            pos.skipWhitespace();
            if (modeId == JAVA_MODE)
              return new JavaTag(tag, pos, TAG_EXPLICIT, 0, currentClass);
            else if (modeId == CPP_MODE)
              return new CppTag(tag, pos, TAG_EXPLICIT);
          }
      }
    return null;
  }

  // If canonical is true, strings like "java.lang.String" are considered to
  // be a single token. Used for "extends" and "implements" clauses.
  private void gatherToken(boolean canonical)
  {
    tokenStart = new Position(pos);
    FastStringBuffer sb = new FastStringBuffer();
    char c;
    if (canonical)
      {
        while (Character.isJavaIdentifierPart(c = pos.getChar()) || c == '.')
          {
            sb.append(c);
            if (!pos.next())
              break;
          }
      }
    else
      {
        while (Character.isJavaIdentifierPart(c = pos.getChar()))
          {
            sb.append(c);
            if (!pos.next())
              break;
          }
      }
    token = sb.toString();
  }

  protected void skipParen()
  {
    if (pos.next())
      {
        int count = 1;
        while (true)
          {
            if (pos.lookingAt("/*"))
              {
                skipComment(pos);
                continue;
              }
            if (pos.lookingAt("//"))
              {
                skipSingleLineComment(pos);
                continue;
              }
            char c = pos.getChar();
            if (c == '"' || c == '\'')
              {
                pos.skipQuote();
                continue;
              }
            if (c == '(')
              ++count;
            else if (c == ')')
              --count;
            if (!pos.next())
              break;
            if (count == 0)
              break;
          }
      }
  }

  protected void skipBrace()
  {
    if (pos.next())
      {
        int count = 1;
        while (true)
          {
            if (pos.lookingAt("/*"))
              {
                skipComment(pos);
                continue;
              }
            if (pos.lookingAt("//"))
              {
                skipSingleLineComment(pos);
                continue;
              }
            char c = pos.getChar();
            if (c == '"' || c == '\'')
              {
                pos.skipQuote();
                continue;
              }
            if (c == '\\')
              {
                // Escape. Ignore this char and the next.
                if (!pos.next())
                  break;
                if (!pos.next())
                  break;
                continue;
              }
            if (c == '{')
              ++count;
            else if (c == '}')
              --count;
            if (!pos.next())
              break;
            if (count == 0)
              break;
          }
      }
  }

  private void skipSemi()
  {
    while (!pos.atEnd())
      {
        if (pos.lookingAt("/*"))
          {
            skipComment(pos);
            continue;
          }
        if (pos.lookingAt("//"))
          {
            skipSingleLineComment(pos);
            continue;
          }
        char c = pos.getChar();
        if (c == '"' || c == '\'')
          {
            pos.skipQuote();
            continue;
          }
        else if (c == ';')
          {
            pos.next();
            break;
          }
        else
          pos.next();
      }
  }

  // Used by the C, C++ and Objective C taggers.
  protected static final void skipPreprocessor(Position pos)
  {
    while (true)
      {
        Line line = pos.getLine();
        Line nextLine = line.next();
        if (nextLine == null)
          {
            pos.setOffset(line.length());
            return;
          }
        pos.moveTo(nextLine, 0);
        if (line.length() == 0 || line.charAt(line.length()-1) != '\\')
          return;
      }
  }
}
