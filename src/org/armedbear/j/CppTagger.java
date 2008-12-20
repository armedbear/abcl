/*
 * CppTagger.java
 *
 * Copyright (C) 1998-2006 Peter Graves
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

import java.util.ArrayList;
import java.util.Stack;

public final class CppTagger extends CTagger implements Constants
{
  // States.
  private static final int NEUTRAL             = 0;
  private static final int CLASS_NAME          = 1;
  private static final int CLASS_PROLOG        = 2;
  private static final int METHOD_NAME         = 3;
  private static final int METHOD_PROLOG       = 4;
  private static final int INITIALIZATION_LIST = 5;

  private static final String classSeparator = "::";

  public CppTagger(SystemBuffer buffer)
  {
    super(buffer);
  }

  public void run()
  {
    ArrayList tags = new ArrayList();
    String className = null;
    Stack classNames = new Stack();
    pos = new Position(buffer.getFirstLine(), 0);
    token = null;
    tokenStart = null;
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
            LocalTag tag = checkForExplicitTag(pos, CPP_MODE);
            if (tag instanceof CppTag)
                tags.add(tag);
            skipSingleLineComment(pos);
            continue;
          }
        if (c == '#' && pos.getOffset() == 0)
          {
            skipPreprocessor(pos);
            continue;
          }
        if (state == METHOD_NAME)
          {
            if (c == '{')
              {
                if (token.equals("DEFINE_PRIMITIVE")
                    || token.equals("DEFINE_SPECIAL_OPERATOR"))
                  {
                    state = NEUTRAL;
                    skipBrace();
                    continue;
                  }
                if (className != null)
                  token = className + classSeparator + token;
                tags.add(new CppTag(token, tokenStart, TAG_METHOD));
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            if (c == ':')
              {
                if (className != null)
                  token = className + classSeparator + token;
                tags.add(new CppTag(token, tokenStart, TAG_METHOD));
                state = INITIALIZATION_LIST;
                pos.skip(1);
                continue;
              }
            if (pos.lookingAt("throw"))
              {
                if (className != null)
                  token = className + classSeparator + token;
                state = METHOD_PROLOG;
                pos.skip(5); // Skip over "throw".
                continue;
              }
            if (pos.lookingAt("const"))
              {
                if (className != null)
                  token = className + classSeparator + token;
                state = METHOD_PROLOG;
                pos.skip(5); // Skip over "const".
                continue;
              }
            state = NEUTRAL; // Fall through...
          }
        if (state == INITIALIZATION_LIST)
          {
            if (c == '{')
              {
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            pos.next();
            continue;
          }
        if (state == CLASS_PROLOG)
          {
            if (c == '{')
              {
                if (className != null)
                  classNames.push(className);
                className = token;
                // Add a tag for the class itself.
                tags.add(new CppTag("class " + token, tokenStart, TAG_CLASS));
                state = NEUTRAL;
                pos.next();
                continue;
              }
            if (c == ';')
              {
                // It was just a declaration.
                pos.next();
                state = NEUTRAL;
                continue;
              }
            pos.next();
            continue;
          }
        if (state == METHOD_PROLOG)
          {
            if (c == '{')
              {
                // Opening brace of method body.
                tags.add(new CppTag(token, tokenStart, TAG_METHOD));
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            if (c == ';')
              {
                // It was just a declaration.
                pos.next();
                state = NEUTRAL;
                continue;
              }
            pos.next();
            continue;
          }
        if (c == '}')
          {
            if (classNames.empty())
              className = null;
            else
              className = (String) classNames.pop();
            pos.next();
            continue;
          }
        if (isIdentifierStart(c))
          {
            gatherToken();
            if (state == CLASS_NAME)
              state = CLASS_PROLOG;
            else if (token.equals("class"))
              state = CLASS_NAME;
            else if (token.equals("operator") || token.endsWith("::operator"))
              {
                gatherOperatorName();
                token = "operator " + token;
                state = METHOD_NAME;
              }
            continue;
          }
        if (c == '(')
          {
            skipParen();
            state = METHOD_NAME;
            continue;
          }
        pos.next();
      }
    buffer.setTags(tags);
  }

  private void gatherToken()
  {
    tokenStart = new Position(pos);
    FastStringBuffer sb = new FastStringBuffer();
    char c;
    while (isIdentifierPart(c = pos.getChar()))
      {
        sb.append(c);
        if (!pos.next())
          break;
      }
    // Token can't end with ':'.
    while (sb.length() > 0 && sb.charAt(sb.length() - 1) == ':')
      sb.setLength(sb.length() - 1);
    token = sb.toString();
  }

  private void gatherOperatorName()
  {
    pos.skipWhitespace();
    tokenStart = new Position(pos);
    FastStringBuffer sb = new FastStringBuffer();
    char c;
    while ((c = pos.getChar()) != '(')
      {
        sb.append(c);
        if (!pos.next())
          break;
      }
    token = sb.toString();
  }

  private static final boolean isIdentifierStart(char c)
  {
    if (c >= 'a' && c <= 'z')
      return true;
    if (c >='A' && c <= 'Z')
      return true;
    if (c == '_' || c == ':' || c == '~')
      return true;
    return false;
  }

  private static final boolean isIdentifierPart(char c)
  {
    if (c >= 'a' && c <= 'z')
      return true;
    if (c >='A' && c <= 'Z')
      return true;
    if (c >= '0' && c <= '9')
      return true;
    if (c == '_' || c == ':' || c == '~')
      return true;
    return false;
  }
}
