/*
 * CTagger.java
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
import gnu.regexp.UncheckedRE;
import java.util.ArrayList;

public class CTagger extends JavaTagger
{
  // States.
  private static final int NEUTRAL        = 0;
  private static final int METHOD_NAME    = 1;
  private static final int PARAMETER_LIST = 2;

  private static RE lynxArgsMacroRE = new UncheckedRE("ARGS[0-9][0-9]?");

  private CMode mode = (CMode) CMode.getMode();

  public CTagger(SystemBuffer buffer)
  {
    super(buffer);
  }

  public void run()
  {
    ArrayList tags = new ArrayList();
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
                if (token != null && !mode.isKeyword(token))
                  tags.add(new CTag(token, tokenStart));
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            else if (mode.isIdentifierStart(c))
              {
                state = PARAMETER_LIST;
                pos.next();
                continue;
              }
            else
              {
                state = NEUTRAL;
                pos.next();
                continue;
              }
          }
        if (state == PARAMETER_LIST)
          {
            if (c == '{')
              {
                if (token != null && !mode.isKeyword(token))
                  tags.add(new CTag(token, tokenStart));
                skipBrace();
                state = NEUTRAL;
                continue;
              }
            else if (c == '(')
              {
                state = NEUTRAL;
                skipParen();
                continue;
              }
            else
              {
                pos.next();
                continue;
              }
          }
        if (c == '}')
          {
            pos.next();
            continue;
          }
        if (mode.isIdentifierStart(c))
          {
            tokenStart = pos.copy();
            String s = gatherToken(pos);
            if (s.startsWith("ARGS") && lynxArgsMacroRE.isMatch(s))
              {
                // Lynx "ARGSnn" macro.
                ;
              }
            else if (s.equals("NOARGS"))
              {
                // Lynx macro.
                state = METHOD_NAME;
              }
            else if (isDefunStart(s))
              {
                // Emacs macro.
                while (true)
                  {
                    c = pos.getChar();
                    if (c == '"')
                      {
                        pos.next();
                        break;
                      }
                    if (!pos.next())
                      break;
                  }
                tokenStart = pos.copy();
                token = gatherDefunName(pos);
                tags.add(new CTag(token, tokenStart));
                while ((c = pos.getChar()) != '{')
                  {
                    if (c == '"' || c == '\'')
                      {
                        pos.skipQuote();
                        continue;
                      }
                    if (c == '/' && pos.lookingAt("/*"))
                      {
                        skipComment(pos);
                        continue;
                      }
                    if (!pos.next())
                      break;
                  }
                if (c == '{')
                  skipBrace();
              }
            else
              token = s;
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

  private String gatherToken(Position pos)
  {
    FastStringBuffer sb = new FastStringBuffer();
    char c;
    while (mode.isIdentifierPart(c = pos.getChar()))
      {
        sb.append(c);
        if (!pos.next())
          break;
      }
    return sb.toString();
  }

  protected static boolean isDefunStart(String s)
  {
    if (s.length() < 5)
      return false;
    char c = s.charAt(0);
    if (c == 'D')
      {
        if (s.equals("DEFUN")) // Emacs, rep
          return true;
        if (s.equals("DEFUN_INT")) // rep
          return true;
      }
    else if (c == 'S')
      {
        if (s.equals("SCM_DEFINE")) // guile
          return true;
      }
    return false;
  }

  protected static String gatherDefunName(Position pos)
  {
    FastStringBuffer sb = new FastStringBuffer();
    while (true)
      {
        char c = pos.getChar();
        if (c == '"')
          {
            pos.next(); // Skip past final quote char.
            break;
          }
        if (c == EOL)
          break;
        sb.append(c);
        if (!pos.next())
          break;
      }
    return sb.toString();
  }
}
