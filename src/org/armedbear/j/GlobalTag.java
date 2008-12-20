/*
 * GlobalTag.java
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
import java.util.List;
import javax.swing.undo.CompoundEdit;

public final class GlobalTag extends Tag
{
  private final String filename;

  private GlobalTag(String name, String signature, String filename)
  {
    super(name, signature);
    this.filename = filename;
  }

  private GlobalTag(String name, String signature, String filename,
                    String canonicalSignature)
  {
    super(name, signature);
    this.filename = filename;
    this.canonicalSignature = canonicalSignature;
  }

  public final String getFileName()
  {
    return filename;
  }

  // Return tag name from line in tag file.
  public static String getTagName(Line line)
  {
    int i = line.getText().indexOf(Tagger.separatorChar);
    if (i < 0) // Should never happen.
      return line.getText();
    else
      return line.substring(0, i);
  }

  // Construct global tag from one line of text in tag file.
  public static GlobalTag makeGlobalTag(String s)
  {
    int index = s.indexOf(Tagger.separatorChar);
    if (index < 0)
      return null;
    String name = s.substring(0, index);
    s = s.substring(index + 1);
    index = s.indexOf(Tagger.separatorChar);
    if (index < 0)
      return null;
    String filename = s.substring(0, index);
    s = s.substring(index + 1);
    index = s.indexOf(Tagger.separatorChar);
    if (index < 0)
      {
        // No canonical signature.
        String signature = s;
        return new GlobalTag(name, signature, filename);
      }
    String signature = s.substring(0, index);
    String canonicalSignature = s.substring(index + 1);
    return new GlobalTag(name, signature, filename, canonicalSignature);
  }

  public String getClassName()
  {
    // Java.
    int index = name.indexOf('.');
    if (index >= 0)
      return name.substring(0, index);
    // C++, Perl.
    index = name.indexOf("::");
    if (index >= 0)
      return name.substring(0, index);
    // C, Lisp.
    return null;
  }

  public String getMethodName()
  {
    // Java
    int index = name.indexOf('.');
    if (index >= 0)
      return name.substring(index + 1);
    // C++
    index = name.indexOf("::");
    if (index >= 0)
      return name.substring(index + 2);
    return name;
  }

  public String getLongName()
  {
    String s = signature.trim();
    if (s.startsWith("DEFUN"))
      {
        // Emacs source.
        return name;
      }
    if (s.startsWith("("))
      {
        // Lisp.
        s = s.substring(1).trim();
        // First word should be "defun" or "defvar" or some such...
        int end = 0;
        for (int i = 0; i < s.length(); i++)
          {
            char c = s.charAt(i);
            if (c == ' ' || c == '\t')
              {
                end = i;
                break;
              }
          }
        String definer = s.substring(0, end);
        s = s.substring(end).trim();
        FastStringBuffer sb = new FastStringBuffer('(');
        sb.append(definer);
        sb.append(' ');
        if (definer.equals("defgeneric") || definer.equals("defmethod"))
          {
            sb.append(s);
            return sb.toString();
          }
        for (int i = 0; i < s.length(); i++)
          {
            char c = s.charAt(i);
            if (c == ' ' || c == '\t')
              {
                sb.append(s.substring(0, i));
                sb.append(" ...");
                return sb.toString();
              }
            if (c == ')')
              {
                sb.append(s.substring(0, i + 1));
                return sb.toString();
              }
          }
        sb.append(s);
        sb.append("  ...");
        return sb.toString();
      }
    if (name.startsWith("class "))
      return name;
    // Strip comment if any.
    // BUG! Only really relevant to Java-like languages.
    int index = s.indexOf("//");
    if (index >= 0)
      s = s.substring(0, index).trim();
    index = s.lastIndexOf(')');
    if (index >= 0)
      s = s.substring(0, index + 1);
    if (s.endsWith("("))
      s = s.substring(0, s.length() - 1);
    // Try to substitute the fully qualified method name.
    // Try Java first.
    String separator = ".";
    int sepLength = 1;
    index = name.indexOf(separator);
    if (index >= 0)
      {
        String methodName = name.substring(index + sepLength);
        index = s.indexOf(methodName);
        if (index >= 0)
          {
            String head = s.substring(0, index);
            String tail = s.substring(index + methodName.length());
            s = head + name + tail;
          }
      }
    else
      {
        // C++.
        separator = "::";
        sepLength = 2;
        index = name.indexOf(separator);
        if (index >= 0)
          {
            String methodName = name.substring(index + sepLength);
            index = s.indexOf(methodName);
            if (index >= 0)
              {
                String head = s.substring(0, index);
                String tail = s.substring(index + methodName.length());
                s = head + methodName + tail;
              }
          }
      }
    return s;
  }

  public String toString()
  {
    // JavaScript.
    if (signature.trim().startsWith("function "))
      return signature + " " + File.getInstance(filename).getName();
    else
      return super.toString();
  }

  public void gotoTag(Editor editor)
  {
    editor.setWaitCursor();
    Buffer buf = Editor.getBuffer(File.getInstance(filename));
    if (buf != null)
      {
        editor.makeNext(buf);
        editor.activate(buf);
        Position pos = findSignatureInCurrentBuffer(buf, signature);
        if (pos != null)
          {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.addUndo(SimpleEdit.FOLD);
            editor.unfoldMethod(pos.getLine());
            editor.moveDotTo(pos);
            TagCommands.centerTag(editor);
            editor.endCompoundEdit(compoundEdit);
            editor.updateDisplay();
          }
      }
    editor.setDefaultCursor();
  }

  private static Position findSignatureInCurrentBuffer(Buffer buffer,
                                                       String signature)
  {
    final List localTags = buffer.getTags(true);
    if (localTags == null)
      return null;
    final int limit = localTags.size();
    for (int i = 0; i < limit; i++)
      {
        LocalTag localTag = (LocalTag) localTags.get(i);
        if (localTag.getSignature().equals(signature))
          return localTag.getPosition();
      }
    // We did not find an exact match. The signature may have changed.
    // Look for a substring containing the function name and argument list
    // only.
    RE re = new UncheckedRE("\\w+\\s*\\(.*\\)");
    REMatch match = re.getMatch(signature);
    if (match != null)
      {
        String sub = match.toString();
        for (int i = 0; i < limit; i++)
          {
            LocalTag localTag = (LocalTag) localTags.get(i);
            if (localTag.getSignature().indexOf(sub) >= 0)
              return localTag.getPosition();
          }
      }
    return null;
  }
}
