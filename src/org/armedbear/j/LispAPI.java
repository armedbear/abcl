/*
 * LispAPI.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j;

import gnu.regexp.REException;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;
import org.armedbear.lisp.AbstractString;
import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.Fixnum;
import org.armedbear.lisp.Function;
import org.armedbear.lisp.GenericFunction;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispCharacter;
import org.armedbear.lisp.LispError;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Package;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.Pathname;
import org.armedbear.lisp.Primitive;
import org.armedbear.lisp.Primitives;
import org.armedbear.lisp.SimpleString;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.TypeError;
import org.armedbear.lisp.UndefinedFunction;
import org.armedbear.lisp.WrongNumberOfArgumentsException;

public final class LispAPI extends Lisp
{
  private static final Preferences preferences = Editor.preferences();

  public static final Package PACKAGE_J = Packages.createPackage("J");
  public static final Package PACKAGE_J_INTERNALS =
    Packages.createPackage("J-INTERNALS");
  static
  {
    try
      {
        PACKAGE_J.usePackage(PACKAGE_CL);
        PACKAGE_J.usePackage(PACKAGE_EXT);
        PACKAGE_J.usePackage(PACKAGE_JAVA);
        PACKAGE_J_INTERNALS.usePackage(PACKAGE_CL);
        PACKAGE_J_INTERNALS.usePackage(PACKAGE_EXT);
        PACKAGE_J_INTERNALS.usePackage(PACKAGE_JAVA);
      }
    catch (Throwable t)
      {
        Log.debug(t);
      }
  }

  public static final Symbol BUFFER_STREAM =
    LispAPI.PACKAGE_J.addExternalSymbol("BUFFER-STREAM");

  public static final Symbol _CURRENT_COMMAND_ =
    exportSpecial("*CURRENT-COMMAND*", PACKAGE_J, NIL);

  public static final Symbol _LAST_COMMAND_ =
    exportSpecial("*LAST-COMMAND*", PACKAGE_J, NIL);

  public static final void eventHandled()
  {
    _LAST_COMMAND_.setSymbolValue(_CURRENT_COMMAND_.getSymbolValue());
    _CURRENT_COMMAND_.setSymbolValue(NIL);
  }

  public static final Editor checkEditor(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == null)
      throw new NullPointerException();
    try
      {
        return (Editor) ((JavaObject)obj).getObject();
      }
    catch (ClassCastException e)
      {
        error(new TypeError("The value " + obj.writeToString() +
                            " is not an editor."));
        // Not reached.
        return null;
      }
  }

  public static final Buffer checkBuffer(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == null)
      throw new NullPointerException();
    if (obj == NIL)
      return Editor.currentEditor().getBuffer();
    try
      {
        return (Buffer) ((JavaObject)obj).getObject();
      }
    catch (ClassCastException e)
      {
        error(new TypeError("The value " + obj.writeToString() +
                            " is not a buffer."));
        // Not reached.
        return null;
      }
  }

  public static final KeyMap checkKeymap(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == null)
      throw new NullPointerException();
    try
      {
        return (KeyMap) ((JavaObject)obj).getObject();
      }
    catch (ClassCastException e)
      {
        error(new TypeError("The value " + obj.writeToString() +
                            " is not a keymap."));
        // Not reached.
        return null;
      }
  }

  private static final Position checkMark(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == null)
      throw new NullPointerException();
    try
      {
        return (Position) ((JavaObject)obj).getObject();
      }
    catch (ClassCastException e)
      {
        error(new TypeError("The value " + obj.writeToString() +
                            " is not a mark."));
        // Not reached.
        return null;
      }
  }

  public static final Line checkLine(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == null)
      throw new NullPointerException();
    try
      {
        return (Line) ((JavaObject)obj).getObject();
      }
    catch (ClassCastException e)
      {
        error(new TypeError("The value " + obj.writeToString() +
                            " is not a line."));
        // Not reached.
        return null;
      }
  }

  // ### current-editor
  private static final Primitive CURRENT_EDITOR =
    new Primitive("current-editor", PACKAGE_J, true, "",
                  "Returns the current editor as a Lisp object.")
    {
      public LispObject execute()
      {
        return new JavaObject(Editor.currentEditor());
      }
    };

  // ### %set-current-editor
  private static final Primitive _SET_CURRENT_EDITOR =
    new Primitive("%set-current-editor", PACKAGE_J, false, "(EDITOR)",
                  "Makes EDITOR the current editor.")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Editor.setCurrentEditor(checkEditor(arg));
        return arg;
      }
    };

  // ### other-editor
  private static final Primitive OTHER_EDITOR =
    new Primitive("other-editor", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        Editor otherEditor = Editor.currentEditor().getOtherEditor();
        return otherEditor != null ? new JavaObject(otherEditor) : NIL;
      }
    };

  // ### current-buffer
  private static final Primitive CURRENT_BUFFER =
    new Primitive("current-buffer", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        return new JavaObject(Editor.currentEditor().getBuffer());
      }
    };

  // ### editor-buffer editor => buffer
  private static final Primitive BUFFER =
    new Primitive("editor-buffer", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return new JavaObject(checkEditor(arg).getBuffer());
      }
    };

  // ### buffer-name
  private static final Primitive BUFFER_NAME =
    new Primitive("buffer-name", PACKAGE_J, true, "&optional buffer")
    {
      public LispObject execute()
      {
        String name = Editor.currentEditor().getBuffer().getTitle();
        return name != null ? new SimpleString(name) : NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        String name = checkBuffer(arg).getTitle();
        return name != null ? new SimpleString(name) : NIL;
      }
    };

  // ### set-buffer-name
  private static final Primitive SET_BUFFER_NAME =
    new Primitive("set-buffer-name", PACKAGE_J, true, "buffer name")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Buffer buffer = checkBuffer(first);
        if (!(second instanceof AbstractString))
          return type_error(second, Symbol.STRING);
        buffer.setTitle(second.getStringValue());
        return second;
      }
    };

  // ### get-buffer
  private static final Primitive GET_BUFFER =
    new Primitive("get-buffer", PACKAGE_J, true, "name")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof AbstractString)
          {
            String name = arg.getStringValue();
            BufferIterator iterator = new BufferIterator();
            while (iterator.hasNext())
              {
                Buffer buffer = iterator.nextBuffer();
                if (buffer.getTitle().equals(name))
                  return new JavaObject(buffer);
              }
            return NIL;
          }
        if (arg instanceof JavaObject)
          {
            if (((JavaObject)arg).getObject() instanceof Buffer)
              return arg;
          }
        return NIL;
      }
    };

  // ### buffer-live-p object => generalized-boolean
  private static final Primitive BUFFER_LIVE_P =
    new Primitive("buffer-live-p", PACKAGE_J, true, "object")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof JavaObject)
          {
            if (((JavaObject)arg).getObject() instanceof Buffer)
              {
                if (Editor.getBufferList().contains((Buffer)((JavaObject)arg).getObject()))
                  return T;
              }
          }
        return NIL;
      }
    };

  // ### buffer-pathname
  private static final Primitive BUFFER_PATHNAME =
    new Primitive("buffer-pathname", PACKAGE_J, true, "&optional buffer")
    {
      public LispObject execute() throws ConditionThrowable
      {
        File file = Editor.currentEditor().getBuffer().getFile();
        if (file != null && file.isLocal())
          {
            String s = file.canonicalPath();
            if (file.isDirectory())
              if (!s.endsWith(LocalFile.getSeparator()))
                s = s.concat(LocalFile.getSeparator());
            return new Pathname(s);
          }
        return NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        File file = checkBuffer(arg).getFile();
        if (file != null && file.isLocal())
          {
            String s = file.canonicalPath();
            if (file.isDirectory())
              if (!s.endsWith(LocalFile.getSeparator()))
                s = s.concat(LocalFile.getSeparator());
            return new Pathname(s);
          }
        return NIL;
      }
    };

  // ### buffer-string
  private static final Primitive BUFFER_STRING =
    new Primitive("buffer-string", PACKAGE_J, true, "&optional buffer")
    {
      public LispObject execute() throws ConditionThrowable
      {
        return new SimpleString(Editor.currentBuffer().getText());
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return new SimpleString(checkBuffer(arg).getText());
      }
    };

  // ### buffer-substring
  private static final Primitive BUFFER_SUBSTRING =
    new Primitive("buffer-substring", PACKAGE_J, true,
                  "start end &optional buffer")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Region region = new Region(Editor.currentEditor().getBuffer(),
                                   checkMark(first),
                                   checkMark(second));
        return new SimpleString(region.toString());
      }
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        Position start = checkMark(first);
        Position end = checkMark(second);
        Region region = new Region(checkBuffer(third), start, end);
        return new SimpleString(region.toString());
      }
    };

  // ### buffer-offset mark &optional buffer
  private static final Primitive BUFFER_OFFSET =
    new Primitive("buffer-offset", PACKAGE_J, true, "mark &optional buffer")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final Position pos = checkMark(arg);
        final Buffer buffer = Editor.currentBuffer();
        int offset = buffer.getAbsoluteOffset(pos);
        return offset >= 0 ? new Fixnum(offset) : NIL;
      }
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final Position pos = checkMark(first);
        final Buffer buffer = checkBuffer(second);
        int offset = buffer.getAbsoluteOffset(pos);
        return offset >= 0 ? new Fixnum(offset) : NIL;
      }
    };

  // ### goto-char position
  private static final Primitive GOTO_CHAR =
    new Primitive("goto-char", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        // Move dot to position.
        final Editor editor = Editor.currentEditor();
        if (arg instanceof Fixnum)
          {
            Position pos = editor.getBuffer().getPosition(((Fixnum)arg).value);
            if (pos != null)
              editor.moveDotTo(pos);
          }
        else
          editor.moveDotTo(checkMark(arg));
        return new JavaObject(editor.getDot());
      }
    };

  // ### move-to-position mark charpos &optional line
  private static final Primitive MOVE_TO_POSITION =
    new Primitive("move-to-position", PACKAGE_J, true,
                  "mark charpos &optional line")
    {
      public LispObject execute(LispObject mark, LispObject charpos)
        throws ConditionThrowable
      {
        Position pos = checkMark(mark);
        pos.setOffset(Fixnum.getValue(charpos));
        return mark;
      }
      public LispObject execute(LispObject mark, LispObject charpos,
                                LispObject line)
        throws ConditionThrowable
      {
        Position pos = checkMark(mark);
        if (line == NIL)
          pos.setOffset(Fixnum.getValue(charpos));
        else
          pos.moveTo(checkLine(line), Fixnum.getValue(charpos));
        return mark;
      }
    };

  // ### current-point
  private static final Primitive CURRENT_POINT =
    new Primitive("current-point", PACKAGE_J, true, "")
    {
      public LispObject execute()
      {
        Position dot = Editor.currentEditor().getDot();
        if (dot != null)
          return new JavaObject(dot.copy());
        return NIL;
      }
    };

  // ### current-mark
  private static final Primitive CURRENT_MARK =
    new Primitive("current-mark", PACKAGE_J, true, "")
    {
      public LispObject execute()
      {
        Position mark = Editor.currentEditor().getMark();
        if (mark == null)
          mark = Editor.currentBuffer().getMark();
        if (mark != null)
          return new JavaObject(mark.copy());
        return NIL;
      }
    };

  // ### buffer-mark buffer => mark
  private static final Primitive BUFFER_MARK =
    new Primitive("buffer-mark", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg)
        throws ConditionThrowable
      {
        final Position mark = checkBuffer(arg).getMark();
        if (mark == null)
          return NIL;
        return new JavaObject(mark.copy());
      }
    };

  // ### %set-buffer-mark buffer mark => mark
  private static final Primitive _SET_BUFFER_MARK =
    new Primitive("%set-buffer-mark", PACKAGE_J, true)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Buffer buffer = checkBuffer(first);
        if (second == NIL)
          buffer.setMark(null);
        else
          buffer.setMark(checkMark(second));
        return second;
      }
    };

  // ### editor-mark editor => mark
  private static final Primitive EDITOR_MARK =
    new Primitive("editor-mark", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg)
        throws ConditionThrowable
      {
        final Position mark = checkEditor(arg).getMark();
        if (mark == null)
          return NIL;
        return new JavaObject(mark.copy());
      }
    };

  // ### %set-editor-mark editor mark => mark
  private static final Primitive _SET_EDITOR_MARK =
    new Primitive("%set-editor-mark", PACKAGE_J, true)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Editor editor = checkEditor(first);
        if (second == NIL)
          editor.setMark(null);
        else
          editor.setMark(checkMark(second));
        return second;
      }
    };

  // ### point-min
  private static final Primitive POINT_MIN =
    new Primitive("point-min", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        final Line line = Editor.currentBuffer().getFirstLine();
        if (line == null)
          return NIL;
        return new JavaObject(new Position(line, 0));
      }
    };

  // ### point-max
  private static final Primitive POINT_MAX =
    new Primitive("point-max", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        Position pos = Editor.currentBuffer().getEnd();
        if (pos == null)
          return NIL;
        return new JavaObject(pos);
      }
    };

  // ### make-mark
  private static final Primitive MAKE_MARK =
    new Primitive("make-mark", PACKAGE_J, true, "line offset")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Line line = checkLine(first);
        int offset = Fixnum.getValue(second);
        return new JavaObject(new Position(line, offset));
      }
    };

  // ### mark-line
  private static final Primitive MARK_LINE =
    new Primitive("mark-line", PACKAGE_J, true, "mark")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return new JavaObject(checkMark(arg).getLine());
      }
    };

  // ### mark-charpos
  private static final Primitive MARK_CHARPOS =
    new Primitive("mark-charpos", PACKAGE_J, true, "mark")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return number(checkMark(arg).getOffset());
      }
    };

  // ### current-line
  private static final Primitive CURRENT_LINE =
    new Primitive("current-line", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        Editor editor = Editor.currentEditor();
        Position dot = editor.getDot();
        if (dot != null)
          return new JavaObject(dot.getLine());
        return NIL;
      }
    };

  // ### line-next
  private static final Primitive LINE_NEXT =
    new Primitive("line-next", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Line next = checkLine(arg).next();
        return next != null ? new JavaObject(next) : NIL;
      }
    };

  // ### line-previous
  private static final Primitive LINE_PREVIOUS =
    new Primitive("line-previous", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Line prev = checkLine(arg).previous();
        return prev != null ? new JavaObject(prev) : NIL;
      }
    };

  // ### line-chars
  private static final Primitive LINE_CHARS =
    new Primitive("line-chars", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        String s = checkLine(arg).getText();
        return s != null ? new SimpleString(s) : NIL;
      }
    };

  // ### line-flags
  private static final Primitive LINE_FLAGS =
    new Primitive("line-flags", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return number(checkLine(arg).flags());
      }
    };

  // ### %set-line-flags
  private static final Primitive _SET_LINE_FLAGS =
    new Primitive("%set-line-flags", PACKAGE_J, false)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Line line = checkLine(first);
        int flags = Fixnum.getValue(second);
        line.setFlags(flags);
        return second;
      }
    };

  // ### line-number
  private static final Primitive LINE_NUMBER =
    new Primitive("line-number", PACKAGE_J, true, "line")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return number(checkLine(arg).lineNumber());
      }
    };

  // ### char-after
  // Returns character immediately after mark.
  private static final Primitive CHAR_AFTER =
    new Primitive("char-after", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return LispCharacter.getInstance(checkMark(arg).getChar());
      }
    };

  // ### char-before
  // Returns character immediately before mark.
  private static final Primitive CHAR_BEFORE =
    new Primitive("char-before", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = checkMark(arg).copy();
        return pos.prev() ? LispCharacter.getInstance(pos.getChar()) : NIL;
      }
    };

  // ### forward-char
  // Move point right N characters (left if N is negative).
  private static final Primitive FORWARD_CHAR =
    new Primitive("forward-char", PACKAGE_J, true, "mark &optional count")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = checkMark(arg);
        return forwardChar(pos, 1);
      }
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Position pos = checkMark(first);
        return forwardChar(pos, Fixnum.getValue(second));
      }
    };

  // ### backward-char
  // Move MARK left COUNT characters (right if COUNT is negative).
  private static final Primitive BACKWARD_CHAR =
    new Primitive("backward-char", PACKAGE_J, true, "mark &optional count")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = checkMark(arg);
        return forwardChar(pos, -1);
      }
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Position pos = checkMark(first);
        return forwardChar(pos, -Fixnum.getValue(second));
      }
    };

  private static final LispObject forwardChar(Position pos, int n)
    throws ConditionThrowable
  {
    if (n != 0)
      {
        if (pos != null)
          {
            if (n > 0)
              {
                while (n-- > 0)
                  {
                    if (!pos.next())
                      return error(new LispError("Reached end of buffer."));
                  }
              }
            else
              {
                while (n++ < 0)
                  {
                    if (!pos.prev())
                      return error(new LispError("Reached beginning of buffer."));
                  }
              }
          }
      }
    return NIL;
  }

  // ### beginning-of-line
  private static final Primitive BEGINNING_OF_LINE =
    new Primitive("beginning-of-line", PACKAGE_J, true)
    {
      public LispObject execute() throws ConditionThrowable
      {
        Editor.currentEditor().bol();
        return NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        int n = (arg != NIL) ? Fixnum.getValue(arg) : 1;
        final Editor editor = Editor.currentEditor();
        Position pos = editor.getDot();
        if (pos != null)
          {
            editor.addUndo(SimpleEdit.MOVE);
            while (--n > 0)
              {
                Line nextLine = pos.getNextLine();
                if (nextLine != null)
                  pos.setLine(nextLine);
                else
                  break;
              }
            pos.setOffset(0);
            editor.moveCaretToDotCol();
          }
        return NIL;
      }
    };

  // ### end-of-line
  private static final Primitive END_OF_LINE =
    new Primitive("end-of-line", PACKAGE_J, true)
    {
      public LispObject execute() throws ConditionThrowable
      {
        Editor.currentEditor().eol();
        return NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        int n = (arg != NIL) ? Fixnum.getValue(arg) : 1;
        final Editor editor = Editor.currentEditor();
        Position pos = editor.getDot();
        if (pos != null)
          {
            editor.addUndo(SimpleEdit.MOVE);
            while (--n > 0)
              {
                Line nextLine = pos.getNextLine();
                if (nextLine != null)
                  pos.setLine(nextLine);
                else
                  break;
              }
            pos.setOffset(pos.getLineLength());
            editor.moveCaretToDotCol();
          }
        return NIL;
      }
    };

  // ### backward-up-list mark
  private static final Primitive BACKWARD_UP_LIST =
    new Primitive("backward-up-list", PACKAGE_J, true, "mark")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = checkMark(arg);
        Position newPos = LispMode.findContainingSexp(pos);
        if (newPos != null)
          pos.moveTo(newPos);
        return arg;
      }
    };

  // ### looking-at mark pattern => generalized-boolean
  private static final Primitive LOOKING_AT =
    new Primitive("looking-at", PACKAGE_J, true, "mark string")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Position pos = checkMark(first);
        if (second instanceof AbstractString)
          {
            String pattern = second.getStringValue();
            if (pos.getLine().substring(pos.getOffset()).startsWith(pattern))
              return T;
            return NIL;
          }
        return type_error(second, Symbol.STRING);
      }
    };

  // ### kill-theme
  private static final Primitive KILL_THEME =
    new Primitive("kill-theme", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        preferences.killTheme();
        return T;
      }
    };

  // ### restore-focus
  private static final Primitive RESTORE_FOCUS =
    new Primitive("restore-focus", PACKAGE_J, true)
    {
      public LispObject execute()
      {
        Editor.currentEditor().setFocusToDisplay();
        return T;
      }
    };

  // ### make-keymap
  private static final Primitive MAKE_KEYMAP =
    new Primitive("make-keymap", PACKAGE_J, true, "")
    {
      public LispObject execute()
      {
        return new JavaObject(new KeyMap());
      }
    };

  public static final Symbol _CURRENT_GLOBAL_MAP_ =
    exportSpecial("*CURRENT-GLOBAL-MAP*", PACKAGE_J, NIL);

  // ### current-global-map
  private static final Primitive CURRENT_GLOBAL_MAP =
    new Primitive("current-global-map", PACKAGE_J, true, "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        return _CURRENT_GLOBAL_MAP_.symbolValue();
      }
    };

  // ### use-global-map keymap => NIL
  private static final Primitive USE_GLOBAL_MAP =
    new Primitive("use-global-map", PACKAGE_J, true, "keymap")
    {
      public LispObject execute(LispObject arg)
        throws ConditionThrowable
      {
        if (arg != NIL)
          KeyMap.setGlobalKeyMap(checkKeymap(arg));
        LispThread.currentThread().setSpecialVariable(_CURRENT_GLOBAL_MAP_,
                                                      arg);
        return NIL; // emacs
      }
    };

  // ### define-key keymap key definition => generalized-boolean
  private static final Primitive DEFINE_KEY =
    new Primitive("define-key", PACKAGE_J, true,
                  "keymap key-designator definition")
    {
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        KeyMap keymap = checkKeymap(first);
        if (!(second instanceof LispCharacter ||
              second instanceof AbstractString))
          return type_error(second, list2(Symbol.OR,
                                          list2(Symbol.CHARACTER,
                                                Symbol.STRING)));
        Object command;
        if (third instanceof AbstractString)
          command = third.getStringValue();
        else if (third instanceof JavaObject)
          command = checkKeymap(third);
        else
          {
            // Verify that the command can be coerced to a function.
            coerceToFunction(third);
            command = third;
          }
        if (second instanceof LispCharacter)
          keymap.mapKey(((LispCharacter)second).value, command);
        else
          keymap.mapKey(second.getStringValue(), command);
        return T;
      }
    };

  // ### global-map-key key command => generalized-boolean
  private static final Primitive GLOBAL_MAP_KEY =
    new Primitive("global-map-key", PACKAGE_J, true, "key command")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        String keyText = first.getStringValue();
        Object command;
        if (second instanceof AbstractString)
          {
            command = second.getStringValue();
          }
        else if (second instanceof JavaObject)
          {
            command = checkKeymap(second);
          }
        else
          {
            // Verify that the command can be coerced to a function.
            coerceToFunction(second);
            command = second;
          }
        return KeyMap.getGlobalKeyMap().mapKey(keyText, command) ? T : NIL;
      }
    };

  // ### global-unmap-key key => generalized-boolean
  private static final Primitive GLOBAL_UNMAP_KEY =
    new Primitive("global-unmap-key", PACKAGE_J, true, "key")
    {
      public LispObject execute(LispObject arg)
        throws ConditionThrowable
      {
        String keyText = arg.getStringValue();
        return KeyMap.getGlobalKeyMap().unmapKey(keyText) ? T : NIL;
      }
    };

  // ### map-key-for-mode key command mode => generalized-boolean
  private static final Primitive MAP_KEY_FOR_MODE =
    new Primitive("map-key-for-mode", PACKAGE_J, true, "key command mode")
    {
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        String keyText = first.getStringValue();
        Object command;
        if (second instanceof AbstractString)
          {
            command = second.getStringValue();
          }
        else
          {
            // Verify that the command can be coerced to a function.
            coerceToFunction(second);
            command = second;
          }
        String modeName = third.getStringValue();
        Mode mode = Editor.getModeList().getModeFromModeName(modeName);
        if (mode == null)
          return error(new LispError("Unknown mode \"".concat(modeName).concat("\"")));
        return mode.getKeyMap().mapKey(keyText, command) ? T : NIL;
      }
    };

  // ### unmap-key-for-mode key mode => generalized-boolean
  private static final Primitive UNMAP_KEY_FOR_MODE =
    new Primitive("unmap-key-for-mode", PACKAGE_J, true, "key mode")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        String keyText = first.getStringValue();
        String modeName = second.getStringValue();
        Mode mode = Editor.getModeList().getModeFromModeName(modeName);
        if (mode == null)
          return error(new LispError("Unknown mode \"".concat(modeName).concat("\"")));
        return mode.getKeyMap().unmapKey(keyText) ? T : NIL;
      }
    };

  // ### set-global-property
  private static final Primitive SET_GLOBAL_PROPERTY =
    new Primitive("set-global-property", PACKAGE_J, true, "key value")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        String key = javaString(first);
        Property property = Property.findProperty(key);
        if (property == null)
          {
            // Not an advertised property.
            if (!(second instanceof AbstractString))
              return type_error(second, Symbol.STRING);
            Editor.setGlobalProperty(key, second.getStringValue());
            return second;
          }
        if (property.isBooleanProperty())
          {
            preferences.setProperty(property,
                                    (second == NIL) ? "false" : "true");
            return second;
          }
        if (property.isIntegerProperty())
          {
            if (second instanceof Fixnum)
              {
                preferences.setProperty(property,
                                        String.valueOf(((Fixnum)second).value));
                return second;
              }
            if (second instanceof AbstractString)
              {
                int value;
                try
                  {
                    value = Integer.parseInt(second.getStringValue());
                  }
                catch (NumberFormatException e)
                  {
                    return error(new LispError(second.writeToString() +
                                               " cannot be converted to a Java integer."));
                  }
                preferences.setProperty(property, value);
                return second;
              }
          }
        // It must be a string property.
        if (!(second instanceof AbstractString))
          return type_error(second, Symbol.STRING);
        preferences.setProperty(property, second.getStringValue());
        return second;
      }
    };

  // ### set-mode-property
  private static final Primitive SET_MODE_PROPERTY =
    new Primitive("set-mode-property", PACKAGE_J, true, "key value mode")
    {
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        if (!(first instanceof AbstractString))
          return type_error(first, Symbol.STRING);
        String key = first.getStringValue();
        if (!(third instanceof AbstractString))
          return type_error(third, Symbol.STRING);
        final Mode mode
          = Editor.getModeList().getModeFromModeName(third.getStringValue());
        if (mode == null)
          return error(new LispError(third.writeToString() +
                                     " does not designate any mode."));
        Property property = Property.findProperty(key);
        if (property == null)
          // Not an advertised property.
          return error(new LispError(first.writeToString() +
                                     " does not designate any property."));
        if (property.isBooleanProperty())
          {
            mode.setProperty(property, second == NIL ? false : true);
            return second;
          }
        else if (property.isIntegerProperty())
          {
            if (second instanceof Fixnum)
              {
                mode.setProperty(property, ((Fixnum)second).value);
                return second;
              }
            if (second instanceof AbstractString)
              {
                int value;
                try
                  {
                    value = Integer.parseInt(second.getStringValue());
                  }
                catch (NumberFormatException e)
                  {
                    return error(new LispError(second.writeToString() +
                                               " cannot be converted to a Java integer."));
                  }
                mode.setProperty(property, value);
                return second;
              }
            return type_error(second, list3(Symbol.OR, Symbol.FIXNUM,
                                            Symbol.STRING));
          }
        // It must be a string property.
        if (!(second instanceof AbstractString))
          return type_error(second, Symbol.STRING);
        mode.setProperty(property, second.getStringValue());
        return second;
      }
    };

  // ### set-buffer-property
  private static final Primitive SET_BUFFER_PROPERTY =
    new Primitive("set-buffer-property", PACKAGE_J, true,
                  "key value &optional buffer")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        return execute(first, second,
                       new JavaObject(Editor.currentEditor().getBuffer()));
      }
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        if (!(first instanceof AbstractString))
          return type_error(first, Symbol.STRING);
        String key = first.getStringValue();
        Property property = Property.findProperty(key);
        if (property == null)
          {
            // Not an advertised property.
            return error(new LispError(first.writeToString() +
                                       " does not designate any property."));
          }
        final Buffer buffer = checkBuffer(third);
        if (property.isBooleanProperty())
          {
            buffer.setProperty(property, second == NIL ? false : true);
            return second;
          }
        if (property.isIntegerProperty())
          {
            if (second instanceof Fixnum)
              {
                buffer.setProperty(property, ((Fixnum)second).value);
                return second;
              }
            if (second instanceof AbstractString)
              {
                int value;
                try
                  {
                    value = Integer.parseInt(second.getStringValue());
                  }
                catch (NumberFormatException e)
                  {
                    return error(new LispError(second.writeToString() +
                                               " cannot be converted to a Java integer."));
                  }
                buffer.setProperty(property, value);
                return second;
              }
            return type_error(second, list3(Symbol.OR, Symbol.FIXNUM,
                                            Symbol.STRING));
          }
        // It must be a string property.
        if (!(second instanceof AbstractString))
          return type_error(second, Symbol.STRING);
        buffer.setProperty(property, second.getStringValue());
        return second;
      }
    };

  // ### get-global-property key => value
  private static final Primitive GET_GLOBAL_PROPERTY =
    new Primitive("get-global-property", PACKAGE_J, true, "key")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        String key = javaString(arg);
        Property property = Property.findProperty(key);
        if (property == null)
          {
            // Not an advertised property.
            return error(new LispError(arg.writeToString() +
                                       " does not designate a property."));
          }
        if (property.isBooleanProperty())
          return preferences.getBooleanProperty(property) ? T : NIL;
        if (property.isIntegerProperty())
          return number(preferences.getIntegerProperty(property));
        String value = preferences.getStringProperty(property);
        return value != null ? new SimpleString(value) : NIL;
      }
    };

  // ### get-buffer-property key &optional buffer => value
  private static final Primitive GET_BUFFER_PROPERTY =
    new Primitive("get-buffer-property", PACKAGE_J, true,
                  "key &optional buffer")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return execute(arg,
                       new JavaObject(Editor.currentEditor().getBuffer()));
      }
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        String key = javaString(first);
        Property property = Property.findProperty(key);
        if (property == null)
          {
            // Not an advertised property.
            return error(new LispError(first.writeToString() +
                                       " does not designate any property."));
          }
        final Buffer buffer = checkBuffer(second);
        if (property.isBooleanProperty())
          return buffer.getBooleanProperty(property) ? T : NIL;
        if (property.isIntegerProperty())
          return number(buffer.getIntegerProperty(property));
        String value = buffer.getStringProperty(property);
        return value != null ? new SimpleString(value) : NIL;
      }
    };

  // ### insert
  private static final Primitive INSERT =
    new Primitive("insert", PACKAGE_J, true, "&rest args")
    {
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        if (args.length == 0)
          return NIL;
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
          return NIL;
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        try
          {
            for (int i = 0; i < args.length; i++)
              {
                LispObject obj = args[i];
                if (obj instanceof LispCharacter)
                  {
                    char c = ((LispCharacter)obj).value;
                    if (c == '\n')
                      {
                        editor.insertLineSeparator();
                        editor.moveCaretToDotCol();
                      }
                    else
                      editor.insertChar(c);
                  }
                else if (obj instanceof AbstractString)
                  {
                    editor.insertString(obj.getStringValue());
                  }
                else
                  return type_error(obj, list3(Symbol.OR, Symbol.CHARACTER,
                                               Symbol.STRING));
              }
            return NIL;
          }
        finally
          {
            editor.endCompoundEdit(compoundEdit);
          }
      }
    };

  // ### delete-region => nil
  private static final Primitive DELETE_REGION =
    new Primitive("delete-region", PACKAGE_J, true)
    {
      public LispObject execute() throws ConditionThrowable
      {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
          return NIL;
        editor.deleteRegion();
        return NIL;
      }
    };

  // ### set-mark pos => pos
  private static final Primitive SET_MARK =
    new Primitive("set-mark", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final Editor editor = Editor.currentEditor();
        if (arg != NIL)
          editor.setMark(checkMark(arg));
        else
          editor.unmark();
        return arg;
      }
    };

  // ### copy-mark mark => copy
  private static final Primitive COPY_MARK =
    new Primitive("copy-mark", PACKAGE_J, true, "mark")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = checkMark(arg);
        return new JavaObject(new Position(pos.getLine(), pos.getOffset()));
      }
    };

  // ### mark= mark1 mark2 => generalized-boolean
  private static final Primitive MARK_EQUAL =
    new Primitive("mark=", PACKAGE_J, true, "mark1 mark2")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        Position pos1 = checkMark(first);
        Position pos2 = checkMark(second);
        if (pos1.getLine() == pos2.getLine())
          if (pos1.getOffset() == pos2.getOffset())
            return T;
        return NIL;
      }
    };

  // ### undo
  private static final Primitive UNDO =
    new Primitive("undo", PACKAGE_J, true, "&optional count")
    {
      public LispObject execute() throws ConditionThrowable
      {
        Editor.currentEditor().undo();
        return NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Editor editor = Editor.currentEditor();
        int count;
        if (arg == NIL)
          count = 1;
        else
          count = Fixnum.getValue(arg);
        for (int i = 0; i < count; i++)
          editor.undo();
        return NIL;
      }
    };

  // ### begin-compound-edit
  private static final Primitive BEGIN_COMPOUND_EDIT =
    new Primitive("begin-compound-edit", PACKAGE_J, false)
    {
      public LispObject execute()
      {
        return new JavaObject(Editor.currentEditor().beginCompoundEdit());
      }
    };

  // ### end-compound-edit
  private static final Primitive END_COMPOUND_EDIT =
    new Primitive("end-compound-edit", PACKAGE_J, false)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            CompoundEdit compoundEdit =
              (CompoundEdit) ((JavaObject)arg).getObject();
            Editor.currentEditor().endCompoundEdit(compoundEdit);
            return NIL;
          }
        catch (ClassCastException e)
          {
            return error(new LispError(arg.writeToString() +
                                       " does not designate a compound edit."));
          }
      }
    };

  // ### %log-debug
  private static final Primitive _LOG_DEBUG =
    new Primitive("%log-debug", PACKAGE_J, false)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Log.debug(arg.getStringValue());
        return arg;
      }
    };

  // ### get-last-event-time
  private static final Primitive GET_LAST_EVENT_INTERNAL_TIME =
    new Primitive("get-last-event-internal-time", PACKAGE_J, true)
    {
      public LispObject execute() throws ConditionThrowable
      {
        return number(Dispatcher.getLastEventMillis());
      }
    };

  public static void invokeOpenFileHook(Buffer buffer)
  {
    try
      {
        Primitives.FUNCALL.execute(PACKAGE_J.intern("INVOKE-HOOK"),
                                   PACKAGE_J.intern("OPEN-FILE-HOOK"),
                                   new JavaObject(buffer));
      }
    catch (Throwable t)
      {
        Log.debug(t);
      }
  }

  public static void invokeBufferActivatedHook(Buffer buffer)
  {
    if (buffer != null)
      {
        try
          {
            Primitives.FUNCALL.execute(PACKAGE_J.intern("INVOKE-HOOK"),
                                       PACKAGE_J.intern("BUFFER-ACTIVATED-HOOK"),
                                       new JavaObject(buffer));
          }
        catch (Throwable t)
          {
            Log.debug(t);
          }
      }
  }

  public static void invokeAfterSaveHook(Buffer buffer)
  {
    try
      {
        Primitives.FUNCALL.execute(PACKAGE_J.intern("INVOKE-HOOK"),
                                   PACKAGE_J.intern("AFTER-SAVE-HOOK"),
                                   new JavaObject(buffer));
      }
    catch (Throwable t)
      {
        Log.debug(t);
      }
  }

  public static void invokeLispShellStartupHook(Buffer buffer, String command)
  {
    try
      {
        Primitives.FUNCALL.execute(PACKAGE_J.intern("INVOKE-HOOK"),
                                   PACKAGE_J.intern("LISP-SHELL-STARTUP-HOOK"),
                                   new JavaObject(buffer),
                                   new SimpleString(command));
      }
    catch (Throwable t)
      {
        Log.debug(t);
      }
  }

  // ### invoke-later
  public static final Primitive INVOKE_LATER =
    new Primitive("invoke-later", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final LispObject fun;
        if (arg instanceof Symbol)
          fun = arg.getSymbolFunction();
        else
          fun = arg;
        if (fun instanceof Function || fun instanceof GenericFunction)
          {
            Runnable r = new Runnable()
              {
                public void run()
                {
                  try
                    {
                      LispThread.currentThread().execute(fun);
                    }
                  catch (Throwable t)
                    {
                      Log.error(t);
                    }
                }
              };
            SwingUtilities.invokeLater(r);
            return NIL;
          }
        return error(new UndefinedFunction(arg));
      }
    };

  // ### make-buffer-stream buffer => stream
  private static final Primitive MAKE_BUFFER_STREAM =
    new Primitive("make-buffer-stream", PACKAGE_J, true)
    {
      public LispObject execute() throws ConditionThrowable
      {
        return new BufferStream(new Buffer(0));
      }

      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return new BufferStream(checkBuffer(arg));
      }
    };

  // ### buffer-stream-buffer stream => buffer
  private static final Primitive BUFFER_STREAM_BUFFER =
    new Primitive("buffer-stream-buffer", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof BufferStream)
          return new JavaObject(((BufferStream)arg).getBuffer());
        return error(new LispError(arg.writeToString() +
                                   "does not designate a buffer stream."));
      }
    };

  // ### pop-to-buffer buffer => buffer
  private static final Primitive POP_TO_BUFFER =
    new Primitive("pop-to-buffer", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg != NIL)
          {
            Buffer buffer = checkBuffer(arg);
            Editor editor = Editor.currentEditor();
            editor.makeNext(buffer);
            editor.activateInOtherWindow(buffer);
          }
        return arg;
      }
    };

  // ### switch-to-buffer buffer => buffer
  private static final Primitive SWITCH_TO_BUFFER =
    new Primitive("switch-to-buffer", PACKAGE_J, true)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Buffer buffer = checkBuffer(arg);
        Editor editor = Editor.currentEditor();
        editor.makeNext(buffer);
        editor.activate(buffer);
        return arg;
      }
    };

  // ### buffer-modified-p buffer => boolean
  private static final Primitive BUFFER_MODIFIED_P =
    new Primitive("buffer-modified-p", PACKAGE_J, true, "buffer")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return checkBuffer(arg).isModified() ? T : NIL;
      }
    };

  // ### set-buffer-modified-p buffer flag => flag
  private static final Primitive SET_BUFFER_MODIFIED_P =
    new Primitive("set-buffer-modified-p", PACKAGE_J, true, "buffer flag")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final Buffer buffer = checkBuffer(first);
        if (second == NIL)
          buffer.unmodified();
        else
          buffer.modified();
        return second;
      }
    };

  // ### %status string &optional editor => generalized-boolean
  private static final Primitive STATUS =
    new Primitive("status", PACKAGE_J, true, "string &optional editor")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof AbstractString)
          {
            final String s = ((AbstractString)arg).getStringValue();
            Runnable r = new Runnable()
              {
                public void run()
                {
                  try
                    {
                      Editor.currentEditor().status(s);
                    }
                  catch (Throwable t)
                    {
                      Log.error(t);
                    }
                }
              };
            SwingUtilities.invokeLater(r);
            return T;
          }
        return type_error(arg, Symbol.STRING);
      }
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        if (first instanceof AbstractString)
          {
            final String s = ((AbstractString)first).getStringValue();
            final Editor editor = checkEditor(second);
            Runnable r = new Runnable()
              {
                public void run()
                {
                  try
                    {
                      editor.status(s);
                    }
                  catch (Throwable t) {
                    Log.error(t);
                  }
                }
              };
            SwingUtilities.invokeLater(r);
            return T;
          }
        return type_error(first, Symbol.STRING);
      }
    };

  // ### %search
  private static final Primitive _SEARCH =
    new Primitive("%search", PACKAGE_J, false,
                  "pattern direction regexp-p buffer start ignore-case-p whole-words-only-p")
    {
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        if (args.length != 7)
          return error(new WrongNumberOfArgumentsException(this));
        final String pattern;
        if (args[0] instanceof AbstractString)
          pattern = args[0].getStringValue();
        else
          return type_error(args[0], Symbol.STRING);
        final boolean backward;
        Symbol direction = checkSymbol(args[1]);
        if (direction == NIL || direction.getName().equals("BACKWARD"))
          backward = true;
        else if (direction.getName().equals("FORWARD"))
          backward = false;
        else
          return error(new LispError("Invalid direction " + direction.writeToString()));
        final Buffer buffer = checkBuffer(args[3]);
        final Position start;
        if (args[4] == NIL)
          start = Editor.currentEditor().getDot();
        else
          start = checkMark(args[4]);
        final boolean ignoreCase = (args[5] != NIL);
        final boolean wholeWordsOnly = (args[6] != NIL);
        Search search = new Search(pattern, ignoreCase, wholeWordsOnly);
        final Position pos;
        if (args[2] != NIL)
          {
            try
              {
                search.setREFromPattern();
              }
            catch (REException e)
              {
                return error(new LispError("Invalid regular expression: \"" +
                                           pattern + '"'));
              }
            if (backward)
              pos = search.reverseFindRegExp(buffer, start);
            else
              pos = search.findRegExp(buffer, start);
          }
        else
          {
            if (backward)
              pos = search.reverseFindString(buffer, start);
            else
              pos = search.findString(buffer, start);
          }
        return pos != null ? new JavaObject(pos) : NIL;
      }
    };

  // ### find-file-buffer pathname => buffer
  private static final Primitive FIND_FILE_BUFFER =
    new Primitive("find-file-buffer", PACKAGE_J, true, "pathname")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final Pathname pathname = Pathname.coerceToPathname(arg);
        final String namestring = pathname.getNamestring();
        if (namestring != null)
          {
            final Editor editor = Editor.currentEditor();
            final Buffer buffer = editor.getBuffer(File.getInstance(namestring));
            if (buffer != null)
              return new JavaObject(buffer);
          }
        return NIL;
      }
    };

  // ### find-beginning-of-defun &optional mark
  private static final Primitive FIND_BEGINNING_OF_DEFUN =
    new Primitive("find-beginning-of-defun", PACKAGE_J, true, "&optional mark")
    {
      public LispObject execute() throws ConditionThrowable
      {
        Position pos =
          LispMode.findBeginningOfDefun(Editor.currentEditor().getDot());
        return pos != null ? new JavaObject(pos) : NIL;
      }
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Position pos = LispMode.findBeginningOfDefun(checkMark(arg));
        return pos != null ? new JavaObject(pos) : NIL;
      }
    };

  // ### defun-at-point => string
  private static final Primitive DEFUN_AT_POINT =
    new Primitive("defun-at-point", PACKAGE_J, true, "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        String s = LispMode.getCurrentDefun(Editor.currentEditor());
        return s != null ? new SimpleString(s) : NIL;
      }
    };

  // ### forward-sexp
  private static final Primitive FORWARD_SEXP =
    new Primitive("forward-sexp", PACKAGE_J, true, "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        LispMode.forwardSexp();
        return NIL;
      }
    };

  // ### backward-sexp
  private static final Primitive BACKWARD_SEXP =
    new Primitive("backward-sexp", PACKAGE_J, true, "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        LispMode.backwardSexp();
        return NIL;
      }
    };
}
