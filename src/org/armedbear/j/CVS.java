/*
 * CVS.java
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

import java.util.List;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;

public final class CVS extends VersionControl implements Constants
{
  public static void cvs()
  {
    MessageDialog.showMessageDialog(
      "The command \"cvs\" requires an argument",
      "Error");
  }

  public static void cvs(String args)
  {
    final Editor editor = Editor.currentEditor();
    final Buffer buffer = editor.getBuffer();
    if (buffer.getFile() == null)
      return;
    buffer.setBusy(true);
    editor.setWaitCursor();
    final String name = buffer.getFile().getName();
    FastStringBuffer sb = new FastStringBuffer("cvs ");
    sb.append(args);
    // "cvs -H" doesn't need a filename.
    if (!args.trim().startsWith("-H "))
      {
        sb.append(' ');
        if (name.indexOf(' ') >= 0)
          {
            // Enclose filename in double quotes since it contains an embedded
            // space.
            sb.append('"');
            sb.append(name);
            sb.append('"');
          }
        else
          sb.append(name);
      }
    final String cmd = sb.toString();
    Runnable commandRunnable = new Runnable()
      {
        public void run()
        {
          final String output =
            command(cmd, buffer.getCurrentDirectory());
          Runnable completionRunnable = new Runnable()
            {
              public void run()
              {
                cvsCompleted(editor, buffer, cmd, output);
              }
            };
          SwingUtilities.invokeLater(completionRunnable);
        }
      };
    new Thread(commandRunnable).start();
  }

  private static void cvsCompleted(Editor editor, Buffer buffer, String cmd,
                                   String output)
  {
    if (output != null && output.length() > 0)
      {
        Buffer buf;
        if (cmd.startsWith("cvs diff"))
          buf = new DiffOutputBuffer(buffer, output, VC_CVS);
        else
          buf = OutputBuffer.getOutputBuffer(output);
        buf.setTitle(cmd);
        editor.makeNext(buf);
        editor.activateInOtherWindow(buf);
      }
    buffer.checkCVS();
    buffer.setBusy(false);
    for (EditorIterator it = new EditorIterator(); it.hasNext();)
      {
        Editor ed = it.nextEditor();
        if (ed.getBuffer() == buffer)
          {
            ed.setDefaultCursor();
            // Update CVS information in status bar.
            ed.getFrame().repaintStatusBar();
          }
      }
  }

  public static void add()
  {
    final Editor editor = Editor.currentEditor();
    final Buffer buffer = editor.getBuffer();
    if (buffer.getFile() == null)
      return;
    buffer.setBusy(true);
    editor.setWaitCursor();
    final String name = buffer.getFile().getName();
    FastStringBuffer sb = new FastStringBuffer("cvs add ");
    if (name.indexOf(' ') >= 0)
      {
        // Enclose filename in double quotes since it contains an embedded
        // space.
        sb.append('"');
        sb.append(name);
        sb.append('"');
      }
    else
      sb.append(name);
    final String cmd = sb.toString();
    Runnable commandRunnable = new Runnable()
      {
        public void run()
        {
          final String output =
            command(cmd, buffer.getCurrentDirectory());
          Runnable completionRunnable = new Runnable()
            {
              public void run()
              {
                addCompleted(editor, buffer, cmd, output);
              }
            };
          SwingUtilities.invokeLater(completionRunnable);
        }
      };
    new Thread(commandRunnable).start();
  }

  private static void addCompleted(Editor editor, Buffer buffer,
                                   String cmd, String output)
  {
    OutputBuffer buf = OutputBuffer.getOutputBuffer(output);
    buf.setTitle(cmd);
    editor.makeNext(buf);
    editor.activateInOtherWindow(buf);
    buffer.checkCVS();
    buffer.setBusy(false);
    for (EditorIterator it = new EditorIterator(); it.hasNext();)
      {
        Editor ed = it.nextEditor();
        if (ed.getBuffer() == buffer)
          {
            ed.setDefaultCursor();
            // Update CVS information in status bar.
            ed.getFrame().repaintStatusBar();
          }
      }
  }

  public static void commit()
  {
    final Editor editor = Editor.currentEditor();
    Buffer parentBuffer = editor.getBuffer();
    if (parentBuffer instanceof DiffOutputBuffer)
      parentBuffer = parentBuffer.getParentBuffer();
    if (parentBuffer == null)
      return;
    if (parentBuffer.getFile() == null)
      return;
    final String title =
      "cvs commit ".concat(parentBuffer.getFile().getName());
    boolean save = false;
    if (parentBuffer.isModified())
      {
        int response =
          ConfirmDialog.showConfirmDialogWithCancelButton(editor,
                                                          CHECK_SAVE_PROMPT,
                                                          title);
        switch (response)
          {
          case RESPONSE_YES:
            save = true;
            break;
          case RESPONSE_NO:
            break;
          case RESPONSE_CANCEL:
            return;
          }
        editor.repaintNow();
      }
    if (!save || parentBuffer.save())
      {
        // Look for existing checkin buffer before making a new one.
        CheckinBuffer checkinBuffer = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();)
          {
            Buffer buf = it.nextBuffer();
            if (buf instanceof CheckinBuffer)
              {
                if (buf.getParentBuffer() == parentBuffer)
                  {
                    checkinBuffer = (CheckinBuffer) buf;
                    break;
                  }
              }
          }
        if (checkinBuffer == null)
          {
            checkinBuffer = new CheckinBuffer(parentBuffer, VC_CVS);
            checkinBuffer.setFormatter(new PlainTextFormatter(checkinBuffer));
            checkinBuffer.setTitle(title);
          }
        editor.makeNext(checkinBuffer);
        editor.activateInOtherWindow(checkinBuffer);
      }
  }

  public static void diff()
  {
    diff(null);
  }

  public static void diff(String args)
  {
    final Editor editor = Editor.currentEditor();
    Buffer parentBuffer = editor.getBuffer();
    if (parentBuffer instanceof CheckinBuffer)
      parentBuffer = parentBuffer.getParentBuffer();
    if (parentBuffer.getFile() == null)
      return;
    final String name = parentBuffer.getFile().getName();
    if (args == null)
      args = "-u";
    FastStringBuffer sb = new FastStringBuffer("cvs diff ");
    sb.append(args);
    sb.append(' ');
    if (name.indexOf(' ') >= 0)
      {
        // Enclose filename in double quotes since it contains an embedded
        // space.
        sb.append('"');
        sb.append(name);
        sb.append('"');
      }
    else
      sb.append(name);
    final String cmd = sb.toString();
    boolean save = false;
    if (parentBuffer.isModified())
      {
        int response =
          ConfirmDialog.showConfirmDialogWithCancelButton(editor,
                                                          CHECK_SAVE_PROMPT, cmd);
        switch (response)
          {
          case RESPONSE_YES:
            save = true;
            break;
          case RESPONSE_NO:
            break;
          case RESPONSE_CANCEL:
            return;
          }
        editor.repaintNow();
      }
    parentBuffer.setBusy(true);
    if (!save || parentBuffer.save())
      {
        // Kill existing diff output buffer if any for same parent buffer.
        for (BufferIterator it = new BufferIterator(); it.hasNext();)
          {
            Buffer b = it.nextBuffer();
            if (b instanceof DiffOutputBuffer)
              {
                if (b.getParentBuffer() == parentBuffer)
                  {
                    b.kill();
                    break; // There should be one at most.
                  }
              }
          }
        final Buffer finalParentBuffer = parentBuffer;
        Runnable commandRunnable = new Runnable()
          {
            public void run()
            {
              final String output =
                command(cmd, finalParentBuffer.getCurrentDirectory());
              Runnable completionRunnable = new Runnable()
                {
                  public void run()
                  {
                    diffCompleted(editor, finalParentBuffer, cmd,
                                  output, VC_CVS);
                  }
                };
              SwingUtilities.invokeLater(completionRunnable);
            }
          };
        new Thread(commandRunnable).start();
      }
  }

  public static void diffDir()
  {
    final Editor editor = Editor.currentEditor();
    final Buffer buffer = editor.getBuffer();
    final String cmd = "cvs diff -u";
    final File directory = buffer.getCurrentDirectory();
    // Kill existing diff output buffer if any for same directory.
    for (BufferIterator it = new BufferIterator(); it.hasNext();)
      {
        Buffer b = it.nextBuffer();
        if (b instanceof DiffOutputBuffer)
          {
            if (directory.equals(((DiffOutputBuffer)b).getDirectory()))
              {
                b.kill();
                break; // There should be one at most.
              }
          }
      }
    final DiffOutputBuffer buf =
      new DiffOutputBuffer(directory, null, VC_CVS);
    buf.setTitle(cmd);
    editor.makeNext(buf);
    Editor ed = editor.activateInOtherWindow(buf);
    ed.setWaitCursor();
    buf.setBusy(true);
    Runnable commandRunnable = new Runnable()
      {
        public void run()
        {
          final String output = command(cmd, directory);
          Runnable completionRunnable = new Runnable()
            {
              public void run()
              {
                processCompleted(buf, output);
              }
            };
          SwingUtilities.invokeLater(completionRunnable);
        }
      };
    new Thread(commandRunnable).start();
  }

  public static void replaceComment(final Editor editor, final String comment)
  {
    if (!(editor.getBuffer() instanceof CheckinBuffer))
      {
        Debug.bug();
        return;
      }
    final CheckinBuffer buffer = (CheckinBuffer) editor.getBuffer();
    String oldComment = extractComment(buffer);
    if (oldComment.equals(comment))
      return;
    insertComment(editor, comment);
  }

  public static String extractComment(final CheckinBuffer buffer)
  {
    return buffer.getText();
  }

  private static void insertComment(final Editor editor, final String comment)
  {
    final CheckinBuffer buffer = (CheckinBuffer) editor.getBuffer();
    try
      {
        buffer.lockWrite();
      }
    catch (InterruptedException e)
      {
        Log.error(e);
        return;
      }
    try
      {
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        editor.selectAll();
        editor.deleteRegion();
        editor.insertString(comment);
        editor.endCompoundEdit(compoundEdit);
        buffer.modified();
      }
    finally
      {
        buffer.unlockWrite();
      }
    final Position end = buffer.getEnd();
    for (EditorIterator it = new EditorIterator(); it.hasNext();)
      {
        Editor ed = it.nextEditor();
        if (ed.getBuffer() == buffer)
          {
            ed.setTopLine(buffer.getFirstLine());
            ed.setDot(end.copy()); // No undo.
            ed.moveCaretToDotCol();
            ed.setUpdateFlag(REPAINT);
            ed.updateDisplay();
          }
      }
  }

  public static void finish(final Editor editor, final CheckinBuffer checkinBuffer)
  {
    final Buffer parentBuffer = checkinBuffer.getParentBuffer();
    if (parentBuffer.getFile() == null)
      return;
    final String name = parentBuffer.getFile().getName();
    final File tempFile = Utilities.getTempFile();
    if (!checkinBuffer.writeFile(tempFile))
      {
        MessageDialog.showMessageDialog(editor,
                                        "Unable to write temporary file ".concat(tempFile.canonicalPath()),
                                        "Error");
        return;
      }
    FastStringBuffer sb = new FastStringBuffer("cvs commit -F ");
    // Enclose both filenames in double quotes in case they contain
    // embedded spaces.
    sb.append('"');
    sb.append(tempFile.canonicalPath());
    sb.append('"');
    sb.append(' ');
    sb.append('"');
    sb.append(name);
    sb.append('"');
    final String cmd = sb.toString();
    editor.setWaitCursor();
    checkinBuffer.setBusy(true);
    parentBuffer.setBusy(true);
    Runnable commandRunnable = new Runnable()
      {
        public void run()
        {
          final CvsCommand cvsCommand =
            new CvsCommand(cmd, parentBuffer.getCurrentDirectory());
          cvsCommand.run();
          tempFile.delete();
          Runnable completionRunnable = new Runnable()
            {
              public void run()
              {
                finishCompleted(editor, checkinBuffer, parentBuffer,
                                cvsCommand, name, tempFile);
              }
            };
          SwingUtilities.invokeLater(completionRunnable);
        }
      };
    new Thread(commandRunnable).start();
  }

  private static void finishCompleted(Editor editor, Buffer checkinBuffer,
                                      Buffer parentBuffer, CvsCommand cvsCommand,
                                      String name, File tempFile)
  {
    checkinBuffer.setBusy(false);
    if (cvsCommand.exitValue() != 0)
      {
        // Error.
        OutputBuffer buf = OutputBuffer.getOutputBuffer(cvsCommand.getOutput());
        buf.setTitle("cvs commit ".concat(name));
        editor.makeNext(buf);
        editor.activate(buf);
        editor.updateDisplay();
      }
    else
      {
        // Success. Kill old diff buffer, if any; its contents are no
        // longer correct.
        for (BufferIterator it = new BufferIterator(); it.hasNext();)
          {
            Buffer b = it.nextBuffer();
            if (b instanceof DiffOutputBuffer)
              {
                if (b.getParentBuffer() == parentBuffer)
                  {
                    b.kill();
                    break; // There should be one at most.
                  }
              }
          }
        if (Editor.getBufferList().contains(checkinBuffer))
          checkinBuffer.kill();
        if (editor.getOtherEditor() != null)
          {
            editor.otherWindow();
            editor.unsplitWindow();
          }
        else
          editor.updateDisplay();
      }
    // The source file may have been modified by the checkin process.
    editor.reactivate(parentBuffer);
    parentBuffer.checkCVS();
    parentBuffer.setBusy(false);
    for (EditorIterator it = new EditorIterator(); it.hasNext();)
      {
        Editor ed = it.nextEditor();
        if (ed.getBuffer().isBusy())
          ed.setWaitCursor();
        else
          ed.setDefaultCursor();
        // Update CVS information in status bar.
        if (ed.getBuffer() == parentBuffer)
          ed.getFrame().repaintStatusBar();
      }
    Editor.restoreFocus();
  }

  public static void log()
  {
    log("");
  }

  public static void log(String args)
  {
    boolean useCurrentFile = true;
    List list = Utilities.tokenize(args);
    for (int i = 0; i < list.size(); i++)
      {
        String arg = (String) list.get(i);
        if (arg.charAt(0) != '-')
          {
            // Must be a filename.
            useCurrentFile = false;
            break;
          }
      }
    final Editor editor = Editor.currentEditor();
    final Buffer parentBuffer = editor.getBuffer();
    FastStringBuffer sb = new FastStringBuffer("cvs log ");
    sb.append(args);
    if (useCurrentFile)
      {
        if (parentBuffer.getFile() == null)
          return;
        final String name = parentBuffer.getFile().getName();
        if (sb.charAt(sb.length() - 1) != ' ')
          sb.append(' ');
        if (name.indexOf(' ') >= 0)
          {
            // Enclose filename in double quotes since it contains an
            // embedded space.
            sb.append('"');
            sb.append(name);
            sb.append('"');
          }
        else
          sb.append(name);
      }
    final String cmd = sb.toString();
    editor.setWaitCursor();
    final String output = command(cmd, parentBuffer.getCurrentDirectory());
    OutputBuffer buf = OutputBuffer.getOutputBuffer(output);
    buf.setTitle(cmd);
    editor.makeNext(buf);
    editor.activateInOtherWindow(buf);
    editor.setDefaultCursor();
  }

  // Implementation.
  private static final String command(String cmd, File workingDirectory)
  {
    CvsCommand cvsCommand = new CvsCommand(cmd, workingDirectory);
    cvsCommand.run();
    return cvsCommand.getOutput();
  }

  private static final class CvsCommand
  {
    final private String cmd;
    final private File workingDirectory;
    private ShellCommand shellCommand;

    public CvsCommand(String cmd, File workingDirectory)
    {
      this.cmd = cmd;
      this.workingDirectory = workingDirectory;
    }

    public void run()
    {
      shellCommand = new ShellCommand(cmd, workingDirectory);
      shellCommand.run();
    }

    public final String getOutput()
    {
      Debug.assertTrue(shellCommand != null);
      return shellCommand.getOutput();
    }

    public final int exitValue()
    {
      Debug.assertTrue(shellCommand != null);
      return shellCommand.exitValue();
    }
  }
}
