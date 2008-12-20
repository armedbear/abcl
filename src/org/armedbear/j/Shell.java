/*
 * Shell.java
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

import gnu.regexp.REMatch;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.StringTokenizer;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;

public class Shell extends CommandInterpreter implements Constants
{
    protected static final String JPTY_NOT_FOUND =
        "Unable to start shell process (jpty not found in PATH)";

    private Process process;
    private String command; // First token on command line.
    private boolean promptIsStderr = true;
    private File oldDir;
    private File currentDir;
    private File initialDir;
    private boolean cygnify;

    protected Shell()
    {
        type = TYPE_SHELL;
        mode = Editor.getModeList().getMode(SHELL_MODE);
        formatter = mode.getFormatter(this);
        setInitialized(true);
    }

    protected Shell(String shellCommand)
    {
        this();
        this.shellCommand = shellCommand;
        if (shellCommand != null) {
            if (shellCommand.indexOf("tcsh") >= 0)
                promptIsStderr = false;
            else if (shellCommand.indexOf("cmd.exe") >= 0)
                promptIsStderr = false;
        }
    }

    protected Shell(String shellCommand, Mode mode)
    {
        type = TYPE_SHELL;
        this.shellCommand = shellCommand;
        this.mode = mode;
        formatter = mode.getFormatter(this);
        setInitialized(true);
    }

    protected synchronized Process getProcess()
    {
        return process;
    }

    protected synchronized void setProcess(Process p)
    {
        process = p;
    }

    private static final String getDefaultShellCommand()
    {
        String s =
            Editor.preferences().getStringProperty(Property.SHELL_FILE_NAME);
        if (s != null && s.length() > 0)
            return s;
        return Platform.isPlatformWindows() ? "cmd.exe /q" : "bash -i";
    }

    protected void initializeHistory()
    {
        history = new History("shell.history", 30);
    }

    private static Shell createShell(String shellCommand)
    {
        if (shellCommand == null) {
            Debug.bug();
            return null;
        }
        Shell shell = new Shell(shellCommand);
        shell.startProcess();
        if (shell.getProcess() == null) {
            Editor.getBufferList().remove(shell);
            String message =
                "Unable to start shell process \"" + shell.shellCommand + "\"";
            MessageDialog.showMessageDialog(message, "Error");
            return null;
        }
        shell.needsRenumbering = true;
        return shell;
    }

    protected void startProcess()
    {
        if (shellCommand == null) {
            Debug.bug();
            return;
        }
        if (Platform.isPlatformWindows())
            if (shellCommand.toLowerCase().indexOf("cmd.exe") < 0)
                cygnify = true;
        // Only set initialDir the first time we run, so that if we restart
        // this shell, it will start up in the same directory each time.
        if (initialDir == null) {
            initialDir = Editor.currentEditor().getCurrentDirectory();
            if (initialDir == null || initialDir.isRemote())
                initialDir = Directories.getUserHomeDirectory();
        }
        // Shell command may contain a space (e.g. "bash -i").
        StringTokenizer st = new StringTokenizer(shellCommand);
        String[] cmdArray;
        int i = 0;
        if (Utilities.haveJpty()) {
            cmdArray = new String[st.countTokens() + 1];
            cmdArray[i++] = "jpty";
        } else
            cmdArray = new String[st.countTokens()];
        while (st.hasMoreTokens())
            cmdArray[i++] = st.nextToken();
        Process p = null;
        try {
            p = Runtime.getRuntime().exec(cmdArray, null,
                                          new java.io.File(initialDir.canonicalPath()));
            setProcess(p);
        }
        catch (Throwable t) {
            setProcess(null);
            return;
        }
        currentDir = initialDir;
        startWatcherThread();
        // See if the process exits right away (meaning jpty couldn't launch
        // the shell command).
        try {
            Thread.sleep(100);
        }
        catch (InterruptedException e) {
            Log.error(e);
        }
        // When the process exits, the watcher thread calls setProcess(null),
        // so check the value of getProcess() here.
        if (getProcess() == null)
            return; // Process exited.
        if (Platform.isPlatformWindows() &&
            shellCommand.toLowerCase().indexOf("cmd.exe") >= 0)
            setPromptRE(DEFAULT_CMD_EXE_PROMPT_PATTERN);
        else
            setPromptRE(Editor.preferences().getStringProperty(Property.SHELL_PROMPT_PATTERN));
        try {
            stdin  = new OutputStreamWriter(p.getOutputStream());
            stdoutThread = new StdoutThread(p.getInputStream());
            stderrThread = new StderrThread(p.getErrorStream());
            stdoutThread.setTimeOut(0);
            stderrThread.setTimeOut(0);
            stdoutThread.start();
            stderrThread.start();
            readOnly = false;
        }
        catch (Throwable t) {
            Log.error(t);
        }
    }

    public void dispose()
    {
        if (!checkProcess()) {
            Log.debug("checkProcess returned false");
            return;
        }
        Thread thread = new Thread("shell dispose") {
            public void run()
            {
                try {
                    stdin.write(3);
                    stdin.flush();
                    stdin.write("exit\n");
                    stdin.flush();
                    stdin.close();
                    final Process p = getProcess();
                    if (p != null) {
                        p.destroy();
                        p.waitFor();
                    }
                }
                catch (IOException e) {
                    Log.error(e);
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    protected void enter(final String s)
    {
        super.enter(s);
        // If it's a local shell (i.e. not telnet or ssh), keep track of the
        // current directory.
        if (type == TYPE_SHELL) {
            ShellTokenizer st = new ShellTokenizer(s);
            command = s.trim();
            String arg = null;
            if (st.hasMoreTokens()) {
                command = st.nextToken();
                if (st.hasMoreTokens())
                    arg = st.nextToken();
            }
            if (command.equals("cd")) {
                if (arg == null) {
                    // for cmd.exe, "cd" == "pwd"
                    if (shellCommand.indexOf("cmd.exe") < 0)
                        changeDirectory(Utilities.getUserHome());
                } else if (arg.equals("-")) {
                    if (oldDir != null)
                        changeDirectory(oldDir.canonicalPath());
                } else
                    changeDirectory(arg);
            } else if (command.equals("pushd"))
                changeDirectory(arg);
        }
    }

    protected boolean checkProcess()
    {
        Process p = getProcess();
        if (p == null)
            return false;
        if (Utilities.isProcessAlive(p))
            return true;
        // Not alive.
        setProcess(null);
        readOnly = true;
        resetUndo();
        return false;
    }

    protected void startWatcherThread()
    {
        Thread thread = new Thread("shell watcher") {
            public void run()
            {
                try {
                    Process p = getProcess();
                    if (p != null)
                        p.waitFor();
                    setProcess(null);
                    if (stdoutThread != null)
                        stdoutThread.join();
                    if (stderrThread != null)
                        stderrThread.join();
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
                Runnable processExitedRunnable = new Runnable() {
                    public void run()
                    {
                        appendString("\nProcess exited\n");
                        setBusy(false);
                        updateDisplayInAllFrames();
                    }
                };
                if (stderrThread != null)
                    SwingUtilities.invokeLater(processExitedRunnable);
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    private void tab()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getMark() != null)
            return;
        final Line dotLine = editor.getDotLine();
        final String dotLineText = dotLine.getText();
        final REMatch match = promptRE.getMatch(dotLineText);
        if (match == null)
            return; // Not at prompt.
        final String prompt = match.toString();
        final String userInput  = dotLineText.substring(match.getEndIndex());
        if (userInput.length() == 0)
            return; // Nothing to complete.
        final ShellTokenizer st = new ShellTokenizer(userInput);
        final String prefix = st.lastToken();
        if (prefix == null)
            return; // Nothing to complete.
        final String toBeCompleted = unescape(prefix);
        final Completion completion =
            new Completion(currentDir, toBeCompleted, shellCommand);
        final String toBeInserted = completion.toString();
        if (!toBeInserted.equals(prefix)) {
            CompoundEdit compoundEdit = beginCompoundEdit();
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().setOffset(dotLineText.lastIndexOf(prefix));
            // Remove prefix from line.
            final String head = dotLine.substring(0, editor.getDotOffset());
            final String tail = dotLine.substring(editor.getDotOffset() + prefix.length());
            editor.addUndo(SimpleEdit.LINE_EDIT);
            dotLine.setText(head.concat(tail));
            // Insert completion.
            editor.addUndo(SimpleEdit.INSERT_STRING);
            editor.insertStringInternal(toBeInserted);
            // Move dot past inserted string.
            editor.moveCaretToDotCol();
            endCompoundEdit(compoundEdit);
            Editor.updateInAllEditors(dotLine);
        } else {
            final List completions = completion.getCompletions();
            final int size = completions.size();
            if (size > 0) {
                dotLine.setFlags(STATE_INPUT);
                editor.insertLineSeparator();
                for (int i = 0; i < size; i++) {
                    String s = (String) completions.get(i);
                    s = unescape(s);
                    int index = s.lastIndexOf('/', s.length()-2);
                    if (index >= 0)
                        s = s.substring(index+1);
                    editor.insertStringInternal(s);
                    editor.getDotLine().setFlags(STATE_OUTPUT);
                    editor.insertLineSeparator();
                }
                if (prompt != null)
                    editor.insertStringInternal(prompt);
                editor.insertStringInternal(userInput);
                editor.getDotLine().setFlags(STATE_INPUT);
                editor.eob();
                editor.getDisplay().setReframe(-2);
                resetUndo();
            }
        }
    }

    private void updateDirectory(String output)
    {
        if (command != null) {
            String s = output;
            int index = s.indexOf('\r');
            if (index >= 0)
                s = s.substring(0, index);
            index = s.indexOf('\n');
            if (index >= 0)
                s = s.substring(0, index);
            if (command.equals("pwd") || command.equals("cd")) {
                changeDirectory(s);
            } else if (command.equals("popd")) {
                // BUG! Directory names with embedded spaces will not be
                // handled correctly!
                index = s.indexOf(' ');
                if (index >= 0)
                    s = s.substring(0, index);
                changeDirectory(s);
            }
        }
    }

    private void changeDirectory(String s)
    {
        if (s != null) {
            s = unescape(s).trim();
            if (s.length() > 0) {
                char c = s.charAt(0);
                if (c == '\'' || c == '"') {
                    s = s.substring(1);
                    final int length = s.length();
                    if (length > 0 && s.charAt(length-1) == c)
                        s = s.substring(0, length-1);
                }
            } else {
                // s.length() == 0
                if (shellCommand.indexOf("cmd.exe") < 0)
                    s = Utilities.getUserHome();
            }
            if (cygnify) {
                if (!s.startsWith(".."))
                    s = Utilities.uncygnify(s);
            }
            if (s.length() > 0) {
                File dir = File.getInstance(currentDir, s);
                if (dir != null && dir.isDirectory()) {
                    if (shellCommand.indexOf("cmd.exe") < 0)
                        oldDir = currentDir;
                    currentDir = dir;
                    for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                        Editor ed = it.nextEditor();
                        if (ed.getBuffer() == this)
                            ed.updateLocation();
                    }
                }
            }
        }
    }

    private String unescape(String s)
    {
        // Is '\' an escape character?
        boolean backslashIsEscape = Platform.isPlatformUnix() || cygnify;

        FastStringBuffer sb = new FastStringBuffer(s.length());
        char quoteChar = 0;
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (quoteChar != 0) {
                if (c == quoteChar)
                    quoteChar = 0;
                else
                    sb.append(c);
            } else if (c == '\'' || c == '"') {
                quoteChar = c;
            } else if (backslashIsEscape && c == '\\') {
                if (i < limit - 1)
                    sb.append(s.charAt(++i));
            } else
                sb.append(c);
        }
        return sb.toString();
    }

    public String getFileNameForDisplay()
    {
        return (currentDir != null) ? currentDir.canonicalPath() : "";
    }

    // For the buffer list.
    public String toString()
    {
        return shellCommand != null ? shellCommand : "";
    }

    public File getCurrentDirectory()
    {
        return currentDir;
    }

    public File getCompletionDirectory()
    {
        return currentDir;
    }

    protected void appendString(String s)
    {
        if (s.indexOf(0x1b) >= 0) {
            // Strip escape sequences used for ls colorization.
            int limit = s.length();
            FastStringBuffer sb = new FastStringBuffer(limit);
            int i = 0;
            while (i < limit) {
                char c = s.charAt(i++);
                if (c == 0x1b) {
                    // Skip escaped chars through 'm'.
                    while (i < limit && s.charAt(i++) != 'm')
                        ;
                } else
                    sb.append(c);
            }
            s = sb.toString();
        }
        super.appendString(s);
    }

    protected void stdOutUpdate(final String s)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                if (s.length() > 0) {
                    updateDirectory(s);
                    appendString(s);
                }
                updateLineFlags();
                updateDisplayInAllFrames();
                resetUndo();
                checkPasswordPrompt();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    protected void stdErrUpdate(final String s)
    {
        if (promptIsStderr) {
            REMatch match = promptRE.getMatch(s);
            if (match != null) {
                // This looks like the prompt.
                // Give stdout a chance to finish.
                try {
                    Thread.sleep(100);
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
            }
        }
        Runnable r = new Runnable() {
            public void run()
            {
                appendString(s);
                updateLineFlags();
                updateDisplayInAllFrames();
                resetUndo();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    protected void updateLineFlags()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        Position endOfOutput = getEndOfOutput();
        if (endOfOutput == null)
            return;
        final Line last = endOfOutput.getLine();
        if (isPasswordPrompt(last)) {
            last.setFlags(STATE_PASSWORD_PROMPT);
            return;
        }
        final int lastFlags = last.flags();
        if (lastFlags != STATE_INPUT && lastFlags != STATE_PROMPT)
            last.setFlags(0);
        // Look at the next-to-last line.
        final Line nextToLast = last.previous();
        // For now, this is a hard-coded test for Mikol's prompt. It should
        // really use a configurable regexp.
        if (nextToLast != null && nextToLast.getText().startsWith("| ")) {
            // Next-to-last line looks like first line of 2-line prompt.
            // See if the last line looks like the second line of the prompt.
            REMatch match = promptRE.getMatch(last.getText());
            if (match != null)
                nextToLast.setFlags(STATE_PROMPT);
        }
    }

    private boolean isPasswordPrompt(Line line)
    {
        final String text = line.trim();
        if (text.startsWith("Enter passphrase") && text.endsWith(":"))
            return true;
        if (text.toLowerCase().endsWith("password:"))
            return true;
        if (text.equals("Response:"))
            return true;
        return false;
    }

    protected void checkPasswordPrompt()
    {
        Position endOfOutput = getEndOfOutput();
        if (endOfOutput != null && isPasswordPrompt(endOfOutput.getLine())) {
            if (!Editor.getBufferList().contains(this))
                return;
            String password =
                PasswordDialog.showPasswordDialog(Editor.currentEditor(),
                                                  "Password:", "Password");
            if (password != null) {
                try {
                    stdin.write(password + "\n");
                    stdin.flush();
                    setEndOfOutput(new Position(getEnd()));
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        }
    }

    public static void shell()
    {
        shell(getDefaultShellCommand());
    }

    public static void shell(String shellCommand)
    {
        if (shellCommand == null) {
            Debug.bug();
            return;
        }
        if (!Editor.checkExperimental())
            return;
        if (Platform.isPlatformWindows()) {
            if (!Platform.isPlatformWindows5())
                return;
        } else {
            // Unix.
            if (!Utilities.haveJpty()) {
                MessageDialog.showMessageDialog(JPTY_NOT_FOUND, "Error");
                return;
            }
        }
        // Look for existing shell buffer.
        Buffer buf = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b instanceof Shell) {
                if (shellCommand.equals(((Shell)b).shellCommand)) {
                    buf = b;
                    break;
                }
            }
        }
        if (buf != null) {
            Shell shell = (Shell) buf;
            if (shell.getProcess() == null)
                shell.startProcess();
        } else
            buf = createShell(shellCommand);
        if (buf != null) {
            final Editor editor = Editor.currentEditor();
            editor.makeNext(buf);
            editor.switchToBuffer(buf);
        }
    }

    public static void shellTab()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Shell && !(buffer instanceof RemoteShell))
            ((Shell)buffer).tab();
    }

    public static void shellInterrupt()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Shell)
            ((Shell)buffer).sendChar(3);
    }
}
