/*
 * Editor.java
 *
 * Copyright (C) 1998-2007 Peter Graves <peter@armedbear.org>
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DropTarget;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.ConnectException;
import java.net.Socket;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Stack;
import java.util.Vector;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.FocusManager;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;
import org.armedbear.j.mail.MailCommands;
import org.armedbear.j.mail.MailboxURL;
import org.armedbear.lisp.Condition;
import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.LispThread;

public final class Editor extends JPanel implements Constants,
    ComponentListener, MouseWheelListener
{
    private static final long startTimeMillis = System.currentTimeMillis();

    private static boolean debug = false;
    private static boolean saveSession = true;

    static File portfile;

    private static EditorList editorList = new EditorList();

    private static PendingOperations pendingOperations = new PendingOperations();

    private static Editor currentEditor;

    private static KillRing killRing = new KillRing();

    public static final KillRing getKillRing()
    {
        return killRing;
    }

    private static String killedColumn;

    private static SessionProperties sessionProperties;

    public static SessionProperties getSessionProperties()
    {
        return sessionProperties;
    }

    private static final Preferences prefs = new Preferences();

    public static final Preferences preferences()
    {
        return prefs;
    }

    private static boolean isRecordingMacro;

    public static synchronized boolean isRecordingMacro()
    {
        return isRecordingMacro;
    }

    public static synchronized void setRecordingMacro(boolean b)
    {
        isRecordingMacro = b;
    }

    static String lookAndFeel;

    private Buffer buffer;

    private final Display display;
    private final Dispatcher dispatcher;
    private final Frame frame;

    private Search lastSearch;

    public final Search getLastSearch()
    {
        return lastSearch;
    }

    public final void setLastSearch(Search search)
    {
        lastSearch = search;
    }

    // The current position in the buffer (that is, in the actual text).
    private Position dot;

    // The position of the other end of the selection, if any,
    private Position mark;

    private Selection selection;
    private boolean isColumnSelection;

    Hashtable views = new Hashtable();

    // BUG! This stuff should be factored somehow...
    private int currentCommand = COMMAND_NOTHING;
    private int lastCommand = COMMAND_NOTHING;

    public final int getCurrentCommand()
    {
        return currentCommand;
    }

    public final void setCurrentCommand(int command)
    {
        currentCommand = command;
    }

    public final int getLastCommand()
    {
        return lastCommand;
    }

    public final void setLastCommand(int command)
    {
        lastCommand = command;
    }

    private static Marker[] bookmarks = new Marker[11];

    private static TagFileManager tagFileManager;

    private static boolean tabsAreVisible = false;

    public static final boolean tabsAreVisible()
    {
        return tabsAreVisible;
    }

    static boolean isMenuSelected = false;

    DirectoryTree localDirectoryTree;

    private static ModeList modeList;

    public static final ModeList getModeList()
    {
        if (modeList == null)
            modeList = ModeList.getInstance();
        return modeList;
    }

    private static final BufferList bufferList = new BufferList();

    public static final BufferList getBufferList()
    {
        return bufferList;
    }

    public static long getStartTimeMillis()
    {
        return startTimeMillis;
    }

    private static String when()
    {
        return String.valueOf(System.currentTimeMillis() - startTimeMillis) + " ms";
    }

    public static void main(String[] args)
    {
        final File currentDir = File.getInstance(System.getProperty("user.dir"));
        boolean forceNewInstance = false;
        boolean restoreSession = true;
        boolean startServer = true;
        int quick = 0;
        File userHomeDir = null;
        List files = null;

        // Process command line.
        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if (arg.startsWith("-")) {
                if (arg.equals("-h") || arg.equals("-help") || arg.equals("--help")) {
                    usage();
                    System.exit(0);
                }
                if (arg.equals("-version")) {
                    version();
                    System.exit(0);
                }
                if (arg.equals("-d") || arg.equals("--debug")) {
                    debug = true;
                    continue;
                }
                if (arg.equals("-q")) {
                    if (quick < 1)
                        quick = 1;
                    continue;
                }
                if (arg.equals("-n") || arg.equals("--no-restore")) {
                    restoreSession = false;
                    continue;
                }
                if (arg.equals("-session")) {
                    if (i < args.length-1)
                        sessionName = args[++i];
                    continue;
                }
                if (arg.equals("--force-new-instance")) {
                    forceNewInstance = true;
                    continue;
                }
                if (arg.equals("--no-session")) {
                    restoreSession = false;
                    saveSession = false;
                    continue;
                }
                if (arg.equals("--no-server")) {
                    startServer = false;
                    continue;
                }
                if (arg.startsWith("--home")) {
                    String home = null;
                    if (arg.equals("--home")) {
                        if (i < args.length-1)
                            home = args[++i];
                    } else if (arg.startsWith("--home="))
                        home = arg.substring(7);
                    else
                        unknown(arg);

                    if (home == null || home.length() == 0)
                        fatal("Option \"--home\" requires an argument.");

                    userHomeDir = File.getInstance(currentDir, home);

                    if (userHomeDir == null || !userHomeDir.isDirectory()) {
                        fatal("Specified home directory \"" +
                            userHomeDir.canonicalPath() +
                            "\" does not exist.");
                    }

                    if (!userHomeDir.canWrite()) {
                        fatal("Specified home directory \"" +
                            userHomeDir.canonicalPath() +
                            "\" is not writable.");
                    }

                    // Specified directory is OK.
                    Utilities.setUserHome(userHomeDir.canonicalPath());

                    continue;
                }
                // If we get here, it's an unknown option.
                unknown(arg);
            } else {
                // It's a file to be opened.
                if (files == null)
                    files = new ArrayList();
                files.add(arg);
            }
        }

        // At this point the user has had a chance to tell us where his home
        // directory is.
        Directories.initialize(userHomeDir);

        boolean alreadyRunning = false;
        portfile = File.getInstance(Directories.getEditorDirectory(), "port");
        if (portfile.exists()) {
            try {
                BufferedReader in = new BufferedReader(new InputStreamReader(portfile.getInputStream()));
                String s = in.readLine();
                in.close();

                int port = Integer.parseInt(s);

                Socket socket = new Socket("localhost", port);

                // No ConnectException. We found a running instance.
                alreadyRunning = true;

                if (!forceNewInstance) {
                    BufferedWriter out =
                        new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
                    File dir = File.getInstance(System.getProperty("user.dir"));
                    out.write(dir.canonicalPath());
                    out.newLine();
                    if (files != null) {
                        for (int i = 0; i < files.size(); i++) {
                            out.write((String)files.get(i));
                            out.newLine();
                        }
                    }
                    out.flush();
                    out.close();
                    socket.close();
                    System.exit(0);
                }
            }
            catch (ConnectException e) {
                portfile.delete();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }

        loadPreferences();
        Log.initialize();
        Directories.moveUnsentMessagesToDraftsFolder();
        loadExtensions();
        if (quick == 0) {
            runStartupScript();
        }
        DefaultLookAndFeel.setLookAndFeel();

        sessionProperties = new SessionProperties();

        if (!alreadyRunning)
            Autosave.recover();

        tagFileManager = new TagFileManager();

        setCurrentEditor(new Editor(null));

        currentEditor.getFrame().updateControls();

        // With Java 1.4, we only need to do this to support the key-pressed
        // hook.
        FocusManager.setCurrentManager(new CustomFocusManager());

        currentEditor.getFrame().placeWindow();

        Buffer toBeActivated = null;

        if (restoreSession) {
            Session session = null;
            if (sessionName != null)
                session = Session.getSession(sessionName);
            if (session == null)
                session = Session.getDefaultSession();
            toBeActivated = session.restore();
        }

        if (files != null) {
            ArrayList list = new ArrayList();
            list.add(currentDir.canonicalPath());
            for (int i = 0; i < files.size(); i++)
                list.add(files.get(i));
            Buffer buf = currentEditor.openFiles(list);
            if (buf != null) {
                Debug.assertTrue(bufferList.contains(buf));
                toBeActivated = buf;
            }
        }

        if (toBeActivated == null)
            toBeActivated = new Directory(currentDir);

        currentEditor.activate(toBeActivated);

        if (startServer)
            Server.startServer();

        Runnable r = new Runnable() {
            public void run()
            {
                currentEditor.getFrame().setVisible(true);
                Sidebar sidebar = currentEditor.getSidebar();
                if (sidebar != null)
                    sidebar.setUpdateFlag(SIDEBAR_ALL);
            }
        };
        SwingUtilities.invokeLater(r);

        if (isLispInitialized())
            LispThread.remove(Thread.currentThread());

        Log.debug("leaving main " + when());
    }

    private static final void usage()
    {
        version();
        System.out.println("Usage: j [options] [+linenum] file");
        System.out.println("Options:");
        System.out.println("  -h, -help");
        System.out.println("  -d, --debug");
        System.out.println("  -n, --no-restore");
        System.out.println("  -version");
        System.out.println("  --force-new-instance");
        System.out.println("  --no-session");
        System.out.println("  --no-server");
        System.out.println("  --home=directory");
    }

    private static final void version()
    {
        String longVersionString = Version.getLongVersionString();
        if (longVersionString != null)
            System.out.println(longVersionString);
        String snapshotInformation = Version.getSnapshotInformation();
        if (snapshotInformation != null)
            System.out.println(snapshotInformation);
    }

    public static final void fatal(String message)
    {
        System.err.println(message);
        System.exit(1);
    }

    private static final void unknown(String arg)
    {
        usage();
        fatal("Unknown option \"" + arg + "\"");
    }

    public Editor(Frame f)
    {
        display = new Display(this);
        dispatcher = new Dispatcher(this);
        init();
        frame = f != null ? f : new Frame(this);
    }

    private void init()
    {
      // Add this editor to the global editor list.
      editorList.add(this);

      setLayout(new BorderLayout());
      display.setDoubleBuffered(true);
      add(display, BorderLayout.CENTER);

      new DropTarget(display, dispatcher);

      addLocationBar();
      addVerticalScrollBar();
      maybeAddHorizontalScrollBar();

      display.addKeyListener(dispatcher);
      display.addMouseListener(dispatcher);
      display.addMouseMotionListener(dispatcher);

      addMouseWheelListener(this);
      addComponentListener(this);
    }

    public static final boolean isDebugEnabled()
    {
        return debug;
    }

    public static final boolean isMailEnabled()
    {
        if (!prefs.getBooleanProperty(Property.ENABLE_EXPERIMENTAL_FEATURES))
            return false;
        if (!prefs.getBooleanProperty(Property.ENABLE_MAIL))
            return false;
        // Mail address must be configured!
        if (prefs.getStringProperty(Property.USER_MAIL_ADDRESS) == null)
            return false;
        return true;
    }

    private LocationBar locationBar;

    public final LocationBar getLocationBar()
    {
        return locationBar;
    }

    public final int getLocationBarHeight()
    {
      if (locationBar != null)
        return locationBar.getHeight();
      else
        return 0;
    }

    public final HistoryTextField getLocationBarTextField()
    {
      return locationBar == null ? null : locationBar.getTextField();
    }

    public final void repaintLocationBar()
    {
      if (locationBar != null)
        locationBar.repaint();
    }

    public void addLocationBar()
    {
      if (locationBar == null)
        {
          locationBar = new LocationBar(this);
          add(locationBar, BorderLayout.NORTH);
        }
    }

    public void removeLocationBar()
    {
      if (locationBar != null)
        {
          remove(locationBar);
          locationBar = null;
        }
    }

    public void updateLocation()
    {
      if (locationBar != null)
        {
          HistoryTextField textField = locationBar.getTextField();
          if (textField == null || textField != frame.getFocusedComponent())
            locationBar.update();
        }
    }

    public boolean isPrimaryEditor()
    {
        return frame.isPrimaryEditor(this);
    }

    public final Editor getOtherEditor()
    {
        return isPrimaryEditor() ? frame.getSecondaryEditor() : frame.getPrimaryEditor();
    }

    public static int indexOf(Editor editor)
    {
        for (int i = getEditorCount() - 1; i >= 0; i--) {
            if (editor == getEditor(i))
                return i;
        }

        return -1;
    }

    public static final EditorList getEditorList()
    {
        return editorList;
    }

    public static final int getEditorCount()
    {
        return editorList.size();
    }

    public static final Editor getEditor(int i)
    {
        return editorList.get(i);
    }

    public static final void removeEditor(Editor editor)
    {
        editorList.remove(editor);
    }

    public final Frame getFrame()
    {
        return frame;
    }

    // Returns height in lines.
    public int getWindowHeight()
    {
        return getDisplay().getHeight() / Display.getCharHeight();
    }

    public void setWindowHeight(int n)
    {
        frame.setWindowHeight(this, n);
    }

    static Vector frames = new Vector();

    public static int indexOf(Frame frame)
    {
        for (int i = getFrameCount() - 1; i >= 0; i--) {
            if (frame == getFrame(i))
                return i;
        }

        return -1;
    }

    public static final int getFrameCount()
    {
        return frames.size();
    }

    public static final Frame getFrame(int i)
    {
        if (i >= 0 && i < frames.size())
            return (Frame) frames.get(i);
        return null;
    }

    public final Buffer getBuffer()
    {
        return buffer;
    }

    public final Mode getMode()
    {
        return buffer.getMode();
    }

    public final int getModeId()
    {
        return buffer.getModeId();
    }

    public final Formatter getFormatter()
    {
        return buffer.getFormatter();
    }

    public final Display getDisplay()
    {
        return display;
    }

    public final Dispatcher getDispatcher()
    {
        return dispatcher;
    }

    public static final TagFileManager getTagFileManager()
    {
        return tagFileManager;
    }

    public final Sidebar getSidebar()
    {
        return frame.getSidebar();
    }

    public final StatusBar getStatusBar()
    {
        return frame.getStatusBar();
    }

    public static final PendingOperations getPendingOperations()
    {
        return pendingOperations;
    }

    private VerticalScrollBar verticalScrollBar;

    private VerticalScrollBarListener verticalScrollBarListener;

    public void addVerticalScrollBar()
    {
        if (verticalScrollBar == null) {
            verticalScrollBar = new VerticalScrollBar(this);
            verticalScrollBar.setMinimum(0);
            add(verticalScrollBar, BorderLayout.EAST);
            verticalScrollBarListener = new VerticalScrollBarListener(this, verticalScrollBar);
            verticalScrollBar.addAdjustmentListener(verticalScrollBarListener);
        }
    }

    public void removeVerticalScrollBar()
    {
        if (verticalScrollBar != null) {
            if (verticalScrollBarListener == null) {
                verticalScrollBar.removeAdjustmentListener(verticalScrollBarListener);
                verticalScrollBarListener = null;
            }
            remove(verticalScrollBar);
            verticalScrollBar = null;
        }
    }

    private HorizontalScrollBar horizontalScrollBar;

    public HorizontalScrollBar getHorizontalScrollBar()
    {
        return horizontalScrollBar;
    }

    private HorizontalScrollBarListener horizontalScrollBarListener;

    public void maybeAddHorizontalScrollBar()
    {
      if (horizontalScrollBar == null)
        {
          if (prefs.getBooleanProperty(Property.ENABLE_HORIZONTAL_SCROLL_BAR))
            {
              horizontalScrollBar = new HorizontalScrollBar(this);
              horizontalScrollBar.setMinimum(0);
              JPanel panel = new JPanel();
              panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
              panel.add(horizontalScrollBar);
              final int height = horizontalScrollBar.getPreferredSize().height;
              panel.add(Box.createRigidArea(new Dimension(height, height)));
              add(panel, BorderLayout.SOUTH);
              horizontalScrollBarListener = new HorizontalScrollBarListener(this);
              horizontalScrollBar.addAdjustmentListener(horizontalScrollBarListener);
            }
        }
    }

    public void removeHorizontalScrollBar()
    {
      if (horizontalScrollBar != null)
        {
          if (horizontalScrollBarListener == null)
            {
              horizontalScrollBar.removeAdjustmentListener(horizontalScrollBarListener);
              horizontalScrollBarListener = null;
            }
          // Remove JPanel containing scroll bar.
          remove(horizontalScrollBar.getParent());
          horizontalScrollBar = null;
        }
    }

    public static synchronized final Editor currentEditor()
    {
        return currentEditor;
    }

    public static synchronized final void setCurrentEditor(Editor editor)
    {
        Editor oldCurrentEditor = currentEditor;
        currentEditor = editor;
        editor.getFrame().setCurrentEditor(editor);
        if (currentEditor != oldCurrentEditor) {
            if (currentEditor != null)
                currentEditor.repaintLocationBar();
            if (oldCurrentEditor != null)
                oldCurrentEditor.repaintLocationBar();
            if (isLispInitialized())
                LispAPI.invokeBufferActivatedHook(currentEditor.getBuffer());
        }
    }

    public static synchronized final Buffer currentBuffer()
    {
        return currentEditor.buffer;
    }

    public static synchronized final Frame getCurrentFrame()
    {
        return currentEditor.getFrame();
    }

    public static Editor createNewFrame()
    {
        Editor ed = new Editor(null);
        ed.getFrame().updateControls();
        ed.getFrame().placeWindow();
        return ed;
    }

    public void newFrame()
    {
        saveView();
        Editor ed = createNewFrame();
        ed.activate(buffer);
        ed.getFrame().setVisible(true);
        setCurrentEditor(ed);
        ed.updateDisplay();
        display.repaint();
        Runnable r = new Runnable() {
            public void run()
            {
                currentEditor.setFocusToDisplay();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public View getCurrentView()
    {
        return (View) views.get(buffer);
    }

    // Might return null.
    public final View getView(SystemBuffer buf)
    {
        return (View) views.get(buf);
    }

    public final void setView(SystemBuffer buf, View view)
    {
        views.put(buf, view);
    }

    public void removeView(SystemBuffer buf)
    {
        views.remove(buf);
    }

    // Find or create a view of buf.
    private View findOrCreateView(Buffer buf)
    {
        View view = (View) views.get(buf);
        if (view == null) {
            view = buf.getLastView();
            if (view == null)
                view = buf.getInitialView();
            views.put(buf, view);
        }
        return view;
    }

    public final void saveView()
    {
        buffer.saveView(this);
    }

    private final void restoreView()
    {
        buffer.restoreView(this);
    }

    public final Position getDot()
    {
        return dot;
    }

    public final Position getDotCopy()
    {
        return dot != null ? new Position(dot) : null;
    }

    public final void setDot(Position pos)
    {
        dot = pos;
    }

    public final void setDot(Line line, int offset)
    {
        dot = new Position(line, offset);
    }

    public void setDot(int lineNumber, int offset)
    {
        Line line = buffer.getLine(lineNumber);
        if (line != null)
            setDot(line, offset);
    }

    public final Line getDotLine()
    {
        return dot.getLine();
    }

    public final int getDotLineNumber()
    {
        return dot.lineNumber();
    }

    public final int getDotOffset()
    {
        return dot.getOffset();
    }

    public final Selection getSelection()
    {
        return selection;
    }

    public final void setSelection(Selection selection)
    {
        this.selection = selection;
    }

    public final Position getMark()
    {
        return mark;
    }

    public void setMarkAtDot()
    {
        setMark(new Position(dot));
        selection = new Selection();
    }

    public void setMark(Position pos)
    {
        mark = pos;
        if (mark == null) {
            selection = null;
            setColumnSelection(false);
        }
    }

    public void setMark(int lineNumber, int offset)
    {
        Line line = buffer.getLine(lineNumber);
        if (line != null)
            setMark(new Position(line, offset));
    }

    public final void setColumnSelection(boolean b)
    {
        isColumnSelection = b;
    }

    public final boolean isColumnSelection()
    {
        return isColumnSelection;
    }

    public final void notSupportedForColumnSelections()
    {
        MessageDialog.showMessageDialog(this,
            "Operation not supported for column selections", "Error");
    }

    public final Line getMarkLine()
    {
        return mark.getLine();
    }

    public final int getMarkLineNumber()
    {
        return mark.lineNumber();
    }

    public final int getMarkOffset()
    {
        return mark.getOffset();
    }

    public File getCurrentDirectory()
    {
        return buffer.getCurrentDirectory();
    }

    public File getCompletionDirectory()
    {
        return buffer.getCompletionDirectory();
    }

    // Cycle through the most plausible possibilities for the tab width of the
    // current buffer.
    public void cycleTabWidth()
    {
        switch (buffer.getTabWidth()) {
            case 2:
                buffer.setTabWidth(4);
                break;
            case 4:
                buffer.setTabWidth(8);
                break;
            case 8:
            default:
                buffer.setTabWidth(2);
                break;
        }
        buffer.saveProperties();
        status("Tab width set to " + buffer.getTabWidth());
        buffer.repaint();
    }

    // Cycle through the most plausible possibilities for the indent size of
    // the current buffer.
    public void cycleIndentSize()
    {
        switch (buffer.getIndentSize()) {
            case 2:
                buffer.setIndentSize(3);
                break;
            case 3:
                buffer.setIndentSize(4);
                break;
            case 4:
                buffer.setIndentSize(8);
                break;
            case 8:
            default:
                buffer.setIndentSize(2);
                break;
        }
        buffer.saveProperties();
        status("Indent size set to " + buffer.getIndentSize());
    }

    public void adjustMarkers(Line line)
    {
        if (line == null)
            return;
        Position pos = null;  // Where all displaced markers go.
        if (line.next() != null)
            pos = new Position(line.next(), 0);
        else if (line.previous() != null)
            pos = new Position(line.previous(), line.previous().length());
        if (pos == null)
            return;
        for (int i = 0; i < Editor.getEditorCount(); i++) {
            Editor ed = Editor.getEditor(i);
            if (ed.getBuffer() == buffer) {
                if (ed.getDotLine() == line) {
                    ed.getDot().moveTo(pos);
                    ed.moveCaretToDotCol();
                }
                if (ed.getMark() != null && ed.getMarkLine() == line)
                    ed.setMark(null); // Take no chances.
                if (ed.getTopLine() == line) {
                    ed.setTopLine(pos.getLine());
                    ed.setUpdateFlag(REPAINT);
                }
            } else {
                // Not presently displayed, but possibly in a stored view.
                View view = (View) ed.views.get(buffer);
                if (view != null) {
                    if (view.dot != null && view.dot.getLine() == line)
                        view.dot.moveTo(pos);
                    if (view.mark != null && view.mark.getLine() == line)
                        view.mark = null;
                    if (view.topLine == line)
                        view.topLine = pos.getLine();
                }
            }
        }
        for (int i = 0; i < bookmarks.length; i++) {
            Marker m = bookmarks[i];
            if (m != null && m.getLine() == line)
                m.setPosition(pos);
        }
    }

    public static Marker[] getBookmarks()
    {
        return bookmarks;
    }

    public void dropBookmark()
    {
        AWTEvent e = dispatcher.getLastEvent();
        if (e != null && e.getID() == KeyEvent.KEY_PRESSED) {
            int keyCode = ((KeyEvent)e).getKeyCode();
            int index = keyCode - KeyEvent.VK_0;
            if (index >= 0 && index < bookmarks.length) {
                if (bookmarks[index] == null ||
                    confirm("Drop Bookmark", "Overwrite existing bookmark?")) {
                    bookmarks[index] = new Marker(buffer, dot);
                    status("Bookmark dropped");
                }
            }
        }
    }

    public void gotoBookmark()
    {
        AWTEvent e = dispatcher.getLastEvent();
        if (e != null && e.getID() == KeyEvent.KEY_PRESSED) {
            int keyCode = ((KeyEvent)e).getKeyCode();
            int index = keyCode - KeyEvent.VK_0;
            if (index >= 0 && index < bookmarks.length) {
                Marker m = bookmarks[index];
                if (m != null)
                    m.gotoMarker(this);
            }
        }
    }

    // Drop a temporary bookmark, overwriting the existing temporary bookmark
    // if one exists.
    public void dropTemporaryMarker()
    {
        bookmarks[10] = new Marker(buffer, dot);
        status("Temporary marker dropped");
    }

    public void gotoTemporaryMarker()
    {
        Marker m = bookmarks[10];
        if (m != null)
            m.gotoMarker(this);
    }

    public void deleteLineSeparator()
    {
        final Line dotLine = getDotLine();
        final Line nextLine = dotLine.next();
        if (nextLine == null)
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (dotLine.length() == 0) {
                adjustMarkers(dotLine);
                // Save original text.
                FastStringBuffer sb = new FastStringBuffer();
                if (dotLine.getOriginalText() != null)
                    sb.append(dotLine.getOriginalText());
                sb.append('\n');
                if (nextLine.getOriginalText() != null)
                    sb.append(nextLine.getOriginalText());
                else
                    sb.append(nextLine.getText());
                nextLine.setOriginalText(sb.toString());
                // Unlink the current line.
                final Line prevLine = dotLine.previous();
                if (prevLine != null)
                    prevLine.setNext(nextLine);
                nextLine.setPrevious(prevLine);
                if (dotLine == buffer.getFirstLine()) {
                    Log.debug("deleteLineSeparator calling buffer.setFirstLine()");
                    buffer.setFirstLine(nextLine);
                    Log.debug("first line = |" + buffer.getFirstLine().getText() + "|");
                }
                if (dotLine == display.getTopLine())
                    display.setTopLine(nextLine);
                dot.moveTo(nextLine, 0);
            } else {
                // Append the next line's text to end of this line.
                dotLine.setText(dotLine.getText() + nextLine.getText());
                // Save original text.
                FastStringBuffer sb = new FastStringBuffer();
                if (dotLine.getOriginalText() != null)
                    sb.append(dotLine.getOriginalText());
                else
                    sb.append(dotLine.getText());
                if (!nextLine.isNew()) {
                    sb.append('\n');
                    if (nextLine.getOriginalText() != null)
                        sb.append(nextLine.getOriginalText());
                    else
                        sb.append(nextLine.getText());
                }
                dotLine.setOriginalText(sb.toString());
                // Move any markers that might be on the next line.
                adjustMarkers(nextLine);
                // Unlink the next line.
                if (nextLine.next() != null)
                    nextLine.next().setPrevious(dotLine);
                dotLine.setNext(nextLine.next());
            }
            buffer.repaint();
            setUpdateFlag(REFRAME);
            buffer.needsRenumbering = true;
            buffer.modified();
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private void deleteNormalChar()
    {
        addUndo(SimpleEdit.LINE_EDIT);
        final Line dotLine = getDotLine();
        final int dotOffset = getDotOffset();
        String head = dotLine.substring(0, dotOffset);
        String tail = "";
        if (dotOffset < dotLine.length() - 1)
            tail = dotLine.substring(dotOffset + 1);
        dotLine.setText(head.concat(tail));
        buffer.modified();
        updateInAllEditors(dotLine);
    }

    // A deletion, not a kill!
    public void delete()
    {
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (mark != null) {
                deleteRegion();
            } else {
                final Line dotLine = getDotLine();
                final int dotOffset = getDotOffset();
                final int length = dotLine.length();
                if (dotOffset < length) {
                    deleteNormalChar();
                } else if (dotOffset == length) {
                    if (dotLine.next() != null) {
                        CompoundEdit compoundEdit = beginCompoundEdit();
                        fillToCaret();
                        addUndo(SimpleEdit.DELETE_LINE_SEP);
                        deleteLineSeparator();
                        endCompoundEdit(compoundEdit);
                    } else
                        status("End of buffer");
                } else {
                    // Shouldn't happen.
                    Debug.bug();
                }
            }
        }
        finally {
            buffer.unlockWrite();
        }
    }

    // A deletion, not a kill!
    public void backspace()
    {
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (mark != null) {
                delete();
            } else if (display.getCaretCol() > buffer.getCol(getDotLine(), getDotLine().length())) {
                // The caret is beyond the end of the actual text on the current line.
                addUndo(SimpleEdit.MOVE);
                --display.caretCol;
                updateDotLine();
            } else if (dot.getOffset() > 0) {
                addUndo(SimpleEdit.LINE_EDIT);
                dot.moveLeft();
                deleteNormalChar();
                moveCaretToDotCol();
            } else if (getDotLine().previous() != null) {
                CompoundEdit compoundEdit = beginCompoundEdit();
                addUndo(SimpleEdit.MOVE);
                dot.moveTo(getDotLine().previous(), getDotLine().previous().length());
                addUndo(SimpleEdit.DELETE_LINE_SEP);
                deleteLineSeparator();
                endCompoundEdit(compoundEdit);
                moveCaretToDotCol();
            }
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private boolean nextChar()
    {
        if (getDotOffset() < getDotLine().length()) {
            dot.skip(1);
            return true;
        }
        if (getDotLine().next() != null) {
            dot.moveTo(getDotLine().next(), 0);
            return true;
        }
        return false;
    }

    private boolean prevChar()
    {
        if (getDotOffset() > 0) {
            dot.skip(-1);
            return true;
        }
        final Line previous = getDotLine().previous();
        if (previous != null) {
            dot.moveTo(previous, previous.length());
            return true;
        }
        return false;
    }

    public char getDotChar()
    {
        Debug.assertTrue(dot != null);
        return dot.getChar();
    }

    public void cppFindMatch()
    {
        if (getDotLine().trim().startsWith("#")) {
            Line line = CMode.findMatchPreprocessor(getDotLine());
            if (line != null)
                moveDotTo(line, 0);
            else
                status("No match");
        } else
            findMatchingChar();
    }

    // If numLines is non-zero, limit the search to that many lines either
    // forward or backward in the buffer.
    public Position findMatchInternal(Position start, int numLines)
    {
        if (start == null)
            return null;
        final String s1 = new String("{([})]");
        final char origChar = start.getChar();
        int index = s1.indexOf(origChar);
        if (index < 0)
            return null;
        final Mode mode = buffer.getMode();
        if (mode.isInComment(buffer, start) || mode.isInQuote(buffer, start))
            return null;
        final int offset = start.getOffset();
        if (offset > 0 && start.getLine().charAt(offset - 1) == '\\') {
            // It's escaped.
            return null;
        }
        final String s2 = new String("})]{([");
        final char match = s2.charAt(index);
        final boolean searchBackwards = index > 2;
        int stopLineNumber = searchBackwards ? 0 : buffer.getLineCount();
        if (numLines != 0)
            stopLineNumber = searchBackwards ? start.lineNumber() - numLines : start.lineNumber() + numLines;
        int count = 1;
        final SyntaxIterator it = mode.getSyntaxIterator(start);
        while (true) {
            char c;
            Position pos = it.getPosition();
            if (searchBackwards) {
                if (pos.lineNumber() < stopLineNumber)
                    return null;
                else
                    c = it.prevChar();
            } else {
                if (pos.lineNumber() > stopLineNumber)
                    return null;
                else
                    c = it.nextChar();
            }
            if (c == SyntaxIterator.DONE)
                return null;
            if (c == origChar)
                ++count;
            else if (c == match)
                --count;
            if (count == 0) {
                // Found it!
                return it.getPosition();
            }
        }
    }

    public void findMatchingChar()
    {
        setWaitCursor();
        Position pos = findDelimiterNearDot();
        if (pos != null) {
            Position match = findMatchInternal(pos, 0);
            if (match != null) {
                // If the match is a right delimiter, we want to put the caret
                // beyond it.
                if ("})]".indexOf(match.getChar()) >= 0)
                    match.next();
                addUndo(SimpleEdit.MOVE);
                unmark();
                updateDotLine();
                dot.moveTo(match);
                updateDotLine();
                moveCaretToDotCol();
            } else
                status("No match");
        }
        setDefaultCursor();
    }

    public void selectSyntax()
    {
        setWaitCursor();
        Position pos = findDelimiterNearDot();
        if (pos != null) {
            Position match = findMatchInternal(pos, 0);
            if (match != null) {
                if ("})]".indexOf(pos.getChar()) >= 0)
                    pos.next();
                else if ("})]".indexOf(match.getChar()) >= 0)
                    match.next();
                if (pos.getLine() != match.getLine()) {
                    // Extend selection to full lines if possible.
                    Region r = new Region(buffer, pos, match);
                    Position begin = r.getBegin();
                    String trim =
                        begin.getLine().substring(0, begin.getOffset()).trim();
                    if (trim.length() == 0) {
                        Position end = r.getEnd();
                        trim = end.getLine().substring(end.getOffset()).trim();
                        if (trim.length() == 0) {
                            // Extend selection to complete lines.
                            begin.setOffset(0);
                            if (end.getNextLine() != null)
                                end.moveTo(end.getNextLine(), 0);
                            else
                                end.setOffset(end.getLineLength());
                            if (pos.isBefore(match)) {
                                pos = begin;
                                match = end;
                            } else {
                                match = begin;
                                pos = end;
                            }
                        }
                    }
                }
                addUndo(SimpleEdit.MOVE);
                unmark();
                dot.moveTo(pos);
                setMarkAtDot();
                updateDotLine();
                dot.moveTo(match);
                updateDotLine();
                moveCaretToDotCol();
                if (dot.getLine() != mark.getLine())
                    setUpdateFlag(REPAINT);
            } else
                status("No match");
        }
        setDefaultCursor();
    }

    private Position findDelimiterNearDot()
    {
        Position pos = dot.copy();
        if ("{([".indexOf(pos.getChar()) >= 0) {
            // The character to the right of the caret is a left delimiter.
            return pos;
        }
        Position saved = dot.copy();
        if (pos.getOffset() > 0) {
            pos.prev();
            if ("})]".indexOf(pos.getChar()) >= 0) {
                // The character to the left of the caret is a right delimiter.
                return pos;
            }
        }
        // There's no delimiter at the exact location of the caret.
        final String delimiters = "{([})]";
        pos.moveTo(saved);
        while (pos.getOffset() > 0) {
            // Look at previous char.
            pos.prev();
            char c = pos.getChar();
            if (delimiters.indexOf(c) >= 0)
                return pos;
            if (!Character.isWhitespace(c) && c != ';')
                break;
        }
        pos.moveTo(saved);
        final int limit = pos.getLineLength();
        while (pos.getOffset() < limit) {
            char c = pos.getChar();
            if (delimiters.indexOf(c) >= 0)
                return pos;
            if (!Character.isWhitespace(c))
                return null;
            // Look at next char.
            pos.next();
        }
        return null;
    }

    public void closeParen()
    {
        insertNormalChar(')');
        if (prefs.getBooleanProperty(Property.HIGHLIGHT_MATCHING_BRACKET) ||
            prefs.getBooleanProperty(Property.HIGHLIGHT_BRACKETS))
            return;
        // Limit search to 50 lines.
        Position match =
            findMatchInternal(new Position(getDotLine(), getDotOffset()-1), 50);
        if (match == null)
            return;
        // We don't want to reframe.
        if (match.lineNumber() < display.getTopLineNumber())
            return;
        if (buffer.getCol(match) < display.getShift())
            return;
        // Move caret to match momentarily and then return.
        Position saved = new Position(dot);
        updateDotLine();
        dot.moveTo(match);
        updateDotLine();
        moveCaretToDotCol();
        display.repaintChangedLines();
        try {
            Thread.sleep(300);
        }
        catch (InterruptedException e) {}
        updateDotLine();
        dot.moveTo(saved);
        moveCaretToDotCol();
        updateDotLine();
    }

    // No undo.
    public void insertLineSeparator()
    {
        Debug.assertTrue(mark == null);
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            buffer.insertLineSeparator(dot);
        }
        finally {
            buffer.unlockWrite();
        }
        final Line dotLine = getDotLine();
        for (int i = 0; i < getEditorCount(); i++) {
            Editor ed = getEditor(i);
            if (ed.getTopLine() == dotLine)
                ed.setTopLine(dotLine.previous());
        }
    }

    public void newline()
    {
        if (!checkReadOnly())
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark != null)
            deleteRegion();
        addUndo(SimpleEdit.INSERT_LINE_SEP);
        insertLineSeparator();
        moveCaretToDotCol();
        endCompoundEdit(compoundEdit);
    }

    public void newlineAndIndent()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = beginCompoundEdit();
            if (mark != null)
                deleteRegion();
            addUndo(SimpleEdit.INSERT_LINE_SEP);
            insertLineSeparator();
            final Mode mode = getMode();
            final Line dotLine = getDotLine();
            int indent;
            if (mode.canIndent()) {
                if (buffer.needsRenumbering())
                    buffer.renumber();
                getFormatter().parseBuffer();
                indent = mode.getCorrectIndentation(dotLine, buffer);
            } else {
                // Can't indent according to context. Match indentation of previous line.
                indent = buffer.getIndentation(dotLine.previous());
            }
            if (indent != buffer.getIndentation(dotLine)) {
                addUndo(SimpleEdit.LINE_EDIT);
                buffer.setIndentation(dotLine, indent);
            }
            if (dotLine.length() > 0) {
                moveDotToIndentation();
                moveCaretToDotCol();
            } else {
                display.setCaretCol(indent - display.getShift());
                if (buffer.getBooleanProperty(Property.RESTRICT_CARET))
                    fillToCaret();
            }
            endCompoundEdit(compoundEdit);
        }
        finally {
            buffer.unlockWrite();
        }
        setUpdateFlag(REFRAME);
    }

    public void insertNormalChar(char c)
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            c = getMode().fixCase(this, c);
            if (mark != null) {
                CompoundEdit compoundEdit = beginCompoundEdit();
                deleteRegion();
                insertChar(c);
                endCompoundEdit(compoundEdit);
            } else {
                // No selection.
                if (buffer.getBooleanProperty(Property.WRAP) &&
                    getDotCol() >= buffer.getIntegerProperty(Property.WRAP_COL))
                {
                    CompoundEdit compoundEdit = beginCompoundEdit();
                    insertChar(c);
                    new WrapText(this).wrapLine();
                    endCompoundEdit(compoundEdit);
                } else
                    insertChar(c);
            }
        }
        finally {
            buffer.unlockWrite();
        }
        moveCaretToDotCol();
    }

    public void tab()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        if (buffer.getBooleanProperty(Property.TAB_ALWAYS_INDENT)) {
            indentLineOrRegion();
            return;
        }
        if (mark == null) {
            // No selection.
            if (dot.getOffset() <= dot.getLine().getIndentation())
                indentLine();
            else
                insertTab();
            return;
        }
        if (getMarkLine() != getDotLine()) {
            // Multi-line selection.
            indentRegion();
            return;
        }
        // Single-line selection.
        Region r = new Region(this);
        if (r.getBeginOffset() <= getDotLine().getIndentation())
            indentLine();
        else
            insertTab();
    }

    public void insertTab()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }

        if (!checkReadOnly())
            return;

        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = beginCompoundEdit();
            if (mark != null)
                deleteRegion();
            if (buffer.getUseTabs())
                insertChar('\t');
            else {
                fillToCaret();
                int tabWidth = buffer.getTabWidth();
                addUndo(SimpleEdit.LINE_EDIT);
                buffer.insertChars(dot, Utilities.spaces(tabWidth - getDotCol() % tabWidth));
                updateInAllEditors(getDotLine());
            }
            moveCaretToDotCol();
            endCompoundEdit(compoundEdit);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public void moveDotToIndentation()
    {
        final Line dotLine = getDotLine();
        final int limit = dotLine.length();
        int i;
        for (i = 0; i < limit; i++) {
            if (!Character.isWhitespace(dotLine.charAt(i)))
                break;
        }
        dot.setOffset(i);
    }

    public void indentLineOrRegion()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        if (getMode().canIndent()) {
            if (mark != null && getMarkLine() != getDotLine()) {
                indentRegion();
            } else {
                unmark();
                indentLine();
            }
        }
    }

    public void indentRegion()
    {
      if (isColumnSelection())
        {
          notSupportedForColumnSelections();
          return;
        }
      if (getMode().canIndent() && mark != null)
        {
          Region r = new Region(this);
          if (r.getBeginLine() == r.getEndLine())
            indentLine();
          else
            {
              if (!checkReadOnly())
                return;
              setWaitCursor();
              Position savedDot = new Position(dot);
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
                  if (buffer.needsParsing())
                    {
                      if (getFormatter().parseBuffer())
                        buffer.repaint();
                    }
                  CompoundEdit compoundEdit = beginCompoundEdit();
                  addUndo(SimpleEdit.MOVE);
                  dot.moveTo(r.getBeginLine(), 0);
                  do
                    {
                      if (!dot.getLine().isBlank())
                        indentLineInternal();
                      dot.moveTo(dot.getNextLine(), 0);
                    }
                  while (dot.getLine() != r.getEndLine());
                  addUndo(SimpleEdit.MOVE);
                  dot = savedDot;
                  if (dot.getOffset() > getDotLine().length())
                    dot.setOffset(getDotLine().length());
                  if (mark.getOffset() > getMarkLine().length())
                    mark.setOffset(getMarkLine().length());
                  moveCaretToDotCol();
                  endCompoundEdit(compoundEdit);
                }
              finally
                {
                  buffer.unlockWrite();
                }
              setUpdateFlag(REFRAME);
              setDefaultCursor();
            }
        }
    }

    public void commentRegion()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        commentRegion(true);
    }

    public void uncommentRegion() {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        commentRegion(false);
    }

    // If argument is false, uncomment the region.
    private void commentRegion(boolean comment)
    {
        if (!checkReadOnly())
            return;
        if (dot == null)
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            commentRegionInternal(comment);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    // If argument is false, uncomment the region.
    private void commentRegionInternal(boolean comment)
    {
        String commentStart = buffer.getCommentStart();
        if (commentStart == null)
            return;
        String commentEnd = buffer.getCommentEnd();
        Position savedDot = new Position(dot);
        Line beginLine, endLine;
        if (mark == null)
            mark = buffer.getMark();
        if (mark != null) {
            Region r = new Region(buffer, dot, mark);
            beginLine = r.getBeginLine();
            endLine = r.getEndLine();
        } else {
            beginLine = getDotLine();
            endLine = getDotLine().next();
        }
        if (endLine == beginLine)
            endLine = beginLine.next();

        CompoundEdit compoundEdit = beginCompoundEdit();

        if (mark != null) {
            addUndo(SimpleEdit.MOVE);
            setMark(null);
            setUpdateFlag(REPAINT);
        }

        if (getDotLine() != beginLine) {
            addUndo(SimpleEdit.MOVE);
            dot.moveTo(beginLine, 0);
        }

        boolean modified = false;

        while (true) {
            Line dotLine = getDotLine();
            String trim = dotLine.trim();
            if (comment) {
                if (trim.length() != 0) {
                    addUndo(SimpleEdit.LINE_EDIT);
                    if (commentEnd != null)
                        dotLine.setText(commentStart + dotLine.getText() + commentEnd);
                    else
                        dotLine.setText(commentStart + dotLine.getText());
                    modified = true;
                    if (dotLine == savedDot.getLine())
                        savedDot.setOffset(savedDot.getOffset() + commentStart.length());
                    updateInAllEditors(dotLine);
                }
            } else {
                // Uncomment.
                if (trim.startsWith(commentStart)) {
                    if (commentEnd == null || trim.endsWith(commentEnd)) {
                        addUndo(SimpleEdit.LINE_EDIT);
                        int index = dotLine.getText().indexOf(commentStart) + commentStart.length();
                        dotLine.setText(dotLine.substring(index));
                        if (commentEnd != null)
                            dotLine.setText(dotLine.substring(0, dotLine.length() - commentEnd.length()));
                        modified = true;
                        int dotCol = 0;
                        if (dotLine == savedDot.getLine()) {
                            // Adjust saved position.
                            savedDot.setOffset(savedDot.getOffset() - index);
                            if (savedDot.getOffset() < 0)
                                savedDot.setOffset(0);
                            dotCol = buffer.getCol(dotLine, savedDot.getOffset());
                        }

                        int oldIndent = buffer.getIndentation(dotLine);
                        int indent = getMode().getCorrectIndentation(dotLine, buffer);
                        if (indent != oldIndent) {
                            buffer.setIndentation(dotLine, indent);
                            if (dotLine == savedDot.getLine()) {
                                // Adjust saved position.
                                if (dotCol >= oldIndent) {
                                    dotCol += indent - oldIndent;
                                    dot.moveToCol(dotCol, buffer.getTabWidth());
                                    savedDot.setOffset(dot.getOffset());
                                }
                            }
                        }
                        updateInAllEditors(dotLine);
                    }
                }
            }

            if (dotLine.next() == endLine)
                break;

            addUndo(SimpleEdit.MOVE);
            dot.moveTo(dotLine.next(), 0);
        }

        addUndo(SimpleEdit.MOVE);
        setDot(savedDot);
        moveCaretToDotCol();

        if (modified)
            buffer.modified();

        endCompoundEdit(compoundEdit);
    }

    public final void moveDotTo(Position pos)
    {
        if (pos != null)
            moveDotTo(pos.getLine(), pos.getOffset());
    }

    public void moveDotTo(Line line, int offset)
    {
        if (dot == null)
            return;
        addUndo(SimpleEdit.MOVE);
        if (mark != null) {
            setMark(null);
            dot.moveTo(line, offset);
            setUpdateFlag(REPAINT);
        } else {
            updateDotLine();
            dot.moveTo(line, offset);
            updateDotLine();
        }
        moveCaretToDotCol();
    }

    public void indentLine()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        if (!checkReadOnly())
            return;
        if (!getMode().canIndent())
            return; // No change.
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (buffer.needsParsing()) {
                if (getFormatter().parseBuffer())
                    buffer.repaint();
            }
            indentLineInternal();
        }
        finally {
            buffer.unlockWrite();
        }
        setUpdateFlag(REFRAME);
    }

    private void indentLineInternal()
    {
        final Line dotLine = getDotLine();
        final int indent = getMode().getCorrectIndentation(dotLine, buffer);
        final int shift = display.getShift();

        if (dotLine.isBlank()) {
            // Put the caret where it needs to go...
            addUndo(SimpleEdit.LINE_EDIT);
            dotLine.setText("");
            dot.setOffset(0);
            display.setCaretCol(indent - shift);
            // Fill if necessary.
            if (buffer.getBooleanProperty(Property.RESTRICT_CARET))
                fillToCaret();
            updateInAllEditors(dotLine);
            return;
        }

        // Line is not blank. Figure out current indentation.
        final int oldIndent = buffer.getIndentation(dotLine);

        FastStringBuffer sb = null;

        if (indent == oldIndent) {
            boolean ok = false;
            if (buffer.getBooleanProperty(Property.INDENT_LINE_FIX_WHITESPACE)) {
                sb = buffer.getCorrectIndentationString(indent);
                if (dotLine.getText().startsWith(sb.toString()))
                    ok = true;
            } else
                ok = true;

            if (ok) {
                // Current indentation is correct. If the caret is in the
                // indentation area, move it to the start of the non-blank
                // text.
                if (display.getCaretCol() + shift < indent) {
                    addUndo(SimpleEdit.MOVE);
                    display.setCaretCol(indent - shift);
                    moveDotToCaretCol();
                }
                return;
            }
        }

        // We need to fix the indentation. Figure out where we want to put the
        // caret when we're done. We want to maintain the existing offset from
        // the start of the non-blank text.
        final int existing = display.getCaretCol() + shift - oldIndent;
        final int goal = existing < 0 ? indent : existing + indent;

        // Strip existing indentation.
        int i = 0;
        while (i < dotLine.length() && Character.isWhitespace(dotLine.charAt(i)))
            ++i;
        String nonBlank = dotLine.substring(i);

        // Get the correct indentation string.
        if (sb == null)
            sb = buffer.getCorrectIndentationString(indent);

        // Add the rest of the line.
        sb.append(nonBlank);

        // Replace the existing text.
        addUndo(SimpleEdit.LINE_EDIT);
        dotLine.setText(sb.toString());
        buffer.modified();
        updateInAllEditors(dotLine);

        // Put the caret where we want it.
        display.setCaretCol(goal - shift);
        moveDotToCaretCol();
    }

    public void save()
    {
        save(buffer);
    }

    public void save(Buffer toBeSaved)
    {
        if (toBeSaved.isLocked())
            return;
        if (toBeSaved.getType() == Buffer.TYPE_NORMAL) {
            if (toBeSaved.isModified()) {
                if (toBeSaved.isUntitled()) {
                    saveAs(toBeSaved);
                } else {
                    setWaitCursor();
                    status("Saving...");
                    if (toBeSaved.getBooleanProperty(Property.REMOVE_TRAILING_WHITESPACE))
                        toBeSaved.removeTrailingWhitespace();
                    if (toBeSaved.save())
                        status("Saving...done");
                    else
                        status("Save failed");
                    setDefaultCursor();
                }
            } else
                status("Not modified");
        }
    }

    public void saveAs()
    {
        saveAs(buffer);
    }

    private void saveAs(Buffer toBeSaved)
    {
        if (toBeSaved.isLocked())
            return;
        if (toBeSaved.getType() == Buffer.TYPE_NORMAL) {
            final String dialogTitle = "Save As";
            File destination =
                SaveFileDialog.getSaveFile(this, dialogTitle);
            if (destination == null)
                return;

            // At this point, if the target file exists, the user has said
            // it's OK to overwrite it.
            repaintNow();

            // Do we have the target file in a buffer?
            Buffer buf = bufferList.findBuffer(destination);
            if (buf != null) {
                // We do. Can we just get rid of it?
                if (!buf.isModified()) {
                    buf.deleteAutosaveFile();
                    bufferList.remove(buf);
                } else {
                    // Buffer is modified.  Make user deal with it.
                    setDefaultCursor();
                    String message = "Target file is in an active buffer.  Please take care of that first.";
                    MessageDialog.showMessageDialog(this, message, dialogTitle);
                    return;
                }
            }

            toBeSaved.saveAs(destination);
        }
    }

    public void saveCopy()
    {
        if (buffer.isLocked())
            return;
        if (buffer.getType() == Buffer.TYPE_NORMAL) {
            final String dialogTitle = "Save Copy";
            final File destination =
                SaveFileDialog.getSaveFile(this, dialogTitle);
            if (destination == null)
                return;

            repaintNow();

            // Do we have the target file in a buffer?
            Buffer buf = bufferList.findBuffer(destination);
            if (buf != null) {
                // We do.  Do we care?
                if (buf.isModified()) {
                    // Buffer is modified.  Make user deal with it.
                    setDefaultCursor();
                    String message = "Target file is in an active buffer.  Please take care of that first.";
                    MessageDialog.showMessageDialog(this, message, "Save Copy");
                    return;
                }
            }

            buffer.saveCopy(destination);
            if (buf != null && buf.isLoaded())
                reload(buf);
        }
    }

    public void saveAll()
    {
        setWaitCursor();
        int numModified = 0;
        int numErrors = 0;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf.getModeId() == CHECKIN_MODE)
                continue;
            if (buf.isUntitled()) {
                setDefaultCursor();
                makeNext(buf);
                activate(buf);
                saveAs();
                setWaitCursor();
            } else if (buf.isModified()) {
                status("Saving modified buffers...");
                ++numModified;
                if (buffer.getFile() != null)
                    if (buffer.getBooleanProperty(Property.REMOVE_TRAILING_WHITESPACE))
                        buffer.removeTrailingWhitespace();
                if (!buf.save())
                    ++numErrors;
            }
        }
        if (numModified == 0)
            status("No modified buffers");
        else if (numErrors == 0)
            status("Saving modified buffers...done");
        else {
            // User will already have seen detailed error information from Buffer.save().
            status("Unable to save all modified buffers");
        }
        setDefaultCursor();
    }

    public boolean okToClose(Buffer buf)
    {
        if (buf.getType() != Buffer.TYPE_NORMAL)
            return true;
        if (buf.isUntitled() || buf.isModified()) {
            makeNext(buf);
            activate(buf);
            repaintNow();
            if (!CloseBufferConfirmationDialog.confirmClose(this, buf))
                return false;
        }
        return true;
    }

    public void closeAll()
    {
        repaintNow();

        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            if (!okToClose(it.nextBuffer()))
                return;
        }

        Marker.invalidateAllMarkers();

        Buffer toBeActivated = null;

        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof Directory && buf.getFile().equals(getCurrentDirectory())) {
                toBeActivated = buf;
                break;
            }
        }

        if (toBeActivated == null)
            toBeActivated = new Directory(getCurrentDirectory());

        setWaitCursor();

        for (int i = 0; i < getEditorCount(); i++) {
            Editor ed = getEditor(i);
            ed.activate(toBeActivated);
        }

        for (BufferIterator iter = new BufferIterator(); iter.hasNext();) {
            Buffer buf = iter.nextBuffer();
            if (buf != toBeActivated) {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    ed.views.remove(buf);
                }
                buf.deleteAutosaveFile();
                iter.remove();
                buf.dispose();
            }
        }

        setSessionName(null);

        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_ALL);
        Sidebar.refreshSidebarInAllFrames();
        setDefaultCursor();
    }

    public void closeOthers()
    {
        repaintNow();

        Buffer toBeActivated = buffer;

        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf != buffer && !okToClose(buf))
                return;
        }

        List markers = Marker.getAllMarkers();
        for (int i = 0; i < markers.size(); i++) {
            Marker m = (Marker) markers.get(i);
            if (m != null && m.getBuffer() != buffer)
                m.invalidate();
        }

        setWaitCursor();

        for (EditorIterator it = new EditorIterator(); it.hasNext();)
            it.nextEditor().activate(toBeActivated);

        for (BufferIterator iter = new BufferIterator(); iter.hasNext();) {
            Buffer buf = iter.nextBuffer();
            if (buf != buffer) {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    ed.views.remove(buf);
                }
                buf.deleteAutosaveFile();
                iter.remove();
                buf.dispose();
            }
        }

        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_ALL);
        Sidebar.refreshSidebarInAllFrames();
        setDefaultCursor();
    }

    public boolean execute(String command) throws NoSuchMethodException
    {
        String[] array = parseCommand(command);
        if (array == null)
            return false;
        String methodName = array[0];
        String parameters = array[1];
        return execute(methodName, parameters);
    }

    public boolean execute(String commandName, String parameters) throws NoSuchMethodException
    {
        if (commandName == null)
            return false;

        try {
            String className = null;
            String methodName = null;
            Method method = null;

            Class[] parameterTypes;
            if (parameters == null)
                parameterTypes = new Class[0];
            else {
                parameterTypes = new Class[1];
                parameterTypes[0] = Class.forName("java.lang.String");
            }

            Command command = CommandTable.getCommand(commandName);
            if (command != null) {
                method = command.getMethod();
                if (method != null) {
                    try {
                        invoke(method, parameters);
                        return true;
                    }
                    catch (IllegalArgumentException e) {
                        // The cached method requires different arguments.
                        // Fall through.
                    }
                }
                // Method is not cached yet.
                className = command.getClassName();
                methodName = command.getMethodName();
                if (className == null) {
                    // Special case. Command is implemented in org.armedbear.j.Editor.
                    method = Editor.class.getMethod(methodName, parameterTypes);
                } else {
                    Class c = Class.forName("org.armedbear.j." + className);
                    if (c != null)
                        method = c.getMethod(methodName, parameterTypes);
                }
                if (method != null) {
                    // Cache the method for nest time.
                    command.setMethod(method);
                    invoke(method, parameters);
                    return true;
                }
            } else {
                // This is the old code path.
                Debug.assertTrue(className == null);
                Debug.assertTrue(methodName == null);
                int index = commandName.indexOf('.');
                if (index < 0) {
                    // No class name.  Must be a method in the Editor class.
                    method = Editor.class.getMethod(commandName, parameterTypes);
                } else if (commandName.length() > index+1) {
                    // Class name was provided.
                    className = commandName.substring(0, index);
                    methodName = commandName.substring(index+1);
                    Class c = null;
                    try {
                        c = Class.forName("org.armedbear.j." + className);
                    }
                    catch (ClassNotFoundException e) {}
                    if (c == null) {
                        // Check for extension class.
                        c = loadExtensionClass(className);
                    }
                    if (c != null) {
                        // Might throw NoSuchMethodException.
                        method = c.getMethod(methodName, parameterTypes);
                    }
                }
                if (method != null) {
                    invoke(method, parameters);
                    return true;
                }
            }
        }
        catch (NoSuchMethodException e) {
            throw e;
        }
        catch (Throwable t) {
            Log.error(t);
        }
        return false;
    }

    private void invoke(Method method, String parameters) throws IllegalArgumentException
    {
        Object[] args;
        if (parameters == null)
            args = new Object[0]; // No arguments.
        else {
            args = new Object[1];
            args[0] = parameters;
        }
        try {
            method.invoke(this, args);
        }
        catch (IllegalArgumentException e) {
            throw e;
        }
        catch (Throwable t) {
            Log.error(t);
        }
    }

    // FIXME Removed hard-coded Control G!
    public static boolean checkKeyboardQuit(Object object)
    {
        if (object instanceof JEvent) {
            JEvent e = (JEvent) object;
            if (e.getID() == JEvent.KEY_PRESSED) {
                if (e.getKeyCode() == 0x47 && e.getModifiers() == 2)
                    return true;
            }
            return false;
        }
        if (object instanceof KeyEvent) {
            KeyEvent e = (KeyEvent) object;
            if (e.getID() == KeyEvent.KEY_PRESSED) {
                if (e.getKeyCode() == 0x47 && e.getModifiers() == 2)
                    return true;
            }
            return false;
        }
        return false;
    }

    private KeyMap requestedKeyMap;
    private EventSequence currentEventSequence;
    private boolean local;

    public boolean handleJEvent(JEvent event)
    {
        char keyChar = event.getKeyChar();
        int keyCode = event.getKeyCode();
        int modifiers = event.getModifiers();
        if (insertingKeyText) {
            insertKeyTextInternal(keyChar, keyCode, modifiers);
            return true;
        }
        KeyMapping mapping = null;
        if (requestedKeyMap != null) {
            if (checkKeyboardQuit(event)) {
                requestedKeyMap = null;
                currentEventSequence = null;
                local = false;
                status("");
                return false;
            }
            mapping = requestedKeyMap.lookup(keyChar, keyCode, modifiers);
            // "When both the global and local definitions of a key are other
            // keymaps, the next character is looked up in both keymaps, with
            // the local definition overriding the global one. The character
            // after the `C-x' is looked up in both the major mode's own keymap
            // for redefined `C-x' commands and in `ctl-x-map'. If the major
            // mode's own keymap for `C-x' commands contains `nil', the
            // definition from the global keymap for `C-x' commands is used."
            // (from the documentation for xemacs 21.4.17)
            if (mapping == null && local) {
                // Not found in local keymap.
                EventSequence copy = currentEventSequence.copy();
                copy.addEvent(event);
                mapping = KeyMap.getGlobalKeyMap().lookupEventSequence(copy);
            }
        } else {
            // Look in mode-specific key map.
            mapping =
                buffer.getMode().getKeyMap().lookup(keyChar, keyCode, modifiers);
            if (mapping != null)
                local = true;
            else
                // Look in global key map.
                mapping = KeyMap.getGlobalKeyMap().lookup(keyChar, keyCode,
                                                          modifiers);
        }
        if (mapping == null) {
            if (event.getID() == JEvent.KEY_TYPED) {
                // Reset.
                requestedKeyMap = null;
                currentEventSequence = null;
                local = false;
            }
            return false;
        }
        if (mapping != null) {
            Object command = mapping.getCommand();
            if (command instanceof KeyMap) {
                // Emacs-style key sequence.
                if (currentEventSequence == null)
                    currentEventSequence = new EventSequence();
                currentEventSequence.addEvent(event);
                requestedKeyMap = (KeyMap) command;
                status(currentEventSequence.getStatusText() + "-");
                return true;
            }
            if (isRecordingMacro())
                Macro.record(this, command);
            if (command instanceof String) {
                requestedKeyMap = null;
                currentEventSequence = null;
                local = false;
                String commandString = (String) command;
                if (commandString.length() > 0 && commandString.charAt(0) == '(') {
                    // A Lisp form.
                    executeCommand(commandString);
                    return true;
                }
                String[] array = parseCommand(commandString);
                if (array != null) {
                    String methodName = array[0];
                    String parameters = array[1];
                    try {
                        return execute(methodName, parameters);
                    }
                    catch (NoSuchMethodException e) {}
                }
            } else if (command instanceof LispObject) {
                requestedKeyMap = null;
                currentEventSequence = null;
                local = false;
                try {
                    LispThread.currentThread().execute(Lisp.coerceToFunction((LispObject)command));
                }
                catch (Throwable t) {
                    Log.error(t);
                }
                return true;
            }
        }
        return false;
    }

    public KeyMapping getKeyMapping(char keyChar, int keyCode, int modifiers)
    {
        if (requestedKeyMap != null)
            return requestedKeyMap.lookup(keyChar, keyCode, modifiers);
        // Look in mode-specific key map.
        KeyMapping mapping =
            buffer.getMode().getKeyMap().lookup(keyChar, keyCode, modifiers);
        if (mapping != null)
            return mapping;
        // Look in global key map.
        return KeyMap.getGlobalKeyMap().lookup(keyChar, keyCode, modifiers);
    }

    // Returns multiple values: mapping, mode.
    // mode == null means it's a global mapping.
    public Object[] getKeyMapping(String command)
    {
        Mode mode = null;
        // Look in buffer-local keymap first.
        KeyMapping mapping = buffer.getKeyMapForMode().getKeyMapping(command);
        // If not found there, try global keymap.
        if (mapping == null) {
            mapping = KeyMap.getGlobalKeyMap().getKeyMapping(command);
            // Don't let a global mapping hide a different mapping of the same
            // keystroke in the buffer-local keymap!
            if (mapping != null) {
                javax.swing.KeyStroke keyStroke =
                    javax.swing.KeyStroke.getKeyStroke(mapping.getKeyCode(),
                        mapping.getModifiers());
                if (buffer.getKeyMapForMode().lookup(keyStroke) != null)
                    mapping = null;
            }
        } else
            mode = getMode();
        Object[] values = new Object[2];
        values[0] = mapping;
        values[1] = mode;
        return values;
    }

    public void pageDown()
    {
        if (dot == null)
            return;
        maybeResetGoalColumn();
        if (mark != null) {
            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            setMark(null);
            setUpdateFlag(REPAINT);
            pageDownInternal();
            endCompoundEdit(compoundEdit);
        } else
            pageDownInternal();
        setCurrentCommand(COMMAND_PAGE_DOWN);
    }

    public void pageDownOtherWindow()
    {
        final Editor ed = frame.getOtherEditor();
        if (ed != null) {
            ed.pageDown();
            ed.updateDisplay();
        }
    }

    public void selectPageDown()
    {
        if (dot == null)
            return;
        maybeResetGoalColumn();
        if (mark == null) {
            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            setMarkAtDot();
            pageDownInternal();
            endCompoundEdit(compoundEdit);
        } else
            pageDownInternal();
        setCurrentCommand(COMMAND_PAGE_DOWN);
    }

    public void pageUp()
    {
        if (dot == null)
            return;
        maybeResetGoalColumn();
        if (mark != null) {
            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            setMark(null);
            setUpdateFlag(REPAINT);
            pageUpInternal();
            endCompoundEdit(compoundEdit);
        } else
            pageUpInternal();
        setCurrentCommand(COMMAND_PAGE_UP);
    }

    public void pageUpOtherWindow()
    {
        final Editor ed = frame.getOtherEditor();
        if (ed != null) {
            ed.pageUp();
            ed.updateDisplay();
        }
    }

    public void selectPageUp()
    {
        if (dot == null)
            return;
        maybeResetGoalColumn();
        if (mark == null) {
            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            setMarkAtDot();
            pageUpInternal();
            endCompoundEdit(compoundEdit);
        } else
            pageUpInternal();
        setCurrentCommand(COMMAND_PAGE_UP);
    }

    private void pageDownInternal()
    {
        Debug.assertTrue(buffer.needsRenumbering == false);
        Line dotLine = getDotLine();
        int numRows = display.getRows();
        Line[] lines = new Line[numRows];
        Line line = getTopLine();
        int dotRow = -1;
        for (int i = 0; i < numRows; i++) {
            lines[i] = line;
            if (line == dotLine)
                dotRow = i;
            if (line != null)
                line = line.nextVisible();
        }
        Line bottomLine = lines[numRows - 1];
        if (bottomLine == null) {
            // We're on the last page already.
            if (dotRow >= 0) {
                Position eob = getEob();
                if (eob != null) {
                    addUndo(SimpleEdit.MOVE);
                    updateDotLine();
                    dot.setLine(eob.getLine());
                    moveDotToGoalCol();
                    updateDotLine();
                }
            }
            return;
        }
        // Not on last page.
        display.setTopLine(bottomLine);
        setUpdateFlag(REPAINT);
        if (dotRow >= 0) {
            line = getTopLine();
            for (int i = 0; i < dotRow; i++) {
                Line next = line.nextVisible();
                if (next == null)
                    break;
                line = next;
            }
            addUndo(SimpleEdit.MOVE);
            dot.setLine(line);
            moveDotToGoalCol();
        }
    }

    private void pageUpInternal()
    {
        if (dot.getLine() == buffer.getFirstLine())
            return;
        Debug.assertTrue(buffer.needsRenumbering == false);
        int topLineNumber = display.getTopLineNumber();
        int dotLineNumber = dot.lineNumber();
        int linesToScroll = display.getRows() - 1;
        boolean dotLineIsVisible = false;
        if (dotLineNumber >= topLineNumber) {
            Line bottomLine = display.getBottomLine();
            if (bottomLine != null)
                if (dotLineNumber <= bottomLine.lineNumber())
                    dotLineIsVisible = true;
        }
        addUndo(SimpleEdit.MOVE);
        if (dotLineIsVisible) {
            for (int i = 0; i < linesToScroll; i++) {
                Line topLinePrevious = getTopLine().previousVisible();
                if (topLinePrevious != null)
                    display.setTopLine(topLinePrevious);
                Line dotLinePrevious = dot.getLine().previousVisible();
                if (dotLinePrevious == null)
                    break;
                dot.setLine(dotLinePrevious);
            }
        } else {
            for (int i = 0; i < linesToScroll; i++) {
                Line dotLinePrevious = dot.getLine().previousVisible();
                if (dotLinePrevious == null)
                    break;
                dot.setLine(dotLinePrevious);
            }
        }
        moveDotToGoalCol();
        setUpdateFlag(REPAINT);
    }

    // Move dot to beginning of block, no undo.
    public void beginningOfBlock()
    {
        if (mark != null) {
            Region r = new Region(buffer, mark, dot);
            dot.moveTo(r.getBegin());
            setMark(null);
            moveCaretToDotCol();
            if (r.getBeginLine() != r.getEndLine())
                setUpdateFlag(REPAINT);
            else
                updateDotLine();
        }
    }

    // Move dot to end of block, no undo.
    public void endOfBlock()
    {
        if (mark != null) {
            Region r = new Region(buffer, mark, dot);
            dot.moveTo(r.getEnd());
            setMark(null);
            moveCaretToDotCol();
            if (r.getBeginLine() != r.getEndLine())
                setUpdateFlag(REPAINT);
            else
                updateDotLine();
        }
    }

    public void right()
    {
        if (dot == null)
            return;
        if (buffer.getBooleanProperty(Property.RESTRICT_CARET)) {
            if (mark == null && getDotOffset() >= getDotLine().length()) {
                // Caret is at end of line.
                if (getDotLine().next() != null) {
                    moveDotTo(getDotLine().next(), 0);
                    setCurrentCommand(COMMAND_RIGHT);
                }
                return;
            }
        }
        if (mark != null || lastCommand != COMMAND_RIGHT)
            addUndo(SimpleEdit.MOVE);
        if (mark != null)
            endOfBlock();
        else if (dot.getOffset() < dot.getLineLength()) {
            dot.skip(1);
            moveCaretToDotCol();
        } else {
            ++display.caretCol;
        }
        updateDotLine();
        setCurrentCommand(COMMAND_RIGHT);
        setUpdateFlag(REFRAME);
    }

    public void selectRight()
    {
        if (dot == null)
            return;
        if (getDotOffset() < getDotLine().length()) {
            if (mark == null || lastCommand != COMMAND_RIGHT)
                addUndo(SimpleEdit.MOVE);
            if (mark == null)
                setMarkAtDot();
            dot.moveRight();
            moveCaretToDotCol();
        } else {
            // We're at or beyond the end of the line.
            if (buffer.getBooleanProperty(Property.RESTRICT_CARET)) {
                if (getDotLine().next() == null)
                    return;
                if (mark == null || lastCommand != COMMAND_RIGHT)
                    addUndo(SimpleEdit.MOVE);
                if (mark == null)
                    setMarkAtDot();
                updateDotLine();
                dot.moveTo(getDotLine().next(), 0);
                moveCaretToDotCol();
            } else {
                // Don't start a new selection, since there's no text there.
                if (lastCommand != COMMAND_RIGHT)
                    addUndo(SimpleEdit.MOVE);
                ++display.caretCol;
            }
        }
        updateDotLine();
        setCurrentCommand(COMMAND_RIGHT);
        setUpdateFlag(REFRAME);
    }

    public void left()
    {
        if (dot == null)
            return;
        final Line dotLine = getDotLine();
        final int dotOffset = getDotOffset();
        final Line prevLine = dotLine.previous();
        if (dotOffset == 0 && prevLine == null)
            return;
        if (mark != null || lastCommand != COMMAND_LEFT)
            addUndo(SimpleEdit.MOVE);
        if (mark != null)
            beginningOfBlock();
        else {
            int absCaretCol = display.getCaretCol() + display.getShift();
            if (absCaretCol > 0 && absCaretCol <= buffer.getCol(dotLine, dotLine.length())) {
                // Back up one character.
                dot.setOffset(dotOffset - 1);
                moveCaretToDotCol();
            } else if (absCaretCol > 0) {
                // We're beyond the end of the text on the line.
                --display.caretCol;
            } else if (dot.getOffset() == 0) {
                // Back up to the end of the text on the previous line.
                update(dotLine);
                setDot(prevLine, prevLine.length());
                moveCaretToDotCol();
            } else {
                // There shouldn't be any other cases.
                Debug.assertTrue(false);
            }
        }
        updateDotLine();
        setCurrentCommand(COMMAND_LEFT);
        setUpdateFlag(REFRAME);
    }

    public void selectLeft()
    {
        if (dot == null)
            return;
        final Line dotLine = getDotLine();
        final int dotOffset = getDotOffset();
        final Line prevLine = dotLine.previous();
        if (dotOffset == 0 && prevLine == null)
            return;
        if (mark == null || lastCommand != COMMAND_LEFT)
            addUndo(SimpleEdit.MOVE);

        // Only start a new selection if we're over some actual text.
        final int absCaretCol = display.getAbsoluteCaretCol();
        final int end = buffer.getCol(dotLine, dotLine.length());
        if (mark == null && absCaretCol <= end)
            setMarkAtDot();

        if (absCaretCol > 0 && absCaretCol <= end) {
            // Back up one character.
            dot.moveLeft();
            moveCaretToDotCol();
        } else if (absCaretCol > 0) {
            // We're beyond the end of the text on the line.
            --display.caretCol;
        } else if (dotOffset == 0) {
            // Back up to the end of the text on the previous line.
            updateDotLine();
            setDot(prevLine, prevLine.length());
            moveCaretToDotCol();
        } else {
            // There shouldn't be any other cases.
            Debug.assertTrue(false);
        }
        updateDotLine();
        setCurrentCommand(COMMAND_LEFT);
        setUpdateFlag(REFRAME);
    }

    public final int getAbsoluteCaretCol()
    {
        return display.getAbsoluteCaretCol();
    }

    public final void setAbsoluteCaretCol(int col)
    {
        display.setAbsoluteCaretCol(col);
    }

    private int goalColumn;

    public final void setGoalColumn(int col)
    {
        goalColumn = col;
    }

    public void moveDotToGoalCol()
    {
        if (buffer.getBooleanProperty(Property.RESTRICT_CARET)) {
            final int limit =
                buffer.getCol(getDotLine(), getDotLine().length());
            moveDotToCol(goalColumn > limit ? limit : goalColumn);
            moveCaretToDotCol();
        } else {
            setAbsoluteCaretCol(goalColumn);
            moveDotToCaretCol();
        }
    }

    // Move caret down one line, keeping it in the same column if possible.
    // Synchronize dot with caret.
    public void down()
    {
        maybeResetGoalColumn();
        display.down(false);
        setCurrentCommand(COMMAND_DOWN);
    }

    public void selectDown()
    {
        maybeResetGoalColumn();
        display.down(true);
        setCurrentCommand(COMMAND_DOWN);
    }

    // Move caret up one line, keeping it in the same column if possible.
    // Synchronize dot with caret.
    public void up()
    {
        maybeResetGoalColumn();
        display.up(false);
        setCurrentCommand(COMMAND_UP);
    }

    public void selectUp()
    {
        maybeResetGoalColumn();
        display.up(true);
        setCurrentCommand(COMMAND_UP);
    }

    private void maybeResetGoalColumn()
    {
        switch (lastCommand) {
            case COMMAND_UP:
            case COMMAND_DOWN:
            case COMMAND_PAGE_UP:
            case COMMAND_PAGE_DOWN:
            case COMMAND_WINDOW_UP:
            case COMMAND_WINDOW_DOWN:
                return;
            default:
                goalColumn = getAbsoluteCaretCol();
                return;
        }
    }

    public void windowUp()
    {
        maybeResetGoalColumn();
        display.windowUp();
        maybeScrollCaret();
        setCurrentCommand(COMMAND_WINDOW_UP);
    }

    public void windowDown()
    {
        maybeResetGoalColumn();
        display.windowDown();
        maybeScrollCaret();
        setCurrentCommand(COMMAND_WINDOW_DOWN);
    }

    public void maybeScrollCaret()
    {
        if (dot == null)
            return;
        // Don't scroll the caret if a region is selected!
        if (mark != null)
            return;
        if (buffer.getBooleanProperty(Property.SCROLL_CARET)) {
            final int dotLineNumber = dot.lineNumber();
            final Line topLine = display.getTopLine();
            if (dotLineNumber < topLine.lineNumber()) {
                // Caret is above window.
                addUndo(SimpleEdit.SCROLL_CARET);
                dot.moveTo(topLine, 0);
                updateDotLine();
                moveCaretToDotCol();
                goalColumn = 0;
                return;
            }
            final Line bottomLine = display.getBottomLine();
            if (dotLineNumber > bottomLine.lineNumber()) {
                // Caret is below window.
                addUndo(SimpleEdit.SCROLL_CARET);
                updateDotLine();
                dot.moveTo(bottomLine, 0);
                updateDotLine();
                moveCaretToDotCol();
                goalColumn = 0;
            }
        }
    }

    public void toCenter()
    {
        display.toCenter();
    }

    public void toTop()
    {
        display.toTop();
    }

    private void selectToPosition(Position pos)
    {
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol()) {
            addUndo(SimpleEdit.MOVE);
            if (mark == null)
                setMarkAtDot();
            if (pos.getLine() != getDotLine())
                setUpdateFlag(REPAINT);
            else
                updateDotLine();
            dot.moveTo(pos);
            moveCaretToDotCol();
        }
    }

    public void bol()
    {
        if (dot != null)
            moveDotTo(dot.getLine(), 0);
    }

    public void home()
    {
        if (dot == null)
            return;
        final boolean extend = prefs.getBooleanProperty(Property.EXTEND_HOME);
        Position pos;
        if (mark != null)
            pos = new Position(new Region(this).getBegin());
        else
            pos = new Position(dot);
        int indent = pos.getLine().getIndentation();
        if (extend) {
            if (pos.getOffset() > indent)
                pos.setOffset(indent);
            else
                pos.setOffset(0);
        } else {
            if (pos.getOffset() > indent)
                pos.setOffset(indent);
            else if (pos.getOffset() == indent)
                pos.setOffset(0);
            else
                pos.setOffset(indent);
        }
        if (mark != null || !pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol()) {
            moveDotTo(pos);
            setCurrentCommand(COMMAND_HOME);
            return;
        }
        // Reaching here, caret is already in column 0.
        if (!extend)
            return;
        if (System.currentTimeMillis() - dispatcher.getLastEventMillis() > 1000) {
            // Timed out.
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_HOME);
            return;
        }
        if (lastCommand == COMMAND_HOME_HOME)
            pos = new Position(buffer.getFirstLine(), 0);
        else if (lastCommand == COMMAND_HOME) {
            pos = new Position(getTopLine(), 0);
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_HOME_HOME);
        } else {
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_HOME);
            return;
        }
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol())
            moveDotTo(pos);
    }

    public void selectHome()
    {
        if (dot == null)
            return;
        final boolean extend = prefs.getBooleanProperty(Property.EXTEND_HOME);
        Position pos;
        if (mark != null)
            pos = new Position(new Region(buffer, mark, dot).getBegin());
        else
            pos = new Position(dot);
        int indent = pos.getLine().getIndentation();
        if (extend) {
            if (pos.getOffset() > indent)
                pos.setOffset(indent);
            else
                pos.setOffset(0);
        } else {
            if (pos.getOffset() > indent)
                pos.setOffset(indent);
            else if (pos.getOffset() == indent)
                pos.setOffset(0);
            else
                pos.setOffset(indent);
        }
        if (mark != null || !pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol()) {
            selectToPosition(pos);
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_SELECT_HOME);
            return;
        }
        // Reaching here, caret is already in column 0.
        if (!prefs.getBooleanProperty(Property.EXTEND_HOME))
            return;
        if (System.currentTimeMillis() - dispatcher.getLastEventMillis() > 1000) {
            // Timed out.
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_SELECT_HOME);
            return;
        }
        if (lastCommand == COMMAND_SELECT_HOME_HOME)
            pos = new Position(buffer.getFirstLine(), 0);
        else if (lastCommand == COMMAND_SELECT_HOME) {
            pos = new Position(getTopLine(), 0);
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_SELECT_HOME_HOME);
        } else {
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_SELECT_HOME);
            return;
        }
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol())
            selectToPosition(pos);
    }

    public void eol()
    {
        if (dot == null)
            return;
        if (mark != null || dot.getOffset() != dot.getLineLength() ||
            buffer.getCol(dot) != display.getCaretCol() + display.getShift())
            moveDotTo(dot.getLine(), dot.getLineLength());
    }

    public void end()
    {
        if (dot == null)
            return;
        Position pos;
        if (mark != null)
            pos = new Position(new Region(buffer, mark, dot).getEnd());
        else
            pos = new Position(dot);
        pos.setOffset(pos.getLineLength());
        if (mark != null || !pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol()) {
            moveDotTo(pos);
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END);
            return;
        }
        // Reaching here, caret is already at end of line.
        if (!prefs.getBooleanProperty(Property.EXTEND_END))
            return;
        if (System.currentTimeMillis() - dispatcher.getLastEventMillis() > 1000) {
            // Timed out.
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END);
            return;
        }
        if (lastCommand == COMMAND_END_END)
            pos = getEob();
        else if (lastCommand == COMMAND_END) {
            Line bottomLine = display.getBottomLine();
            if (bottomLine != null)
                pos = new Position(bottomLine, bottomLine.length());
            else
                pos = getEob();
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END_END);
        } else {
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END);
            return;
        }
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol())
            moveDotTo(pos);
    }

    public void selectEnd()
    {
        if (dot == null)
            return;
        Position pos;
        if (mark != null)
            pos = new Position(new Region(buffer, mark, dot).getEnd());
        else
            pos = new Position(dot);
        pos.setOffset(pos.getLineLength());
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol()) {
            selectToPosition(pos);
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END);
            return;
        }
        // Reaching here, caret is already at end of line.
        if (!prefs.getBooleanProperty(Property.EXTEND_END))
            return;
        if (System.currentTimeMillis() - dispatcher.getLastEventMillis() > 1000) {
            // Timed out.
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END);
            return;
        }
        if (lastCommand == COMMAND_END_END)
            pos = getEob();
        else if (lastCommand == COMMAND_END) {
            Line bottomLine = display.getBottomLine();
            if (bottomLine != null)
                pos = new Position(bottomLine, bottomLine.length());
            else
                pos = getEob();
            setUpdateFlag(REFRAME);
            setCurrentCommand(COMMAND_END_END);
        } else {
            setCurrentCommand(COMMAND_END);
            return;
        }
        if (!pos.equals(dot) ||
            buffer.getCol(pos) != display.getAbsoluteCaretCol())
            selectToPosition(pos);
    }

    public void bob()
    {
        if (buffer.getFirstLine() != null)
            moveDotTo(buffer.getFirstLine(), 0);
    }

    public void selectBob()
    {
        if (dot == null)
            return;
        if (buffer.getFirstLine() == null)
            return;
        addUndo(SimpleEdit.MOVE);
        if (mark == null)
            setMarkAtDot();
        dot.moveTo(buffer.getFirstLine(), 0);
        moveCaretToDotCol();
        setUpdateFlag(REPAINT);
    }

    private final Position getEob()
    {
        return buffer.getEnd();
    }

    public void eob()
    {
        moveDotTo(getEob());
    }

    public void selectEob()
    {
        if (buffer.getFirstLine() == null)
            return;
        Line line = buffer.getFirstLine();
        while (line.next() != null)
            line = line.next();
        addUndo(SimpleEdit.MOVE);
        if (mark == null)
            setMarkAtDot();
        dot.moveTo(line, line.length());
        moveCaretToDotCol();
        setUpdateFlag(REPAINT);
    }

    public void top()
    {
        if (dot == null)
            return;
        if (getDotLine() != getTopLine()) {
            addUndo(SimpleEdit.MOVE);
            updateDotLine();
            dot.setLine(getTopLine());
            moveDotToCaretCol();
        }
    }

    public void bottom()
    {
        if (dot == null)
            return;
        Line line = display.getBottomLine();
        if (line != getDotLine()) {
            addUndo(SimpleEdit.MOVE);
            updateDotLine();
            dot.setLine(line);
            updateDotLine();
            moveDotToCaretCol();
        }
    }

    public void selectWord()
    {
        if (dot == null)
            return;
        AWTEvent e = dispatcher.getLastEvent();
        CompoundEdit compoundEdit = null;
        if (e instanceof MouseEvent) {
            compoundEdit = beginCompoundEdit();
            mouseMoveDotToPoint((MouseEvent)e);
        }
        if (inWord()) {
            addUndo(SimpleEdit.MOVE);
            while (getDotOffset() > 0) {
                dot.moveLeft();
                if (!inWord()) {
                    dot.moveRight();
                    break;
                }
            }
            setMarkAtDot();
            final int limit = getDotLine().length();
            while (inWord() && getDotOffset() < limit)
                dot.moveRight();
            moveCaretToDotCol();
        }
        if (compoundEdit != null)
            endCompoundEdit(compoundEdit);
    }

    public void mouseMoveDotToPoint()
    {
        AWTEvent e = dispatcher.getLastEvent();
        if (e instanceof MouseEvent)
            mouseMoveDotToPoint((MouseEvent)e);
    }

    public void mouseMoveDotToPoint(MouseEvent e)
    {
        addUndo(SimpleEdit.MOVE);
        if (mark != null)
            unmark();
        display.moveCaretToPoint(e.getPoint());
        if (buffer.getBooleanProperty(Property.RESTRICT_CARET))
            moveCaretToDotCol();

        // Dec 13 2002 6:30 PM
        // Without this, focus ends up in the location bar textfield if you
        // click in the edit window after using the openFile completion list
        // to open a file. Weird.
        Editor.restoreFocus();
    }

    public void mouseSelect()
    {
        if (dot != null) {
            AWTEvent e = dispatcher.getLastEvent();
            if (e instanceof MouseEvent) {
                MouseEvent mouseEvent = (MouseEvent) e;
                Position pos = display.positionFromPoint(mouseEvent.getPoint());
                addUndo(SimpleEdit.MOVE);
                Position min, max;
                if (mark == null) {
                    // New selection.
                    setMarkAtDot();
                    dot = pos;
                    moveCaretToDotCol();
                    Region r = new Region(this);
                    min = r.getBegin();
                    max = r.getEnd();
                } else {
                    // Adjust existing selection.
                    Region r = new Region(this);
                    min = r.getBegin();
                    max = r.getEnd();
                    if (pos.isBefore(r.getBegin())) {
                        min = pos;
                        mark = r.getEnd();
                    } else if (pos.isAfter(r.getEnd())) {
                        max = pos;
                        mark = r.getBegin();
                    } else {
                        // Click was inside selected region.
                        if (Position.getDistance(r.getBegin(), pos) < Position.getDistance(r.getEnd(), pos)) {
                            // Click was closer to beginning of region.
                            mark = r.getEnd();
                        } else
                            mark = r.getBegin();
                    }
                    dot = pos;
                    moveCaretToDotCol();
                }
                // Minimize repaint.
                if (max.lineNumber() - min.lineNumber() < display.getRows()) {
                    Line line = min.getLine();
                    Line endLine = max.getLine();
                    while (line != null && line != endLine) {
                        update(line);
                        line = line.next();
                    }
                    update(endLine);
                } else
                    setUpdateFlag(REPAINT);
            }
        }
    }

    public void mouseSelectColumn()
    {
        if (dot != null) {
            AWTEvent e = dispatcher.getLastEvent();
            if (e instanceof MouseEvent) {
                MouseEvent mouseEvent = (MouseEvent) e;
                if (getMark() == null)
                    setMarkAtDot();
                display.moveCaretToPoint(mouseEvent.getPoint());
                setColumnSelection(true);
                display.setUpdateFlag(REPAINT);
            }
        }
    }

    private JPopupMenu popup;

    public void mouseShowContextMenu()
    {
        AWTEvent e = dispatcher.getLastEvent();
        if (e instanceof MouseEvent) {
            MouseEvent mouseEvent = (MouseEvent) e;
            int x = mouseEvent.getX();
            int y = mouseEvent.getY();
            popup = buffer.getMode().getContextMenu(this);
            if (popup != null) {
                Dimension dimPopup = popup.getPreferredSize();
                Dimension dimDisplay = display.getSize();
                int xMax = dimDisplay.width - dimPopup.width - 5;
                int yMax = dimDisplay.height - dimPopup.height - 5;
                if (x > xMax)
                    x = xMax;
                else
                    ++x;
                if (y > yMax)
                    y = yMax;
                else
                    ++y;
                popup.show(mouseEvent.getComponent(), x, y);
            }
        }
    }

    public final JPopupMenu getPopup()
    {
        return popup;
    }

    public final void setPopup(JPopupMenu popup)
    {
        this.popup = popup;
    }

    public void killPopup()
    {
        if (popup != null) {
            popup.setVisible(false);
            popup = null;
            restoreFocus();
        }
    }

    private boolean inWord()
    {
        return getMode().isIdentifierPart(getDotChar());
    }

    private boolean inWhitespace()
    {
        return Character.isWhitespace(getDotChar());
    }

    private void skipWhitespace()
    {
        while (inWhitespace())
            if (!nextChar())
                break;
    }

    private void nextWord()
    {
        if (dot == null)
            return;
        if (inWord()) {
            while (nextChar())
                if (!inWord())
                    break;
            skipWhitespace();
        } else if (inWhitespace()) {
            skipWhitespace();
        } else {
            // Not in word or whitespace.
            while (nextChar() && !inWord() && !inWhitespace())
                ;
            skipWhitespace();
        }
    }

    private void prevWord()
    {
        if (dot == null)
            return;
        if (!prevChar())
            return;
        if (inWord()) {
            while (prevChar() && inWord())
                ;
            if (!inWord())
                nextChar();
        } else if (inWhitespace()) {
            while (prevChar() && inWhitespace())
                ;
            if (inWord()) {
                while (prevChar() && inWord())
                    ;
                if (!inWord())
                    nextChar();
            } else {
                while (prevChar() && !inWord() && !inWhitespace())
                    ;
                if (inWord() || inWhitespace())
                    nextChar();
            }
        } else {
            // Not in word or whitespace.
            while (prevChar()) {
                if (inWord())
                    break;
                if (inWhitespace())
                    break;
            }
            if (inWord() || inWhitespace())
                nextChar();
        }
    }

    public void wordRight()
    {
        if (dot == null)
            return;
        updateDotLine();
        addUndo(SimpleEdit.MOVE);
        endOfBlock();
        nextWord();
        moveCaretToDotCol();
        updateDotLine();
    }

    public void wordLeft()
    {
        if (dot == null)
            return;
        updateDotLine();
        addUndo(SimpleEdit.MOVE);
        beginningOfBlock();
        prevWord();
        moveCaretToDotCol();
        updateDotLine();
    }

    public void selectWordRight()
    {
        if (dot == null)
            return;
        addUndo(SimpleEdit.MOVE);
        if (mark == null)
            setMarkAtDot();
        updateDotLine();
        nextWord();
        moveCaretToDotCol();
        updateDotLine();
    }

    public void selectWordLeft()
    {
        if (dot == null)
            return;
        addUndo(SimpleEdit.MOVE);
        if (mark == null)
            setMarkAtDot();
        updateDotLine();
        prevWord();
        moveCaretToDotCol();
        updateDotLine();
    }

    public void selectAll()
    {
        if (dot == null)
            return;
        pushPosition();
        addUndo(SimpleEdit.MOVE);
        unmark();
        Line line = buffer.getFirstLine();
        dot.moveTo(line, 0);
        display.setCaretCol(0);
        display.setShift(0);
        setMarkAtDot();
        while (line.next() != null)
            line = line.next();
        dot.moveTo(line, line.length());
        moveCaretToDotCol();
        display.setUpdateFlag(REPAINT);
    }

    // Moves dot to the requested absolute column, based on the tab size of
    // the buffer. If the requested column is past the end of the line, dot is
    // moved to the end of the line.
    public void moveDotToCol(int goal)
    {
        if (dot == null)
            return;

        dot.moveToCol(goal, buffer.getTabWidth());
        updateDotLine();

        // Support tab chars in buffer. If we're not beyond the end of the
        // line, make sure we're on an actual character.
        if (dot.getOffset() < dot.getLineLength())
            moveCaretToDotCol();
    }

    public final void moveDotToCaretCol()
    {
        moveDotToCol(display.getAbsoluteCaretCol());
    }

    public final void moveCaretToDotCol()
    {
        display.moveCaretToDotCol();
    }

    public final void repaintDisplay()
    {
        display.setUpdateFlag(REPAINT);
        display.repaint();
    }

    public final void repaintNow()
    {
        display.repaintNow();
    }

    // Adds whitespace to fill the area between the end of the actual text on
    // a line and the location of the caret, if it's beyond the end of the
    // text. Dot is moved to the end of the appended whitespace. Does nothing
    // if caret is not past end of text.
    public void fillToCaret()
    {
        final int where = display.getAbsoluteCaretCol();
        final Line dotLine = getDotLine();
        String s = getFillString(dotLine, where);
        if (s != null) {
            try {
                buffer.lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                addUndo(SimpleEdit.LINE_EDIT);
                dotLine.setText(dotLine.getText().concat(s));
                buffer.modified();
                dot.setOffset(dotLine.length());
            }
            finally {
                buffer.unlockWrite();
            }
        }
    }

    private String getFillString(Line line, int where)
    {
        int end = buffer.getCol(line, line.length());
        if (where <= end)
            return null;
        final int width = where - end;

        // For sanity, only use actual tab chars at beginning of line!
        if (buffer.getUseTabs() && line.length() == 0) {
            FastStringBuffer sb = new FastStringBuffer(width);
            int col = 0;
            final int tabWidth = buffer.getTabWidth();
            while (col + tabWidth <= width) {
                sb.append('\t');
                col += tabWidth;
            }
            while (col < width) {
                sb.append(' ');
                ++col;
            }
            return sb.toString();
        } else
            return Utilities.spaces(width);
    }

    public final int getDotCol()
    {
        return buffer.getCol(dot);
    }

    // Insert string at dot, put dot at end of inserted string.
    // No undo.
    public void insertStringInternal(String s)
    {
        updateInAllEditors(getDotLine());
        buffer.insertString(dot, s);
    }

    // Fills the space (if any) between dot and caret and inserts
    // the char in question.
    public void insertChar(char c)
    {
        final Line dotLine = getDotLine();
        if (getDotOffset() > dotLine.length()) {
            // Shouldn't happen.
            Debug.bug();
            Log.error("insertChar dot offset = " + getDotOffset() +
                      " dotLine length = " + dotLine.length());
            // Enforce sanity and carry on.
            dot.setOffset(dotLine.length());
        }
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            addUndo(SimpleEdit.LINE_EDIT);
            fillToCaret();
            FastStringBuffer sb =
                new FastStringBuffer(dotLine.substring(0, getDotOffset()));
            sb.append(c);
            sb.append(dotLine.substring(getDotOffset()));
            dotLine.setText(sb.toString());
            dot.moveRight();
            moveCaretToDotCol();
            buffer.modified();
        }
        finally {
            buffer.unlockWrite();
        }
        updateInAllEditors(dotLine);
    }

    public void insertChar()
    {
        if (!checkReadOnly())
            return;
        String input = InputDialog.showInputDialog(this, "Character:", "Insert Character");
        if (input == null || input.length() == 0)
            return;
        repaintNow();
        int c = parseNumericInput(input);
        if (c >= 0 && c < 0xfffe)
            insertChar((char) c);
        else
            MessageDialog.showMessageDialog(this, "Invalid character", "Insert Character");
    }

    public void insertByte()
    {
        if (!checkReadOnly())
            return;
        String input = InputDialog.showInputDialog(this, "Byte:", "Insert Byte");
        if (input == null || input.length() == 0)
            return;
        repaintNow();
        int c = parseNumericInput(input);
        if (c >= 0 && c <= 255) {
            byte[] bytes = new byte[1];
            bytes[0] = (byte) c;
            String encoding = prefs.getStringProperty(Property.DEFAULT_ENCODING);
            try {
                String s = new String(bytes, encoding);
                insertChar(s.charAt(0));
            }
            catch (UnsupportedEncodingException e) {
                Log.error(e);
                MessageDialog.showMessageDialog(this,
                    "Unsupported encoding \"" + encoding + "\"", "Insert Byte");
            }
        } else
            MessageDialog.showMessageDialog(this,
                "Invalid byte \"" + input + "\"", "Insert Byte");
    }

    // Used only by insertChar and insertByte. Doesn't understand a leading
    // minus sign.
    private static int parseNumericInput(String input)
    {
        int n = -1;
        input = input.trim();
        try {
            if (input.startsWith("0x") || input.startsWith("0X"))
                n = Integer.parseInt(input.substring(2), 16);
            else if (input.startsWith("0"))
                n = Integer.parseInt(input, 8);
            else
                n = Integer.parseInt(input, 10);
        }
        catch (NumberFormatException e) {
            Log.error(e);
        }
        return n;
    }

    public void electricSemi()
    {
        if (!checkReadOnly())
            return;
        if (mark != null || getDotLine().flags() == STATE_COMMENT ||
            getMode().isInQuote(buffer, dot)) {
            insertNormalChar(';');
        } else {
            CompoundEdit compoundEdit = beginCompoundEdit();
            insertChar(';');
            moveCaretToDotCol();
            indentLine();
            if (buffer.getBooleanProperty(Property.AUTO_NEWLINE)) {
                boolean b = true;
                String s = dot.getLine().trim();
                if (s.startsWith("for")) {
                    char c = s.charAt(3);
                    if (c == ' ' || c == '\t' || c == '(')
                        b = false;
                }
                if (b)
                    newlineAndIndent();
            }
            endCompoundEdit(compoundEdit);
        }
    }

    public void electricColon()
    {
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            electricColonInternal();
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private void electricColonInternal()
    {
        final Line dotLine = getDotLine();
        final int dotOffset = getDotOffset();
        if (mark != null || dotOffset != dotLine.length()) {
            insertNormalChar(':');
            return;
        }
        if (dotLine.flags() == STATE_COMMENT || getMode().isInQuote(buffer, dot)) {
            insertNormalChar(':');
            return;
        }
        CompoundEdit compoundEdit = beginCompoundEdit();
        insertChar(':');
        moveCaretToDotCol();
        indentLine();
        if (buffer.getBooleanProperty(Property.AUTO_NEWLINE))
            newlineAndIndent();
        endCompoundEdit(compoundEdit);
    }

    public void electricStar()
    {
        if (!checkReadOnly())
            return;

        // The intention here is to line up the '*' under the '*' of
        // the previous line if the current line is blank and if the
        // previous line begins with "/*".
        if (getDotLine().isBlank()) {
            if (buffer.needsParsing()) {
                if (getFormatter().parseBuffer())
                    buffer.repaint();
            }
            CompoundEdit compoundEdit = beginCompoundEdit();
            insertNormalChar('*');
            indentLine();
            endCompoundEdit(compoundEdit);
        } else
            insertNormalChar('*');
    }

    public void electricPound()
    {
        if (!checkReadOnly())
            return;
        if (mark == null && getDotLine().isBlank()) {
            try {
                buffer.lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                addUndo(SimpleEdit.LINE_EDIT);
                getDotLine().setText("#");
                dot.setOffset(1);
                buffer.modified();
            }
            finally {
                buffer.unlockWrite();
            }
            updateInAllEditors(getDotLine());
            moveCaretToDotCol();
        } else
            insertNormalChar('#');
    }

    public void electricOpenBrace()
    {
        electricBraceInternal('{');
    }

    public void electricCloseBrace()
    {
        electricBraceInternal('}');
    }

    private void electricBraceInternal(char c)
    {
        if (!checkReadOnly())
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark == null && getDotLine().isBlank()) {
            addUndo(SimpleEdit.LINE_EDIT);
            getDotLine().setText("");
            dot.setOffset(0);
            insertChar(c);
            indentLine();
            eol();
            if (buffer.getBooleanProperty(Property.AUTO_NEWLINE))
                newlineAndIndent();
        } else {
            insertNormalChar(c);
            indentLine();
        }
        endCompoundEdit(compoundEdit);
    }

    public void electricCloseAngleBracket()
    {
        if (!checkReadOnly())
            return;
        if (mark == null) {
            int modeId = getModeId();
            if (modeId == XML_MODE || modeId == HTML_MODE) {
                CompoundEdit compoundEdit = beginCompoundEdit();
                insertChar('>');
                moveCaretToDotCol();
                if (buffer.getBooleanProperty(Property.AUTO_INDENT)) {
                    if (modeId == HTML_MODE &&
                        getDotLine().substring(0, getDotOffset()).endsWith(
                            "</pre>")) {
                        ; // No autoindent after "</pre>" in HTML mode.
                    } else {
                        indentLine();
                    }
                }
                endCompoundEdit(compoundEdit);
                return;
            }
        }
        // Otherwise...
        insertNormalChar('>');
    }

    public void gotoline(int lineNumber)
    {
        Line line = buffer.getLine(lineNumber);
        if (line != null)
            setDot(line, 0);
    }

    public void saveState()
    {
        if (saveSession) {
            // Make sure information about current buffer is up-to-date.
            saveView();

            Session.saveDefaultSession();
            if (sessionName != null)
                if (prefs.getBooleanProperty(Property.AUTOSAVE_NAMED_SESSIONS))
                    Session.saveCurrentSession();
            sessionProperties.saveWindowPlacement();
            sessionProperties.save();
        }
    }

    // It might make sense to move this code into the Buffer class.
    public void reload(Buffer buf)
    {
        setWaitCursor();
        if (buf.getFile() instanceof SshFile)
            return; // Not supported.
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == buf)
                ed.saveView();
        }

        // May be asynchronous.
        buf.reload();
        setDefaultCursor();
    }

    public void revertBuffer()
    {
        final File file = buffer.getFile();
        if (file instanceof SshFile)
            return; // Not supported.
        if (buffer.isModified()) {
            String prompt = "Discard changes to " + file.canonicalPath() + "?";
            if (!confirm("Revert Buffer", prompt))
                return;
            reload(buffer);
        }
    }

    // Returns true if the buffer is active and there has been some change
    // that requires us to redraw the menus, title bar or display, false
    // otherwise.
    public boolean reactivate(Buffer buf)
    {
        if (buf instanceof ImageBuffer)
            return ((ImageBuffer)buf).reactivate();

        if (buf.getType() != Buffer.TYPE_NORMAL)
            return false;
        if (buf.isUntitled())
            return false;

        // BUG! Why don't we reload binary mode buffers?
        if (buf.getModeId() == BINARY_MODE)
            return false;

        final File file = buf.getFile();
        if (file == null || file.isRemote() || !file.isFile())
            return false;

        boolean changed = false;

        // Check read-only status even if buffer is not loaded so buffer list
        // will be correct.
        if (buf.readOnly == file.canWrite()) {
            // Read-only status has changed.
            buf.readOnly = !buf.readOnly;
            changed = true;
            // Let the user know if the file associated with a modified buffer
            // is no longer writable.
            if (buf.readOnly && buf.isLoaded() && buf.isModified())
                MessageDialog.showMessageDialog(
                    file.canonicalPath().concat(" is no longer writable"),
                    "Warning");
        }

        if (buf.isLoaded()) {
            if (file.lastModified() != buf.getLastModified()) {
                if (buf.isModified()) {
                    String prompt = file.canonicalPath() +
                        " has changed on disk. Reload and lose current changes?";
                    if (confirm("Reload File From Disk", prompt)) {
                        reload(buf);
                        changed = true;
                    } else
                        buf.setLastModified(file.lastModified());
                } else {
                    // No need for confirmation.
                    reload(buf);
                    changed = true;
                }
            }
        }

        return changed;
    }

    public void setFocus(JComponent c)
    {
        frame.setFocus(c);
    }

    public JComponent getFocusedComponent()
    {
        return frame.getFocusedComponent();
    }

    public void setFocusToDisplay()
    {
        frame.setFocus(display);
    }

    public static final void restoreFocus()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                if (currentEditor != null)
                    currentEditor.setFocusToDisplay();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void componentHidden(ComponentEvent e)
    {
    }

    public void componentMoved(ComponentEvent e)
    {
    }

    public void componentResized(ComponentEvent e)
    {
        updateScrollBars();
    }

    public void componentShown(ComponentEvent e)
    {
    }

    public void mouseWheelMoved(MouseWheelEvent e)
    {
        // Without this, focus ends up in the location bar textfield if you use
        // the mouse wheel in the edit window after using the openFile
        // completion list to open a file (Blackdown 1.4.1-01).
        // See also mouseMoveDotToPoint(MouseEvent).
        setFocusToDisplay();

        if (e.getWheelRotation() < 0)
            display.windowUp(5);
        else
            display.windowDown(5);
    }

    public void ensureActive()
    {
        if (!frame.isActive()) {
            for (int i = 0; i < getFrameCount(); i++) {
                Frame f = getFrame(i);
                if (f.isActive()) {
                    f.dispatchEvent(new WindowEvent(f, WindowEvent.WINDOW_DEACTIVATED));
                    break;
                }
            }
            frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_ACTIVATED));
        }
    }

    public void quit()
    {
        maybeExit();
    }

    public void saveAllExit()
    {
        tagFileManager.setEnabled(false);
        saveAll();
        maybeExit(); // May never return.
        tagFileManager.setEnabled(true);
    }

    private void maybeExit()
    {
        int numModifiedBuffers = 0;

        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            if (it.nextBuffer().isModified())
                ++numModifiedBuffers;
        }

        if (numModifiedBuffers > 0) {
            FastStringBuffer sb = new FastStringBuffer("Really exit with ");
            sb.append(numModifiedBuffers);
            sb.append(" modifed buffer");
            if (numModifiedBuffers > 1)
                sb.append('s');
            sb.append('?');
            if  (!confirm("Really exit?", sb.toString()))
                return;
        }

        setWaitCursor();

        saveState();
        RecentFiles.getInstance().save();

        // Delete all autosave files.
        for (BufferIterator iter = new BufferIterator(); iter.hasNext();)
            iter.nextBuffer().deleteAutosaveFile();

        Autosave.deleteCatalogFile();

        // Call dispose on all buffers.
        for (BufferIterator it = new BufferIterator(); it.hasNext();)
            it.nextBuffer().dispose();

        // Clean up temporary directory.
        Directories.cleanTempDirectory();

        // Clean up cache  directory.
        Cache.cleanup();

        Server.stopServer();
        pendingOperations.run();
        setDefaultCursor();
        System.exit(0);
    }

    public void killFrame()
    {
        if (getFrameCount() == 1) {
            // Does not return if OK to exit.
            maybeExit();
        } else {
            // Move frame being closed to end of list.
            if (indexOf(frame) != getFrameCount() - 1) {
                frames.remove(frame);
                frames.add(frame);
            }
            sessionProperties.saveWindowPlacement();
            frames.remove(frame);
            frame.dispose();

            Editor ed = frame.getSecondaryEditor();
            if (ed != null)
                removeEditor(ed);
            removeEditor(this);
            setCurrentEditor(getEditor(0));
        }
    }

    // See if we have the requested file in a buffer. If not, and if the file
    // actually exists, make a new buffer for it.
    public static Buffer getBuffer(File file)
    {
        if (file == null)
            return null;
        Buffer buf = bufferList.findBuffer(file);
        if (buf != null)
            return buf;
        if (file.isRemote())
            return Buffer.createBuffer(file);
        if (file.isDirectory())
            return new Directory(file);
        if (file.isFile()) {
            if (!file.canRead()) {
                MessageDialog.showMessageDialog("File is not readable",
                    "Error");
                return null;
            }
            return Buffer.createBuffer(file);
        }
        if (file.exists()) {
            // The file exists, but it's neither a directory nor a "normal"
            // file. This can occur on Linux if an SMB mount goes south.

            // Not a very informative error message, but this is what bash says
            // in the SMB mount case.
            currentEditor.status("I/O error");
        }
        return null;
    }

    public void nextBuffer()
    {
        Buffer buf = bufferList.getNextPrimaryBuffer(buffer);
        if (buf == null)
            return;
        if (buf.isPaired()) {
            Buffer secondary = buf.getSecondary();
            if (secondary != null) {
                if (secondary.getLastActivated() > buf.getLastActivated())
                    buf = secondary;
            }
        }
        if (buf != buffer)
            switchToBuffer(buf);
    }

    public void prevBuffer()
    {
        Buffer buf = bufferList.getPreviousPrimaryBuffer(buffer);
        if (buf == null)
            return;
        if (buf.isPaired()) {
            Buffer secondary = buf.getSecondary();
            if (secondary != null) {
                if (secondary.getLastActivated() > buf.getLastActivated())
                    buf = secondary;
            }
        }
        if (buf != buffer)
            switchToBuffer(buf);
    }

    public void switchToBuffer(Buffer buf)
    {
        if (buf != null) {
            if (!buf.isPaired() && (buffer == null || !buffer.isPaired())) {
                // This is the easy case. Both the buffer we're switching in
                // and the buffer we're switching out are unpaired.
                activate(buf);
            } else {
                // We're either switching in a paired buffer or switching out
                // a paired buffer (or both). Delegate to our frame, since we
                // may end up closing this editor.
                frame.switchToBuffer(buf);
            }
            Sidebar sidebar = getSidebar();
            if (sidebar != null)
                sidebar.setBuffer();
        } else
            Debug.bug();
    }

    public void makeNext(final Buffer buf)
    {
        bufferList.makeNext(buf, buffer);
    }

    public void newBuffer()
    {
        Buffer buf = new Buffer(0);
        makeNext(buf);
        switchToBuffer(buf);
    }

    public final void openFile()
    {
        AWTEvent e = dispatcher.getLastEvent();
        if (e != null && e.getSource() instanceof MenuItem) {
            Runnable r = new Runnable() {
                public void run()
                {
                    setFocusToTextField();
                }
            };
            SwingUtilities.invokeLater(r);
        } else
            setFocusToTextField();
    }

    public void openFileInOtherWindow()
    {
      saveView();
      boolean alreadySplit = (getOtherEditor() != null);
      if (!alreadySplit)
        splitWindow();
      final Editor ed = getOtherEditor();
      if (ed.getLocationBar() != null)
        {
          Runnable r = new Runnable()
            {
              public void run()
              {
                frame.setFocus(ed.getLocationBar().getTextField());
              }
            };
          SwingUtilities.invokeLater(r);
          setCurrentEditor(ed);
          if (alreadySplit)
            {
              // Current editor has changed.
              repaint();
              ed.repaint();
            }
        }
    }

    public Buffer openFile(File file)
    {
        Buffer buf = getBuffer(file);
        if (buf != null) {
            Debug.assertTrue(bufferList.contains(buf));
            return buf;
        }
        if (file.isRemote())
            return null;
        // File is local.
        if (!file.exists()) {
            if (confirm("Create file?",
                        file.canonicalPath() + " does not exist. Create?"))
                return Buffer.createBuffer(file);
        }
        return null;
    }

    public Buffer openFiles(List list)
    {
        if (list == null)
            return null;
        final int listSize = list.size();
        if (listSize < 2)
            return null;
        Buffer toBeActivated = null;
        // First string is directory.
        String dirname = (String) list.get(0);
        File directory = File.getInstance(dirname);
        History openFileHistory = new History("openFile.file");
        int lineNumber = -1;
        for (int i = 1; i < listSize; i++) {
            String s = (String) list.get(i);
            if (s == null || s.length() == 0)
                continue;
            if (s.charAt(0) == '+') {
                try {
                    lineNumber = Integer.parseInt(s.substring(1)) - 1;
                }
                catch (NumberFormatException e) {}
                continue;
            }
            // Aliases.
            String value = getAlias(s);
            if (value != null)
                s = value;
            if (s.startsWith("pop://") || s.startsWith("{")) {
                MailboxURL url = MailboxURL.parse(s);
                if (url != null) {
                    Buffer buf = MailCommands.getMailboxBuffer(this, url);
                    if (buf != null) {
                        makeNext(buf);
                        toBeActivated = buf;
                    }
                }
                continue;
            }
            File file = File.getInstance(directory, s);
            if (file == null) {
                MessageDialog.showMessageDialog(this, "Invalid path ".concat(s),
                    "Invalid Path");
                continue;
            }
            if (Utilities.isFilenameAbsolute(s) || s.startsWith("./") || s.startsWith(".\\"))
                ; // No tricks.
            else if (!file.exists()) {
                // Look in source and include paths as appropriate.
                File f = Utilities.findFile(this, s);
                if (f != null)
                    file = f;
                else {
                    // Not found in source or include path.
                    if (s.startsWith("www."))
                        file = File.getInstance("http://".concat(s));
                    else if (s.startsWith("ftp."))
                        file = File.getInstance("ftp://".concat(s));
                }
            }
            if (file.isLocal() && !file.exists()) {
                if (!Utilities.checkParentDirectory(file, "Open File"))
                    continue;
            }
            Buffer buf = openFile(file);
            if (buf != null) {
                Debug.assertTrue(bufferList.contains(buf));
                openFileHistory.append(file.netPath());
                if (lineNumber >= 0) {
                    // Line number was specified on command line.
                    if (buf.isLoaded()) {
                        if (buf == buffer) {
                            // Current buffer.
                            Line line = buffer.getLine(lineNumber);
                            if (line == null) {
                                if (mark != null)
                                    unmark();
                                updateDotLine();
                                dot.moveTo(getEob());
                                updateDotLine();
                                moveCaretToDotCol();
                            } else {
                                addUndo(SimpleEdit.MOVE);
                                if (mark != null)
                                    unmark();
                                updateDotLine();
                                dot.moveTo(line, 0);
                                updateDotLine();
                                moveCaretToDotCol();
                            }
                        } else {
                            // Not current buffer.
                            Buffer oldBuffer = buffer;
                            if (buffer != null)
                                saveView();
                            buffer = buf;
                            findOrCreateView(buffer);
                            restoreView();
                            addUndo(SimpleEdit.MOVE);
                            if (mark != null)
                                unmark();
                            Line line = buffer.getLine(lineNumber);
                            if (line == null) {
                                line = buffer.getFirstLine();
                                while (line.next() != null)
                                    line = line.next();
                                dot.moveTo(line, line.length());
                            } else
                                dot.moveTo(line, 0);
                            saveView();
                            View view = (View) views.get(buffer);
                            if (view != null) {
                                view.shift = 0;
                                view.caretCol = getDotCol();
                            }
                            buffer = oldBuffer;
                            if (buffer != null)
                                restoreView();
                        }
                    } else {
                        // Not yet loaded.
                        View view = findOrCreateView(buf);
                        view.lineNumber = lineNumber;
                        view.offs = 0;
                    }
                }
                Debug.assertTrue(bufferList.contains(buf));
                makeNext(buf);
                Debug.assertTrue(bufferList.contains(buf));
                toBeActivated = buf;
            }
        }
        openFileHistory.save();
        return toBeActivated;
    }

    public void unmark()
    {
        if (mark != null) {
            setMark(null);
            display.setUpdateFlag(REPAINT); // BUG! Not always necessary!
        }
    }

    public void cancelBackgroundProcess()
    {
        BackgroundProcess backgroundProcess = buffer.getBackgroundProcess();
        if (backgroundProcess != null)
            backgroundProcess.cancel();
    }

    // Calls buffer.setMark(null), then returns after doing exactly one thing.
    public void escape()
    {
        buffer.setMark(null); // keyboard-quit

        // Cancel background process (if any).
        BackgroundProcess backgroundProcess = buffer.getBackgroundProcess();
        if (backgroundProcess != null) {
            backgroundProcess.cancel();
            return;
        }

        if (popup != null) {
            if (popup.isVisible()) {
                killPopup();
                return;
            }
            popup = null;
            // We haven't really done anything yet. Fall through...
        }

        if (lastCommand == COMMAND_EXPAND) {
            Expansion expansion = Expansion.getLastExpansion();
            if (expansion != null) {
                expansion.undo(this);
                return;
            }
        }

        if (buffer instanceof RemoteBuffer && buffer.isEmpty()) {
            killBuffer();
            return;
        }

        if (escapeInternal())
            return;

        if (selection != null && selection.getSavedDot() != null)
            moveDotTo(selection.getSavedDot());
        else if (mark != null)
            moveDotTo(mark);
    }

    public boolean escapeInternal()
    {
        if (buffer instanceof CompilationBuffer || buffer.isTransient()) {
            if (buffer.unsplitOnClose()) {
                buffer.windowClosing();
                otherWindow();
                unsplitWindow();
            }
            maybeKillBuffer(buffer);
            restoreFocus();
            Sidebar.refreshSidebarInAllFrames();
            return true;
        }
        if (buffer.getModeId() == CHECKIN_MODE) {
            otherWindow();
            unsplitWindow();
            if (!buffer.isModified())
                maybeKillBuffer(buffer);
            restoreFocus();
            return true;
        }
        // Check for transient buffer in other editor in current frame.
        Editor ed = getOtherEditor();
        if (ed != null) {
            Buffer buf = ed.getBuffer();
            if (buf instanceof CompilationBuffer || buf.isTransient()) {
                if (buf.unsplitOnClose())
                    unsplitWindow();
                maybeKillBuffer(buf);
                if (!buf.unsplitOnClose())
                    ed.updateDisplay();
                Sidebar.refreshSidebarInAllFrames();
                return true;
            }
            if (buf.getModeId() == CHECKIN_MODE) {
                unsplitWindow();
                if (!buf.isModified())
                    maybeKillBuffer(buf);
                return true;
            }
        }
        return false;
    }

    public void tempBufferQuit()
    {
        if (buffer instanceof CompilationBuffer || buffer.isTransient()) {
            if (buffer.unsplitOnClose()) {
                buffer.windowClosing();
                otherWindow();
                unsplitWindow();
            }
            maybeKillBuffer(buffer);
            restoreFocus();
            Sidebar.refreshSidebarInAllFrames();
            return;
        }
    }

    public void stamp()
    {
        if (!checkReadOnly())
            return;
        Date now = new Date(System.currentTimeMillis());
        String dateString = null;
        String stampFormat = buffer.getStringProperty(Property.STAMP_FORMAT);
        if (stampFormat != null) {
            try {
                SimpleDateFormat df = new SimpleDateFormat(stampFormat);
                dateString = df.format(now);
            }
            catch (Throwable t) {
                // Fall through...
            }
        }
        if (dateString == null) {
            SimpleDateFormat df = new SimpleDateFormat("MMM d yyyy h:mm a");
            dateString = df.format(now);
        }
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = beginCompoundEdit();
            if (mark != null)
                delete();
            fillToCaret();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal(dateString);
            buffer.modified();
            endCompoundEdit(compoundEdit);
            moveCaretToDotCol();
            updateInAllEditors(getDotLine());
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public Search getSearchAtDot()
    {
        if (dot == null)
            return null;
        String pattern = null;
        boolean wholeWordsOnly = false;
        if (mark != null) {
            // No action if there's a multi-line selection.
            if (getMarkLine() == getDotLine())
                pattern = (new Region(buffer, dot, mark)).toString();
        } else {
            pattern = tokenAt(dot);
            wholeWordsOnly = true;
        }
        if (pattern != null && pattern.length() != 0)
            return new Search(pattern, false, wholeWordsOnly);
        else
            return null;
    }

    // Assumes dot is on first char of found pattern.
    public void markFoundPattern(Search search)
    {
        if (search.isRegularExpression() && search.isMultilinePattern()) {
            REMatch match = search.getMatch();
            if (match != null) {
                setDot(buffer.getPosition(match.getStartIndex()));
                setMark(buffer.getPosition(match.getEndIndex()));
                final Line markLine = getMarkLine();
                for (Line line = getDotLine(); line != null; line = line.next()) {
                    update(line);
                    if (line == markLine)
                        break;
                }
                moveCaretToDotCol();
            }
        } else {
            final int context = 2; // This could be a preference.
            Position saved = dot.copy();

            // Move dot to end of found pattern.
            int length;
            if (search.getMatch() != null)
                length = search.getMatch().toString().length();
            else
                length = search.getPatternLength();

            // Found pattern might go beyond end of line.
            dot.setOffset(Math.min(dot.getOffset() + length, getDotLine().length()));

            // Set mark at end of pattern.
            moveCaretToDotCol();
            setMarkAtDot();

            // Make sure end of pattern is actually visible, with additional
            // context as appropriate.
            int absCol = getDotCol() + context;
            display.ensureColumnVisible(getDotLine(), absCol);

            // Restore dot to original position at start of pattern.
            dot = saved;

            // Make sure start of pattern is actually visible, with additional
            // context as appropriate.
            absCol = getDotCol() - context;
            if (absCol < 0)
                absCol = 0;
            display.ensureColumnVisible(getDotLine(), absCol);
            moveCaretToDotCol();
        }
    }

    public void findNext()
    {
        if (lastSearch != null) {
            Position start;
            if (mark != null) {
                Region r = new Region(this);
                start = new Position(r.getBegin());
            } else
                start = new Position(dot);
            if (!start.next())
                return;
            setWaitCursor();
            Position pos = lastSearch.find(buffer, start);
            setDefaultCursor();
            if (pos != null) {
                moveDotTo(pos);
                markFoundPattern(lastSearch);
                if (lastSearch instanceof FindInFiles) {
                    if (buffer.getFile() != null) {
                        ListOccurrencesInFiles buf =
                            ((FindInFiles)lastSearch).getOutputBuffer();
                        if (buf != null)
                            buf.follow(buffer.getFile(), getDotLine());
                    }
                }
                return;
            }
            if (lastSearch instanceof FindInFiles) {
                Editor ed = getOtherEditor();
                if (ed != null) {
                    ListOccurrencesInFiles buf =
                        ((FindInFiles)lastSearch).getOutputBuffer();
                    if (ed.getBuffer() == buf) {
                        buf.findNextOccurrence(ed);
                        return;
                    }
                }
            }
            lastSearch.notFound(this);
        }
    }

    public void findPrev()
    {
        if (lastSearch != null) {
            Position start;
            if (mark != null) {
                Region r = new Region(this);
                start = new Position(r.getBegin());
            } else
                start = new Position(dot);
            if (!start.prev())
                return;
            setWaitCursor();
            Position pos = lastSearch.reverseFind(buffer, start);
            setDefaultCursor();
            if (pos != null) {
                moveDotTo(pos);
                markFoundPattern(lastSearch);
                if (lastSearch instanceof FindInFiles) {
                    if (buffer.getFile() != null) {
                        ListOccurrencesInFiles buf =
                            ((FindInFiles)lastSearch).getOutputBuffer();
                        if (buf != null)
                            buf.follow(buffer.getFile(), getDotLine());
                    }
                }
                return;
            }
            if (lastSearch instanceof FindInFiles) {
                Editor ed = getOtherEditor();
                if (ed != null) {
                    ListOccurrencesInFiles buf =
                        ((FindInFiles)lastSearch).getOutputBuffer();
                    if (ed.getBuffer() == buf) {
                        buf.findPreviousOccurrence(ed);
                        return;
                    }
                }
            }
            lastSearch.notFound(this);
        }
    }

    public void incrementalFind()
    {
      if (dot == null)
        return;

        // Use location bar.
      if (locationBar != null)
        {
          locationBar.setLabelText(LocationBar.PROMPT_PATTERN);
          HistoryTextField textField = locationBar.getTextField();
          textField.setHandler(new IncrementalFindTextFieldHandler(this, textField));
          textField.setHistory(new History("incrementalFind.pattern"));
          textField.setText("");
          setFocusToTextField();
        }
    }

    public String getCurrentText()
    {
        String s = getSelectionOnCurrentLine();
        if (s == null)
            s = getTokenAtDot();
        return (s != null && s.length() > 0) ? s : null;
    }

    public String getSelectionOnCurrentLine()
    {
        if (dot !=  null && mark != null && getMarkLine() == getDotLine())
            return new Region(this).toString();
        else
            return null;
    }

    public String getFilenameAtDot()
    {
        if (dot == null)
            return null;
        Position pos;
        if (mark != null) {
            Region r = new Region(this);

            // Trust the user if there's a highlighted selection on a single
            // line.
            if (r.getBeginLine() == r.getEndLine())
                return r.toString();

            // Otherwise, we want the beginning of the marked region.
            pos = r.getBegin();
        } else
            pos = new Position(dot);
        final Line line = pos.getLine();
        final int limit = line.length();
        int offset = pos.getOffset();
        if (offset == limit)
            --offset;
        FastStringBuffer sb = new FastStringBuffer();
        if (offset >= 0 && offset < limit) {
            char c = line.charAt(offset);
            if (Utilities.isFilenameChar(c)) {
                while (offset > 0) {
                    c = line.charAt(--offset);
                    if (!Utilities.isFilenameChar(c)){
                        ++offset;
                        break;
                    }
                }

                // Now we're looking at the first char of the filename.
                sb.append(line.charAt(offset));
                while (++offset < limit) {
                    c = line.charAt(offset);
                    if (Utilities.isFilenameChar(c)) {
                        sb.append(c);
                    } else if (sb.toString().startsWith("http://")) {
                        // Be more permissive since there may be an appended
                        // query.
                        if (!Character.isWhitespace(c))
                            sb.append(c);
                        else
                            break;
                    } else
                        break;
                }

                // Now we're looking at the first char past the end of the
                // filename. If the filename starts with "http://", make sure
                // it doesn't end with normal punctuation (as is often the
                // case with links embedded in text).
                int length = sb.length();
                while (length > 0) {
                    c = sb.charAt(length-1);
                    if (".,:;)]>".indexOf(c) >= 0)
                        --length;
                    else
                        break;
                }
                sb.setLength(length);

                RE re = new UncheckedRE(" line [0-9]+");
                REMatch match = re.getMatch(line.getText().substring(offset));
                if (match != null && match.getStartIndex() == 0)
                    sb.append(match.toString());
            }
        }
        return sb.toString();
    }

    private String getTokenAtDot()
    {
        // If a selection is marked, return the token at the beginning of the
        // marked region.
        if (mark != null) {
            Region r = new Region(this);
            return tokenAt(r.getBegin());
        }
        return tokenAt(dot);
    }

    private String tokenAt(Position pos)
    {
        return getMode().getIdentifier(pos);
    }

    public void findNextWord()
    {
        if (dot == null)
            return;
        String pattern = getTokenAtDot();
        if (pattern == null || pattern.length() == 0)
            return;
        lastSearch = new Search(pattern, false, true);
        Position start;
        if (mark != null && dot.isBefore(mark))
            start = new Position(mark);
        else
            start = new Position(dot);
        Position pos = lastSearch.find(buffer.getMode(), start);
        if (pos != null && pos.equals(start)) {
            if (pos.next())
                pos = lastSearch.find(buffer.getMode(), pos);
        }
        if (pos != null && !pos.equals(start)) {
            moveDotTo(pos);
            markFoundPattern(lastSearch);
        } else
            lastSearch.notFound(this);
    }

    public void findPrevWord()
    {
        if (dot == null)
            return;
        String pattern = getTokenAtDot();
        if (pattern == null || pattern.length() == 0)
            return;
        lastSearch = new Search(pattern, false, true);
        boolean found = false;
        Position start = null;
        if (mark != null)
            start = new Region(this).getBegin();
        else
            start = new Position(dot);
        if (start.prev()) {
            Position pos = lastSearch.reverseFind(buffer, start);
            if (pos != null && pos.getLine() == start.getLine()) {
                if (pos.getOffset() + lastSearch.getPatternLength() > start.getOffset()) {
                    // We've found the instance we started with. Keep looking.
                    start = new Position(pos);
                    if (start.prev())
                        pos = lastSearch.reverseFind(buffer, start);
                    else
                        pos = null;
                }
            }
            if (pos != null) {
                found = true;
                moveDotTo(pos);
                markFoundPattern(lastSearch);
            }
        }
        if (!found)
            lastSearch.notFound(this);
    }

    public void findFirstOccurrence()
    {
        if (dot == null)
            return;
        String pattern = getTokenAtDot();
        if (pattern == null || pattern.length() == 0)
            return;
        lastSearch = new Search(pattern, false, true);
        Position pos = lastSearch.find(buffer.getMode(),
                                       new Position(buffer.getFirstLine(), 0));
        if (pos != null) {
            moveDotTo(pos);
            markFoundPattern(lastSearch);
        } else
            lastSearch.notFound(this);
    }

    public void copyPath()
    {
        if (buffer instanceof Directory) {
            String path = ((Directory) buffer).getPathAtDot();
            if (path != null) {
                killRing.appendNew(path);
                killRing.copyLastKillToSystemClipboard();
                status("Path copied to clipboard");
            }
        }
    }

    public void copyRegion()
    {
        if (dot == null)
            return;
        String message = null;
        if (mark != null) {
            Region r = new Region(this);
            if (isColumnSelection()) {
                killedColumn = r.toString();
                message = "Column selection stored";
            } else {
                killRing.appendNew(r.toString());
                message = "Region copied to clipboard";
            }
        } else if (buffer.getMark() != null) {
            Region r = new Region(buffer, dot, buffer.getMark());
            killRing.appendNew(r.toString());
            message = "Region copied to clipboard";
        } else if (!getDotLine().isBlank()) {
            killRing.appendNew(getDotLine().getText() + System.getProperty("line.separator"));
            message = "Line copied to clipboard";
        } else
            return; // Nothing to do.
        if (!isColumnSelection())
            killRing.copyLastKillToSystemClipboard();
        if (message != null)
            status(message);
    }

    public void copyAppend()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }

        if (dot == null)
            return;

        String message = null;

        if (mark != null) {
            Region r = new Region(buffer, mark, dot);
            killRing.appendToCurrent(r.toString());
            message = "Region appended to clipboard";
        } else if (!getDotLine().isBlank()) {
            killRing.appendToCurrent(getDotLine().getText() + System.getProperty("line.separator"));
            message = "Line appended to clipboard";
        } else
            return; // Nothing to do.

        killRing.copyLastKillToSystemClipboard();

        if (message != null)
            status(message);
    }

    // Handles undo, updates display and marks buffer modified.
    public void deleteRegion()
    {
        if (mark == null)
            return;
        if (getMarkLine() != getDotLine() || getMarkOffset() != getDotOffset()) {
            try {
                buffer.lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                Region r = new Region(this);
                if (isColumnSelection()) {
                    deleteColumn(r);
                } else {
                    // A hard update is only necessary if the region spans a
                    // line boundary.
                    boolean hard = getDotLine() != getMarkLine();

                    // Save undo information before calling r.delete() so
                    // the modified flag will be correct if we revert.
                    CompoundEdit compoundEdit = beginCompoundEdit();
                    addUndo(SimpleEdit.MOVE);
                    dot.moveTo(r.getBegin());
                    addUndoDeleteRegion(r);

                    // Sets buffer modified flag.
                    r.delete();

                    endCompoundEdit(compoundEdit);

                    if (hard)
                        buffer.repaint();
                    else
                        updateInAllEditors(getDotLine());
                }
            }
            finally {
                buffer.unlockWrite();
            }
            moveCaretToDotCol();
        }
        setMark(null);
    }

    // Leaves dot at beginning of deleted region.
    private void deleteColumn(Region r)
    {
        Debug.assertTrue(r.isColumnRegion());
        final int beginCol = r.getBeginCol();
        final int endCol = r.getEndCol();
        CompoundEdit compoundEdit = beginCompoundEdit();
        addUndo(SimpleEdit.MOVE);
        dot.moveTo(r.getBegin());
        while (true) {
            addUndo(SimpleEdit.LINE_EDIT);
            Line line = getDotLine();
            deleteLineRegion(line, beginCol, endCol);
            updateInAllEditors(line);
            if (line == r.getEndLine())
                break;
            if (line.next() == null)
                break;
            dot.moveTo(line.next(), 0);
        }
        addUndo(SimpleEdit.MOVE);
        dot.moveTo(r.getBegin());
        endCompoundEdit(compoundEdit);
        buffer.modified();
    }

    private void deleteLineRegion(Line line, int beginCol, int endCol)
    {
        String text = Utilities.detab(line.getText(), buffer.getTabWidth());
        if (text.length() < beginCol)
            return; // No change.
        String head = text.substring(0, beginCol);
        if (text.length() < endCol) {
            line.setText(head);
            return;
        }
        String tail = text.substring(endCol);
        line.setText(head.concat(tail));
    }

    // This really is a kill!
    public void killRegion()
    {
        if (!checkReadOnly())
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            killRegionInternal();
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private void killRegionInternal()
    {
        if (mark == null)
            mark = buffer.getMark();
        if (mark != null) {
            if (getMarkLine() != getDotLine() || getMarkOffset() != getDotOffset()) {
                // A hard update is only necessary if the region spans a line
                // boundary.
                boolean hard = getDotLine() != getMarkLine();
                if (isColumnSelection()) {
                    Region r = new Region(this);
                    killedColumn = r.toString();
                    deleteColumn(r);
                } else {
                    Region r = new Region(this);
                    String kill = r.toString();
                    if (lastCommand == COMMAND_KILL)
                        killRing.appendToCurrent(kill);
                    else
                        killRing.appendNew(kill);
                    killRing.copyLastKillToSystemClipboard();

                    // Save undo information before calling Region.delete so
                    // modified flag will be correct if we revert.
                    CompoundEdit compoundEdit = beginCompoundEdit();
                    addUndo(SimpleEdit.MOVE);
                    dot.moveTo(r.getBegin());
                    addUndoDeleteRegion(r);

                    // Sets buffer modified flag.
                    r.delete();

                    endCompoundEdit(compoundEdit);
                }
                moveCaretToDotCol();
                if (hard)
                    buffer.repaint();
                else
                    updateInAllEditors(getDotLine());
                setCurrentCommand(COMMAND_KILL);
            }
            setMark(null);
        } else {
            // No selection.  Use current line.
            final Line dotLine = getDotLine();
            final Line nextLine = dotLine.next();

            // Last line is a special case.
            if (nextLine == null) {
                CompoundEdit compoundEdit = beginCompoundEdit();
                dot.setOffset(0);
                killLine();
                endCompoundEdit(compoundEdit);
                setCurrentCommand(COMMAND_KILL);
                return;
            }

            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            dot.moveTo(dotLine, 0);
            mark = new Position(nextLine, 0);

            Region r = new Region(this);
            String kill = r.toString();
            if (lastCommand == COMMAND_KILL)
                killRing.appendToCurrent(kill);
            else
                killRing.appendNew(kill);
            killRing.copyLastKillToSystemClipboard();

            // Save undo information before calling Region.delete so
            // modified flag will be correct if we revert.
            addUndo(SimpleEdit.MOVE);
            dot.moveTo(r.getBegin());
            addUndoDeleteRegion(r);

            // Sets buffer modified flag.
            r.delete();

            addUndo(SimpleEdit.MOVE);
            mark = null;
            endCompoundEdit(compoundEdit);
            moveCaretToDotCol();
            buffer.repaint();
            setCurrentCommand(COMMAND_KILL);
        }
    }

    public void killAppend()
    {
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        setLastCommand(COMMAND_KILL); // Force append.
        killRegion();
    }

    // Copies text from dot to end of line to kill ring and then deletes that
    // text. If dot is already at end of line, deletes newline and copies it
    // to kill ring.
    public void killLine()
    {
        if (!checkReadOnly())
            return;

        if (dot.getOffset() == dot.getLineLength() && dot.getNextLine() == null)
            return;

        CompoundEdit compoundEdit = beginCompoundEdit();

        addUndo(SimpleEdit.MOVE);
        unmark();

        if (getDotOffset() < getDotLine().length()){
            setMarkAtDot();
            if (dot.getLine().isBlank() && dot.getNextLine() != null)
                dot.moveTo(dot.getNextLine(), 0);
            else
                dot.setOffset(dot.getLineLength());
        } else if (dot.getOffset() == dot.getLineLength()) {
            fillToCaret(); // We might be beyond the end of the actual text on the line.
            setMarkAtDot();
            dot.moveTo(dot.getNextLine(), 0);
        }

        killRegion();

        endCompoundEdit(compoundEdit);
    }

    public void deleteWordRight()
    {
        deleteOrKillWordRight(false);
    }

    public void killWordRight()
    {
        deleteOrKillWordRight(true);
    }

    private void deleteOrKillWordRight(boolean isKill)
    {
        if (!checkReadOnly())
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        addUndo(SimpleEdit.MOVE);
        unmark();
        fillToCaret();
        setMarkAtDot();
        if (inWord()) {
            while (inWord() && nextChar())
                ;
            while (inWhitespace() && nextChar())
                ;
        } else if (inWhitespace()) {
            while (inWhitespace() && nextChar())
                ;
        } else {
            while (!inWhitespace() && !inWord() && nextChar())
                ;
            while (inWhitespace() && nextChar())
                ;
        }
        if (isKill)
            killRegion();
        else
            deleteRegion();
        endCompoundEdit(compoundEdit);
    }

    public void deleteWordLeft()
    {
        deleteOrKillWordLeft(false);
    }

    public void killWordLeft()
    {
        deleteOrKillWordLeft(true);
    }

    private void deleteOrKillWordLeft(boolean isKill)
    {
        if (!checkReadOnly())
            return;
        if (getDotOffset() == 0 && getDotLine().previous() == null)
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        addUndo(SimpleEdit.MOVE);
        unmark();
        setMarkAtDot();
        prevChar();
        if (inWord()) {
            while (getDotOffset() > 0 && inWord() && prevChar())
                ;
            if (!inWord())
                nextChar();
        } else if (inWhitespace()) {
            while (inWhitespace() && prevChar())
                ;
            if (!inWhitespace())
                nextChar();
        } else {
            while (!inWhitespace() && !inWord() && prevChar())
                ;
            while (inWhitespace() && prevChar())
                ;
            nextChar();
        }
        if (isKill)
            killRegion();
        else
            deleteRegion();
        endCompoundEdit(compoundEdit);
    }

    public boolean canPaste()
    {
        if (buffer.isReadOnly())
            return false;
        if (killRing.size() > 0)
            return true;
        String toBeInserted = null;
        Transferable t = getToolkit().getSystemClipboard().getContents(this);
        if (t != null) {
            try {
                toBeInserted = (String) t.getTransferData(DataFlavor.stringFlavor);
            }
            catch (Exception e) {}
        }
        return toBeInserted != null;
    }

    public void paste()
    {
        if (!checkReadOnly())
            return;
        setWaitCursor();
        String toBeInserted = null;
        Transferable t = getToolkit().getSystemClipboard().getContents(this);
        if (t != null) {
            try {
                toBeInserted = (String) t.getTransferData(DataFlavor.stringFlavor);
            }
            catch (Exception e) {}
        }
        if (toBeInserted != null && toBeInserted.length() > 0)
            killRing.appendNew(toBeInserted);
        // Even if we already have the text to be inserted, we MUST call
        // killRing.pop() here so that killRing.indexOfNextPop and
        // killRing.lastPaste are set correctly.
        toBeInserted = killRing.pop();
        if (toBeInserted != null) {
            paste(toBeInserted);
            setCurrentCommand(COMMAND_PASTE);
        }
        setDefaultCursor();
    }

    public void cyclePaste()
    {
        if (lastCommand == COMMAND_PASTE) {
            setWaitCursor();
            String s = killRing.popNext();
            if (s != null) {
                undo();
                paste(s);
                setCurrentCommand(COMMAND_PASTE);
            }
            setDefaultCursor();
        } else
            paste();
    }

    public void mousePaste()
    {
        if (dot == null)
            return;
        if (!checkReadOnly())
            return;
        if (isColumnSelection()) {
            notSupportedForColumnSelections();
            return;
        }
        AWTEvent e = dispatcher.getLastEvent();
        if (!(e instanceof MouseEvent))
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark != null) {
            Region r = new Region(this);
            killRing.appendNew(r.toString());
            killRing.copyLastKillToSystemClipboard();
            addUndo(SimpleEdit.MOVE);
            setMark(null);
        }
        mouseMoveDotToPoint((MouseEvent) e);
        paste();
        endCompoundEdit(compoundEdit);
    }

    public static void promoteLastPaste()
    {
        killRing.promoteLastPaste();
    }

    public void paste(String toBeInserted)
    {
        paste(toBeInserted, false);
    }

    public void paste(String toBeInserted, boolean leavePasteSelected)
    {
        if (!checkReadOnly())
            return;
        if (toBeInserted == null || toBeInserted.length() == 0)
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            pasteInternal(toBeInserted, leavePasteSelected);
        }
        finally {
            buffer.unlockWrite();
        }
        setUpdateFlag(REFRAME);
    }

    private void pasteInternal(String toBeInserted, boolean leavePasteSelected)
    {
        final Mode mode = buffer.getMode();
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark == null && Utilities.isLinePaste(toBeInserted) &&
            mode.acceptsLinePaste(this) && buffer.getBooleanProperty(Property.AUTO_PASTE_LINES))
        {
            // We want to the caret to be in the same column when we're done.
            final int absCaretCol = display.getAbsoluteCaretCol();

            final Line prevLine = getDotLine().previous();

            addUndo(SimpleEdit.MOVE);
            dot.setOffset(0);
            Position begin = dot.copy();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal(toBeInserted);

            if (prevLine != null && mode.canIndentPaste()) {
                // Indent inserted lines according to context.

                // Make sure line flags are correct.
                if (getFormatter().parseBuffer())
                    buffer.repaint();

                // Dot is at the beginning of the line following the inserted
                // block.
                Position savedDot = dot.copy();

                // First move dot to start of inserted block.
                addUndo(SimpleEdit.MOVE);
                dot.moveTo(prevLine.next(), 0);

                while (dot.getLine() != null && dot.getLine() != savedDot.getLine()) {
                    if (!dot.getLine().isBlank())
                        indentLineInternal();
                    addUndo(SimpleEdit.MOVE);
                    dot.moveTo(dot.getNextLine(), 0);
                }

                // Restore dot.
                dot = savedDot;
            }

            if (leavePasteSelected) {
                setMark(begin);
                final Line dotLine = getDotLine();
                for (Line line = begin.getLine(); line != null; line = line.nextVisible()) {
                    update(line);
                    if (line == dotLine)
                        break;
                }
            } else {
                // Restore caret column.
                addUndo(SimpleEdit.MOVE);
                display.setCaretCol(absCaretCol - display.getShift());
                moveDotToCaretCol();
            }
        } else {
            if (mark != null)
                deleteRegion();
            fillToCaret();
            Position begin = dot.copy();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal(toBeInserted);
            moveCaretToDotCol();
            if (leavePasteSelected) {
                setMark(begin);
                final Line dotLine = getDotLine();
                for (Line line = begin.getLine(); line != null; line = line.nextVisible()) {
                    update(line);
                    if (line == dotLine)
                        break;
                }
            }
        }
        endCompoundEdit(compoundEdit);
        buffer.modified();
        if (getFormatter().parseBuffer())
            buffer.repaint();
    }

    public void pasteColumn()
    {
        if (!checkReadOnly())
            return;
        if (killedColumn == null || killedColumn.length() == 0)
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            pasteColumnInternal(killedColumn);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private void pasteColumnInternal(String toBeInserted)
    {
        Position pos = new Position(dot);
        final int col = display.getAbsoluteCaretCol();
        CompoundEdit compoundEdit = beginCompoundEdit();
        while (true) {
            final int index = toBeInserted.indexOf('\n');
            final String s = index >= 0 ? toBeInserted.substring(0, index) : toBeInserted;
            if (index >= 0)
                toBeInserted = toBeInserted.substring(index + 1);
            final Line dotLine = getDotLine();
            String text = Utilities.detab(dotLine.getText(), buffer.getTabWidth());
            if (text.length() < col)
                text = text.concat(Utilities.spaces(col - text.length()));
            Debug.assertTrue(text.length() >= col);
            final String head = text.substring(0, col);
            final String tail = text.substring(col);
            addUndo(SimpleEdit.LINE_EDIT);
            FastStringBuffer sb = new FastStringBuffer(head);
            sb.append(s);
            sb.append(tail);
            dotLine.setText(sb.toString());
            updateInAllEditors(dotLine);
            pos = new Position(dotLine, head.length() + s.length());
            if (toBeInserted.length() == 0)
                break;
            if (dotLine.next() == null) {
                addUndo(SimpleEdit.MOVE);
                dot.setOffset(dotLine.length());
                addUndo(SimpleEdit.INSERT_LINE_SEP);
                buffer.insertLineSeparator(dot);
            } else {
                addUndo(SimpleEdit.MOVE);
                dot.moveTo(dotLine.next(), 0);
            }
        }
        addUndo(SimpleEdit.MOVE);
        dot.moveTo(pos);
        moveCaretToDotCol();
        endCompoundEdit(compoundEdit);
    }

    public void insertString(String toBeInserted)
    {
        if (toBeInserted == null || toBeInserted.length() == 0)
            return;
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark != null)
            delete();
        fillToCaret();
        addUndo(SimpleEdit.INSERT_STRING);
        insertStringInternal(toBeInserted);
        updateInAllEditors(dot.getLine());
        moveCaretToDotCol();
        endCompoundEdit(compoundEdit);
        if (getFormatter().parseBuffer())
            buffer.repaint();
    }

    public void centerDialog(JDialog d)
    {
        Dimension parent = frame.getSize();
        Dimension window = d.getSize();
        Point p = frame.getLocation();
        p.translate((parent.width - window.width) / 2,
            (parent.height - window.height) / 2);
        d.setLocation(p);
    }

    public boolean confirm(String title, String text)
    {
        int response = ConfirmDialog.showConfirmDialog(this, text, title);
        repaintNow();
        return response == RESPONSE_YES;
    }

    public int confirmAll(String title, String text)
    {
        int response = ConfirmDialog.showConfirmAllDialog(this, text, title);
        repaintNow();
        return response;
    }

    public void killBuffer()
    {
        try {
            if (buffer.isSecondary()) {
                buffer.windowClosing();
                otherWindow();
                unsplitWindow();
                currentEditor.maybeKillBuffer(buffer);
                restoreFocus();
                return;
            }
            Buffer buf = buffer.getSecondary();
            if (buf != null) {
                unsplitWindow();
                maybeKillBuffer(buf);
                return;
            }
            // Normal buffer.
            maybeKillBuffer(buffer);
            // If we're left with two editors showing exactly the same thing,
            // unsplit the window.
            Frame frame = currentEditor.getFrame();
            if (frame.getEditorCount() == 2) {
                Editor p = frame.getPrimaryEditor();
                Editor s = frame.getSecondaryEditor();
                boolean unsplit = false;
                if (p.getDot() != null && p.getDot().equals(s.getDot())) {
                    if (p.getMark() == null && s.getMark() == null)
                        unsplit = true;
                    else if (p.getMark() != null && p.getMark().equals(s.getMark()))
                        unsplit = true;
                }
                if (unsplit)
                    unsplitWindow();
            }
        }
        finally {
            Sidebar.refreshSidebarInAllFrames();
        }
    }

    public void maybeKillBuffer(Buffer toBeKilled)
    {
        if (!bufferList.contains(toBeKilled)) {
            Debug.bug("maybeKillBuffer buffer not in list " + toBeKilled);
            return;
        }

        // Don't kill the last buffer if it's a directory.
        if (bufferList.size() == 1 && toBeKilled instanceof Directory)
            return;

        // Cancel background process if any.
        BackgroundProcess backgroundProcess = toBeKilled.getBackgroundProcess();
        if (backgroundProcess != null) {
            Log.debug("maybeKillBuffer calling backgroundProcess.cancel...");
            backgroundProcess.cancel();
            // backgroundProcess.cancel() may have killed the buffer, so
            // verify that it's still in the list.
            if (!bufferList.contains(toBeKilled)) {
                Log.debug("maybeKillBuffer buffer is no longer in list");
                return;
            }
        }

        Mode mode = toBeKilled.getMode();
        if (mode == null || mode.confirmClose(this, toBeKilled))
            toBeKilled.kill();
    }

    public void clearStatusText()
    {
        StatusBar statusBar = getStatusBar();
        if (statusBar != null) {
            statusBar.setText(null);
            statusBar.repaint();
        }
    }

    public void activate(Buffer buf)
    {
        if (buf == null)
            return;
        Debug.assertTrue(bufferList.contains(buf));
        if (buf == buffer)
            return;
        if (!buf.initialized())
            buf.initialize();
        clearStatusText();
        if (buffer != null && bufferList.contains(buffer)) {
            // Save information about buffer being deactivated.
            buffer.autosave();
            saveView();
            RecentFiles.getInstance().bufferDeactivated(buffer, dot);
        }

        // Read-only status may have changed. (We could be switching back from
        // a shell buffer.)
        reactivate(buf);

        buf.setLastActivated(System.currentTimeMillis());
        if (buf.isLoaded()) {
            buffer = buf;
            bufferActivated(false);
        } else {
            setWaitCursor();
            int result = LOAD_FAILED;
            try {
                result = buf.load();
            }
            catch (OutOfMemoryError e) {
                buf.kill();
                Sidebar.setUpdateFlagInAllFrames(SIDEBAR_ALL);
                MessageDialog.showMessageDialog(this,
                                                "Insufficient memory to load buffer",
                                                "Error");
                return;
            }
            switch (result) {
                case LOAD_COMPLETED:
                    buffer = buf;
                    bufferActivated(true);
                    break;
                case LOAD_PENDING:
                    buffer = buf;
                    buffer.setBusy(true);
                    bufferPending();
                    break;
                case LOAD_FAILED:
                    setDefaultCursor();
                    buffer = buf;
                    bufferActivated(true);
                    MessageDialog.showMessageDialog(this,
                                                    "Unable to load buffer",
                                                    "Error");
                    break;
                default:
                    Debug.assertTrue(false);
            }
        }
    }

    public Editor activateInOtherWindow(Buffer buf)
    {
        return frame.activateInOtherWindow(this, buf);
    }

    public Editor activateInOtherWindow(Buffer buf, float split)
    {
        return frame.activateInOtherWindow(this, buf, split);
    }

    public Editor displayInOtherWindow(Buffer buf)
    {
        return frame.displayInOtherWindow(this, buf);
    }

    public void bufferActivated(boolean firstTime)
    {
        if (buffer.getModeId() == IMAGE_MODE) {
            setDot(null);
            setMark(null);
            display.setTopLine(null);
            display.setShift(0);
            display.setCaretCol(0);
        } else {
            findOrCreateView(buffer);
            restoreView();

            // If the buffer has already been loaded, the caret position will
            // be restored correctly.
            if (firstTime)
                moveCaretToDotCol();
        }

        if (dot != null && dot.getOffset() > dot.getLineLength()) {
            dot.setOffset(dot.getLineLength());
            moveCaretToDotCol();
        }

        frame.updateTitle();
        frame.setMenu();
        frame.setToolbar();

        if (buffer.isBusy())
            setWaitCursor();
        else
            setDefaultCursor();

        setUpdateFlag(REFRAME);
        reframe();
        setUpdateFlag(REPAINT);

        RecentFiles.getInstance().bufferActivated(buffer);

        if (buffer.isTaggable()) {
            tagFileManager.addToQueue(buffer.getCurrentDirectory(),
                buffer.getMode());
        }

        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_ALL);

        if (isLispInitialized()) {
            if (firstTime)
                LispAPI.invokeOpenFileHook(buffer);
            LispAPI.invokeBufferActivatedHook(buffer);
        }
    }

    private void bufferPending()
    {
        // Find or create a view of this buffer.
        findOrCreateView(buffer);
        restoreView();

        frame.updateTitle();
        frame.setMenu();
        frame.setToolbar();

        display.repaint();

        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_ALL);
        Sidebar sidebar = getSidebar();
        if (sidebar != null)
            sidebar.setBuffer();
    }

    // Find or create another frame in which to activate the specified buffer.
    public Editor activateInOtherFrame(Buffer buf)
    {
        Editor ed = null;
        if (getEditorCount() == 1) {
            ed = createNewFrame();
            ed.activate(buf);
            ed.getFrame().setVisible(true);
            ed.updateDisplay();
        } else {
            for (int i = 0; i < getEditorCount(); i++) {
                ed = getEditor(i);
                if (ed != this) {
                    ed.activate(buf);
                    ed.getFrame().toFront();
                    break;
                }
            }
        }
        return ed;
    }

    public void nextFrame()
    {
        int count = getEditorCount();
        if (count > 1) {
            Editor ed = null;
            for (int i = 0; i < count; i++) {
                ed = Editor.getEditor(i);
                if (ed == this) {
                    if (++i == count)
                        i = 0;
                    ed = Editor.getEditor(i);
                    ed.getFrame().toFront();
                    ed.requestFocusLater();
                    break;
                }
            }
        }
    }

    private void requestFocusLater()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                Editor.this.requestFocus();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void toggleSidebar()
    {
        frame.frameToggleSidebar();
    }

    public void sidebarListBuffers()
    {
        ensureActive();

        if (frame.getSidebar() == null)
            toggleSidebar();

        if (frame.getSidebar() != null)
            frame.getSidebar().activateBufferList();
    }

    public void sidebarListTags()
    {
        if (!frame.isActive())
            return;

        if (getMode().getSidebarComponent(this) != null) {
            if (frame.getSidebar() == null)
                toggleSidebar();
            if (frame.getSidebar() != null)
                frame.getSidebar().activateNavigationComponent();
        }
    }

    public void toggleToolbar()
    {
        frame.frameToggleToolbar();
    }

    public final boolean addUndo(int type)
    {
        return SimpleEdit.addUndo(this, type);
    }

    public final boolean addUndoDeleteRegion(Region r)
    {
        buffer.addEdit(new UndoDeleteRegion(this, r));
        return true;
    }

    public final CompoundEdit beginCompoundEdit()
    {
        return buffer.beginCompoundEdit();
    }

    public final void endCompoundEdit(CompoundEdit compoundEdit)
    {
        buffer.endCompoundEdit(compoundEdit);
    }

    public void undo()
    {
        setWaitCursor();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            buffer.undo();
            checkDotInOtherFrames();
            setCurrentCommand(COMMAND_UNDO);
        }
        catch (Throwable t) {
            Log.error(t);
        }
        finally {
            buffer.unlockWrite();
            setDefaultCursor();
        }
    }

    public void redo()
    {
        setWaitCursor();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            buffer.redo();
            checkDotInOtherFrames();
        }
        catch (Throwable t) {
            Log.error(t);
        }
        finally {
            buffer.unlockWrite();
            setDefaultCursor();
        }
    }

    private void checkDotInOtherFrames()
    {
        if (getEditorCount() > 1) {
            for (int i = 0; i < getEditorCount(); i++) {
                Editor ed = getEditor(i);
                if (ed != this && ed.getBuffer() == buffer) {
                    if (ed.getDotOffset() > ed.getDotLine().length()) {
                        ed.getDot().setOffset(ed.getDotLine().length());
                        ed.moveCaretToDotCol();
                        ed.updateDotLine();
                    }
                }
            }
        }
    }

    public final void jumpToLine(int lineNumber)
    {
        jumpToLine(lineNumber, 0);
    }

    public void jumpToLine(int lineNumber, int offset)
    {
        Line line = buffer.getLine(lineNumber);
        if (line != null) {
            if (offset < 0)
                offset = 0;
            else if (offset > line.length())
                offset = line.length();
            moveDotTo(line, offset);
            setUpdateFlag(REFRAME);
        } else
            eob();
    }

    public void offset()
    {
        status(String.valueOf(buffer.getAbsoluteOffset(dot)));
    }

    public void executeCommand()
    {
      // Use location bar.
      if (locationBar != null)
        {
          locationBar.setLabelText(LocationBar.PROMPT_COMMAND);
          HistoryTextField textField = locationBar.getTextField();
          textField.setHandler(new ExecuteCommandTextFieldHandler(this, textField));
          textField.setHistory(new History("executeCommand.input", 30));
          textField.recallLast();
          textField.selectAll();
          AWTEvent e = dispatcher.getLastEvent();
          if (e != null && e.getSource() instanceof MenuItem) {
            Runnable r = new Runnable() {
              public void run()
              {
                setFocusToTextField();
              }
            };
            SwingUtilities.invokeLater(r);
          } else
            setFocusToTextField();
        }
    }

    public void executeCommand(String input)
    {
        executeCommand(input, false);
    }

    public void executeCommand(String input, final boolean interactive)
    {
        input = Utilities.trimLeading(input);
        if (input.length() == 0)
            return;
        if (input.charAt(0) == '(') {
            // Lisp form.
            try {
                String result = Interpreter.evaluate(input).writeToString();
                if (interactive)
                    status(result);
            }
            catch (Throwable t) {
                String message = null;
                if (t instanceof ConditionThrowable) {
                    try {
                        LispObject obj = ((ConditionThrowable)t).getCondition();
                        if (obj instanceof Condition) {
                            try {
                                message = ((Condition)obj).getConditionReport();
                            }
                            catch (Throwable ignored) {
                                // At least we tried.
                            }
                        }
                    }
                    catch (Throwable ignored) {}
                }
                if (message == null || message.length() == 0)
                    message = t.getMessage();
                if (message != null && message.length() > 0) {
                    FastStringBuffer sb = new FastStringBuffer(message);
                    sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
                    message = sb.toString();
                } else
                    message = String.valueOf(t);
                MessageDialog.showMessageDialog(this, message, "Error");
            }
            return;
        }
        int index = input.indexOf('=');
        if (index >= 0) {
            String key = input.substring(0, index).trim();
            if (key.indexOf(' ') < 0 && key.indexOf('\t') < 0) {
                String value = input.substring(index+1).trim();
                setProperty(key, value);
                return;
            }
        }
        String[] array = parseCommand(input);
        if (array != null) {
            final String command = array[0];
            final String parameters = array[1];
            Runnable r = new Runnable() {
                public void run()
                {
                    try {
                        StatusBar statusBar = getStatusBar();
                        statusBar.setText("");
                        execute(command, parameters);
                        if (interactive && parameters == null) {
                            // Suggest key binding if one is available.
                            Object[] values = getKeyMapping(command);
                            Debug.assertTrue(values != null);
                            Debug.assertTrue(values.length == 2);
                            KeyMapping mapping = (KeyMapping) values[0];
                            Mode mode = (Mode) values[1];
                            if (mapping != null) {
                                String statusText = statusBar.getText();
                                boolean append =
                                    statusText != null && statusText.length() > 0;
                                FastStringBuffer sb = new FastStringBuffer();
                                if (append) {
                                    sb.append(statusText);
                                    sb.append("          ");
                                }
                                sb.append(command);
                                sb.append(" is mapped to ");
                                sb.append(mapping.getKeyText());
                                if (mode != null) {
                                    sb.append(" (");
                                    sb.append(mode);
                                    sb.append(" mode)");
                                } else
                                    sb.append(" (global mapping)");
                                status(sb.toString());
                            }
                        }
                    }
                    catch (NoSuchMethodException e) {
                        FastStringBuffer sb =
                            new FastStringBuffer("Unknown command \"");
                        sb.append(command);
                        sb.append('"');
                        MessageDialog.showMessageDialog(Editor.this,
                            sb.toString(), "Error");
                    }
                }
            };
            if (SwingUtilities.isEventDispatchThread()) {
                r.run();
            } else {
                try {
                    SwingUtilities.invokeAndWait(r);
                }
                catch (Throwable t) {
                    Log.debug(t);
                }
            }
        }
    }

    private static String[] parseCommand(String command)
    {
        command = Utilities.trimLeading(command);
        // Command name is terminated by whitespace or '('.
        char delimiter = '\0';
        int index = -1;
        int commandLength = command.length();
        for (int i = 0; i < commandLength; i++) {
            char c = command.charAt(i);
            if (c == '(' || Character.isWhitespace(c)) {
                delimiter = c;
                index = i;
                break;
            }
        }
        String methodName, parameters;
        if (index < 0) {
            methodName = command;
            parameters = null;
        } else {
            methodName = command.substring(0, index);
            parameters = Utilities.trimLeading(command.substring(index));
            if (delimiter != '(') {
                if (parameters.startsWith("("))
                    delimiter = '(';
            }
            if (delimiter == '(') {
                // Strip parens.
                int length = parameters.length();
                if (length < 2)
                    return null; // Error.
                if (parameters.charAt(length - 1) != ')')
                    return null; // Error.
                parameters = parameters.substring(1, length - 1).trim();
                length = parameters.length();
                if (length == 0)
                    parameters = null;
                else {
                    // Strip required quotes.
                    if (length < 2)
                        return null; // Error.
                    if (parameters.charAt(0) != '"' || parameters.charAt(length - 1) != '"')
                        return null; // Error.
                    parameters = parameters.substring(1, length - 1); // Done.
                }
            }
        }
        String[] array = new String[2];
        array[0] = methodName;
        array[1] = parameters;
        return array;
    }

    // Set a buffer-specific property.
    private void setProperty(String key, String value)
    {
        Property property = Property.findProperty(key);
        if (property == null) {
            MessageDialog.showMessageDialog(
                "Property \"" + key + "\" not found",
                "Error");
            return;
        }
        final boolean succeeded;
        if (value.length() == 0) {
            succeeded = buffer.removeProperty(property);
        } else {
            succeeded = buffer.setPropertyFromString(property, value);
            if (!succeeded)
                invalidPropertyValue(property, value);
        }
        if (succeeded)
            buffer.saveProperties();
    }

    // No error checking.
    public static void setGlobalProperty(String key, String value)
    {
        if (value == null || value.length() == 0)
            prefs.removeProperty(key);
        else
            prefs.setProperty(key, value);
    }

    private void invalidPropertyValue(Property property, String value)
    {
        if (property.isIntegerProperty())
            status("Invalid integer value \"" + value + "\"");
        else if (property.isBooleanProperty())
            status("Invalid boolean value \"" + value + "\"");
    }

    public void slideIn()
    {
        slide(buffer.getIndentSize());
    }

    public void slideOut()
    {
        slide(-buffer.getIndentSize());
    }

    private void slide(int amount)
    {
        if (!checkReadOnly())
            return;
        Region r = mark != null ? new Region(this) : null;
        if (r != null && (r.getBeginOffset() != 0 || r.getEndOffset() != 0))
            return; // If a block is marked, it must be a block of full lines.
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (r == null) {
                CompoundEdit compoundEdit = beginCompoundEdit();
                int dotCol = getDotCol();
                int oldIndent = buffer.getIndentation(getDotLine());
                int newIndent = oldIndent + amount;
                addUndo(SimpleEdit.LINE_EDIT);
                buffer.setIndentation(getDotLine(), newIndent);
                updateInAllEditors(getDotLine());
                if (dotCol < oldIndent) {
                    // Caret was originally in indentation area. Move caret to
                    // start of text. This ensures that the caret is on an
                    // actual character, in case the indentation got entabbed.
                    moveDotToCol(newIndent);
                } else {
                    // Move caret with text.
                    display.setCaretCol(display.getCaretCol() + amount);
                    moveDotToCaretCol();
                }
                endCompoundEdit(compoundEdit);
                buffer.modified();
            } else {
                // If a block is marked, it must be a block of full lines.
                CompoundEdit compoundEdit = beginCompoundEdit();
                Position saved = new Position(dot);
                Line line = r.getBeginLine();
                while (line != r.getEndLine()) {
                    addUndo(SimpleEdit.MOVE);
                    dot.moveTo(line, 0);
                    addUndo(SimpleEdit.LINE_EDIT);
                    buffer.setIndentation(getDotLine(),
                        buffer.getIndentation(getDotLine()) + amount);
                    updateInAllEditors(getDotLine());
                    line = line.next();
                }
                addUndo(SimpleEdit.MOVE);
                dot = saved;
                endCompoundEdit(compoundEdit);
                buffer.modified();
            }
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public void dirHome()
    {
        if (buffer instanceof Directory)
            ((Directory) buffer).home();
    }

    public void dirTagFile()
    {
        if (buffer instanceof Directory)
            ((Directory) buffer).tagFileAtDot();
    }

    public void dirBrowseFile()
    {
        if (buffer instanceof Directory && !buffer.getFile().isRemote()) {
            Directory d = (Directory) buffer;
            d.browseFileAtDot();
        }
    }

    public void dirDeleteFiles()
    {
        if (mark != null && getMarkLine() != getDotLine()) {
            MessageDialog.showMessageDialog(this,
                "This operation is not supported with multi-line text selections.",
                "Delete Files");
            return;
        }
        if (buffer instanceof Directory) {
            if (buffer.getFile() instanceof SshFile) {
                MessageDialog.showMessageDialog(this, "Deletions are not yet supported in ssh directory buffers.", "Error");
                return;
            }
            ((Directory)buffer).deleteFiles();
        }
    }

    public void dirCopyFile()
    {
        if (buffer instanceof Directory && buffer.getFile().isLocal())
            ((Directory) buffer).copyFileAtDot();
    }

    public void dirGetFile()
    {
        if (buffer instanceof Directory && buffer.getFile() instanceof FtpFile)
            ((Directory) buffer).getFileAtDot();
    }

    public void dirMoveFile()
    {
        if (buffer instanceof Directory && buffer.getFile().isLocal())
            ((Directory) buffer).moveFileAtDot();
    }

    public void dirRescan()
    {
        if (buffer instanceof Directory) {
            setWaitCursor();
            ((Directory) buffer).rescan();
            setDefaultCursor();
        }
    }

    public void dirHomeDir()
    {
        File homeDir = File.getInstance(Utilities.getUserHome());
        if (buffer instanceof Directory) {
            if (!buffer.getFile().equals(homeDir))
                ((Directory) buffer).changeDirectory(homeDir);
        } else {
            Buffer buf = getBuffer(homeDir);
            if (buf != null) {
                makeNext(buf);
                activate(buf);
            }
        }
    }

    public void dirUpDir()
    {
        if (buffer instanceof Directory)
            ((Directory) buffer).upDir();
    }

    public void setFocusToTextField()
    {
      if (locationBar != null)
        frame.setFocus(locationBar.getTextField());
    }

    public void wrapRegion()
    {
        if (!checkReadOnly())
            return;
        if (dot == null || mark == null)
            return;
        // Must be line block.
        if (dot.getOffset() != 0 || mark.getOffset() != 0)
            return;
        new WrapText(this).wrapRegion();
    }

    public void wrapParagraph()
    {
        if (!checkReadOnly())
            return;
        new WrapText(this).wrapParagraph();
    }

    public void unwrapParagraph()
    {
        if (!checkReadOnly())
            return;
        new WrapText(this).unwrapParagraph();
    }

    public void wrapParagraphsInRegion()
    {
        if (!checkReadOnly())
            return;
        new WrapText(this).wrapParagraphsInRegion();
    }

    public void visibleTabs()
    {
        tabsAreVisible = !tabsAreVisible;
        if (tabsAreVisible)
            status("Tabs are visible");
        else
            status("Tabs are not visible");
        for (int i = 0; i < getEditorCount(); i++) {
            Editor ed = getEditor(i);
            ed.getDisplay().repaint();
        }
    }

    public void insertBraces()
    {
        CompoundEdit compoundEdit = beginCompoundEdit();
        insertChar('{');
        indentLine();
        eol();
        newlineAndIndent();
        insertChar('}');
        indentLine();
        up();
        eol();
        newlineAndIndent();
        endCompoundEdit(compoundEdit);
    }

    public void insertParentheses()
    {
        if (!checkReadOnly())
            return;
        boolean parensRequireSpaces =
            buffer.getBooleanProperty(Property.PARENS_REQUIRE_SPACES);
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (mark != null) {
            Position begin, end;
            if (mark.isBefore(dot)) {
                begin = new Position(mark);
                end = new Position(dot);
            } else {
                begin = new Position(dot);
                end = new Position(mark);
            }
            addUndo(SimpleEdit.MOVE);
            setMark(null);
            dot.moveTo(end);
            if (parensRequireSpaces)
                insertChar(' ');
            insertChar(')');
            addUndo(SimpleEdit.MOVE);
            dot.moveTo(begin);
            insertChar('(');
            if (parensRequireSpaces)
                insertChar(' ');
        } else {
            fillToCaret();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal(parensRequireSpaces ? "(  )" : "()");
            addUndo(SimpleEdit.MOVE);
            dot.skip(parensRequireSpaces ? -2 : -1);
        }
        moveCaretToDotCol();
        endCompoundEdit(compoundEdit);
    }

    public void movePastCloseAndReindent()
    {
        Position pos = new Position(dot);
        int count = 1;
        if (pos.getChar() == ')') {
            count = 0;
        } else {
            while (pos.next()) {
                char c = pos.getChar();
                if (c == '(')
                    ++count;
                else if (c == ')')
                    --count;
                if (count == 0)
                    break;
            }
        }
        if (count == 0) {
            try {
                buffer.lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                CompoundEdit compoundEdit = beginCompoundEdit();
                addUndo(SimpleEdit.MOVE);
                unmark();
                dot.moveTo(pos);
                if (Utilities.isWhitespace(getDotLine().substring(0, getDotOffset()))) {
                    justOneSpace();
                    addUndo(SimpleEdit.MOVE);
                    dot.skip(-1);
                    deleteNormalChar();
                }
                addUndo(SimpleEdit.MOVE);
                dot.next();
                newlineAndIndent();
                endCompoundEdit(compoundEdit);
            }
            finally {
                buffer.unlockWrite();
            }
        }
    }

    public void electricQuote()
    {
        CompoundEdit compoundEdit = beginCompoundEdit();
        if (getDotChar() == '"') {
            addUndo(SimpleEdit.MOVE);
            dot.skip(1);
            newlineAndIndent();
        } else {
            fillToCaret();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal("\"\"");
            addUndo(SimpleEdit.MOVE);
            dot.skip(-1);
        }
        moveCaretToDotCol();
        endCompoundEdit(compoundEdit);
    }

    public void justOneSpace()
    {
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = beginCompoundEdit();
            addUndo(SimpleEdit.MOVE);
            unmark();
            while (inWhitespace() && nextChar())
                ;
            setMarkAtDot();
            while (prevChar()) {
                if (!inWhitespace()) {
                    nextChar();
                    break;
                }
            }

            deleteRegion();
            insertChar(' ');
            endCompoundEdit(compoundEdit);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public final void updateDotLine()
    {
        display.lineChanged(dot.getLine());
    }

    // Adds line to changed line list of current frame only.
    public final void update(Line line)
    {
        display.lineChanged(line);
    }

    /**
     * Adds line to changed line list of all editors in which the current
     * buffer is displayed.
     *
     * @param line      the line
     */
    public static void updateInAllEditors(Line line)
    {
        if (line != null) {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == currentEditor.getBuffer())
                    ed.getDisplay().lineChanged(line);
            }
        }
    }

    /**
     * Adds line to changed line list of all editors in which the specified
     * buffer is displayed.
     *
     * @param buffer    the buffer
     * @param line      the line
     */
    public static void updateInAllEditors(Buffer buffer, Line line)
    {
        if (line != null) {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == buffer)
                    ed.getDisplay().lineChanged(line);
            }
        }
    }

    public void updateScrollBars()
    {
        if (buffer == null)
            return; // Avoid NPE.
        if (!displayReady)
            return;
        updateVerticalScrollBar();
        updateHorizontalScrollBar();
    }

    boolean inScrollBarUpdate = false;

    public void updateVerticalScrollBar()
    {
        if (verticalScrollBar != null) {
            inScrollBarUpdate = true;
            int y;
            if (getTopLine() != null)
                y = buffer.getY(getTopLine()) + display.getPixelsAboveTopLine();
            else
                y = display.getPixelsAboveTopLine();
            verticalScrollBar.setValues(y, display.getHeight(), 0, buffer.getDisplayHeight());
            inScrollBarUpdate = false;
        }
    }

    public void updateHorizontalScrollBar()
    {
        if (horizontalScrollBar != null)
            horizontalScrollBar.setValues(display.getShift() * Display.getCharWidth(),
                display.getWidth(), 0, buffer.getDisplayWidth());
    }

    public void updateDisplay()
    {
        if (dot != null) {
            if (dot.isHidden()) {
                buffer.appendUndoFold(this);
                show(getDotLine());
            }
            reframe();
        }
        display.repaintChangedLines();
        updateScrollBars();
        Sidebar sidebar = getSidebar();
        if (sidebar != null)
            sidebar.setUpdateFlag(SIDEBAR_POSITION);
        frame.repaintStatusBar();
        if (buffer.isBusy())
            setWaitCursor();
        else
            setDefaultCursor();
    }

    public void updateDisplayLater()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                updateDisplay();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    // Update display of buf in all windows showing it.
    public static void updateDisplayLater(final Buffer buf)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                for (int i = 0; i < getEditorCount(); i++) {
                    Editor ed = getEditor(i);
                    if (ed.getBuffer() == buf)
                        ed.updateDisplay();
                }
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public final Line getTopLine()
    {
        return display.getTopLine();
    }

    public final void setTopLine(Line line)
    {
        display.setTopLine(line);
    }

    public final void setUpdateFlag(int mask)
    {
        display.setUpdateFlag(mask);
    }

    public final void reframe()
    {
        display.reframe();
    }

    public boolean checkReadOnly()
    {
        boolean readOnly = buffer.isReadOnly();
        if (readOnly && buffer.getBooleanProperty(Property.P4_AUTO_EDIT)) {
            if (buffer.getType() == Buffer.TYPE_NORMAL) {
                File file = buffer.getFile();
                if (file != null && file.isLocal() && file.isFile())
                    if (P4.autoEdit(this))
                        readOnly = buffer.isReadOnly();
            }
        }
        if (readOnly) {
            status("Buffer is read only");
            return false;
        }
        if (buffer.isLocked())
            return false;
        if (dot == null)
            return false;
        return true;
    }

    public static boolean checkExperimental()
    {
        return prefs.getBooleanProperty(Property.ENABLE_EXPERIMENTAL_FEATURES);
    }

    public void status(String s)
    {
        frame.setStatusText(s);
    }

    private static boolean displayReady;

    public static final boolean displayReady()
    {
        return displayReady;
    }

    public static final void setDisplayReady(boolean b)
    {
        displayReady = b;
    }

    private static final Cursor waitCursor =
        Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);

    public final void setWaitCursor()
    {
        display.setCursor(waitCursor);
    }

    public final void setDefaultCursor()
    {
        final Cursor cursor;
        if (displayReady && buffer != null)
            cursor = buffer.getDefaultCursor();
        else
            cursor = waitCursor;
        display.setCursor(cursor);
    }

    public final void setCursor(Cursor cursor)
    {
        display.setCursor(cursor);
    }

    public static void loadPreferences()
    {
        prefs.reload();
        debug = prefs.getBooleanProperty(Property.DEBUG);
    }

    private boolean insertingKeyText = false;

    public void insertKeyText()
    {
        if (!checkReadOnly())
            return;
        insertingKeyText = true; // The real work is done in handleKeyEvent.
    }

    private void insertKeyTextInternal(char keyChar, int keyCode, int modifiers)
    {
        Log.debug("keycode = 0x" + Integer.toString(keyCode, 16));
        Log.debug("modifiers = 0x" + Integer.toString(modifiers, 16));
        Log.debug("character = " + String.valueOf(keyChar));
        Log.debug("character = 0x" + Integer.toString((int) keyChar, 16));

        insertingKeyText = false;

        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            KeyMapping km;
            if (keyCode != 0)
                km = new KeyMapping(keyCode, modifiers, null);
            else
                km = new KeyMapping(keyChar, null);

            CompoundEdit compoundEdit = beginCompoundEdit();
            if (mark != null)
                delete();
            fillToCaret();
            addUndo(SimpleEdit.INSERT_STRING);
            insertStringInternal(km.toString());
            buffer.modified();
            moveCaretToDotCol();
            endCompoundEdit(compoundEdit);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public void whatChar()
    {
        if (dot.getOffset() < dot.getLineLength()) {
            char c = getDotChar();
            FastStringBuffer sb = new FastStringBuffer(Integer.toString(c));
            sb.append("  0x");
            sb.append(Integer.toHexString(c));
            if (c >= ' ' && c < 0x7f) {
                sb.append("  '");
                if (c == '\'')
                    sb.append('\\');
                sb.append(c);
                sb.append('\'');
            }
            status(sb.toString());
        }
    }

    public void jmips()
    {
        setWaitCursor();
        long loopsPerSecond = 0;
        long loops = 1;
        Thread thread = Thread.currentThread();
        int oldPriority = thread.getPriority();
        thread.setPriority(Thread.MAX_PRIORITY);
        do {
            long start = System.currentTimeMillis();
            for (long i = loops; i >= 0; i--)
                ;
            long elapsed = System.currentTimeMillis() - start;
            if (elapsed >= 1000) {
                loopsPerSecond = (loops / elapsed) * 1000;
                break;
            }
            loops *= 2;
        } while (true);
        thread.setPriority(oldPriority);
        String s = String.valueOf(((float) loopsPerSecond) / 500000);
        currentEditor.status(s);
        setDefaultCursor();
    }

    public void httpDeleteCookies()
    {
        Cookie.deleteCookies();
    }

    private static Class extensionClass = null;

    private static void loadExtensions()
    {
        String extension = prefs.getStringProperty(Property.EXTENSION);
        if (extension != null) {
            Log.debug("loading extension " + extension);
            try {
                ExtensionClassLoader loader = new ExtensionClassLoader();
                extensionClass = loader.loadClass(extension, true);
                if (extensionClass != null) {
                    Method method = extensionClass.getMethod("run", new Class[0]);
                    if (method != null)
                        method.invoke(extensionClass.newInstance(), new Class[0]);
                } else
                    Log.error("extension " + extension + " not found");
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    private static Class loadExtensionClass(String className)
    {
        Class c = null;
        try {
            ExtensionClassLoader loader = new ExtensionClassLoader();
            c = loader.loadClass(className, true);
        }
        catch (Exception e) {
            Log.error(e);
        }
        return c;
    }

    private static void runStartupScript()
    {
        File file =
            File.getInstance(Directories.getEditorDirectory(), "init.lisp");
        if (file != null && file.isFile()) {
            try {
                long start = System.currentTimeMillis();
                JLisp.runStartupScript(file);
                long elapsed = System.currentTimeMillis() - start;
                FastStringBuffer sb = new FastStringBuffer("loaded ");
                sb.append(file.canonicalPath());
                sb.append(" (");
                sb.append(elapsed);
                sb.append(" ms)");
                Log.info(sb.toString());
            }
            catch (Throwable t) {
                Log.error(t);
                Log.error("error loading " + file.canonicalPath());
            }
        }
    }

    public static void runLispCommand(String command)
    {
        try {
            JLisp.runLispCommand(command);
        }
        catch (Throwable t) {
            Log.error(t);
        }
    }

    private static boolean isLispInitialized;

    public static synchronized boolean isLispInitialized()
    {
        return isLispInitialized;
    }

    public static synchronized void setLispInitialized(boolean b)
    {
        isLispInitialized = b;
    }

    public static void invokeHook(String hook)
    {
        invokeHook(hook, null);
    }

    public static void invokeHook(String hook, String args)
    {
        FastStringBuffer sb = new FastStringBuffer("(invoke-hook '");
        sb.append(hook);
        if (args != null && args.length() > 0) {
            sb.append(' ');
            sb.append(args);
        }
        sb.append(')');
        runLispCommand(sb.toString());
    }

    public void mode()
    {
        String modeName =
            InputDialog.showInputDialog(this, "New mode:", "Change Mode");
        if (modeName != null) {
            modeName = modeName.trim();
            if (modeName.length() > 0) {
                repaintNow();
                mode(modeName);
            }
        }
    }

    public void mode(String modeName)
    {
        int modeId = getModeList().getModeIdFromModeName(modeName);
        if (modeId < 0) {
            MessageDialog.showMessageDialog("Unknown mode \"" + modeName + '"',
                "Error");
        } else if (modeId != buffer.getMode().getId()) {
            if (buffer.isModified() && modeId == BINARY_MODE ) {
                String prompt =
                    "Buffer will be reloaded in binary mode; discard changes?";
                if (!confirm("Change Mode", prompt))
                    return;
            }
            Mode mode = getModeList().getMode(modeId);
            if (mode != null) {
                setWaitCursor();
                buffer.changeMode(mode);
                buffer.saveProperties();
                setDefaultCursor();
            }
        }
    }

    public void defaultMode()
    {
        Mode mode = buffer.getDefaultMode();
        if (mode != null && mode != buffer.getMode()) {
            if (buffer.isModified()) {
                FastStringBuffer sb =
                    new FastStringBuffer("Buffer will be reloaded in ");
                sb.append(mode.toString());
                sb.append(" mode; discard changes?");
                if (!confirm("Change Mode", sb.toString()))
                    return;
            }
            setWaitCursor();
            buffer.changeMode(mode);
            setDefaultCursor();
        }
    }

    public void textMode()
    {
        if (buffer.getModeId() == BINARY_MODE) {
            if (buffer.isModified()) {
                String prompt =
                    "Buffer will be reloaded in text mode; discard changes?";
                if (!confirm("Change Mode", prompt))
                    return;
            }
            setWaitCursor();
            buffer.changeMode(modeList.getMode(PLAIN_TEXT_MODE));
            setDefaultCursor();
        }
    }

    public void splitWindow()
    {
        currentEditor.getFrame().splitWindow();
    }

    public void unsplitWindow()
    {
        IdleThread.killFollowContextTask();
        frame.unsplitWindow();
    }

    public void killWindow()
    {
        frame.unsplitWindowKeepOther();
        Sidebar sidebar = getSidebar();
        if (sidebar != null) {
            sidebar.setUpdateFlag(SIDEBAR_ALL);
            sidebar.refreshSidebar();
        }
    }

    public void otherWindow()
    {
        final Editor ed = frame.getOtherEditor();
        if (ed != null) {
            saveView();
            setCurrentEditor(ed);
            ed.getBuffer().setLastActivated(System.currentTimeMillis());
            ed.setFocusToDisplay();
            if (ed.getDot() != null) {
                ed.update(ed.getDotLine());
                ed.getDisplay().repaintChangedLines();
            }
            if (dot != null) {
                updateDotLine();
                display.repaintChangedLines();
            }
            frame.setMenu();
            frame.setToolbar();
            Sidebar sidebar = getSidebar();
            if (sidebar != null) {
                sidebar.setUpdateFlag(SIDEBAR_ALL);
                sidebar.refreshSidebar();
            }
        }
    }

    public void enlargeWindow()
    {
        frame.enlargeWindow(this, 1);
    }

    public void enlargeWindow(int n)
    {
        frame.enlargeWindow(this, n);
    }

    public void shrinkWindowIfLargerThanBuffer()
    {
        final Frame frame = getFrame();
        int n = getBuffer().getLineCount();
        if (n < getWindowHeight())
            frame.setWindowHeight(this, n);
    }

    public void fold()
    {
        if (dot == null)
            return;
        if (!foldRegionInternal() && !foldExplicit())
            foldNearLine(getDotLine());
    }

    public void foldRegion()
    {
        if (dot == null)
            return;
        foldRegionInternal();
    }

    private boolean foldRegionInternal()
    {
        if (mark == null)
            return false;
        if (dot.getLine() == mark.getLine())
            return false;
        if (dot.getOffset() > 0)
            return false;
        if (mark.getOffset() > 0)
            return false;
        Region r = new Region(buffer, mark, dot);
        Line begin = r.getBegin().getLine().next();
        Line end = r.getEnd().getLine();
        addUndo(SimpleEdit.FOLD);
        setMark(null);
        for (Line line = begin; line != end; line = line.next())
            line.hide();
        buffer.renumber();
        unhideDotInAllFrames(buffer);
        return true;
    }

    private boolean foldExplicit()
    {
        final Line dotLine = getDotLine();
        String text = dotLine.getText();
        Line begin = null;
        Line end = null;
        if (text.indexOf(EXPLICIT_FOLD_END) >= 0) {
            // Current line contains an end marker.
            int count = 1;
            end = dotLine.next();
            begin = dotLine.previous();
            while (begin != null) {
                text = begin.getText();
                if (text.indexOf(EXPLICIT_FOLD_START) >= 0) {
                    --count;
                    if (count == 0) {
                        begin = begin.next();
                        break;
                    }
                } else if (text.indexOf(EXPLICIT_FOLD_END) >= 0) {
                    ++count;
                }
                begin = begin.previous();
            }
        } else if (text.indexOf(EXPLICIT_FOLD_START) >= 0) {
            int count = 1;
            begin = dotLine.next();
            if (begin == null)
                return false;
            end = begin.next();
            while (end != null) {
                text = end.getText();
                if (text.indexOf(EXPLICIT_FOLD_START) >= 0) {
                    ++count;
                } else if (text.indexOf(EXPLICIT_FOLD_END) >= 0) {
                    --count;
                    if (count == 0) {
                        end = end.next();
                        break;
                    }
                }
                end = end.next();
            }
        }
        if (begin == null)
            return false;
        addUndo(SimpleEdit.FOLD);
        for (Line line = begin; line != end; line = line.next())
            line.hide();
        buffer.renumber();
        unhideDotInAllFrames(buffer);
        return true;
    }

    public void foldNearLine(Line line)
    {
        while (line != null && line.isBlank())
            line = line.previous();
        if (line == null)
            return;
        setWaitCursor();
        Line next = line.next();
        while (next != null && next.isBlank())
            next = next.next();
        if (next != null) {
            final String trim = line.trim();
            switch (getModeId()) {
                case JAVA_MODE:
                case JAVASCRIPT_MODE:
                case C_MODE:
                case CPP_MODE:
                case PERL_MODE:
                case PHP_MODE:
                    if (trim.endsWith("{")) {
                        if (!next.isHidden()) {
                            fold(next);
                            return;
                        }
                    }
                    if (trim.startsWith("}")) {
                        // We're at the end of a code block. Find the start of
                        // the block and fold from there.
                        Position end =
                            new Position(line, line.getText().indexOf('}'));
                        Position start = findMatchInternal(end, 0);
                        if (start != null) {
                            foldNearLine(start.getLine());
                            return;
                        }
                    }
                    if (next.trim().startsWith("}")) {
                        // Fold block containing current line.
                        Position end =
                            new Position(next, next.getText().indexOf('}'));
                        Position start = findMatchInternal(end, 0);
                        if (start != null) {
                            foldNearLine(start.getLine());
                            return;
                        }
                    } else if (next.trim().endsWith("{")) {
                        Line nextNext = next.next();
                        while (nextNext != null && nextNext.isBlank())
                            nextNext = nextNext.next();
                        if (nextNext != null && !nextNext.isHidden()) {
                            fold(nextNext);
                            return;
                        }
                    }
                    break;
                case XML_MODE: {
                    if (trim.startsWith("/>") || trim.startsWith("</")) {
                        Line prev = line.previous();
                        while (prev != null && prev.isBlank())
                            prev = prev.previous();
                        if (prev != null) {
                            int indent =
                                buffer.getCol(line, line.getIndentation());
                            int prevIndent =
                                buffer.getCol(prev, prev.getIndentation());
                            if (indent < prevIndent) {
                                fold(prev);
                                return;
                            }
                        }
                    } else if (trim.startsWith("<")) {
                        int indent =
                            buffer.getCol(line, line.getIndentation());
                        int nextIndent =
                            buffer.getCol(next, next.getIndentation());
                        if (indent < nextIndent) {
                            fold(next);
                            return;
                        }
                    }
                }
                default:
                    break;
            }
        }
        fold(line);
    }

    private void fold(Line target)
    {
        if (target == null)
            return;
        int indent = buffer.getCol(target, target.getIndentation());
        if (indent == 0)
            return;
        Line begin = target;
        while (true) {
            Line prev = begin.previous();
            if (prev == null)
                break;
            if (prev.isBlank() || getMode().isCommentLine(prev) ||
                isLabelLine(prev) || isPreprocessorLine(prev)) {
                begin = prev;
                continue;
            }
            if (buffer.getCol(prev, prev.getIndentation()) < indent)
                break;
            if (prev.getText().endsWith("{"))
                break;
            begin = prev;
        }
        Line end = target.next();
        while (end != null) {
            if (end.isBlank() || getMode().isCommentLine(end) ||
                isLabelLine(end) || isPreprocessorLine(end)) {
                end = end.next();
                continue;
            }
            if (buffer.getCol(end, end.getIndentation()) < indent)
                break;
            end = end.next();
        }
        addUndo(SimpleEdit.FOLD);
        for (Line line = begin; line != end; line = line.next())
            line.hide();
        buffer.renumber();
        unhideDotInAllFrames(buffer);
    }

    private static RE labelRE = new UncheckedRE("^\\s*\\w+:");

    private boolean isLabelLine(Line line)
    {
        Mode mode = getMode();
        if (mode instanceof JavaMode || mode instanceof PerlMode)
            return labelRE.getMatch(line.getText()) != null;
        return false;
    }

    private boolean isPreprocessorLine(Line line)
    {
        if (getMode() instanceof CMode)
            if (line.trim().startsWith("#"))
                return true;

        return false;
    }

    // BUG! This method does more than its name suggests...
    public static void unhideDotInAllFrames(Buffer buffer)
    {
        for (int i = 0; i < Editor.getEditorCount(); i++) {
            Editor ed = Editor.getEditor(i);
            if (ed.getBuffer() == buffer) {
                // Make sure dot is visible.
                if (ed.getDot().isHidden()) {
                    ed.setMark(null);
                    Line line = ed.getDotLine().previousVisible();
                    if (line != null) {
                        ed.setDot(line, 0);
                        ed.moveCaretToDotCol();
                    }
                }
                ed.setUpdateFlag(REFRAME);
                ed.reframe();
                ed.getDisplay().repaint();
            }
        }
    }

    public void unfold()
    {
        if (dot == null)
            return;
        Line dotLine = getDotLine();
        Line begin = null;
        if (dotLine.isHidden()) {
            begin = dotLine;
            while (begin.previous() != null && begin.previous().isHidden())
                begin = begin.previous();
        } else {
            // Look for next fold.
            begin = dotLine.next();
            while (begin != null && !begin.isHidden())
                begin = begin.next();
        }
        if (begin == null)
            return;
        if (!begin.isHidden())
            return;
        Line end = begin;
        while (true) {
            Line line = end.next();
            if (line == null)
                break;
            if (!line.isHidden())
                break;
            end = line;
        }
        if (begin != null && end != null) {
            addUndo(SimpleEdit.FOLD);
            for (Line line = begin; line != end.next(); line = line.next())
                line.unhide();
            buffer.renumber();
            for (int i = 0; i < Editor.getEditorCount(); i++) {
                Editor ed = Editor.getEditor(i);
                if (ed.getBuffer() == buffer)
                    ed.getDisplay().repaint();
            }
        }
    }

    public void unfold(Line line)
    {
        if (line == null)
            return;
        if (!line.isHidden())
            return;
        Line begin = line;
        while (begin.previous() != null && begin.previous().isHidden())
            begin = begin.previous();
        Line end = line.next();
        while (end != null && end.isHidden())
            end = end.next();
        addUndo(SimpleEdit.FOLD);
        for (Line l = begin; l != end; l = l.next())
            l.unhide();
        buffer.renumber();
        for (int i = 0; i < Editor.getEditorCount(); i++) {
            Editor ed = Editor.getEditor(i);
            if (ed.getBuffer() == buffer)
                ed.getDisplay().repaint();
        }
    }

    private void show(Line target)
    {
        if (target == null)
            return;
        if (!target.isHidden())
            return;
        Line begin = target;
        while (begin.previous() != null && begin.previous().isHidden())
            begin = begin.previous();
        Line end = target.next();
        while (end != null && end.isHidden())
            end = end.next();
        for (Line line = begin; line != end; line = line.next())
            line.show();
        buffer.renumber();
        for (int i = 0; i < Editor.getEditorCount(); i++) {
            Editor ed = Editor.getEditor(i);
            if (ed.getBuffer() == buffer)
                ed.getDisplay().repaint();
        }
    }

    public void unfoldAll()
    {
        addUndo(SimpleEdit.FOLD);
        for (Line line = buffer.getFirstLine(); line != null; line = line.next())
            line.show();
        buffer.renumber();
        for (int i = 0; i < Editor.getEditorCount(); i++) {
            Editor ed = Editor.getEditor(i);
            if (ed.getBuffer() == buffer)
                ed.getDisplay().repaint();
        }
    }

    public void foldMethods()
    {
        Mode mode = getMode();
        if (mode instanceof JavaMode || mode instanceof PerlMode) {
            setWaitCursor();
            List tags = buffer.getTags();
            if (tags != null) {
                addUndo(SimpleEdit.FOLD);
                for (Line line = buffer.getFirstLine(); line != null; line = line.next())
                    line.show();
                for (int i = 0; i < tags.size(); i++) {
                    LocalTag tag = (LocalTag) tags.get(i);
                    if (tag.getType() == TAG_METHOD)
                        foldMethod(tag.getLine());
                }
                unhideDotInAllFrames(buffer);
            }
        }
    }

    private final void foldMethod(Line line)
    {
        foldOrUnfoldMethod(line, true);
    }

    public final void unfoldMethod(Line line)
    {
        foldOrUnfoldMethod(line, false);
    }

    private void foldOrUnfoldMethod(Line line, boolean fold)
    {
        Mode mode = getMode();
        while (line != null) {
            String s = line.trim();
            if (mode instanceof PerlMode)
                s = PerlMode.trimSyntacticWhitespace(s);
            else if (mode instanceof JavaMode)
                s = JavaMode.trimSyntacticWhitespace(s);
            if (s.endsWith("{"))
                break;
            line = line.next();
        }
        if (line == null)
            return;
        if (line.next() == null)
            return; // Nothing to fold.
        Position start = new Position(line, line.length());
        Line begin = line.next();
        final SyntaxIterator it = mode.getSyntaxIterator(start);
        Position match = null;
        int count = 1;
        while (true) {
            char c = it.nextChar();
            if (c == SyntaxIterator.DONE)
                break;
            if (c == '{')
                ++count;
            else if (c == '}')
                --count;
            if (count == 0) {
                // Found it!
                match = it.getPosition();
                break;
            }
        }
        if (match == null)
            return;
        Line end = match.getLine();
        if (fold) {
            for (Line toBeHidden = begin; toBeHidden != end && toBeHidden != null; toBeHidden = toBeHidden.next())
                toBeHidden.hide();
        } else {
            for (Line toBeShown = begin; toBeShown != end && toBeShown != null; toBeShown = toBeShown.next())
                toBeShown.show();
        }
        buffer.needsRenumbering = true;
    }

    private static Aliases aliases;

    private static final Aliases getAliases()
    {
        if (aliases == null)
            aliases = new Aliases();
        return aliases;
    }

    public static final File getAliasesFile()
    {
        return aliases != null ? aliases.getFile() : null;
    }

    public static final void reloadAliases()
    {
        if (aliases != null)
            aliases.reload();
    }

    public final String getAlias(String alias)
    {
        return getAliases().get(alias);
    }

    public final void setAlias(String alias, String value)
    {
        getAliases().setAlias(alias, value);
    }

    public final void setAliasForBuffer(String alias, Buffer buf)
    {
        getAliases().setAliasForBuffer(alias, buf);
    }

    public final void removeAlias(String alias)
    {
        getAliases().remove(alias);
    }

    public void setEncoding()
    {
        File file = buffer.getFile();
        if (file != null) {
            InputDialog d =
                new InputDialog(this, "Encoding:", "Set Encoding",
                    buffer.getSaveEncoding());
            d.setHistory(new History("setEncoding"));
            centerDialog(d);
            d.show();
            String encoding = d.getInput();
            if (encoding != null)
                setEncoding(encoding);
        }
    }

    public void setEncoding(String encoding)
    {
        File file = buffer.getFile();
        if (file != null) {
            if (Utilities.isSupportedEncoding(encoding)) {
                file.setEncoding(encoding);
                buffer.saveProperties();
            } else {
                FastStringBuffer sb =
                    new FastStringBuffer("Unsupported encoding \"");
                sb.append(encoding);
                sb.append('"');
                MessageDialog.showMessageDialog(this, sb.toString(), "Error");
            }
        }
    }

    private static final ArrayList protectList = new ArrayList();

    // Add reference to global list to protect obj from garbage collection.
    public static synchronized void protect(Object obj)
    {
        protectList.add(obj);
    }

    private static String sessionName;

    public static String getSessionName()
    {
        return sessionName;
    }

    public static void setSessionName(String name)
    {
        sessionName = name;
        // The session name is displayed in the frame's title bar.
        for (int i = 0; i < getFrameCount(); i++) {
            Frame frame = getFrame(i);
            if (frame != null)
                frame.titleChanged();
        }
    }

    // Position stack.
    private static Stack positionStack = new Stack();

    public static List getPositionStack()
    {
        return positionStack;
    }

    public void pushPosition()
    {
        pushMarker(new Marker(buffer, dot));
        status("Position saved");
    }

    public void popPosition()
    {
        if (positionStack.empty()) {
            status("Position stack is empty");
        } else {
            Marker m = (Marker) positionStack.pop();
            if (m != null)
                m.gotoMarker(this);
        }
    }

    public static void pushMarker(Marker m)
    {
        while (positionStack.size() >= 30)
            positionStack.removeElementAt(0);
        positionStack.push(m);
    }

    public static void resetDisplay()
    {
        if (!displayReady())
            return;
        // Force formatters to be re-initialized.
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf.getFormatter() != null)
                buf.getFormatter().reset();
        }
        Display.initializeStaticValues();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Display display = it.nextEditor().getDisplay();
            display.initialize();
            display.repaint();
        }
        Editor.restoreFocus();
    }
}
