/*
 * Frame.java
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

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowStateListener;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

public final class Frame extends JFrame implements Constants, ComponentListener,
    FocusListener, WindowListener, WindowStateListener
{
    private Editor[] editors = new Editor[2];
    private Editor currentEditor;
    private ToolBar toolbar;
    private boolean showToolbar;
    private AdjustPlacementRunnable adjustPlacementRunnable;
    private Rectangle rect;
    private int extendedState;
    private final StatusBar statusBar;

    public Frame(Editor editor)
    {
        Editor.frames.add(this);
        addComponentListener(this);
        addWindowListener(this);
        addFocusListener(this);
        addWindowStateListener(this);
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        statusBar = new StatusBar(this);
        getContentPane().add(statusBar, "South");
        final SessionProperties sessionProperties =
            Editor.getSessionProperties();
        showToolbar = sessionProperties.getShowToolbar(this);
        currentEditor = editors[0] = editor;
        if (sessionProperties.getShowSidebar(this)) {
            sidebar = new Sidebar(this);
            sidebarSplitPane = createSidebarSplitPane();
            getContentPane().add(sidebarSplitPane, "Center");
        } else
            getContentPane().add(editors[0], "Center");
        titleChanged();
    }

    public void titleChanged()
    {
        FastStringBuffer sb =
            new FastStringBuffer(Version.getShortVersionString());
        String sessionName = Editor.getSessionName();
        if (sessionName != null) {
            sb.append(" [");
            sb.append(sessionName);
            sb.append(']');
        }
        if (Editor.isDebugEnabled()) {
            sb.append("     Java ");
            sb.append(System.getProperty("java.version"));
            sb.append(' ');
            sb.append(System.getProperty("java.vendor"));
        }
        setTitle(sb.toString());
    }

    public void storeExtendedState(int state)
    {
        extendedState = state;
    }

    public int retrieveExtendedState()
    {
        return extendedState;
    }

    public Rectangle getRect()
    {
        return rect;
    }

    protected void processEvent(java.awt.AWTEvent e)
    {
        if (!(e instanceof KeyEvent))
            super.processEvent(e);
    }

    public int getEditorCount()
    {
        int count = 0;
        if (editors[0] != null)
            ++count;
        if (editors[1] != null)
            ++count;
        return count;
    }

    public Editor getEditor()
    {
        return editors[0];
    }

    public final Editor getCurrentEditor()
    {
        return currentEditor;
    }

    // May return null.
    public final Editor getOtherEditor()
    {
        return currentEditor == editors[0] ? editors[1] : editors[0];
    }

    public final void setCurrentEditor(Editor editor)
    {
        Debug.assertTrue(editor != null);
        Debug.assertTrue(editor == editors[0] || editor == editors[1]);
        currentEditor = editor;
    }

    public final Editor getPrimaryEditor()
    {
        return editors[0];
    }

    public final Editor getSecondaryEditor()
    {
        return editors[1];
    }

    public final boolean isPrimaryEditor(Editor ed)
    {
        return ed != null && ed == editors[0];
    }

    public final boolean contains(Editor ed)
    {
        return ed != null && (ed == editors[0] || ed == editors[1]);
    }

    public void updateTitle()
    {
        if (editors[0] != null)
            editors[0].updateLocation();
        if (editors[1] != null)
            editors[1].updateLocation();
    }

    private Sidebar sidebar;

    public final Sidebar getSidebar()
    {
        return sidebar;
    }

    private SplitPane sidebarSplitPane;

    public final SplitPane getSidebarSplitPane()
    {
        return sidebarSplitPane;
    }

    private SplitPane createSidebarSplitPane()
    {
        SplitPane splitPane =
            new SplitPane(SplitPane.HORIZONTAL_SPLIT,
                sidebar, getEditorPane());
        int dividerLocation =
            Editor.getSessionProperties().getSidebarWidth(this);
        splitPane.setDividerLocation(dividerLocation);
        splitPane.setBorder(null);
        return splitPane;
    }

    private void addSidebar()
    {
        if (sidebarSplitPane != null)
            getContentPane().remove(sidebarSplitPane);
        sidebar = new Sidebar(this);
        sidebarSplitPane = createSidebarSplitPane();
        getContentPane().add(sidebarSplitPane, "Center");
        validate();
        currentEditor.setFocusToDisplay();
        sidebar.setUpdateFlag(SIDEBAR_ALL);
    }

    private JComponent editorPane;

    public final JComponent getEditorPane()
    {
        if (editorPane != null)
            return editorPane;
        else
            return editors[0];
    }

    public void frameToggleSidebar()
    {
        if (sidebar == null) {
            // Add sidebar.
            getContentPane().remove(getEditorPane());
            addSidebar();
        } else {
            // Save state before removing sidebar.
            Editor.getSessionProperties().saveSidebarState(this);
            // Remove sidebar.
            getContentPane().remove(sidebarSplitPane);
            sidebarSplitPane = null;
            sidebar = null;
            getContentPane().add(getEditorPane(), "Center");
        }
        validate();
        editors[0].updateScrollBars();
        if (editors[1] != null)
            editors[1].updateScrollBars();
        currentEditor.setFocusToDisplay();
    }

    public final StatusBar getStatusBar()
    {
        return statusBar;
    }

    public void repaintStatusBar()
    {
        if (statusBar != null)
            statusBar.repaint();
    }

    public void setStatusText(String text)
    {
        if (statusBar != null && text != null && !text.equals(statusBar.getText())) {
            statusBar.setText(text);
            statusBar.repaintNow();
        }
    }

    public boolean getShowToolbar()
    {
        return showToolbar;
    }

    public void setToolbar()
    {
        if (showToolbar && ToolBar.isToolBarEnabled()) {
            // We want a toolbar.
            ToolBar tb = currentEditor.getMode().getToolBar(this);
            if (tb != toolbar) {
                if (toolbar != null) {
                    getContentPane().remove(toolbar);
                    toolbar = null;
                }
                if (tb != null) {
                    getContentPane().add(toolbar = tb, "North");
                    toolbar.repaint();
                }
                getContentPane().validate();
            }
        } else {
            // We don't want a toolbar.
            if (toolbar != null) {
                getContentPane().remove(toolbar);
                getContentPane().validate();
            }
        }
    }

    public void frameToggleToolbar()
    {
        showToolbar = !showToolbar;
        if (toolbar != null) {
            if (!showToolbar) {
                getContentPane().remove(toolbar);
                toolbar = null;
                getContentPane().validate();
            }
        } else {
            if (showToolbar && ToolBar.isToolBarEnabled()) {
                ToolBar tb = currentEditor.getMode().getToolBar(this);
                if (tb != null) {
                    getContentPane().add(toolbar = tb, "North");
                    toolbar.repaint();
                    getContentPane().validate();
                }
            }
        }
        // Save new state.
        Editor.getSessionProperties().setShowToolbar(this, showToolbar);
    }

    private ToolBar defaultToolBar;

    public ToolBar getDefaultToolBar()
    {
        if (defaultToolBar == null)
            defaultToolBar = new DefaultToolBar(this);

        return defaultToolBar;
    }

    public void addToolbar(ToolBar tb)
    {
        Debug.assertTrue(toolbar == null);
        if (tb != null) {
            toolbar = tb;
            getContentPane().add(toolbar, "North");
            toolbar.repaint();
        }
        // Make sure toolbar doesn't steal focus.
        Runnable r = new Runnable() {
            public void run()
            {
                JComponent c = getFocusedComponent();
                if (c != null)
                    c.requestFocus();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void maybeAddToolbar()
    {
        if (toolbar != null)
            return;
        ToolBar tb = currentEditor.getMode().getToolBar(this);
        if (tb != null)
            addToolbar(tb);
    }

    public void removeToolbar()
    {
        if (toolbar != null) {
            getContentPane().remove(toolbar);
            toolbar = null;
            getContentPane().validate();
        }
    }

    public void setMenu()
    {
        final Mode mode = currentEditor.getMode();
        final MenuBar oldMenuBar = (MenuBar) getJMenuBar();
        if (oldMenuBar == null ||
            Platform.isPlatformMacOSX() ||
            oldMenuBar.getMenuName() != mode.getMenuName())
        {
            setJMenuBar(mode.createMenuBar(this));
            validate();
        }
    }

    public void placeWindow()
    {
        final SessionProperties sessionProperties =
            Editor.getSessionProperties();
        if (editors[0] == Editor.getEditor(0)) {
            // Initial window placement.
            Rectangle desired = sessionProperties.getWindowPlacement(0);
            if (desired.width == 0 || desired.height == 0) {
                // Use reasonable defaults.
                Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
                desired.width = dim.width - 100;
                if (desired.width > 800)
                    desired.width = 800;
                desired.height = dim.height - 100;
                desired.x = (dim.width - desired.width) / 2;
                desired.y = (dim.height - desired.height) / 2;
            }
            int extendedState = sessionProperties.getExtendedState(0);
            adjustPlacementRunnable =
                new AdjustPlacementRunnable(this, extendedState);
            setBounds(desired);
        } else {
            // BUG! Should not be hardcoded to 1!
            Rectangle desired = sessionProperties.getWindowPlacement(1);
            if (desired.width == 0 || desired.height == 0) {
                // Default positioning is cascaded.
                desired = Editor.getCurrentFrame().getBounds();
                Insets insets = Editor.getCurrentFrame().getInsets();
                desired.x += insets.left;
                desired.width -= insets.left;
                desired.y += insets.top;
                desired.height -= insets.top;
            }
            setBounds(desired);
            int extendedState = sessionProperties.getExtendedState(1);
            if (extendedState != 0)
                setExtendedState(extendedState);
        }
    }

    public void splitWindow()
    {
        if (editors[1] == null) {
            Editor.getSessionProperties().saveSidebarState(this);
            final int height = editors[0].getHeight();
            editors[0].saveView();
            editors[1] = new Editor(this);
            editors[1].activate(editors[0].getBuffer());
            editors[1].updateLocation();
            SplitPane sp = new SplitPane(SplitPane.VERTICAL_SPLIT,
                                         editors[0], editors[1]);
            sp.setBorder(null);
            sp.setDividerLocation(height / 2);
            editorPane = sp;
            if (sidebar != null) {
                sidebarSplitPane.setRightComponent(editorPane);
                int dividerLocation =
                    Editor.getSessionProperties().getSidebarWidth(this);
                sidebarSplitPane.setDividerLocation(dividerLocation);
            } else {
                // No sidebar.
                getContentPane().remove(editors[0]);
                getContentPane().add(editorPane, "Center");
            }
            validate();
            editors[0].setUpdateFlag(REFRAME);
            editors[1].updateDisplay();
            restoreFocus();
            updateControls();
        }
    }

    public void switchToBuffer(final Buffer buf)
    {
        Debug.bugIfNot(buf.isPaired() ||
            (getEditorCount() == 2 && editors[0].getBuffer().isPaired()));
        final Buffer primary;
        final Buffer secondary;
        if (buf.isPrimary()) {
            primary = buf;
            secondary = buf.getSecondary();
        } else {
            Debug.bugIfNot(buf.isSecondary());
            primary = buf.getPrimary();
            Debug.bugIfNot(primary != null);
            secondary = buf;
        }
        if (getEditorCount() == 2) {
            // Window is already split.
            if (secondary != null) {
                // Activate primary in editor 0.
                // Activate secondary in editor 1.
                if (editors[0].getBuffer() != primary)
                    editors[0].activate(primary);
                if (editors[1].getBuffer() != secondary)
                    editors[1].activate(secondary);
                // Adjust split pane divider location.
                if (editorPane instanceof SplitPane) {
                    SplitPane sp = (SplitPane) editorPane;
                    int height = sp.getHeight();
                    float split = secondary.getSplit();
                    int dividerLocation =
                        (int)(height * (1 - split) - sp.getDividerSize());
                    sp.setDividerLocation(dividerLocation);
                }
                Editor.setCurrentEditor(buf == primary ? editors[0] : editors[1]);
                editors[0].updateDisplay();
                editors[1].updateDisplay();
            } else {
                // No secondary.
                Debug.bugIfNot(secondary == null);
                // We don't need a split window. Close editor 1.
                Editor keep = editors[0];
                Editor kill = editors[1];
                // Save information about the buffer in the editor that we're
                // going to close.
                final Buffer b = kill.getBuffer();
                if (b != null) {
                    b.autosave();
                    kill.saveView();
                    RecentFiles.getInstance().bufferDeactivated(b,
                        kill.getDot());
                    b.windowClosing();
                }
                unsplitInternal(keep, kill);
                // Activate primary in editor 0.
                Debug.bugIfNot(editors[0] == keep);
                keep.activate(primary);
            }
        } else {
            // Window is not split.
            Debug.bugIfNot(getEditorCount() == 1);
            if (secondary != null) {
                // Split the window, activate primary in editor 0, activate
                // secondary in editor 1.
                splitWindow(primary, secondary, secondary.getSplit());
                Editor.setCurrentEditor(buf == primary ? editors[0] : editors[1]);
                editors[0].updateDisplay();
                editors[1].updateDisplay();
                restoreFocus();
            } else {
                // Only one editor, no secondary.
                Debug.bugIfNot(editors[0] != null);
                Debug.bugIfNot(editors[1] == null);
                Debug.bugIfNot(secondary == null);
                // Activate primary in editor 0.
                editors[0].activate(primary);
            }
        }
        buf.setLastActivated(System.currentTimeMillis());
        if (Editor.isDebugEnabled()) {
            if (buf.isPrimary()) {
                Debug.bugIfNot(getPrimaryEditor().getBuffer() == buf);
            } else {
                Debug.bugIfNot(buf.isSecondary());
                Buffer bufPrimary = buf.getPrimary();
                Debug.bugIfNot(primary != null);
                Debug.bugIfNot(getPrimaryEditor().getBuffer() == bufPrimary);
            }
        }
    }

    public void splitWindow(Buffer buf1, Buffer buf2, float split)
    {
        if (editors[1] == null) {
            final SessionProperties sessionProperties =
                Editor.getSessionProperties();
            sessionProperties.saveSidebarState(this);
            final int height = editors[0].getHeight();
            editors[0].saveView();
            editors[0].activate(buf1);
            editors[1] = new Editor(this);
            editors[1].activate(buf2);
            editors[1].updateLocation();
            SplitPane sp = new SplitPane(SplitPane.VERTICAL_SPLIT,
                                         editors[0], editors[1]);
            sp.setBorder(null);
            int dividerLocation =
                (int)(height * (1 - split) - sp.getDividerSize());
            sp.setDividerLocation(dividerLocation);
            editorPane = sp;
            if (sidebar != null) {
                sidebarSplitPane.setRightComponent(editorPane);
                dividerLocation = sessionProperties.getSidebarWidth(this);
                sidebarSplitPane.setDividerLocation(dividerLocation);
            } else {
                // No sidebar.
                getContentPane().remove(editors[0]);
                getContentPane().add(editorPane, "Center");
            }
            validate();
            editors[0].setUpdateFlag(REFRAME | REPAINT);
            editors[1].setUpdateFlag(REFRAME | REPAINT);
            updateControls();
        }
    }

    public void enlargeWindow(Editor editor, int n)
    {
        if (editorPane instanceof SplitPane) {
            final SplitPane sp = (SplitPane) editorPane;
            final int charHeight = Display.getCharHeight();
            int dividerLocation = sp.getDividerLocation();
            if (editor == editors[0])
                dividerLocation += charHeight;
            else
                dividerLocation -= charHeight;
            sp.setDividerLocation(dividerLocation);
        }
    }

    // Set window height to N lines.
    public void setWindowHeight(Editor editor, int n)
    {
      if (editorPane instanceof SplitPane)
        {
          SplitPane sp = (SplitPane) editorPane;
          Editor otherEditor = (editor == editors[0]) ? editors[1] : editors[0];
          int charHeight = Display.getCharHeight();
          HorizontalScrollBar scrollBar = editor.getHorizontalScrollBar();
          int scrollBarHeight = (scrollBar != null) ? scrollBar.getHeight() : 0;
          int minHeightForOtherWindow =
            otherEditor.getLocationBarHeight() + charHeight * 4 + scrollBarHeight;
          int availableHeight =
            sp.getHeight() - minHeightForOtherWindow - sp.getDividerSize();
          int requestedHeight =
            editor.getLocationBarHeight() + charHeight * n + scrollBarHeight;
          int height = Math.min(requestedHeight, availableHeight);
          if (editor == editors[0])
            sp.setDividerLocation(height);
          else if (editor == editors[1])
            sp.setDividerLocation(sp.getHeight() - sp.getDividerSize() - height);
        }
    }

    public final Editor activateInOtherWindow(Editor editor, Buffer buffer)
    {
        // Switch to other window.
        return openInOtherWindow(editor, buffer, 0.5F, true);
    }

    public final Editor activateInOtherWindow(Editor editor, Buffer buffer,
        float split)
    {
        // Switch to other window.
        return openInOtherWindow(editor, buffer, split, true);
    }

    public final Editor displayInOtherWindow(Editor editor, Buffer buffer)
    {
        // Don't switch to other window.
        return openInOtherWindow(editor, buffer, 0.5F, false);
    }

    private Editor openInOtherWindow(Editor editor, Buffer buffer, float split,
        boolean switchWindows)
    {
        editor.saveView();
        Editor otherEditor = null;
        if (editors[1] == null) {
            Debug.assertTrue(editor == editors[0]);
            editors[1] = new Editor(this);
            editors[1].activate(buffer);
            editors[1].updateLocation();
            SplitPane sp = new SplitPane(SplitPane.VERTICAL_SPLIT,
                                         editor, editors[1]);
            sp.setBorder(null);
            int dividerLocation =
                (int)(editor.getHeight() * (1 - split) - sp.getDividerSize());
            sp.setDividerLocation(dividerLocation);
            editorPane = sp;
            if (sidebar != null) {
                sidebarSplitPane.setRightComponent(editorPane);
                sidebarSplitPane.setDividerLocation(
                    Editor.getSessionProperties().getSidebarWidth(this));
            } else {
                // No sidebar.
                getContentPane().remove(editor);
                getContentPane().add(editorPane, "Center");
            }
            validate();
            otherEditor = editors[1];
        } else {
            // Second window is already open.
            if (editor == editors[0])
                otherEditor = editors[1];
            else if (editor == editors[1])
                otherEditor = editors[0];
            else
                Debug.assertTrue(false);
            otherEditor.activate(buffer);
            otherEditor.updateLocation();
        }
        if (switchWindows) {
            Editor.setCurrentEditor(otherEditor);
            setMenu();
            setToolbar();
        }
        editors[0].setUpdateFlag(REFRAME | REPAINT);
        editors[0].updateDisplay();
        editors[1].setUpdateFlag(REFRAME | REPAINT);
        editors[1].updateDisplay();
        currentEditor.setFocusToDisplay();
        restoreFocus();
        updateControls();
        return otherEditor;
    }

    public void closeEditor(Editor editor)
    {
        if (editors[1] == null)
            return;
        if (editor != editors[0] && editor != editors[1])
            return;
        promoteSecondaryBuffers();
        Editor keep = editor == editors[0] ? editors[1] : editors[0];
        Editor kill = editor;
        unsplitInternal(keep, kill);
    }

    public void unsplitWindow()
    {
        if (editors[1] == null)
            return;
        promoteSecondaryBuffers();
        Editor keep = currentEditor;
        Editor kill = getOtherEditor();
        unsplitInternal(keep, kill);
    }

    public void unsplitWindowKeepOther()
    {
        if (editors[1] == null)
            return;
        promoteSecondaryBuffers();
        Editor keep = getOtherEditor();
        Editor kill = currentEditor;
        unsplitInternal(keep, kill);
    }

    public void promoteSecondaryBuffers()
    {
        Buffer buffer = editors[0].getBuffer();
        if (buffer.isSecondary())
            buffer.promote();
        if (editors[1] != null) {
            buffer = editors[1].getBuffer();
            if (buffer.isSecondary())
                buffer.promote();
        }
    }

    private void unsplitInternal(final Editor keep, final Editor kill)
    {
      Editor.getSessionProperties().saveSidebarState(this);
      if (sidebar != null)
        {
          sidebarSplitPane.setRightComponent(keep);
          int dividerLocation =
            Editor.getSessionProperties().getSidebarWidth(this);
          sidebarSplitPane.setDividerLocation(dividerLocation);
        }
      else
        {
          // No sidebar.
          getContentPane().remove(editorPane);
          getContentPane().add(keep, "Center");
        }
      validate();
      editorPane = null;
      Editor.removeEditor(kill);
      editors[0] = keep;
      editors[1] = null;
      Buffer buffer = keep.getBuffer();
      if (buffer.isSecondary())
        buffer.promote();
      if (keep.getLocationBar() == null)
        keep.addLocationBar();
      Editor.setCurrentEditor(keep);
      keep.setUpdateFlag(REFRAME);
      keep.reframe();
      restoreFocus();
      statusBar.repaint();
      updateControls();
    }

    public void updateControls()
    {
        boolean enable = editors[1] != null;
        LocationBar locationBar = editors[0].getLocationBar();
        if (locationBar != null) {
            JButton closeButton = locationBar.getCloseButton();
            if (closeButton != null)
                closeButton.setEnabled(enable);
        }
        if (editors[1] != null) {
            locationBar = editors[1].getLocationBar();
            if (locationBar != null) {
                JButton closeButton = locationBar.getCloseButton();
                if (closeButton != null)
                    closeButton.setEnabled(enable);
            }
        }
    }

    private boolean active;

    public final boolean isActive()
    {
        return active;
    }

    public void reactivate()
    {
        if (currentEditor.getBuffer() == null)
            return;
        boolean changed = false;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            if (currentEditor.reactivate(it.nextBuffer()))
                changed = true;
        }
        if (changed) {
            for (int i = 0; i < Editor.getFrameCount(); i++) {
                Frame frame = Editor.getFrame(i);
                frame.setMenu();
            }
            Sidebar.repaintBufferListInAllFrames();
        }
    }

    public void windowActivated(WindowEvent e)
    {
        active = true;
        Editor.setCurrentEditor(currentEditor);
        setFocus(currentEditor.getDisplay());
        repaint();
        // 1.4.0-rc hangs if we call reactivate() directly here.
        Runnable r = new Runnable() {
            public void run()
            {
                reactivate();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void windowDeactivated(WindowEvent e)
    {
        active = false;
        // Show/hide caret.
        editors[0].repaint();
        if (editors[1] != null)
            editors[1].repaint();
    }

    public void windowOpened(WindowEvent e)
    {
        if (adjustPlacementRunnable != null) {
            adjustPlacementRunnable.run();
            adjustPlacementRunnable = null;
        }
    }

    public void windowClosing(WindowEvent e)
    {
        editors[0].killFrame();
    }

    public void windowClosed(WindowEvent e)
    {
    }

    public void windowIconified(WindowEvent e)
    {
    }

    public void windowDeiconified(WindowEvent e)
    {
    }

    public void windowStateChanged(WindowEvent e)
    {
        int newState = e.getNewState();
        if (newState == 0) {
            // Not maximized.
            if (rect != null)
                setBounds(rect);
        }
        storeExtendedState(newState);
    }

    private JComponent focusedComponent;

    public void setFocus(JComponent c)
    {
        boolean change = focusedComponent != c;
        if (c != null)
            c.requestFocus();
        if (change) {
            JComponent lastFocusedComponent = focusedComponent;
            focusedComponent = c;
            // Update display of current line (show/hide caret) in all
            // windows, as required.
            for (int i = 0; i < editors.length; i++) {
                Editor editor = editors[i];
                if (editor != null && editor.getDot() != null) {
                    Display display = editor.getDisplay();
                    if (display == focusedComponent || display == lastFocusedComponent) {
                        editor.updateDotLine();
                        display.repaintChangedLines();
                    }
                }
            }
        }
    }

    public JComponent getFocusedComponent()
    {
        return focusedComponent;
    }

    private static final Cursor waitCursor =
        Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);

    public final void setWaitCursor()
    {
        setCursor(waitCursor);
        editors[0].setWaitCursor();
        if (editors[1] != null)
            editors[1].setWaitCursor();
    }

    public final void setDefaultCursor()
    {
        setCursor(Cursor.getDefaultCursor());
        editors[0].setDefaultCursor();
        if (editors[1] != null)
            editors[1].setDefaultCursor();
    }

    public void resetDisplay()
    {
        if (toolbar != null) {
            getContentPane().remove(toolbar);
            toolbar = null;
        }
        defaultToolBar = null;
        for (int i = 0; i < editors.length; i++) {
            Editor editor = editors[i];
            if (editor != null) {
                editor.removeLocationBar();
                editor.removeVerticalScrollBar();
                editor.removeHorizontalScrollBar();
            }
        }
        DefaultLookAndFeel.setLookAndFeel();
        final Mode mode = currentEditor.getMode();
        setJMenuBar(mode.createMenuBar(this));
        final SessionProperties sessionProperties = Editor.getSessionProperties();
        if (sessionProperties.getShowToolbar(this) && ToolBar.isToolBarEnabled()) {
            ToolBar tb = mode.getToolBar(this);
            if (tb != null)
                addToolbar(tb);
        }
        if (sidebarSplitPane != null) {
            // Save state before removing sidebar.
            sessionProperties.saveSidebarState(this);
            // Remove sidebar.
            getContentPane().remove(sidebarSplitPane);
            sidebarSplitPane = null;
            sidebar = null;

            if (Platform.isJava14()) {
                // With Sun Java 1.4.0 FCS, if the following 3 lines of code
                // are removed, focus is lost when this method is called from
                // Buffer.saveLocal() after the preferences file is saved.
                // When this happens, focus can be recovered by switching to a
                // different Sawfish workspace and back again, at which point
                // any keystrokes that were lost are replayed accurately into
                // the buffer.

                // Not that it makes any sense to do this... ;)
                getContentPane().add(getEditorPane(), "Center");
                currentEditor.getDisplay().requestFocus();
                getContentPane().remove(getEditorPane());
            }

            sidebar = new Sidebar(this);
            sidebarSplitPane = createSidebarSplitPane();
            getContentPane().add(sidebarSplitPane, "Center");
            sidebar.setUpdateFlag(SIDEBAR_ALL);
        }
        for (int i = 0; i < editors.length; i++) {
            Editor editor = editors[i];
            if (editor != null) {
                editor.addLocationBar();
                editor.updateLocation();
                editor.addVerticalScrollBar();
                editor.maybeAddHorizontalScrollBar();
                editor.getDisplay().initialize();
            }
        }
        updateControls();
        validate();
    }

    public static final void restoreFocus()
    {
        Editor.restoreFocus();
    }

    private Object folderTree;

    public final Object getFolderTree()
    {
        return folderTree;
    }

    public final void setFolderTree(Object obj)
    {
        folderTree = obj;
    }

    public void componentResized(ComponentEvent e)
    {
        if (extendedState != 6) {
            // Not maximized.
            rect = getBounds();
        }
    }

    public void componentMoved(ComponentEvent e)
    {
        if (extendedState != 6) {
            // Not maximized.
            rect = getBounds();
        }
    }

    public void componentShown(ComponentEvent e) {}

    public void componentHidden(ComponentEvent e) {}

    public void focusGained(FocusEvent e)
    {
        currentEditor.setFocusToDisplay();
    }

    public void focusLost(FocusEvent e) {}
}
