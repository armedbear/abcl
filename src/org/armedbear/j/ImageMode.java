/*
 * ImageMode.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.awt.Color;

public final class ImageMode extends AbstractMode implements Constants, Mode
{
    private static final String MENU_NAME = "ImageMode";
    private static final ImageMode mode = new ImageMode();

    private ImageMode()
    {
        super(IMAGE_MODE, IMAGE_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
    }

    public static final ImageMode getMode()
    {
        return mode;
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey('c', "imageCycleBackground");
        km.mapKey('f', "imageFit");
        km.mapKey('r', "imageRestore");
        km.mapKey('=', "imageZoomIn");
        km.mapKey('-', "imageZoomOut");
    }

    public String getMenuName()
    {
        return MENU_NAME;
    }

    public MenuBar createMenuBar(Frame frame)
    {
        MenuBar menuBar = new MenuBar(MENU_NAME);
        menuBar.add(new Menu("File", 'F'));
        menuBar.add(new Menu("View", 'V'));
        menuBar.add(new Menu("Image", 'I'));
        menuBar.add(new Menu("Help", 'H'));
        return menuBar;
    }

    public void populateMenu(Editor editor, Menu menu)
    {
        final String text = menu.getText();
        if (text == "File") {
            menu.add(editor, "New", 'N', "newBuffer");
            menu.add(editor, "Open...", 'O', "openFile");
            menu.add(editor, "Save All", 'A', "saveAll");
            menu.add(editor, "Close", 'C', "killBuffer");
            menu.add(editor, "Close All", 'L', "closeAll");
            menu.add(editor, "Close Others", 'H', "closeOthers");
            menu.add(editor, "Reload", 'R', "revertBuffer");
            menu.addSeparator();
            menu.add(editor, "Next Buffer", 'T', "nextBuffer");
            menu.add(editor, "Previous Buffer", 'R', "prevBuffer");
            menu.addSeparator();
            menu.add(editor, "New Frame", 'M', "newFrame");
            menu.add(editor, "Execute Command...", 'D', "executeCommand");
            menu.addSeparator();
            menu.add(editor, "Save All/Exit", '/', "saveAllExit");
            menu.add(editor, "Exit", 'X', "quit");
        } else if (text == "Image") {
            menu.add(editor, "Zoom In", 'I', "imageZoomIn");
            menu.add(editor, "Zoom Out", 'O', "imageZoomOut");
            menu.add(editor, "Scale to Fit Display", 'F', "imageFit");
            menu.add(editor, "Restore Original Size", 'R', "imageRestore");
            menu.add(editor, "Change Background Color", 'C', "imageCycleBackground");
        } else
            super.populateMenu(editor, menu);
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return null;
    }

    public static void imageCycleBackground()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof ImageBuffer) {
            ((ImageBuffer)buffer).cycleBackground();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == buffer)
                    ed.getDisplay().repaint();
            }
        }
    }

    public static void imageZoomIn()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof ImageBuffer)
            ((ImageBuffer)buffer).zoomIn();
    }

    public static void imageZoomOut()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof ImageBuffer)
            ((ImageBuffer)buffer).zoomOut();
    }

    public static void imageFit()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof ImageBuffer)
            ((ImageBuffer)buffer).fit();
    }

    public static void imageRestore()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof ImageBuffer)
            ((ImageBuffer)buffer).restore();
    }
}
