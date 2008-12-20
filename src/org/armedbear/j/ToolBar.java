/*
 * ToolBar.java
 *
 * Copyright (C) 2000-2004 Peter Graves
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
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JToolBar;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class ToolBar extends JToolBar implements ActionListener, ToolBarConstants
{
    private static final int STYLE_DEFAULT   = 0;
    private static final int STYLE_TEXT_ONLY = 1;
    private static final int STYLE_ICON_ONLY = 2;

    private static final Preferences preferences = Editor.preferences();

    protected Frame frame;
    protected int style = STYLE_DEFAULT;

    public ToolBar(Frame frame)
    {
        this(frame, STYLE_DEFAULT);
    }

    public ToolBar(Frame frame, int style)
    {
        this.frame = frame;
        this.style = style;
        setFloatable(false);
        setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.gray));
    }

    public ToolBarButton addButton(String text, String iconFile, String methodName)
    {
        return addButton(text, iconFile, methodName, true);
    }

    public ToolBarButton addButton(String text, String iconFile, String methodName,
                                   boolean enabled)
    {
        ToolBarButton button = new ToolBarButton(frame, methodName, this);
        switch (style) {
            case STYLE_DEFAULT:
                if (textEnabled())
                    button.setText(text);
                else
                    button.setToolTipText(text);
                if (iconsEnabled())
                    button.setIconFromFile(iconFile);
                button.setHorizontalTextPosition(ToolBarButton.CENTER);
                button.setVerticalTextPosition(ToolBarButton.BOTTOM);
                break;
            case STYLE_ICON_ONLY:
                button.setIconFromFile(iconFile);
                button.setToolTipText(text);
                break;
            case STYLE_TEXT_ONLY:
                button.setText(text);
                break;
        }
        button.setRolloverEnabled(isRolloverEnabled());
        button.setEnabled(enabled);
        add(button);
        return button;
    }

    public ToolBarButton maybeAddInboxButton()
    {
        if (Editor.isMailEnabled())
            if (preferences.getStringProperty(Property.INBOX) != null)
                return addButton("Inbox", ICON_MAIL_INBOX, "inbox");
        return null;
    }

    public static final boolean isToolBarEnabled()
    {
        return textEnabled() || iconsEnabled();
    }

    private static final boolean textEnabled()
    {
        // Defaults to true for j's default look and feel.
        return preferences.getBooleanProperty(Property.TOOL_BAR_SHOW_TEXT,
            Editor.lookAndFeel == null);
    }

    private static final boolean iconsEnabled()
    {
        // Defaults to true in all cases.
        return preferences.getBooleanProperty(Property.TOOL_BAR_SHOW_ICONS);
    }

    public static final boolean isRolloverEnabled()
    {
        // Defaults to true for j's default look and feel.
        return preferences.getBooleanProperty(Property.TOOL_BAR_IS_ROLLOVER,
            Editor.lookAndFeel == null);
    }

    public void actionPerformed(ActionEvent e)
    {
        final Editor editor = frame.getCurrentEditor();
        editor.setFocusToDisplay();
        editor.getDispatcher().actionPerformed(e);
    }

    public static ToolBar createToolBar(Frame frame, File file)
    {
        if (file == null)
            return null;
        if (!file.isFile())
            return null;
        XMLReader xmlReader = Utilities.getDefaultXMLReader();
        if (xmlReader == null)
            return null;
        try {
            ToolBar toolBar = new ToolBar(frame);
            Handler handler = new Handler(toolBar);
            xmlReader.setContentHandler(handler);
            InputSource inputSource = new InputSource(file.getInputStream());
            xmlReader.parse(inputSource);
            return toolBar;
        }
        catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

    private static class Handler extends DefaultHandler implements ContentHandler
    {
        private final ToolBar toolBar;

        public Handler(ToolBar toolBar)
        {
            this.toolBar = toolBar;
        }

        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException
        {
            if (localName.equals("button") || qName.equals("button")) {
                String label = attributes.getValue("", "label");
                String icon = attributes.getValue("", "icon");
                String command = attributes.getValue("", "command");
                toolBar.addButton(label, icon, command);
            } else if (localName.equals("separator") || qName.equals("separator"))
                toolBar.addSeparator();
        }
    }
}
