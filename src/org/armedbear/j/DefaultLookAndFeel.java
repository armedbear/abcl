/*
 * DefaultLookAndFeel.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import javax.swing.BorderFactory;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

public final class DefaultLookAndFeel extends DefaultMetalTheme
{
    private static final Preferences preferences = Editor.preferences();

    private final ColorUIResource primary1 = new ColorUIResource(0, 0, 0); // Black.

    private FontUIResource plainFont;

    public static void setLookAndFeel()
    {
        // This is the default.
        String lookAndFeelClassName =
            "javax.swing.plaf.metal.MetalLookAndFeel";

        Editor.lookAndFeel =
            preferences.getStringProperty(Property.LOOK_AND_FEEL);

        if (Editor.lookAndFeel == null) {
            if (Platform.isPlatformMacOSX())
                Editor.lookAndFeel = "Aqua";
        }

        if (Editor.lookAndFeel != null) {
            // User has indicated a preference.
            if (Editor.lookAndFeel.equals("Metal")) {
                ; // Default look and feel, but don't do customizations.
            } else if (Editor.lookAndFeel.equals("Motif")) {
                lookAndFeelClassName =
                    "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
            } else if (Editor.lookAndFeel.equals("Windows")) {
                lookAndFeelClassName =
                    "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
            } else if (Editor.lookAndFeel.equals("Aqua")) {
                lookAndFeelClassName = "com.apple.mrj.swing.MacLookAndFeel";
                // Jun 21 2002 7:16 AM
                // Using the menu bar at the top of the screen (and having it
                // actually work) seems to require some further unknown
                // magic...
                //System.setProperty("com.apple.macos.useScreenMenuBar", "true");
            } else {
                // Not recognized. Revert to default behavior.
                Editor.lookAndFeel = null;
            }
        }
        if (Editor.lookAndFeel == null) {
            // Default customizations.
            MetalLookAndFeel.setCurrentTheme(new DefaultLookAndFeel());
            UIManager.put("Tree.collapsedIcon",
                          Utilities.getIconFromFile("collapsed.png"));
            UIManager.put("Tree.expandedIcon",
                          Utilities.getIconFromFile("expanded.png"));
        } else {
            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        }
        try {
            UIManager.setLookAndFeel(lookAndFeelClassName);
        }
        catch (Exception e) {}
        // We want to do this in any case.
        UIManager.put("ToolBarUI", "org.armedbear.j.ToolBarUI");
        UIManager.put("ButtonUI", "org.armedbear.j.ButtonUI");
        UIManager.put("LabelUI", "org.armedbear.j.LabelUI");
    }

    private DefaultLookAndFeel()
    {
        String name = preferences.getStringProperty(Property.DIALOG_FONT_NAME);
        int size = preferences.getIntegerProperty(Property.DIALOG_FONT_SIZE);
        Font font = new Font(name, Font.PLAIN, size);
        plainFont = new FontUIResource(font);
    }

    public void addCustomEntriesToTable(UIDefaults table)
    {
        table.put("Button.border", BorderFactory.createRaisedBevelBorder());
        table.put("TextField.border", BorderFactory.createLoweredBevelBorder());
        table.put("SplitPaneUI", "javax.swing.plaf.basic.BasicSplitPaneUI");
        table.put("ScrollBarUI", "org.armedbear.j.ScrollBarUI");
        table.put("TreeUI", "javax.swing.plaf.basic.BasicTreeUI");
        table.put("SplitPane.dividerSize", new Integer(3));
        table.put("ScrollBar.background", new Color(0xe0e0e0));
        table.put("ScrollBar.foreground", new Color(0xc0c0c0));
        table.put("ScrollBar.track", new Color(0xe0e0e0));
        table.put("ScrollBar.trackHighlight", Color.black);
        table.put("ScrollBar.thumb", new Color(0xc0c0c0));
        table.put("ScrollBar.thumbHighlight", Color.white);
        table.put("ScrollBar.thumbDarkShadow", Color.black);
        table.put("ScrollBar.thumbShadow", new Color(0x808080));
        table.put("ScrollBar.width", new Integer(16));
        table.put("Button.textIconGap", new Integer(1));
        table.put("ToolTipUI", "org.armedbear.j.ToolTipUI");
    }

    protected ColorUIResource getPrimary1()
    {
        return primary1;
    }

    public FontUIResource getControlTextFont()
    {
        return plainFont;
    }

    public FontUIResource getSystemTextFont()
    {
        return plainFont;
    }

    public FontUIResource getUserTextFont()
    {
        return plainFont;
    }

    public FontUIResource getMenuTextFont()
    {
        return plainFont;
    }

    public FontUIResource getWindowTitleFont()
    {
        return plainFont;
    }

    public FontUIResource getSubTextFont()
    {
        return plainFont;
    }
}
