/*
 * PropertiesDialog.java
 *
 * Copyright (C) 1998-2007 Peter Graves
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

import java.awt.Cursor;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

public final class PropertiesDialog extends AbstractDialog implements Constants
{
    private static final String TEXT_LF   = "LF";
    private static final String TEXT_CR   = "CR";
    private static final String TEXT_CRLF = "CR+LF";

    private final Editor editor;
    private final Buffer buffer;

    private JComboBox modeComboBox;
    private JTextField tabWidthTextField;
    private JTextField indentSizeTextField;
    private JTextField wrapColumnTextField;
    private CheckBox useTabsCheckBox;
    private CheckBox indentBeforeBraceCheckBox;
    private CheckBox indentAfterBraceCheckBox;
    private JComboBox lineSeparatorComboBox;

    public PropertiesDialog()
    {
        super(Editor.currentEditor(), "Properties", true);

        editor = Editor.currentEditor();
        buffer = editor.getBuffer();
        final int modeId = buffer.getModeId();

        final int WIDTH = 30;

        if (!buffer.isUntitled()) {
            JPanel group = new JPanel();
            group.setLayout(new BoxLayout(group, BoxLayout.Y_AXIS));
            group.setAlignmentX(LEFT_ALIGNMENT);
            Border etchedBorder = BorderFactory.createEtchedBorder();
            Border titledBorder = BorderFactory.createTitledBorder(etchedBorder, "File");
            group.setBorder(titledBorder);

            final File file = buffer.getFile();
            StaticTextField fileTextField =
                new StaticTextField(file.isRemote() ? file.netPath() :
                                    file.canonicalPath());
            group.add(fileTextField);

            StaticTextField modificationTextField = null;

            if (file.getProtocol() == File.PROTOCOL_HTTP) {
                if (buffer.getLastModified() != 0) {
                    Date date = new Date(buffer.getLastModified());
                    modificationTextField = new StaticTextField("Last modified ".concat(date.toString()));
                }
            } else if (file.isLocal()) {
                Date date = new Date(file.lastModified());
                FastStringBuffer sb = new FastStringBuffer();
                if (buffer.isModified())
                    sb.append("Modified:");
                else
                    sb.append("Not modified;");
                sb.append(" last saved ");
                sb.append(date.toString());
                modificationTextField = new StaticTextField(sb.toString());
            }

            if (modificationTextField != null) {
                group.add(Box.createVerticalStrut(6));
                group.add(modificationTextField);
            }

            String encoding = buffer.getSaveEncoding();
            if (encoding != null) {
                group.add(Box.createVerticalStrut(6));
                group.add(new StaticTextField(encoding));
            } else
                Debug.bug();

            buffer.checkCVS();
            CVSEntry cvsEntry = buffer.getCVSEntry();
            if (cvsEntry != null) {
                String revision = cvsEntry.getRevision();
                if (revision != null && revision.length() > 0) {
                    FastStringBuffer sb = new FastStringBuffer("CVS ");
                    if (revision.equals("0")) {
                        sb.append(" (locally added)");
                    } else {
                        sb.append(" revision ");
                        sb.append(revision);
                        final long last_modified = buffer.getLastModified();
                        final long checkout = cvsEntry.getCheckoutTime();
                        if (last_modified != checkout) {
                            Log.debug("last_modified = " + last_modified);
                            Log.debug("checkout      = " + checkout);
                            if (Math.abs(last_modified - checkout) >= 1000)
                                sb.append(" (locally modified)");
                        }
                    }
                    group.add(Box.createVerticalStrut(6));
                    group.add(new StaticTextField(sb.toString()));
                }
            }

            if (Editor.checkExperimental()) {
                String s = P4.getStatusString(file);
                if (s != null) {
                    group.add(Box.createVerticalStrut(6));
                    group.add(new StaticTextField(s));
                }
            }

            if (buffer.isReadOnly()) {
                if (file.getProtocol() == File.PROTOCOL_HTTP)
                    ;
                else if (modeId == BINARY_MODE)
                    ;
                else if (modeId == IMAGE_MODE)
                    ;
                else if (modeId == WEB_MODE)
                    ;
                else {
                    StaticTextField readOnlyTextField =
                        new StaticTextField("File is read only on disk", WIDTH);
                    group.add(Box.createVerticalStrut(6));
                    group.add(readOnlyTextField);
                }
            }

            mainPanel.add(group);
        }

        if (modeId != IMAGE_MODE && modeId != WEB_MODE){
            // Mode combo box.
            modeComboBox = new JComboBox(getPermissibleModes());
            Dimension dim = modeComboBox.getPreferredSize();
            modeComboBox.setMinimumSize(dim);
            modeComboBox.setMaximumSize(dim);

            JPanel flow = new JPanel();
            flow.setLayout(new BoxLayout(flow, BoxLayout.X_AXIS));
            flow.setAlignmentX(LEFT_ALIGNMENT);
            Label label = new Label("Mode:");
            flow.add(label);
            label.setDisplayedMnemonic('M');

            modeComboBox.setSelectedItem(buffer.getMode().toString());
            label.setLabelFor(modeComboBox);
            modeComboBox.addKeyListener(this);

            flow.add(Box.createHorizontalStrut(5));
            flow.add(modeComboBox);

            addVerticalStrut();
            mainPanel.add(flow);

            if (modeId != ARCHIVE_MODE && modeId != BINARY_MODE) {
                flow = new JPanel();
                flow.setLayout(new BoxLayout(flow, BoxLayout.X_AXIS));
                flow.setAlignmentX(LEFT_ALIGNMENT);
                label = new Label("Tab width:");
                label.setDisplayedMnemonic('T');
                flow.add(label);
                flow.add(Box.createHorizontalStrut(5));
                tabWidthTextField = new JTextField(3);
                dim = tabWidthTextField.getPreferredSize();
                tabWidthTextField.setMinimumSize(dim);
                tabWidthTextField.setMaximumSize(dim);
                tabWidthTextField.setAlignmentX(LEFT_ALIGNMENT);
                tabWidthTextField.setText(String.valueOf(buffer.getTabWidth()));
                label.setLabelFor(tabWidthTextField);
                flow.add(tabWidthTextField);

                flow.add(Box.createHorizontalStrut(5));

                label = new Label("Indent size:");
                label.setDisplayedMnemonic('I');
                flow.add(label);
                flow.add(Box.createHorizontalStrut(5));
                indentSizeTextField = new JTextField(3);
                dim = indentSizeTextField.getPreferredSize();
                indentSizeTextField.setMinimumSize(dim);
                indentSizeTextField.setMaximumSize(dim);
                indentSizeTextField.setAlignmentX(LEFT_ALIGNMENT);
                indentSizeTextField.setText(String.valueOf(buffer.getIndentSize()));
                label.setLabelFor(indentSizeTextField);
                flow.add(indentSizeTextField);

                flow.add(Box.createHorizontalStrut(5));

                label = new Label("Wrap column:");
                label.setDisplayedMnemonic('W');
                flow.add(label);
                flow.add(Box.createHorizontalStrut(5));
                wrapColumnTextField = new JTextField(3);
                dim = wrapColumnTextField.getPreferredSize();
                wrapColumnTextField.setMinimumSize(dim);
                wrapColumnTextField.setMaximumSize(dim);
                wrapColumnTextField.setAlignmentX(LEFT_ALIGNMENT);
                wrapColumnTextField.setText(String.valueOf(buffer.getIntegerProperty(Property.WRAP_COL)));
                label.setLabelFor(wrapColumnTextField);
                flow.add(wrapColumnTextField);

                addVerticalStrut();
                mainPanel.add(flow);

                useTabsCheckBox = new CheckBox("Use tabs", buffer.getUseTabs());
                useTabsCheckBox.setMnemonic('U');
                addVerticalStrut();
                addCheckBox(useTabsCheckBox);
                useTabsCheckBox.setEnabled(true);

                switch (modeId) {
                    case JAVA_MODE:
                    case JAVASCRIPT_MODE:
                    case C_MODE:
                    case CPP_MODE:
                    case PERL_MODE:
                    case TCL_MODE:
                        indentBeforeBraceCheckBox =
                            new CheckBox("Indent before '{'",
                                         buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE));
                        indentBeforeBraceCheckBox.setMnemonic('b');
                        addVerticalStrut();
                        addCheckBox(indentBeforeBraceCheckBox);
                        indentAfterBraceCheckBox =
                            new CheckBox("Indent after '{'",
                                         buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE));
                        indentAfterBraceCheckBox.setMnemonic('a');
                        addVerticalStrut();
                        addCheckBox(indentAfterBraceCheckBox);
                    default:
                        break;
                }

                addVerticalStrut();

                // Line separator combo box.
                Vector v = new Vector();
                v.add(TEXT_LF);
                v.add(TEXT_CRLF);
                v.add(TEXT_CR);
                lineSeparatorComboBox = new JComboBox(v);
                dim = lineSeparatorComboBox.getPreferredSize();
                lineSeparatorComboBox.setMinimumSize(dim);
                lineSeparatorComboBox.setMaximumSize(dim);

                flow = new JPanel();
                flow.setLayout(new BoxLayout(flow, BoxLayout.X_AXIS));
                flow.setAlignmentX(LEFT_ALIGNMENT);
                label = new Label("Use");
                flow.add(label);
                label.setDisplayedMnemonic('L');

                if (buffer.lineSeparator == null)
                    buffer.lineSeparator = System.getProperty("line.separator");

                if (buffer.lineSeparator.equals("\n"))
                    lineSeparatorComboBox.setSelectedItem(TEXT_LF);
                else if (buffer.lineSeparator.equals("\r\n"))
                    lineSeparatorComboBox.setSelectedItem(TEXT_CRLF);
                else if (buffer.lineSeparator.equals("\r"))
                    lineSeparatorComboBox.setSelectedItem(TEXT_CR);

                label.setLabelFor(lineSeparatorComboBox);

                flow.add(Box.createHorizontalStrut(5));
                flow.add(lineSeparatorComboBox);

                flow.add(Box.createHorizontalStrut(5));
                flow.add(new Label("as line separator when saving file"));

                mainPanel.add(flow);
            }

            addVerticalStrut();
        }

        addOKCancel();

        if (tabWidthTextField != null)
            tabWidthTextField.addKeyListener(this);

        if (indentSizeTextField != null)
            indentSizeTextField.addKeyListener(this);

        if (wrapColumnTextField != null)
            wrapColumnTextField.addKeyListener(this);

        pack();

        if (tabWidthTextField != null)
            tabWidthTextField.requestFocus();
    }

    private String[] getPermissibleModes()
    {
        ModeList modeList = Editor.getModeList();
        int fileType = buffer.getFileType();
        if (fileType == FILETYPE_ZIP) {
            String[] array = new String[2];
            array[0] = ARCHIVE_MODE_NAME;
            array[1] = BINARY_MODE_NAME;
            return array;
        }
        if (fileType == FILETYPE_BINARY || fileType == FILETYPE_JPEG) {
            String[] array;
            if (Editor.getModeList().modeAccepts(IMAGE_MODE, buffer.getFile().getName())) {
                array = new String[2];
                array[0] = IMAGE_MODE_NAME;
                array[1] = BINARY_MODE_NAME;
            } else {
                array = new String[1];
                array[0] = BINARY_MODE_NAME;
            }
            return array;
        }
        ArrayList list = new ArrayList();
        synchronized (modeList) {
            Iterator it = modeList.iterator();
            while (it.hasNext()) {
                ModeListEntry entry = (ModeListEntry) it.next();
                if (entry.isSelectable())
                    list.add(entry.getDisplayName());
            }
        }
        return (String[]) list.toArray(new String[list.size()]);
    }

    private boolean save()
    {
        if (modeComboBox != null) {
            String modeName = (String) modeComboBox.getSelectedItem();
            if (modeName != null && !modeName.equals(buffer.getModeName())) {
                Mode newMode = Editor.getModeList().getModeFromModeName(modeName);
                if (buffer.getModeId() == BINARY_MODE || newMode.getId() == BINARY_MODE) {
                    if (buffer.isModified()) {
                        FastStringBuffer sb = new FastStringBuffer("Buffer will be reloaded in ");
                        if (newMode.getId() == BINARY_MODE)
                            sb.append("binary");
                        else
                            sb.append(newMode.toString());
                        sb.append(" mode; discard changes?");
                        boolean confirmed = editor.confirm(buffer.getFile().getName(), sb.toString());
                        if (!confirmed)
                            return false;
                    }
                }
                paint(getGraphics());
                editor.repaintNow();
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                editor.setWaitCursor();
                buffer.changeMode(newMode);
                editor.setDefaultCursor();
                setCursor(Cursor.getDefaultCursor());
            }
        }
        boolean error = false;
        if (tabWidthTextField != null) {
            try {
                String s = tabWidthTextField.getText();
                int tabWidth = Integer.parseInt(s);
                if (tabWidth > 0 && tabWidth <= 8)
                    buffer.setTabWidth(tabWidth);
                else
                    error = true;
            }
            catch (NumberFormatException e) {
                error = true;
            }
            if (error) {
                MessageDialog.showMessageDialog(editor, "Invalid tab width", "Error");
                return false;
            }
        }
        if (indentSizeTextField != null) {
            try {
                String s = indentSizeTextField.getText();
                int indentSize = Integer.parseInt(s);
                if (indentSize > 0 && indentSize <= 8)
                    buffer.setIndentSize(indentSize);
                else
                    error = true;
            }
            catch (NumberFormatException e) {
                error = true;
            }
            if (error) {
                MessageDialog.showMessageDialog(editor, "Invalid indent size", "Error");
                return false;
            }
        }
        if (wrapColumnTextField != null) {
            try {
                String s = wrapColumnTextField.getText();
                int wrapCol = Integer.parseInt(s);
                if (wrapCol >= 16)
                    buffer.setProperty(Property.WRAP_COL, wrapCol);
                else
                    error = true;
            }
            catch (NumberFormatException e) {
                error = true;
            }
            if (error) {
                MessageDialog.showMessageDialog(editor, "Invalid wrap column", "Error");
                return false;
            }
        }
        if (useTabsCheckBox != null)
            buffer.setProperty(Property.USE_TABS, useTabsCheckBox.isSelected());
        if (indentBeforeBraceCheckBox != null)
            buffer.setProperty(Property.INDENT_BEFORE_BRACE, indentBeforeBraceCheckBox.isSelected());
        if (indentAfterBraceCheckBox != null)
            buffer.setProperty(Property.INDENT_AFTER_BRACE, indentAfterBraceCheckBox.isSelected());
        if (lineSeparatorComboBox != null) {
            String s = (String) lineSeparatorComboBox.getSelectedItem();
            if (s.equals(TEXT_LF))
                buffer.lineSeparator = "\n";
            else if (s.equals(TEXT_CRLF))
                buffer.lineSeparator = "\r\n";
            else if (s.equals(TEXT_CR))
                buffer.lineSeparator = "\r";
        }
        return true;
    }

    protected void ok()
    {
        if (save()) {
            buffer.saveProperties();
            buffer.repaint();
            dispose();
        }
    }

    public static void properties()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        switch (buffer.getType()) {
            case Buffer.TYPE_DIRECTORY:
            case Buffer.TYPE_SHELL:
            case Buffer.TYPE_TELNET:
            case Buffer.TYPE_SSH:
            case Buffer.TYPE_MAN:
                return;
            default:
                break;
        }
        if (buffer.getModeId() == SEND_MAIL_MODE)
            return;
        if (buffer.getFile() == null)
            return;
        PropertiesDialog d = new PropertiesDialog();
        editor.centerDialog(d);
        d.show();
    }

    public static void listProperties()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        FastStringBuffer sb = new FastStringBuffer();
        PropertyList properties = buffer.getProperties();
        if (properties != null) {
            Set keySet = properties.keySet();
            if (keySet != null) {
                // Sort keys.
                ArrayList keys = new ArrayList(keySet);
                Collections.sort(keys);
                Iterator it = keys.iterator();
                while (it.hasNext()) {
                    Property property = (Property) it.next();
                    Object value = properties.getProperty(property);
                    sb.append(property.getDisplayName());
                    sb.append(" = ");
                    sb.append(value);
                    sb.append('\n');
                }
            }
        }
        if (sb.length() > 0) {
            String output = sb.toString();
            OutputBuffer buf = OutputBuffer.getOutputBuffer(output);
            buf.setFormatter(new PropertiesFormatter(buf));
            if (buf != null) {
                sb.setText("listProperties");
                if (buffer.getFile() != null) {
                    sb.append(' ');
                    sb.append(buffer.getFile().getName());
                }
                buf.setTitle(sb.toString());
                editor.makeNext(buf);
                editor.displayInOtherWindow(buf);
            }
        }
    }
}
