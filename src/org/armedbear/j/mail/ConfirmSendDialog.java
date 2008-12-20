/*
 * ConfirmSendDialog.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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

package org.armedbear.j.mail;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import org.armedbear.j.AbstractDialog;
import org.armedbear.j.CheckBox;
import org.armedbear.j.Editor;
import org.armedbear.j.History;
import org.armedbear.j.HistoryTextField;
import org.armedbear.j.KeyMapping;
import org.armedbear.j.Label;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.Property;
import org.armedbear.j.SessionProperties;

public final class ConfirmSendDialog extends AbstractDialog
{
    private static final int TEXTFIELD_WIDTH = 22;

    private static final String fromKey         = "confirmSend.from";
    private static final String bccAddSenderKey = "confirmSend.bccAddSender";
    private static final String bccAddOtherKey  = "confirmSend.bccAddOther";
    private static final String bccOtherKey     = "confirmSend.bccOther";
    private static final String smtpKey         = "confirmSend.smtp";

    private final Editor editor;
    private final SendMail sm;
    private final SessionProperties sessionProperties;

    private final HistoryTextField fromTextField;
    private final CheckBox bccAddSenderCheckBox;
    private final CheckBox bccAddOtherCheckBox;
    private final HistoryTextField bccOtherTextField;
    private final HistoryTextField smtpTextField;

    private final History fromHistory;
    private final History bccOtherHistory;
    private final History smtpHistory;

    private String from;
    private String smtp;
    private boolean bccAddSender;
    private boolean bccAddOther;
    private String bccOther;

    public ConfirmSendDialog(Editor editor, SendMail sm)
    {
        super(editor, "Confirm Send", true);
        this.editor = editor;
        this.sm = sm;
        sessionProperties = Editor.getSessionProperties();
        Label label = new Label("From:");
        label.setDisplayedMnemonic('F');
        fromTextField = new HistoryTextField(TEXTFIELD_WIDTH);
        fromHistory = new History(fromKey);
        fromTextField.setHistory(fromHistory);
        if (fromHistory.size() == 0) {
            MailAddress ma = Mail.getUserMailAddress();
            if (ma != null)
                fromHistory.append(ma.toString());
        }
        fromTextField.recallLast();
        addLabelAndTextField(label, fromTextField);
        addVerticalStrut();
        bccAddSenderCheckBox = new CheckBox("Bcc sender",
            sessionProperties.getBooleanProperty(bccAddSenderKey, false));
        bccAddSenderCheckBox.setMnemonic('S');
        bccAddSenderCheckBox.addKeyListener(this);
        mainPanel.add(bccAddSenderCheckBox);
        bccAddOtherCheckBox = new CheckBox("Bcc other:",
            sessionProperties.getBooleanProperty(bccAddOtherKey, false));
        bccAddOtherCheckBox.setMnemonic('O');
        bccAddOtherCheckBox.addKeyListener(this);
        bccAddOtherCheckBox.addActionListener(this);
        mainPanel.add(bccAddOtherCheckBox);
        JPanel panel = new JPanel();
        panel.setAlignmentX(LEFT_ALIGNMENT);
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(Box.createHorizontalStrut(17));
        bccOtherTextField = new HistoryTextField(TEXTFIELD_WIDTH);
        bccOtherHistory = new History(bccOtherKey);
        bccOtherTextField.setHistory(bccOtherHistory);
        bccOtherTextField.recallLast();
        bccOtherTextField.addKeyListener(this);
        bccOtherTextField.setEnabled(bccAddOtherCheckBox.isSelected());
        panel.add(bccOtherTextField);
        mainPanel.add(panel);
        addVerticalStrut();
        smtpTextField = new HistoryTextField(TEXTFIELD_WIDTH);
        smtpHistory = new History(smtpKey);
        if (smtpHistory.size() == 0) {
            smtp = Editor.preferences().getStringProperty(Property.SMTP);
            if (smtp != null)
                smtpHistory.append(smtp);
        }
        smtpTextField.setHistory(smtpHistory);
        smtpTextField.recallLast();
        label = new Label("SMTP server:");
        label.setDisplayedMnemonic('M');
        addLabelAndTextField(label, smtpTextField);
        addVerticalStrut();
        addOKCancel();
        pack();
        fromTextField.requestFocus();
    }

    public String getFrom()
    {
        return from;
    }

    public String getSmtp()
    {
        return smtp;
    }

    public boolean bccAddSender()
    {
        return bccAddSender;
    }

    public boolean bccAddOther()
    {
        return bccAddOther;
    }

    public String getBccOther()
    {
        return bccOther;
    }

    protected void ok()
    {
        from = fromTextField.getText().trim();
        bccAddSender = bccAddSenderCheckBox.isSelected();
        bccAddOther = bccAddOtherCheckBox.isSelected();
        bccOther = bccOtherTextField.getText().trim();
        smtp = smtpTextField.getText().trim();
        // Validation.
        if (from.length() == 0 || from.indexOf('@') < 0) {
            MessageDialog.showMessageDialog(
                "You must enter a valid \"From\" address",
                "Error");
            fromTextField.requestFocus();
            return;
        }
        if (bccAddOther) {
            if (bccOther.length() == 0 || bccOther.indexOf('@') < 0) {
                MessageDialog.showMessageDialog(
                    "You must enter a valid \"Bcc\" address",
                    "Error");
                bccOtherTextField.requestFocus();
                return;
            }
        }
        if (smtp.length() == 0) {
            MessageDialog.showMessageDialog(
                "You must specify the SMTP server",
                "Error");
            smtpTextField.requestFocus();
            return;
        }
        // Save state.
        fromHistory.append(from);
        fromHistory.save();
        sessionProperties.setBooleanProperty(bccAddSenderKey, bccAddSender);
        sessionProperties.setBooleanProperty(bccAddOtherKey, bccAddOther);
        if (bccOther.length() > 0) {
            bccOtherHistory.append(bccOther);
            bccOtherHistory.save();
        }
        smtpHistory.append(smtp);
        smtpHistory.save();
        dispose();
    }

    public void keyPressed(KeyEvent e)
    {
        // Treat the user's mapping(s) for the send command like Enter.
        KeyMapping mapping =
            editor.getKeyMapping(e.getKeyChar(), e.getKeyCode(),
                                 e.getModifiers());
        if (mapping != null && mapping.getCommand() == "send") {
            e.consume();
            enter();
        } else
            super.keyPressed(e);
    }

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();
        if (cmd != null && cmd.equals(bccAddOtherCheckBox.getText()))
            bccOtherTextField.setEnabled(bccAddOtherCheckBox.isSelected());
        else
            super.actionPerformed(e);
    }
}
