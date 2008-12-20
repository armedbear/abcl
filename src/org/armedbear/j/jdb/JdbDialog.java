/*
 * JdbDialog.java
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

package org.armedbear.j.jdb;

import java.util.Iterator;
import java.util.List;
import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import org.armedbear.j.AbstractDialog;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.History;
import org.armedbear.j.HistoryTextField;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.Utilities;

public final class JdbDialog extends AbstractDialog
{
    // History.
    private static final String mainClassKey      = "jdb.mainClass";
    private static final String mainClassArgsKey  = "jdb.mainClassArgs";
    private static final String classPathKey      = "jdb.classPath";
    private static final String sourcePathKey     = "jdb.sourcePath";
    private static final String javaHomeKey       = "jdb.javaHome";
    private static final String javaExecutableKey = "jdb.javaExecutable";
    private static final String vmArgsKey         = "jdb.vmArgs";

    private final Editor editor;
    private final HistoryTextField classPathTextField;
    private final HistoryTextField mainClassArgsTextField;
    private final HistoryTextField mainClassTextField;
    private final HistoryTextField sourcePathTextField;
    private final HistoryTextField javaHomeTextField;
    private final HistoryTextField javaExecutableTextField;
    private final HistoryTextField vmArgsTextField;
    private final History mainClassHistory;
    private final History mainClassArgsHistory;
    private final History classPathHistory;
    private final History sourcePathHistory;
    private final History javaHomeHistory;
    private final History javaExecutableHistory;
    private final History vmArgsHistory;
    private final JCheckBox startSuspendedCheckBox;
    private final JdbSession session;

    public JdbDialog(Editor editor)
    {
        super(editor, "Jdb", true);
        this.editor = editor;
        JLabel label;
        JPanel panel;
        JPanel applicationPanel = Utilities.createPanel("Application");
        mainClassTextField = new HistoryTextField(30);
        mainClassHistory = new History(mainClassKey);
        mainClassTextField.setHistory(mainClassHistory);
        label = new JLabel("Main class:");
        label.setDisplayedMnemonic('M');
        addLabelAndTextField(applicationPanel, label, mainClassTextField);
        addVerticalStrut(applicationPanel);
        mainClassArgsTextField = new HistoryTextField(30);
        mainClassArgsHistory = new History(mainClassArgsKey);
        mainClassArgsTextField.setHistory(mainClassArgsHistory);
        label = new JLabel("Arguments for main class:");
        label.setDisplayedMnemonic('A');
        addLabelAndTextField(applicationPanel, label, mainClassArgsTextField);
        mainPanel.add(applicationPanel);
        JPanel debuggeeVMPanel= Utilities.createPanel("Debuggee VM");
        classPathTextField = new HistoryTextField(30);
        classPathHistory = new History(classPathKey);
        classPathTextField.setHistory(classPathHistory);
        label = new JLabel("Class path:");
        label.setDisplayedMnemonic('C');
        addLabelAndTextField(debuggeeVMPanel, label, classPathTextField);
        addVerticalStrut(debuggeeVMPanel);
        javaHomeTextField = new HistoryTextField(30);
        javaHomeHistory = new History(javaHomeKey);
        javaHomeTextField.setHistory(javaHomeHistory);
        label = new JLabel("Java home:");
        label.setDisplayedMnemonic('J');
        addLabelAndTextField(debuggeeVMPanel, label, javaHomeTextField);
        addVerticalStrut(debuggeeVMPanel);
        javaExecutableTextField = new HistoryTextField(30);
        javaExecutableHistory = new History(javaExecutableKey);
        javaExecutableTextField.setHistory(javaExecutableHistory);
        label = new JLabel("Java executable:");
        label.setDisplayedMnemonic('X');
        addLabelAndTextField(debuggeeVMPanel, label, javaExecutableTextField);
        addVerticalStrut(debuggeeVMPanel);
        vmArgsTextField = new HistoryTextField(30);
        vmArgsHistory = new History(vmArgsKey);
        vmArgsTextField.setHistory(vmArgsHistory);
        label = new JLabel("Arguments for Java executable:");
        label.setDisplayedMnemonic('R');
        addLabelAndTextField(debuggeeVMPanel, label, vmArgsTextField);
        debuggeeVMPanel.add(Box.createVerticalStrut(3));
        startSuspendedCheckBox = new JCheckBox("Start suspended");
        startSuspendedCheckBox.setMnemonic('U');
        startSuspendedCheckBox.addKeyListener(this);
        debuggeeVMPanel.add(startSuspendedCheckBox);
        mainPanel.add(debuggeeVMPanel);
        JPanel sourcePanel = Utilities.createPanel("Source");
        sourcePathTextField = new HistoryTextField(30);
        sourcePathHistory = new History(sourcePathKey);
        sourcePathTextField.setHistory(sourcePathHistory);
        label = new JLabel("Source path:");
        label.setDisplayedMnemonic('S');
        addLabelAndTextField(sourcePanel, label, sourcePathTextField);
        mainPanel.add(sourcePanel);
        addOKCancel();
        Jdb jdb = Jdb.findJdb();
        if (jdb != null) {
            session = jdb.getSession();
        } else {
            session = new JdbSession();
            // Load default session.
            session.loadDefaults();
        }
        setDialogDefaults(session);
        pack();
        mainClassTextField.requestFocus();
    }

    private void addLabelAndTextField(JPanel panel, JLabel label,
        JTextField textField)
    {
        label.setLabelFor(textField);
        if (label.getBorder() == null)
            label.setBorder(new EmptyBorder(0, 0, 3, 0));
        panel.add(label);
        panel.add(textField);
        textField.addKeyListener(this);
    }

    private void addVerticalStrut(JPanel panel)
    {
        panel.add(Box.createVerticalStrut(6));
    }

    private void setDialogDefaults(JdbSession session)
    {
        mainClassTextField.setText(session.getMainClass());
        mainClassArgsTextField.setText(session.getMainClassArgs());
        String classPath = session.getClassPath();
        if (classPath == null || classPath.length() < 1)
            classPath = System.getProperty("class.path");
        classPathTextField.setText(classPath);
        String javaHome = session.getJavaHome();
        if (javaHome == null || javaHome.length() == 0)
            javaHome = System.getProperty("java.home");
        javaHomeTextField.setText(javaHome);
        String javaExecutable = session.getJavaExecutable();
        if (javaExecutable == null || javaExecutable.length() == 0)
            javaExecutable = "java";
        javaExecutableTextField.setText(javaExecutable);
        vmArgsTextField.setText(session.getVMArgs());
        startSuspendedCheckBox.setSelected(session.getStartSuspended());
        sourcePathTextField.setText(session.getSourcePath());
    }

    public JdbSession getSession()
    {
        return session;
    }

    protected void ok()
    {
        String mainClass = mainClassTextField.getText();
        if (mainClass == null || mainClass.length() < 1) {
            mainClassTextField.requestFocus();
            MessageDialog.showMessageDialog("No main class specified", "Error");
            return;
        }
        mainClassHistory.append(mainClass);
        mainClassHistory.save();
        String mainClassArgs = mainClassArgsTextField.getText();
        mainClassArgsHistory.append(mainClassArgs);
        mainClassArgsHistory.save();
        String classPath = classPathTextField.getText();
        List list = Utilities.getDirectoriesInPath(classPath);
        for (Iterator it = list.iterator(); it.hasNext();) {
            String s = (String) it.next();
            File file = File.getInstance(s);
            if (file == null){
                classPathTextField.requestFocus();
                MessageDialog.showMessageDialog(
                    "Invalid class path component \"" + s + '"',
                    "Error");
                return;
            }
            // File might be directory or jar file.
            if (!file.exists()){
                classPathTextField.requestFocus();
                MessageDialog.showMessageDialog(
                    "Class path component \"" + s + "\" does not exist",
                    "Error");
                return;
            }
        }
        classPathHistory.append(classPath);
        classPathHistory.save();
        String javaHome = javaHomeTextField.getText();
        javaHomeHistory.append(javaHome);
        javaHomeHistory.save();
        String javaExecutable = javaExecutableTextField.getText();
        javaExecutableHistory.append(javaExecutable);
        javaExecutableHistory.save();
        String vmArgs = vmArgsTextField.getText();
        vmArgsHistory.append(vmArgs);
        vmArgsHistory.save();
        String sourcePath = sourcePathTextField.getText();
        list = Utilities.getDirectoriesInPath(sourcePath);
        for (Iterator it = list.iterator(); it.hasNext();) {
            String s = (String) it.next();
            File file = File.getInstance(s);
            if (file == null){
                sourcePathTextField.requestFocus();
                MessageDialog.showMessageDialog(
                    "Invalid source path component \"" + s + '"',
                    "Error");
                return;
            }
            if (!file.isDirectory()){
                sourcePathTextField.requestFocus();
                MessageDialog.showMessageDialog(
                    "Source path component \"" + s + "\" does not exist",
                    "Error");
                return;
            }
        }
        sourcePathHistory.append(sourcePath);
        sourcePathHistory.save();
        session.setMainClass(mainClass);
        session.setMainClassArgs(mainClassArgs);
        session.setClassPath(classPath);
        session.setJavaHome(javaHome);
        session.setJavaExecutable(javaExecutable);
        session.setVMArgs(vmArgs);
        session.setStartSuspended(startSuspendedCheckBox.isSelected());
        session.setSourcePath(sourcePath);
        dispose();
    }
}
