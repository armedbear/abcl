package awt;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

public class AwtDialogPromptStream extends DialogPromptStream {

	Dialog dialog = new Dialog((Frame)null, true);
	private Label prompt = new Label();
	private TextField input = new TextField(32);
	
	public AwtDialogPromptStream() {
		this("Prompt");
	}
	
	public AwtDialogPromptStream(String title) {
		super();
		dialog.setTitle(title);
		Panel tmpPanel = new Panel();
		tmpPanel.add(prompt);
		tmpPanel.add(input);
		dialog.add(tmpPanel);
		JButton okBtn = new JButton("Ok");
		okBtn.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				synchronized(dialog) {
					dialog.dispose();
				}
			}
		});
		tmpPanel = new Panel(new FlowLayout());
		tmpPanel.add(okBtn);
		dialog.add(tmpPanel, BorderLayout.SOUTH);
	}
	
	@Override
	protected void closeDialog() {
		dialog.dispose();
	}
	
	@Override
	protected String readInputFromModalDialog(String promptText) {
		prompt.setText(promptText);
		dialog.pack();
		dialog.setVisible(true);
		return input.getText();
	}
	
}
