
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import org.armedbear.lisp.Stream;

/**
 * A bidirectional stream that captures input from a modal dialog. The
 * dialog reports a label (prompt line) which shows to the user
 * everything that has been printed to the stream up to the moment
 * when the dialog became visible. It is usable as a drop-in
 * replacement for e.g. *debug-io*.<br /> This is an abstract class
 * that does not depend on any GUI library. Subclasses are expected to
 * provide the actual code to show the dialog and read input from the
 * user.
 *
 * @author Alessio Stalla
 *
 */
public abstract class DialogPromptStream extends Stream {

	StringWriter writtenSoFar = new StringWriter();
	private Reader reader = new Reader() {

		private StringReader stringReader = null;
		private int inputSize = 0;
		
		public void close() throws IOException {
			closeDialog();
		}

		public int read(char[] cbuf, int off, int len) throws IOException {
			if(stringReader == null) {
				writtenSoFar.flush();
				String promptText = writtenSoFar.toString();
				writtenSoFar.getBuffer().delete(0, Integer.MAX_VALUE);
				String inputStr = readInputFromModalDialog(promptText) + System.getProperty("line.separator", "\n");
				stringReader = new StringReader(inputStr);
				inputSize = inputStr.length();
			}
			int read = stringReader.read(cbuf, off, len);
			if(read != -1) {
				inputSize -= read;
			}
			if(read == -1 || inputSize == 0) {
				inputSize = 0;
				stringReader = null;
			}
			return read;
		}
		
	};

	/**
	 * Inits this stream. Should be called by subclasses' constructors.
	 */
	protected DialogPromptStream() {
		super(org.armedbear.lisp.Symbol.SYSTEM_STREAM);
		initAsCharacterOutputStream(writtenSoFar);
		initAsCharacterInputStream(reader);
	}
	
	/**
	 * Closes the dialog when this stream is closed, aborting the read operation.
	 */
	protected abstract void closeDialog();
	
	/**
	 * Shows the dialog and blocks the calling thread until the user has closed the dialog.
	 * @param promptText the text to be shown to the user (the prompt).
	 * @return a string holding input from the user.
	 */
	protected abstract String readInputFromModalDialog(String promptText); 
	
}
