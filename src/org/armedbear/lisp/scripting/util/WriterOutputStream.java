package org.armedbear.lisp.scripting.util;

import java.io.*;

public class WriterOutputStream extends OutputStream {
	
    private final Reader reader;
    private final Writer writer;
    private final PipedOutputStream outPipe;
 
    public WriterOutputStream(Writer writer) throws IOException {
        this(writer, null);
    }
 
    public WriterOutputStream(final Writer writer, String encoding) throws IOException {
        this.writer = writer;
        outPipe = new PipedOutputStream();
        InputStream inPipe = new PipedInputStream(outPipe);
        reader = (encoding == null) ? new InputStreamReader(inPipe) : new InputStreamReader(inPipe, encoding);
    }
 
	@Override
	public void write(int b) throws IOException {
		doWrite(b);
		writer.flush();
	}
    
    @Override
	public void flush() throws IOException {
		super.flush();
	}

	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		super.write(b, off, len);
	}

	@Override
	public void write(byte[] b) throws IOException {
		super.write(b);
	}

	public synchronized void close() throws IOException {
        close(reader);
        close(writer);
        close(outPipe);
    }
 
    private static void close(Closeable cl) {
        try {
            cl.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
 
    private void doWrite(int n) throws IOException {
    	outPipe.write(n);
    	outPipe.flush();
    	n = reader.read();
    	writer.write(n);
    }
    
}