package org.armedbear.lisp.scripting.util;

import java.io.*;

public class ReaderInputStream extends InputStream {
	
    private final Reader reader;
    private final Writer writer;
    private final PipedInputStream inPipe;
 
    public ReaderInputStream(Reader reader) throws IOException {
        this(reader, null);
    }
 
    public ReaderInputStream(final Reader reader, String encoding) throws IOException {
        this.reader = reader;
        inPipe = new PipedInputStream();
        OutputStream outPipe = new PipedOutputStream(inPipe);
        writer = (encoding == null) ? new OutputStreamWriter(outPipe) : new OutputStreamWriter(outPipe, encoding);
    }
 
    public int read() throws IOException {
    	if(doRead()) {
    		return inPipe.read();
    	} else {
    		return -1;
    	}
    }
 
    public int read(byte b[]) throws IOException {
        return super.read(b);
    }
 
    public int read(byte b[], int off, int len) throws IOException {
    	if(len <= 0) {
    		return 0;
    	}
    	int n = read();
    	if(n == -1) {
    		return -1;
    	} else {
    		b[off] = (byte)n;
    	}
        return 1;
    }
 
    public long skip(long n) throws IOException {
        return super.skip(n);
    }
 
    public int available() throws IOException {
        return 0;
    }
 
    public synchronized void close() throws IOException {
        close(reader);
        close(writer);
        close(inPipe);
    }
 
    private static void close(Closeable cl) {
        try {
            cl.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
 
    private boolean doRead() throws IOException {
    	int n = reader.read();
        if(n == -1) {
        	return false;
        }
        writer.write(n);
        writer.flush();
        return true;
    }
    
}