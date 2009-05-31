/*
 * RandomAccessCharacterFile.java
 *
 * Copyright (C) 2008 Hideo at Yokohama
 * Copyright (C) 2008 Erik Huelsmann
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp.util;

import java.io.IOException;
import java.io.PushbackInputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

public class RandomAccessCharacterFile {

    private class RandomAccessInputStream extends PushbackInputStream {

        public RandomAccessInputStream() {
            super(null);
        }
        
        private byte[] read_buf = new byte[1];

        @Override
        public int read() throws IOException {
            int len = read(read_buf);
            if (len == 1) {
                // byte is signed, char is unsigned, int is signed.
                // buf can hold 0xff, we want it as 0xff in int, not -1.
                return 0xff & (int) read_buf[0];
            } else {
                return -1;
            }
        }
                
        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            return RandomAccessCharacterFile.this.read(b, off, len);
        }

        @Override
        public void unread(int b) throws IOException {
            RandomAccessCharacterFile.this.unreadByte((byte)b);
        }

        @Override
        public void unread(byte[] b, int off, int len) throws IOException {
            for (int i = 0; i < len; i++)
                this.unread(b[off+i]);
        }

        @Override
        public void unread(byte[] b) throws IOException {
            this.unread(b, 0, b.length);
        }

        @Override
        public int available() throws IOException {
            return (int)(RandomAccessCharacterFile.this.length()
                            - RandomAccessCharacterFile.this.position());
        }

        @Override
        public synchronized void mark(int readlimit) {
        }

        @Override
        public boolean markSupported() {
            return false;
        }

        @Override
        public synchronized void reset() throws IOException {
            throw new IOException("Operation not supported");
        }

        @Override
        public long skip(long n) throws IOException {
            RandomAccessCharacterFile.this.position(RandomAccessCharacterFile.this.position()+n);
            return n;
        }

        @Override
        public int read(byte[] b) throws IOException {
            return this.read(b, 0, b.length);
        }

        @Override
        public void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }
    }

    private class RandomAccessOutputStream extends OutputStream {

        private RandomAccessOutputStream() {
        }

        private byte[] buf = new byte[1];
        public void write(int b) throws IOException {
            buf[0] = (byte)b;
            write(buf);
        }

        @Override
            public void write(byte[] b, int off, int len) throws IOException {
            RandomAccessCharacterFile.this.write(b, off, len);
        }

        @Override
        public void flush() throws IOException {
            RandomAccessCharacterFile.this.flush();
        }

        @Override
        public void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }
    }
    
    // dummy reader which we need to call the Pushback constructor
    // because a null value won't work
    private static Reader staticReader = new StringReader("");
    
    private class RandomAccessReader extends PushbackReader {

        private RandomAccessReader() {
                // because we override all methods of Pushbackreader,
                // staticReader will never be referenced
                super(staticReader);
        }

            @Override
        public void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }
        
        private char[] read_buf = new char[1];

        @Override
        public int read() throws IOException {
            int n = this.read(read_buf);
            
            if (n == 1)
                return read_buf[0];
            else
                return -1;
        }

        @Override
        public void unread(int c) throws IOException {
            RandomAccessCharacterFile.this.unreadChar((char)c);
        }

        @Override
        public void unread(char[] cbuf, int off, int len) throws IOException {
            for (int i = 0; i < len; i++)
                this.unread(cbuf[off+i]);
        }

        @Override
        public void unread(char[] cbuf) throws IOException {
            this.unread(cbuf, 0, cbuf.length);
        }

        @Override
        public int read(CharBuffer target) throws IOException {
            //FIXME: to be implemented
            throw new IOException("Not implemented");
        }

        @Override
        public int read(char[] cbuf) throws IOException {
            return RandomAccessCharacterFile.this.read(cbuf, 0, cbuf.length);
        }
        
        @Override
        public int read(char[] cb, int off, int len) throws IOException {
            return RandomAccessCharacterFile.this.read(cb, off, len);
        }
    }

    private class RandomAccessWriter extends Writer {

        private RandomAccessWriter() {
        }

        public void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }

        public void flush() throws IOException {
            RandomAccessCharacterFile.this.flush();
        }

        @Override
            public void write(char[] cb, int off, int len) throws IOException {
            RandomAccessCharacterFile.this.write(cb, off, len);
        }

    }


    final static int BUFSIZ = 4*1024; // setting this to a small value like 8 is helpful for testing.
	
    private RandomAccessWriter writer;
    private RandomAccessReader reader;
    private RandomAccessInputStream inputStream;
    private RandomAccessOutputStream outputStream;
    private FileChannel fcn;
    private long fcnpos; /* where fcn is pointing now. */
    private long fcnsize; /* the file size */
	
    private Charset cset;
    private CharsetEncoder cenc;
    private CharsetDecoder cdec;
	
    /**
     * bbuf is treated as a cache of the file content.
     * If it points to somewhere in the middle of the file, it holds the copy of the file content,
     * even when you are writing a large chunk of data.  If you write in the middle of a file,
     * bbuf first gets filled with contents of the data, and only after that any new data is
     * written on bbuf.
     * The exception is when you are appending data at the end of the file.
     */
    private ByteBuffer bbuf;
    private boolean bbufIsDirty; /* whether bbuf holds data that must be written. */
    private long bbufpos; /* where the beginning of bbuf is pointing in the file now. */

    public RandomAccessCharacterFile(RandomAccessFile raf, String encoding) throws IOException {

        fcn = raf.getChannel();
        fcnpos = fcn.position();
        fcnsize = fcn.size();

        cset = (encoding == null) ? Charset.defaultCharset() : Charset.forName(encoding);
        cdec = cset.newDecoder();
        cdec.onMalformedInput(CodingErrorAction.REPLACE);
        cdec.onUnmappableCharacter(CodingErrorAction.REPLACE);
        cenc = cset.newEncoder();

        bbuf = ByteBuffer.allocate(BUFSIZ);

        // there is no readable data available in the buffers.
        bbuf.flip();

        // there is no write pending data in the buffers.
        bbufIsDirty = false;

        bbufpos = fcn.position();

        reader = new RandomAccessReader();
        writer = new RandomAccessWriter();
        inputStream = new RandomAccessInputStream();
        outputStream = new RandomAccessOutputStream();
    }
	
    public Writer getWriter() {
        return writer;
    }
	
    public PushbackReader getReader() {
        return reader;
    }
	
    public PushbackInputStream getInputStream() {
        return inputStream;
    }
	
    public OutputStream getOutputStream() {
        return outputStream;
    }
	
    public void close() throws IOException {
        internalFlush(true);
        fcn.close();
    }
	
    public void flush() throws IOException {
        internalFlush(false);
    }

    private int read(char[] cb, int off, int len) throws IOException {
        CharBuffer cbuf = CharBuffer.wrap(cb, off, len);
        boolean decodeWasUnderflow = false;
        boolean atEof = false;
        while ((cbuf.remaining() > 0) && dataIsAvailableForRead() && ! atEof) {
            if ((bbuf.remaining() == 0) || decodeWasUnderflow) {
                // need to read from the file.

                if (bbufIsDirty) {
                    bbuf.flip();
                    fcn.position(bbufpos);
                    fcn.write(bbuf);
                    bbufpos = bbufpos+bbuf.position();
                    bbuf.clear();
                } else {
                    fcn.position(bbufpos + bbuf.limit());
                    bbufpos += bbuf.position();
                    bbuf.compact();
                }

                atEof = (fcn.read(bbuf) == -1);
                fcnpos = fcn.position();
                // update bbufpos.
                bbuf.flip();
            }
            CoderResult r = cdec.decode(bbuf, cbuf, pointingAtEOF() );
            decodeWasUnderflow = (CoderResult.UNDERFLOW == r);
        }
        if (cbuf.remaining() == len) {
            return -1;
        } else {
            return len - cbuf.remaining();
        }
    }

    private boolean dataIsAvailableForRead() throws IOException {
        return ((bbuf.remaining() > 0) || (fcn.position() < fcn.size()));
    }
	
    private boolean pointingAtEOF() {
        return (bbuf.remaining() == 0) && (fcnpos == fcnsize);
    }

    private void write(char[] cb, int off, int len) throws IOException {
        CharBuffer cbuf = CharBuffer.wrap(cb, off, len);
        encodeAndWrite(cbuf, false, false);
    }

    private void internalFlush(boolean endOfFile) throws IOException {
        if (endOfFile) {
            CharBuffer cbuf = CharBuffer.allocate(0);
            encodeAndWrite(cbuf, true, endOfFile);
        } else {
            flushBbuf();
        }
    }

    private void encodeAndWrite(CharBuffer cbuf, boolean flush, boolean endOfFile) throws IOException {
        if (bbufpos == fcnsize) {
            bbuf.clear();
        }
        while (cbuf.remaining() > 0) {
            CoderResult r = cenc.encode(cbuf, bbuf, endOfFile);
            bbufIsDirty = true;
            long curpos = bbufpos + bbuf.position();
            if (curpos > fcnsize) {
                // the file is extended.
                fcnsize = curpos;
            }
            if (CoderResult.OVERFLOW == r || bbuf.remaining() == 0) {
                flushBbuf();
                bbufpos += bbuf.limit();
                bbuf.clear();
                if (fcnpos < fcnsize) {
                    fcn.read(bbuf);
                    bbuf.flip();
                    fcnpos += bbuf.remaining();
                }
            // if we are at the end of file, bbuf is simply cleared.
            // in that case, bbufpos + bbuf.position points to the EOF, not fcnpos.
            }
        }
        if (bbuf.position() > 0 && bbufIsDirty && flush) {
            flushBbuf();
        }
    }

    public void position(long newPosition) throws IOException {
        flushBbuf();
        long bbufend = bbufpos + bbuf.limit();
        if (newPosition >= bbufpos && newPosition < bbufend) {
            // near seek. within existing data of bbuf.
            bbuf.position((int)(newPosition - bbufpos));
        } else {
            // far seek. discard the buffer.
            flushBbuf();
            fcn.position(newPosition);
            fcnpos = newPosition;
            bbuf.clear();
            bbuf.flip(); // "there is no useful data on this buffer yet."
            bbufpos = fcnpos;
        }
    }
	
    public long position() throws IOException {
        flushBbuf();
        return bbufpos + bbuf.position(); // the logical position within the file.
    }

    public long length() throws IOException {
        flushBbuf();
        return fcn.size();
    }

    private void flushBbuf() throws IOException {
        if (! bbufIsDirty)
            return;

        if (fcnpos != bbufpos)
            fcn.position(bbufpos);

        bbuf.position(0);
        if (bbufpos + bbuf.limit() > fcnsize) {
            // the buffer is at the end of the file.
            // area beyond fcnsize does not have data.
            bbuf.limit((int)(fcnsize - bbufpos));
        }
        fcn.write(bbuf);
        fcnpos = bbufpos + bbuf.limit();
        bbufIsDirty = false;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        int pos = off;
        boolean atEof = false;
        while (pos - off < len && dataIsAvailableForRead()
               && ! atEof) {
            if (bbuf.remaining() == 0) {
                // need to read from the file.
                flushBbuf(); // in case bbuf is dirty.
                // update bbufpos.
                bbufpos += bbuf.limit();
                // if reads and writes are mixed, we may need to seek first.
                if (bbufpos != fcnpos) {
                    fcn.position(bbufpos);
                }
                // need to read data from file.
                bbuf.clear();
                atEof = (fcn.read(bbuf) == -1);
                bbuf.flip();
                fcnpos = bbufpos + bbuf.remaining();
            }
            int want = len - pos;
            if (want > bbuf.remaining()) {
                want = bbuf.remaining();
            }
            bbuf.get(b, pos, want);
            pos += want;
        }
        return pos - off;
    }
        
    // a method corresponding to the good ol' ungetc in C.
    // This function may fail when using (combined) character codes that use
    // escape sequences to switch between sub-codes.
    // ASCII, ISO-8859 series, any 8bit code are OK, all unicode variations are OK,
    // but applications of the ISO-2022 encoding framework can have trouble.
    // Example of such code is ISO-2022-JP which is used in Japanese e-mail.
    private CharBuffer singleCharBuf;
    private ByteBuffer shortByteBuf;
    public void unreadChar(char c) throws IOException {
        // algorithm :
        //  1. encode c into bytes, to find out how many bytes it corresponds to
        //  2. move the position backwards that many bytes.
        //  ** we stop here.  Don't bother to write the bytes to the buffer,
        //     assuming that it is the same as the original data.
        //     If we allow to write back different characters, the buffer must get 'dirty'
        //     but that would require read/write permissions on files you use unreadChar,
        //     even if you are just reading for some tokenizer.
        //
        //  So we don't do the following.
        //  3. write the bytes.
        //  4. move the position back again.
        if (singleCharBuf == null) {
            singleCharBuf = CharBuffer.allocate(1);
            shortByteBuf = ByteBuffer.allocate((int)cenc.maxBytesPerChar());
        }
        singleCharBuf.clear();
        singleCharBuf.append(c);
        singleCharBuf.flip();
        shortByteBuf.clear();
        cenc.encode(singleCharBuf, shortByteBuf, false);
        int n = shortByteBuf.position();
        long pos = position() - n;
        position(pos);
    }
	
    public void unreadByte(byte b) throws IOException {
        long pos = position() - 1;
        position(pos);
    }

    private void write(byte[] b, int off, int len) throws IOException {
        int pos = off;
        if (len > bbuf.limit()) {
            if (bbufIsDirty)
                flushBbuf();
            fcn.write(ByteBuffer.wrap(b, off, len));
            fcnpos = fcn.position();
            if (fcnpos > fcnsize)
                fcnsize = fcnpos;
        }
        while (pos < off + len) {
            int want = len;
            if (want > bbuf.remaining()) {
                want = bbuf.remaining();
            }
            bbuf.put(b, pos, want);
            pos += want;
            bbufIsDirty = true;
            long curpos = bbufpos + bbuf.position();
            if (curpos > fcnsize) {
                // the file is extended.
                fcnsize = curpos;
            }
            if (bbuf.remaining() == 0) {
                flushBbuf();
                bbufpos += bbuf.limit();
                bbuf.clear();
                if (fcn.position() < fcnsize) {
                    bbufpos = fcn.position();
                    fcn.read(bbuf);
                    bbuf.flip();
                    fcnpos += bbuf.remaining();
                }
                // if we are at the end of file, bbuf is simply cleared.
                // in that case, bbufpos + bbuf.position points to the EOF, not fcnpos.
            }
        }
    }
}
