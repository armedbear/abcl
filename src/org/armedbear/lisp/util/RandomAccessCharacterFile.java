/*
 * RandomAccessCharacterFile.java
 *
 * Copyright (C) 2008 Hideo at Yokohama
 * Copyright (C) 2008-2009 Erik Huelsmann
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
import org.armedbear.lisp.Debug;

public class RandomAccessCharacterFile {

    private class RandomAccessInputStream extends PushbackInputStream {

        public RandomAccessInputStream() {
            super(null);
        }

        private byte[] read_buf = new byte[1];

        @Override
        public final int read() throws IOException {
            int len = read(read_buf);
            if (len == 1) {
                // byte is signed, char is unsigned, int is signed.
                // buf can hold 0xff, we want it as 0xff in int, not -1.
                return 0xff & (int) read_buf[0];
            } else {
                return -1;
            }
            // ### BUG: 'int read()' is to return a *codepoint*,
            // not the half of a surrogate pair!
        }

        @Override
        public final int read(byte[] b, int off, int len) throws IOException {
            return RandomAccessCharacterFile.this.read(b, off, len);
        }

        @Override
        public final void unread(int b) throws IOException {
            RandomAccessCharacterFile.this.unreadByte((byte)b);
        }

        @Override
        public final void unread(byte[] b, int off, int len) throws IOException {
            for (int i = 0; i < len; i++)
                this.unread(b[off+i]);
        }

        @Override
        public final void unread(byte[] b) throws IOException {
            this.unread(b, 0, b.length);
        }

        @Override
        public final int available() throws IOException {
            return (int)(RandomAccessCharacterFile.this.length()
                            - RandomAccessCharacterFile.this.position());
        }

        @Override
        public final synchronized void mark(int readlimit) {
        }

        @Override
        public final boolean markSupported() {
            return false;
        }

        @Override
        public final synchronized void reset() throws IOException {
            throw new IOException("Operation not supported");
        }

        @Override
        public final long skip(long n) throws IOException {
            RandomAccessCharacterFile.this.position(RandomAccessCharacterFile.this.position()+n);
            return n;
        }

        @Override
        public final int read(byte[] b) throws IOException {
            return this.read(b, 0, b.length);
        }

        @Override
        public final void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }
    }

    private class RandomAccessOutputStream extends OutputStream {

        RandomAccessOutputStream() {
        }

        private byte[] buf = new byte[1];
        public final void write(int b) throws IOException {
            buf[0] = (byte)b;
            RandomAccessCharacterFile.this.write(buf, 0, 1);
        }

        @Override
        public final void write(byte[] b) throws IOException {
            RandomAccessCharacterFile.this.write(b, 0, b.length);
        }

        @Override
        public final void write(byte[] b, int off, int len) throws IOException {
            RandomAccessCharacterFile.this.write(b, off, len);
        }

        @Override
        public final void flush() throws IOException {
            RandomAccessCharacterFile.this.flush();
        }

        @Override
        public final void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }
    }

    // dummy reader which we need to call the Pushback constructor
    // because a null value won't work
    static Reader staticReader = new StringReader("");

    private class RandomAccessReader extends PushbackReader {

        RandomAccessReader() {
                // because we override all methods of Pushbackreader,
                // staticReader will never be referenced
                super(staticReader);
        }

        @Override
        public final void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }

        private char[] read_buf = new char[1];

        @Override
        public final int read() throws IOException {
            int n = this.read(read_buf);

            if (n == 1)
                return read_buf[0];
            else
                return -1;
            // ### BUG: 'int read()' is to return a codepoint!
            // not the half of a surrogate pair!
        }

        @Override
        public final void unread(int c) throws IOException {
            RandomAccessCharacterFile.this.unreadChar((char)c);
        }

        @Override
        public final void unread(char[] cbuf, int off, int len) throws IOException {
            for (int i = 0; i < len; i++)
                this.unread(cbuf[off+i]);
        }

        @Override
        public final void unread(char[] cbuf) throws IOException {
            this.unread(cbuf, 0, cbuf.length);
        }

        @Override
        public final int read(CharBuffer target) throws IOException {
            //FIXME: to be implemented
            throw new IOException("Not implemented");
        }

        @Override
        public final int read(char[] cbuf) throws IOException {
            return RandomAccessCharacterFile.this.read(cbuf, 0, cbuf.length);
        }

        @Override
        public final int read(char[] cb, int off, int len) throws IOException {
            return RandomAccessCharacterFile.this.read(cb, off, len);
        }

        @Override
        public final boolean ready() throws IOException {
            return true;
        }
    }

    private class RandomAccessWriter extends Writer {

        RandomAccessWriter() {
        }

        public final void close() throws IOException {
            RandomAccessCharacterFile.this.close();
        }

        public final void flush() throws IOException {
            RandomAccessCharacterFile.this.flush();
        }

        @Override
        public final void write(char[] cb, int off, int len) throws IOException {
            RandomAccessCharacterFile.this.write(cb, off, len);
        }

    }


    final static int BUFSIZ = 4*1024; // setting this to a small value like 8 is helpful for testing.

    private RandomAccessWriter writer;
    private RandomAccessReader reader;
    private RandomAccessInputStream inputStream;
    private RandomAccessOutputStream outputStream;
    private FileChannel fcn;

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
    private boolean bbufIsReadable; /* whether bbuf.remaining() contains readable content. */
    private long bbufpos; /* where the beginning of bbuf is pointing in the file now. */

    public RandomAccessCharacterFile(RandomAccessFile raf, String encoding) throws IOException {

        fcn = raf.getChannel();

        setEncoding(encoding);
        bbuf = ByteBuffer.allocate(BUFSIZ);

        // there is no readable data available in the buffers.
        bbuf.flip();

        // there is no write pending data in the buffers.
        bbufIsDirty = false;

        bbufIsReadable = false;

        bbufpos = fcn.position();

        reader = new RandomAccessReader();
        writer = new RandomAccessWriter();
        inputStream = new RandomAccessInputStream();
        outputStream = new RandomAccessOutputStream();
    }

    public void setEncoding(String encoding) {
        cset = (encoding == null)
            ? Charset.defaultCharset() : Charset.forName(encoding);
        cdec = cset.newDecoder();
        cdec.onMalformedInput(CodingErrorAction.REPLACE);
        cdec.onUnmappableCharacter(CodingErrorAction.REPLACE);
        cenc = cset.newEncoder();
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

    public final void close() throws IOException {
        internalFlush(true);
        fcn.close();
    }

    public final void flush() throws IOException {
        internalFlush(false);
    }

    private final boolean ensureReadBbuf(boolean force) throws IOException {
        boolean bufReady = true;

        if ((bbuf.remaining() == 0) || force || ! bbufIsReadable) {
            // need to read from the file.

            if (bbufIsDirty) {
                bbuf.flip();
                fcn.position(bbufpos);
                fcn.write(bbuf);
                bbufpos += bbuf.position();
                bbuf.clear();
            } else {
                int bbufEnd = bbufIsReadable ? bbuf.limit() : bbuf.position();
                fcn.position(bbufpos + bbufEnd);
                bbufpos += bbuf.position();
                bbuf.compact();
            }

            bufReady = (fcn.read(bbuf) != -1);
            bbuf.flip();
            bbufIsReadable = true;
        }

        return bufReady;
    }

    final int read(char[] cb, int off, int len) throws IOException {
        CharBuffer cbuf = CharBuffer.wrap(cb, off, len);
        boolean decodeWasUnderflow = false;
        boolean atEof = false;
        while ((cbuf.remaining() > 0) && ! atEof) {
            int oldRemaining = cbuf.remaining();
            atEof = ! ensureReadBbuf(decodeWasUnderflow);
            CoderResult r = cdec.decode(bbuf, cbuf, atEof );
            if (oldRemaining == cbuf.remaining()
                && CoderResult.OVERFLOW == r) {
                // if this happens, the decoding failed
                // but the bufs didn't advance. Advance
                // them manually and do manual replacing,
                // otherwise we loop endlessly. This occurs
                // at least when parsing latin1 files with
                // lowercase o-umlauts in them
                // Note that this is at the moment copy-paste
                // with DecodingReader.read()
                cbuf.put('?');
                bbuf.get();
            }
            decodeWasUnderflow = (CoderResult.UNDERFLOW == r);
        }
        if (cbuf.remaining() == len) {
            return -1;
        } else {
            return len - cbuf.remaining();
        }
    }

    final void write(char[] cb, int off, int len) throws IOException {
        CharBuffer cbuf = CharBuffer.wrap(cb, off, len);
        encodeAndWrite(cbuf, false, false);
    }

    private final void internalFlush(boolean endOfFile) throws IOException {
        if (endOfFile) {
            CharBuffer cbuf = CharBuffer.allocate(0);
            encodeAndWrite(cbuf, true, endOfFile);
        } else {
            flushBbuf(false);
        }
    }

    private final void encodeAndWrite(CharBuffer cbuf, boolean flush,
                                      boolean endOfFile) throws IOException {
        while (cbuf.remaining() > 0) {
            CoderResult r = cenc.encode(cbuf, bbuf, endOfFile);
            bbufIsDirty = true;
            if (CoderResult.OVERFLOW == r || bbuf.remaining() == 0) {
                flushBbuf(false);
                bbuf.clear();
            }
            if (r.isUnmappable()) {
                throw new RACFUnmappableCharacterException(cbuf.position(),
                                                           cbuf.charAt(cbuf.position()),
                                                           cset.name());
            }
            if (r.isMalformed()) {
                // We don't really expect Malformed, but not handling it
                // will cause an infinite loop if we don't...
                throw new RACFMalformedInputException(cbuf.position(),
                                                      cbuf.charAt(cbuf.position()),
                                                      cset.name());
            }
            // UNDERFLOW is the normal condition where cbuf runs out
            // before bbuf is filled.
        }
        if (bbuf.position() > 0 && bbufIsDirty && flush) {
            flushBbuf(false);
        }
    }

    public final void position(long newPosition) throws IOException {
        flushBbuf(true);
        long bbufend = bbufpos // in case bbuf is readable, its contents is valid
            + (bbufIsReadable ? bbuf.limit() : bbuf.position()); // beyond position()
        if (newPosition >= bbufpos && newPosition < bbufend) {
            // near seek. within existing data of bbuf.
            bbuf.position((int)(newPosition - bbufpos));
        } else {
            fcn.position(newPosition);
            // far seek; discard the buffer (it's already cleared)
            bbuf.clear();
            bbuf.flip(); // "there is no useful data on this buffer yet."
            bbufpos = newPosition;
        }
    }

    public final long position() throws IOException {
        return bbufpos + bbuf.position(); // the logical position within the file.
    }

    public final long length() throws IOException {
        flushBbuf(false);
        return fcn.size();
    }

    private final void flushBbuf(boolean commitOnly) throws IOException {
        if (! bbufIsDirty)
            return;

        fcn.position(bbufpos);

        // if the buffer is dirty, the modifications have to be
        // before position(): before re-positioning, this.position()
        // calls this function.
        if (commitOnly || bbufIsReadable) {
            ByteBuffer dup = bbuf.duplicate();
            dup.flip();
            fcn.write(dup);
            return;
        }
        bbuf.flip();
        fcn.write(bbuf);

        bbufpos += bbuf.position();
        bbuf.clear();
        bbuf.flip(); // there's no useable data in this buffer
        bbufIsDirty = false;
        bbufIsReadable = false;
    }

    public final int read(byte[] b, int off, int len) throws IOException {
        int pos = off;
        boolean atEof = false;
        while (pos - off < len && ! atEof) {

            atEof = ! ensureReadBbuf(false);
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
    public final void unreadChar(char c) throws IOException {
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

    public final void unreadByte(byte b) throws IOException {
        long pos = position() - 1;
        position(pos);
    }

    final void write(byte[] b, int off, int len) throws IOException {
        int pos = off;
        while (pos < off + len) {
            int want = len - pos + off;
            if (want > bbuf.remaining()) {
                want = bbuf.remaining();
            }
            bbuf.put(b, pos, want);
            pos += want;
            bbufIsDirty = true;
            if (bbuf.remaining() == 0) {
                flushBbuf(false);
                bbuf.clear();
            }
        }
    }
}
