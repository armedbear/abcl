/*
 * SeekableStringWriter.java
 *
 * Copyright (C) 2016 Olof-Joachim Frahm
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

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.io.Writer;
import java.text.MessageFormat;

public final class SeekableStringWriter extends Writer {
    private final StringBuffer stringBuffer;
    private int offset = 0;

    public SeekableStringWriter() {
        stringBuffer = new StringBuffer();
    }

    public SeekableStringWriter(int initialSize) {
        stringBuffer = new StringBuffer(initialSize);
    }

    public SeekableStringWriter append(char c) {
        write(c);
        return this;
    }

    public SeekableStringWriter append(CharSequence csq) {
        write(csq.toString());
        return this;
    }

    public SeekableStringWriter append(CharSequence csq, int start, int end) {
        write(csq.subSequence(start, end).toString());
        return this;
    }

    @Override
    public void write(char[] cbuf) {
        _write(cbuf, 0, cbuf.length);
    }

    @Override
    public void write(char[] cbuf, int off, int len) {
        int bufLen = cbuf.length;

        if (off < 0 || off > bufLen || len < 0 || off + len > bufLen)
            throw new IllegalArgumentException();

        _write(cbuf, off, len);
    }

    @Override
    public void write(int c) {
      try {
        if (offset == stringBuffer.length())
            stringBuffer.append((char) c);
        else
            stringBuffer.setCharAt(offset, (char) c);
        ++offset;
      } catch (IndexOutOfBoundsException e) {
        error(new JavaException(e));
      }
    }

    @Override
    public void write(String str) {
        write(str, 0, str.length());
    }

    @Override
    public void write(String str, int off, int len) {
        write(str.toCharArray(), off, len);
    }

    private void _write(char[] cbuf, int off, int len) {
        int strLen = stringBuffer.length();
        int space = strLen - offset;

        int written = Math.min(len, space);

        if (written > 0)
            stringBuffer.replace(offset, offset + written, new String(cbuf, off, written));

        if (written < len)
            stringBuffer.append(cbuf, off + written, len - written);

        offset += len;
    }

    public void seek(int offset) {
        if (offset < 0 || offset > stringBuffer.length())
            throw new IllegalArgumentException();
        this.offset = offset;
    }

    public StringBuffer getBuffer() {
        return stringBuffer;
    }

    public int getOffset() {
        return offset;
    }

    @Override
    public String toString() {
        return stringBuffer.toString();
    }

    @Override
    public void close() {}

    @Override
    public void flush() {}

  public String toStringAndClear() {
    String result = stringBuffer.toString();
    stringBuffer.delete(0, stringBuffer.length());
    offset = 0;
    return result;
  }
    
}
