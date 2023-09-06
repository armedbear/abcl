package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.text.MessageFormat;
import java.util.Arrays;

/**

   A SIMPLE-VECTOR specialized on 8, 16, 32, and 64 unsigned byte
   types backed by a java.nio.Buffer implmentation.

*/
public final class BasicVectorBuffer
  extends BasicVector
{
  //  boolean directAllocation; directly allocate Buffer don't have backing arrays  TODO subclass that behavior
  Buffer data;

  public BasicVectorBuffer(Class type, int capacity) {
    super(type, capacity);
    switch (specializedOn) {
    case U8:
      data = ByteBuffer.allocate(capacity);
      break;
    case U16:
      data = ShortBuffer.allocate(capacity);
      break;
    case U32:
      data = IntBuffer.allocate(capacity);
      break;
    case U64:
      data = LongBuffer.allocate(capacity);
      break;
    }
  }

  // TODO constructor that takes an existing ByteBuffer as its backing store

  public byte[] asByteArray() {
    return (byte[])((ByteBuffer)data).array();
  }
  public short[] asShortArray() {
    return (short[])((ShortBuffer)data).array();
  }
  public int[] asIntArray() {
    return (int[])((IntBuffer)data).array();
  }
  public long[] asLongArray() {
    return (long[])((LongBuffer)data).array();
  }

  public LispObject getDescription() {
    StringBuffer sb = new StringBuffer("A simple vector specialized on ");
    switch (specializedOn) {
    case U8:
      sb.append("(UNSIGNED-BYTE 8)");
      break;
    case U16:
      sb.append("(UNSIGNED-BYTE 16)");
      break;
    case U32:
      sb.append("(UNSIGNED-BYTE 32)");
      break;
    case U64:
      sb.append("(UNSIGNED-BYTE 64)");
      break;
    }
    sb.append(" with ");
    sb.append(capacity);
    sb.append(" elements");
    return new SimpleString(sb);

  }

  @Override
  public LispObject elt(int i) {
    return AREF(i);
  }

  @Override
  public LispObject AREF(int i) {
    try {
      switch (specializedOn) {
      case U8:
        return coerceFromJavaByte(((ByteBuffer)data).get(i));
      case U16:
        return Fixnum.getInstance(Short.toUnsignedInt(((ShortBuffer)data).get(i)));
      case U32:
        return Fixnum.getInstance(Integer.toUnsignedLong(((IntBuffer)data).get(i)));
      case U64:
        return LispInteger.getUnsignedInstance(((LongBuffer)data).get(i));
      }
      return program_error("Bad ELT in BasicVectorBuffer.");
    }  catch (ArrayIndexOutOfBoundsException e) {
      return badIndex(i, capacity);
    }
  }

  @Override
  public void aset(int i, LispObject n) {
    try {
      switch (specializedOn) {
      case U8:
        byte b = coerceToJavaByte(n);
        ((ByteBuffer)data).put(i, b);
        break;
      case U16:
        short s = coerceToJavaUnsignedShort(n);
        ((ShortBuffer)data).put(i, s);
        break;
      case U32:
        int v  = coerceToJavaUnsignedInt(n);
        ((IntBuffer)data).put(i, v);
        break;
      case U64:
        LispInteger lispInteger = LispInteger.coerce(n);
        long l = LispInteger.asUnsignedLong(lispInteger);
        ((LongBuffer)data).put(i, l);
        break;
      }
    } catch (IndexOutOfBoundsException e) {
      badIndex(i, capacity);
    }
  }

  @Override
  public LispObject SVREF(int i) {
    return AREF(i);
  }

  @Override
  public void svset(int i, LispObject newValue) {
    aset(i, newValue);
  }

  @Override
  public LispObject subseq(int start, int end) {
    int length = start - end;
    try {
      BasicVectorBuffer result = null;
      switch (specializedOn) {
      case U8:
        result = new BasicVectorBuffer(ByteBuffer.class, length);
        ((ByteBuffer)data).get(result.asByteArray(), start, length);
        break;
      case U16:
        result = new BasicVectorBuffer(ShortBuffer.class, length);
        ((ShortBuffer)data).get(result.asShortArray(), start, length);
        break;
      case U32:
        result = new BasicVectorBuffer(IntBuffer.class, length);
        ((IntBuffer)data).get(result.asIntArray(), start, length);
        break;
      case U64:
        result = new BasicVectorBuffer(LongBuffer.class, length);
        ((LongBuffer)data).get(result.asLongArray(), start, length);
        break;
      }
      return result;
    } catch (ArrayIndexOutOfBoundsException e) {
      String m
        = MessageFormat.format("The bounding indices {0} and {1} are bad for a sequence of length {2}.", start, end, length());
      return type_error(m, new JavaObject(e), NIL); // Not really a type_error, as there is not one type
    }
  }

  @Override
  public void fill(LispObject obj) { // TODO switch on CLAZZ
    byte b = coerceToJavaByte(obj);
    fill(b);
  }

  public void fill(byte b) {
    Arrays.fill(((ByteBuffer)data).array(), b);
  }

  // Does AbstractVector.deleteEq() could work, as well but is it faster?
  /**
  @Override
  public LispObject deleteEq(LispObject item) {
    byte b = coerceToJavaByte(item);
    return deleteEq(b);
  }

  public LispObject deleteEq(byte b) {
    final int limit = capacity;
    int i = 0;
    int j = 0;
    ByteBuffer buffer = (ByteBuffer) data;
    while (i < limit) {
      byte value = buffer.get(i++);
      if (value != b) {
        buffer.put(j++, value);
      }
    }
    if (j < limit) {
      shrink(j);
    }
    return this;
  }
  */

  //
  // TODO check on use of AbstractVector.deleteEql()
  //
  
  @Override
  public void shrink(int n) {
    if (n < capacity) {
      // thunk on CLAZZ
      BasicVectorBuffer result
        = new BasicVectorBuffer(ByteBuffer.class, n);
      ((ByteBuffer)data).get(result.asByteArray(), 0, n);
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    error(new LispError());
  }

  @Override
  public LispObject reverse() {
    BasicVectorBuffer result = new BasicVectorBuffer(type, capacity);
    int i, j;
    // switch (onSpecialization) {
    // case U8:
      ByteBuffer source = (ByteBuffer)data;
      ByteBuffer destination = (ByteBuffer)result.data;
    //   break;
    // case U16:
    //   break;
    // case U32:
    //   break;
    // case U64:
    //   break;
    // }
    for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
      destination.put(i, source.get(j));
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity() - 1;
    // switch (onSpecialization) {
    // case U8:
    ByteBuffer buffer = (ByteBuffer)data;
    while (i < j) {
      byte temp = buffer.get(i);
      buffer.put(i, buffer.get(j));
      buffer.put(j, temp);
      ++i;
      --j;
    }
    //   break;
    // case U16:
    //   break;
    // case U32:
    //   break;
    // case U64:
    //   break;
    // }

    return this;
  }

  public AbstractVector replace(AbstractVector source,
                                int targetStart, int targetEnd,
                                int sourceStart, int sourceEnd)
  {
    if (source instanceof BasicVectorBuffer) {
      byte[] sourceBytes = (byte[]) ((BasicVectorBuffer)source).data.array();
      System.arraycopy(sourceBytes, sourceStart,
                       data.array(), targetStart,
                       Math.min(targetEnd - targetStart, sourceEnd - sourceStart));
      return this;
    } else {
      return super.replace(source, targetStart, targetEnd, sourceStart, sourceEnd);
    }
  }
}
                                                                
