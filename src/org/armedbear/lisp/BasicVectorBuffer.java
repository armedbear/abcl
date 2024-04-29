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
// Only code paths for (UNSIGNED-BYTE 8) types right now.
public final class BasicVectorBuffer
  extends BasicVector
{
  //  boolean directAllocation; directly allocate Buffer don't have backing arrays  TODO subclass that behavior


  /** The u8 bytes for this vector */
  ByteBuffer bytes;  

  /** A view of the underlying bytes by specialization */
  Buffer data;

  public BasicVectorBuffer(Class type, int capacity) {
    super(type, capacity);
    bytes = ByteBuffer.allocate(capacity * specializedOn.totalBytes);

    switch (specializedOn) {
    case U8:
      data = bytes;
      break;
    case U16:
      data = bytes.asShortBuffer();
      break;
    case U32:
      data = bytes.asIntBuffer();
      break;
    case U64:
      data = bytes.asLongBuffer();
      break;
    }
  }

  byte[] asByteArray() {
    if (data.hasArray()) {
      return (byte[])data.array();
    }
    program_error("Unable to get underlying bytes for BasicVectorBuffer.");
    // not reached
    return null;
  }

  short[] asShortArray() {
    if (data.hasArray()) {
      return (short[])data.array();
    }
    program_error("Unable to get underlying shorts for BasicVectorBuffer.");
    // not reached
    return null;
  }

  int[] asIntArray() {
    if (data.hasArray()) {
      return (int[])data.array();
    }
    program_error("Unable to get underlying ints for BasicVectorBuffer.");
    // not reached
    return null;
  }

  long[] asLongArray() {
    if (data.hasArray()) {
      return (long[])data.array();
    }
    program_error("Unable to get underlying longs for BasicVectorBuffer.");
    // not reached
    return null;
  }


  // TODO constructor that takes an existing ByteBuffer as its backing store

  // public byte[] asByteArray() {
  //   return (byte[])((ByteBuffer)data).array();
  // }
  // public short[] asShortArray() {
  //   return (short[])((ShortBuffer)data).array();
  // }
  // public int[] asIntArray() {
  //   return (int[])((IntBuffer)data).array();
  // }
  // public long[] asLongArray() {
  //   return (long[])((LongBuffer)data).array();
  // }

  public LispObject getDescription() {
    StringBuffer sb
      = new StringBuffer("A simple vector baced with a java.nio.Buffer implementation. ")
      .append("\n");
    sb.append("Whose superimplementation is ").append("\n")
      .append(super.getDescription());

    return new SimpleString(sb);
  }

  @Override
  public LispObject elt(int i) {
    return SVREF(i);
  }

  @Override
  public LispObject SVREF(int i) {
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
      return program_error("Bad array reference in BasicVectorBuffer for " + i);
    }  catch (ArrayIndexOutOfBoundsException e) {
      return badIndex(i, capacity);
    }
  }

  @Override
  public void svset(int i, LispObject n) {
    LispInteger o = coerceToElementType(n);
    try {
      switch (specializedOn) {
      case U8:
        byte b = coerceToJavaByte(o);
        ((ByteBuffer)data).put(i, b);
        break;
      case U16:
        short s = coerceToJavaUnsignedShort(o);
        ((ShortBuffer)data).put(i, s);
        break;
      case U32:
        int v  = coerceToJavaUnsignedInt(o);
        ((IntBuffer)data).put(i, v);
        break;
      case U64:
        long l = LispInteger.asUnsignedLong(o);
        ((LongBuffer)data).put(i, l);
        break;
      }
    } catch (IndexOutOfBoundsException e) {
      badIndex(i, capacity);
    }
  }

  @Override
  public LispObject AREF(int i) {
    return SVREF(i);
  }

  @Override
  public void aset(int i, LispObject newValue) {
    svset(i, newValue);
  }

  @Override
  public LispObject subseq(int start, int end) {
    int length = end - start;
    try {
      BasicVectorBuffer result = null;
      switch (specializedOn) {
      case U8:
        result = new BasicVectorBuffer(Byte.class, length);
        break;
      case U16:
        result = new BasicVectorBuffer(Short.class, length);
        break;
      case U32:
        result = new BasicVectorBuffer(Integer.class, length);
        break;
      case U64:
        result = new BasicVectorBuffer(Long.class, length);
        break;
      }
      result.bytes.put(asByteArray(), start, length * specializedOn.totalBytes);      
      return result;
    } catch (ArrayIndexOutOfBoundsException e) {
      String m
        = MessageFormat.format("The bounding indices {0} and {1} are bad for a sequence of length {2}.",
                               start, end, length());
      // Not really a type_error, as there is not one type
      return type_error(m, new JavaObject(e), NIL);
    }
  }

  @Override
  public void fill(LispObject obj) { 
    LispInteger o = coerceToElementType(obj);
    switch (specializedOn) {
    case U8:
      byte b = coerceToJavaByte(o);
      Arrays.fill(asByteArray(), b);
      break;
    case U16:
      short s = coerceToJavaUnsignedShort(o);
      Arrays.fill(asShortArray(), s);
      break;
    case U32:
      int i = coerceToJavaUnsignedShort(o);
      Arrays.fill(asIntArray(), i);
      break;
    case U64:
      long l = LispInteger.asUnsignedLong(o);
      Arrays.fill(asLongArray(), l);      
      break;
    }
  }

  // AbstractVector.deleteEq() should work, as well but is it faster?
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

  // TODO check on use of AbstractVector.deleteEql()
  
  @Override
  public void shrink(int n) {
    if (n < capacity) {
      bytes.limit(n * specializedOn.totalBytes);      
      switch (specializedOn) {
      case U8:
        break;
      case U16:
        data = bytes.asShortBuffer();
        data.limit(n);
        break;
      case U32:
        data = bytes.asIntBuffer();
        data.limit(n);
        break;
      case U64:
        data = bytes.asLongBuffer();
        data.limit(n);
        break;
      }
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    simple_error("Unable to shrink vector ~a to size ~a.", this, n);
  }

  @Override
  public LispObject reverse() {
    BasicVectorBuffer result = null;
    int i, j;
    switch (specializedOn) {
    case U8:
      result = new BasicVectorBuffer(Byte.class, capacity);
      ByteBuffer byteSource = bytes;
      ByteBuffer byteDestination = result.bytes;
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        byteDestination.put(i, byteSource.get(j));
      }
      break;
    case U16:
      result = new BasicVectorBuffer(Short.class, capacity);
      ShortBuffer shortSource = (ShortBuffer)data;
      ShortBuffer shortDestination = (ShortBuffer)result.data;
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        shortDestination.put(i, shortSource.get(j));
      }
      break;
    case U32:
      result = new BasicVectorBuffer(Integer.class, capacity);
      IntBuffer intSource = (IntBuffer)data;
      IntBuffer intDestination = (IntBuffer)result.data;
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        intDestination.put(i, intSource.get(j));
      }
      break;
    case U64:
      result = new BasicVectorBuffer(Long.class, capacity);
      LongBuffer longSource = (LongBuffer)data;
      LongBuffer longDestination = (LongBuffer)result.data;
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        longDestination.put(i, longSource.get(j));
      }
      break;
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity() - 1;
    switch (specializedOn) {
    case U8:
      ByteBuffer byteBuffer = (ByteBuffer)data;
      while (i < j) {
        byte temp = byteBuffer.get(i);
        byteBuffer.put(i, byteBuffer.get(j));
        byteBuffer.put(j, temp);
        ++i; --j;
      }
      break;
    case U16:
      ShortBuffer shortBuffer = (ShortBuffer)data;
      while (i < j) {
        short temp = shortBuffer.get(i);
        shortBuffer.put(i, shortBuffer.get(j));
        shortBuffer.put(j, temp);
        ++i; --j;
      }
      break; 
    case U32:
      IntBuffer intBuffer = (IntBuffer)data;
      while (i < j) {
        int temp = intBuffer.get(i);
        intBuffer.put(i, intBuffer.get(j));
        intBuffer.put(j, temp);
        ++i; --j;
      }
      break; 
    case U64:
      LongBuffer longBuffer = (LongBuffer)data;
      while (i < j) {
        long temp = longBuffer.get(i);
        longBuffer.put(i, longBuffer.get(j));
        longBuffer.put(j, temp);
        ++i; --j;
      }
      break; 
    }
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
                                                                
