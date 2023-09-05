package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.text.MessageFormat;
import java.util.Arrays;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVectorBuffer
  extends SimpleVector
{
  //  boolean directAllocation; directly allocate Buffer don't have backing arrays
  Buffer data;
  BufferType specialization;

  public enum BufferType {
    BYTE, SHORT, INT, LONG
  }
  
  public BasicVectorBuffer(Class type, int capacity) {
    this.type = type;
    this.capacity = capacity;
    if (type.equals(ByteBuffer.class)) {
      specialization = BufferType.BYTE;
    } else if (type.equals(ShortBuffer.class)) {
      specialization = BufferType.SHORT;
    } else if (type.equals(IntBuffer.class)) {
      specialization = BufferType.INT;
    } else if (type.equals(LongBuffer.class)) {
      specialization = BufferType.LONG;
    }
    switch (specialization) {
    case BYTE:
      data = ByteBuffer.allocate(capacity);
      break;
    case SHORT:
      data = ShortBuffer.allocate(capacity);
      break;
    case INT:
      data = IntBuffer.allocate(capacity);
      break;
    case LONG:
      data = LongBuffer.allocate(capacity);
      break;
    }
  }

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
    

  @Override
  public LispObject typeOf() {
    switch (specialization) {
    case BYTE:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_8, new Cons(Fixnum.getInstance(capacity)));
    case SHORT:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_16, new Cons(Fixnum.getInstance(capacity)));      
    case INT:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_32, new Cons(Fixnum.getInstance(capacity)));      
    case LONG:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_64, new Cons(Fixnum.getInstance(capacity)));      
    }
    return program_error("Unreachable");
  }

  public LispObject getDescription() {
    StringBuffer sb = new StringBuffer("A simple vector specialized on ");
    switch (specialization) {
    case BYTE:
      sb.append("(UNSIGNED-BYTE 8)");
      break;
    case SHORT:
      sb.append("(UNSIGNED-BYTE 16)");
      break;
    case INT:
      sb.append("(UNSIGNED-BYTE 32)");
      break;
    case LONG:
      sb.append("(UNSIGNED-BYTE 64)");
      break;
    }
    sb.append(" with ");
    sb.append(capacity);
    sb.append(" elements");
    return new SimpleString(sb);
  }

  public LispObject typep(LispObject type) {
    // FIXME type based on CLAZZ and capacity
    if (type instanceof Cons) {
      if (type.car().equals(Symbol.SIMPLE_VECTOR)) {
        LispObject vectorType = type.cdr();
        switch (specialization) {
        case BYTE:
          if (vectorType.equals(UNSIGNED_BYTE_8)) {
              return T;
            }
          break;
        case SHORT:
          if (vectorType.equals(UNSIGNED_BYTE_16)) {
            return T;
          }
          break;
        case INT:
          if (vectorType.equals(UNSIGNED_BYTE_32)) {
            return T;
          }
          break;
        case LONG:
          if (vectorType.equals(UNSIGNED_BYTE_64)) {
            return T;
          }
          break;
        }
      }
    }
    return super.typep(type);
  }

  @Override
  public LispObject getElementType() {
    if (type.equals(ByteBuffer.class)) {
      return UNSIGNED_BYTE_8;
    } else if (type.equals(ShortBuffer.class)) {
      return UNSIGNED_BYTE_16;
    } else if (type.equals(IntBuffer.class)) {
      return UNSIGNED_BYTE_32;
    } else if (type.equals(LongBuffer.class)) {
      return UNSIGNED_BYTE_64;
    }
    return super.getElementType();
  }

  @Override
  public LispObject elt(int i) {
    return AREF(i);
  }

  @Override
  public LispObject AREF(int i) {
    try {
      switch (specialization) {
      case BYTE:
        return coerceFromJavaByte(((ByteBuffer)data).get(i));
      case SHORT:
        return Fixnum.getInstance(Short.toUnsignedInt(((ShortBuffer)data).get(i)));
      case INT:
        return Fixnum.getInstance(Integer.toUnsignedLong(((IntBuffer)data).get(i)));
      case LONG:
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
      switch (specialization) {
      case BYTE:
        byte b = coerceToJavaByte(n);
        ((ByteBuffer)data).put(i, b);
        break;
      case SHORT:
        short s = coerceToJavaUnsignedShort(n);
        ((ShortBuffer)data).put(i, s);
        break;
      case INT:
        int v  = coerceToJavaUnsignedInt(n);
        ((IntBuffer)data).put(i, v);
        break;
      case LONG:
        //        long v = ???
        //          ((IntBuffer)data).put(i, v);
        program_error("Unimplemented aset on long");
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
      switch (specialization) {
      case BYTE:
        result = new BasicVectorBuffer(ByteBuffer.class, length);
        ((ByteBuffer)data).get(result.asByteArray(), start, length);
        break;
      case SHORT:
        result = new BasicVectorBuffer(ShortBuffer.class, length);
        ((ShortBuffer)data).get(result.asShortArray(), start, length);
        break;
      case INT:
        result = new BasicVectorBuffer(IntBuffer.class, length);
        ((IntBuffer)data).get(result.asIntArray(), start, length);
        break;
      case LONG:
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


  // TODO AbstractVector.deleteEq() could work, as well but is it faster?
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
    // thunk on CLAZZ
    BasicVectorBuffer result
      = new BasicVectorBuffer(ByteBuffer.class, capacity);
    
    int i, j;
    ByteBuffer source = (ByteBuffer)data;
    ByteBuffer destination = (ByteBuffer)result.data;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
      destination.put(i, source.get(j));
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity() - 1;
    ByteBuffer buffer = (ByteBuffer)data;
    while (i < j) {
      byte temp = buffer.get(i);
      buffer.put(i, buffer.get(j));
      buffer.put(j, temp);
      ++i;
      --j;
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
                                                                
