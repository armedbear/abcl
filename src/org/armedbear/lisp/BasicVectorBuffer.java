package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.text.MessageFormat;
import java.util.Arrays;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVectorBuffer<T extends Buffer>
  extends SimpleVector
{
  boolean directAllocation;
  T data;
  
  public BasicVectorBuffer(Class<ByteBuffer> type, int capacity, boolean directAllocation) {
    this.clazz = type;
    this.capacity = capacity;
    this.directAllocation = directAllocation;
    data = (T) ByteBuffer.allocate(capacity);
  }

  // public BasicVectorBuffer(int capacity) {
  //   this(ByteBuffer.class, capacity, false);
  // }

  @Override
  public LispObject classOf() {
    if (clazz.equals(ByteBuffer.class)) {
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_8, new Cons(Fixnum.getInstance(capacity)));
    }
    return program_error("Unimplemented classOf()");
  }

  public LispObject getDescription() {
    StringBuffer sb = new StringBuffer("A simple vector specialized on UNSIGNED_BYTE_8 with ");
    sb.append(capacity);
    sb.append(" elements");
    return new SimpleString(sb);
  }

  public LispObject typep(LispObject type)
  {
    if (type == Symbol.SIMPLE_VECTOR)
      return T;
    if (type == Symbol.SIMPLE_ARRAY)
      return T;
    if (type == BuiltInClass.SIMPLE_VECTOR)
      return T;
    if (type == BuiltInClass.SIMPLE_ARRAY)
      return T;
    // TODO return type based on CLAZZ and capacity
    if (type instanceof Cons) {
      if (type.car().equals(Symbol.SIMPLE_VECTOR)
          && type.cdr().equals(UNSIGNED_BYTE_8)) {
        return T;
      }
    }
    return super.typep(type);
  }

  @Override
  public LispObject getElementType() { 
    return UNSIGNED_BYTE_8;
    // TODO return type based on CLAZZ
  }

  @Override
  public LispObject elt(int i) {
    try {
      // TODO switch on clazz
      return coerceFromJavaByte(((ByteBuffer)data).get(i));
    } catch (ArrayIndexOutOfBoundsException e) {
      return badIndex(i, capacity);
    }
  }

  @Override
  public LispObject AREF(int i) {
    return elt(i);
  }

  @Override
  public void aset(int i, LispObject n) {
    aset(i, coerceToJavaByte(n));
  }

  public void aset(int i, byte n) {
    try {
      ((ByteBuffer)data).put(i, n);
    } catch (IndexOutOfBoundsException e) {
      badIndex(i, capacity);
    }
  }

  @Override
  public LispObject SVREF(int i) {
    return elt(i);
  }

  @Override
  public void svset(int i, LispObject newValue) {
    aset(i, newValue);
  }

  @Override
  public LispObject subseq(int start, int end) {
    try {
      // TODO: switch on clazz
      int length = start - end;
      BasicVectorBuffer<ByteBuffer> result
        = new BasicVectorBuffer<ByteBuffer>(ByteBuffer.class, length, this.directAllocation);
      ((ByteBuffer)data).get(result.data.array(), start, length);
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
      BasicVectorBuffer<ByteBuffer> result
        = new BasicVectorBuffer<ByteBuffer>(ByteBuffer.class, n, this.directAllocation);
      ((ByteBuffer)data).get(result.data.array(), 0, n);
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
    BasicVectorBuffer<ByteBuffer> result
      = new BasicVectorBuffer<ByteBuffer>(ByteBuffer.class, capacity, this.directAllocation);
    
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
                                                                
