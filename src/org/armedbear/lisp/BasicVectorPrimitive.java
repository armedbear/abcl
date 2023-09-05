package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.lang.reflect.Array;
import java.nio.Buffer;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVectorPrimitive<T> extends SimpleVector {
  T[] data;
  Class type;
  
  public BasicVectorPrimitive(Class type, int capacity) {
    this.type = type;
    data = (T[]) Array.newInstance(type, capacity);
  }

  public LispObject typeOf() {
    if (type.equals(Byte.class)) {
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_8, new Cons(Fixnum.getInstance(capacity)));
    } else if (type.equals(Short.class)) {
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_16, new Cons(Fixnum.getInstance(capacity)));
    } else if (type.equals(Integer.class)) {
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_32, new Cons(Fixnum.getInstance(capacity)));
    } else if (type.equals(Long.class)) {
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_64, new Cons(Fixnum.getInstance(capacity)));
    }
    return program_error("BasicVectorPrimitive couldn't determine type.");
  }

  //public LispObject classOf()
  
  //  public LispObject getDescription() {
  //    StringBuffer sb = new StringBuffer(super.getDescription().toString());

  public LispObject typep(LispObject type) {
    if (type instanceof Cons) {
      if (type.car().equals(Symbol.SIMPLE_VECTOR)
          && (type.cdr().equals(getElementType()))) {
        return T;
      }
    }
    return super.typep(type);
  }

  @Override
  public LispObject getElementType() {
    if (type.equals(Byte.class)) {
      return UNSIGNED_BYTE_8;
    } else if (type.equals(Short.class)) {
      return UNSIGNED_BYTE_16;
    } else if (type.equals(Integer.class)) {
      return UNSIGNED_BYTE_32;
    } else if (type.equals(Long.class)) {
      return UNSIGNED_BYTE_64;
    }
    return program_error("Unknown element type: " + type);
  }

    // do these work?
  //  public LispObject elt(int index)
  //  public LispObject AREF(int index)
  //   public LispObject SVREF(int index)
  //   public void svset(int index, LispObject newValue)
  //   public LispObject subseq(int start, int end)
  //  public void fill(LispObject obj) {


  //   public LispObject deleteEq(LispObject item)
  //   public LispObject deleteEql(LispObject item)

  
  public void shrink(int n) {
    if (n < capacity) {
      BasicVectorPrimitive newArray = new BasicVectorPrimitive<>(type, n);
      System.arraycopy(data, 0, newArray.data, 0, n);
      data = (T[])newArray.data;
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    error(new LispError());
  }

  public LispObject reverse() {
    BasicVectorPrimitive result = new BasicVectorPrimitive<>(type, capacity);
    int i, j;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
      result.data[i] = data[j];
    }
    return result;
  }
  //   public LispObject nreverse()

  //public AbstractVector adjustArray
  // public AbstractVector adjustArray(int newCapacity, AbstractArray displacedTo, int displacement)

}
