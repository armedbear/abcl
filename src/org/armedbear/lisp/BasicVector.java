package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

public class BasicVector
  extends SimpleVector
{
  public enum Specialization {
    U8, U16, U32, U64
  }
  Specialization specializedOn;

  public BasicVector(Class type) {
    if (type.equals(Byte.class)) {
      specializedOn = Specialization.U8;
    } else if (type.equals(Short.class)) {
      specializedOn = Specialization.U16;
    } else if (type.equals(Integer.class)) {
      specializedOn = Specialization.U32;
    } else if (type.equals(Long.class)) {
      specializedOn = Specialization.U64;
    }
  }

  public BasicVector(Class type, int capacity) {
    this(type);
    this.capacity = capacity;
  }

  @Override
  public LispObject typeOf() {
    switch (specializedOn) {
    case U8:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_8, new Cons(Fixnum.getInstance(capacity)));
    case U16:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_16, new Cons(Fixnum.getInstance(capacity)));      
    case U32:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_32, new Cons(Fixnum.getInstance(capacity)));      
    case U64:
      return list(Symbol.SIMPLE_VECTOR, UNSIGNED_BYTE_64, new Cons(Fixnum.getInstance(capacity)));      
    }
    return program_error("Unreachable");
  }

  public LispObject typep(LispObject type) {
    if (type instanceof Cons) {
      if (type.car().equals(Symbol.SIMPLE_VECTOR)) {
        LispObject vectorType = type.cdr();
        switch (specializedOn) {
        case U8:
          if (vectorType.equals(UNSIGNED_BYTE_8)) {
              return T;
            }
          break;
        case U16:
          if (vectorType.equals(UNSIGNED_BYTE_16)) {
            return T;
          }
          break;
        case U32:
          if (vectorType.equals(UNSIGNED_BYTE_32)) {
            return T;
          }
          break;
        case U64:
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
    switch (specializedOn) {
    case U8:
      return UNSIGNED_BYTE_8;
    case U16:
      return UNSIGNED_BYTE_16;
    case U32:
      return UNSIGNED_BYTE_32;
    case U64:
      return UNSIGNED_BYTE_64;
    }
    return program_error("Unknown element type: " + type);
  }



}
