package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

/** 
 A basic vector is a specialized vector that is not displaced to another
 array, has no fill pointer, and is not expressly adjustable.
 
 All BasicVectors are children of SimpleVector.
*/
abstract public class BasicVector
  extends SimpleVector
{
  public enum Specialization {
    U8(1), U16(2), U32(4), U64(8);

    public final int totalBytes;

    private Specialization(int bytes) {
      totalBytes = bytes;
    }
      
  }
  Specialization specializedOn;

  public BasicVector(Class type) {
    if (type.equals(Byte.class) || type.equals(byte.class)) {
      specializedOn = Specialization.U8;
    } else if (type.equals(Short.class) || type.equals(short.class)) {
      specializedOn = Specialization.U16;
    } else if (type.equals(Integer.class) || type.equals(int.class)) {
      specializedOn = Specialization.U32;
    } else if (type.equals(Long.class)|| type.equals(long.class)) {
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

  @Override
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
    sb.append(" with ").append(capacity).append(" elements").append(".")
      .append("\n");
    return new SimpleString(sb);
  }

  // should be coerceToUnsignedElementType???
  LispInteger coerceToElementType(LispObject o) {
    LispInteger result = LispInteger.coerceAsUnsigned(o);
    switch (specializedOn) {
    case U8:
      if (result.isLessThan(0)
          || result.isGreaterThan(255)) {
        return (LispInteger) type_error(result, UNSIGNED_BYTE_8);
      }
      break;
    case U16: 
      if (result.isLessThan(0)
          || result.isGreaterThan(65536)) {
        return (LispInteger) type_error(result, UNSIGNED_BYTE_16);
      }
      break;
    case U32:
      if (result.isLessThan(0)
          || result.isGreaterThan(Bignum.MAX_UNSIGNED_BYTE_32)) {
        return (LispInteger) type_error(result, UNSIGNED_BYTE_32);
      }
      break;
    case U64:
      if (result.isLessThan(0)
          || result.isGreaterThan(Bignum.MAX_UNSIGNED_BYTE_64)) {
        return (LispInteger) type_error(result, UNSIGNED_BYTE_32);
      }
      break;
    }  
    return result;
  }


}
