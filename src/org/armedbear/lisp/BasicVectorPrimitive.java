package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.lang.reflect.Array;
import java.nio.Buffer;
import java.text.MessageFormat;
import java.util.Arrays;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVectorPrimitive
  extends BasicVector
{
  byte[] u8;
  short[] u16;
  int[] u32;
  long[] u64;
  
  public BasicVectorPrimitive(Class type, int capacity) {
    super(type, capacity);
    switch (specializedOn) {
    case U8:
      u8 = new byte[capacity];
    case U16:
      u16 = new short[capacity];
    case U32:
      u32 = new int[capacity];
    case U64:
      u64 = new long[capacity];
    }
  }

  public BasicVectorPrimitive(byte[] data) {
    super(Byte.class, data.length);
    u8 = data;
  }
  public BasicVectorPrimitive(short[] data) {
    super(Short.class, data.length);
    u16 = data;
  }
  public BasicVectorPrimitive(int[] data) {
    super(Integer.class, data.length);
    u32 = data;
  }
  public BasicVectorPrimitive(long[] data) {
    super(Long.class, data.length);
    u64 = data;
  }

  
  @Override
  public LispObject elt(int i) {
    return AREF(i);
  }

  @Override
  public LispObject AREF(int i) {
    return SVREF(i);
  }

  @Override
  public void aset(int i, LispObject n) {
    svset(i, n);
  }

  public LispObject SVREF(int i) {
    try {
      switch (specializedOn) {
      case U8:
        return coerceFromJavaByte(u8[i]);
      case U16:
        return Fixnum.getInstance(Short.toUnsignedInt(u16[i]));
      case U32:
        return Fixnum.getInstance(Integer.toUnsignedLong(u32[i]));
      case U64:
        return LispInteger.getUnsignedInstance(u64[i]);
      }
    }  catch (ArrayIndexOutOfBoundsException e) {
      return badIndex(i, capacity);
    }
    return program_error("Supposedly unreachable code in BasicVectorPrimitive");
  }

  public void svset(int i, LispObject n) {
    try {
      switch (specializedOn) {
      case U8:
        byte b = coerceToJavaByte(n);
        u8[i] = b;
        break;
      case U16:
        short s = coerceToJavaUnsignedShort(n);
        u16[i] = s;
        break;
      case U32:
        int v  = coerceToJavaUnsignedInt(n);
        u32[i] = v;
        break;
      case U64:
        //        long v = ???
        //          ((IntBuffer)data).put(i, v);
        program_error("Unimplemented aset on long");
        break;
      }
    } catch (IndexOutOfBoundsException e) {
      badIndex(i, capacity);
    }

  }
  public LispObject subseq(int start, int end) {
    try {
      switch (specializedOn) {
      case U8:
        byte[] bytes = Arrays.copyOfRange(u8, start, end);
        return new BasicVectorPrimitive(bytes);
      case U16:
        short[] shorts = Arrays.copyOfRange(u16, start, end);
        return new BasicVectorPrimitive(shorts);
      case U32:
        int[] ints = Arrays.copyOfRange(u32, start, end);
        return new BasicVectorPrimitive(ints);
      case U64:
        long[] longs = Arrays.copyOfRange(u64, start, end);
        return new BasicVectorPrimitive(longs);
      }
    } catch (ArrayIndexOutOfBoundsException e) {
      String m
        = MessageFormat.format("The bounding indices {0} and {1} are bad for a sequence of length {2}.", start, end, length());
      return type_error(m, new JavaObject(e), NIL); // Not really a type_error, as there is not one type
    }
    return program_error("Unreachable");
  }

  @Override
  public void fill(LispObject obj) {
    switch (specializedOn) {
    case U8:
      byte b = coerceToJavaByte(obj);
      Arrays.fill(u8, b);
      break;
    case U16:
      short s = coerceToJavaUnsignedShort(obj);
      Arrays.fill(u16, s);
      break;
    case U32:
      int i = coerceToJavaUnsignedInt(obj);
      Arrays.fill(u32,i);
      break;
    case U64:
      program_error("Unimplemented fill of U64");
      break;
    }
  }

  //   public LispObject deleteEq(LispObject item)    
  //   public LispObject deleteEql(LispObject item)

  public void shrink(int n) {
    if (n < capacity) {
      BasicVectorPrimitive newArray = new BasicVectorPrimitive(type, n);
      switch (specializedOn) {
      case U8:
        System.arraycopy(u8, 0, newArray.u8, 0, n);
        u8 = newArray.u8;
        break;
      case U16:
        System.arraycopy(u16, 0, newArray.u16, 0, n);
        u16 = newArray.u16;
        break;
      case U32:
        System.arraycopy(u32, 0, newArray.u32, 0, n);
        u32 = newArray.u32;
        break;
      case U64:
        System.arraycopy(u64, 0, newArray.u64, 0, n);
        u64 = newArray.u64;
        break;
      }
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    error(new LispError());
  }

  public LispObject reverse() {
    BasicVectorPrimitive result = new BasicVectorPrimitive(type, capacity);
    int i, j;
    switch (specializedOn) {
    case U8:
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        result.u8[i] = u8[j];
      }
      break;
    case U16:
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        result.u16[i] = u16[j];
      }
      break;
    case U32:
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        result.u32[i] = u32[j];
      }
      break;
    case U64:
      for (i = 0, j = capacity - 1; i < capacity; i++, j--) { 
        result.u64[i] = u64[j];
      }
      break;
    }
    return result;
  }
  public LispObject nreverse() {
    int i = 0;
    int j = capacity - 1;
    switch (specializedOn) {
    case U8:
      while (i < j) {
        byte temp = u8[i];
        u8[i] = u8[j];
        u8[j] = temp;
        ++i; --j;
      }
      break;
    case U16:
      while (i < j) {
        short temp = u16[i];
        u16[i] = u16[j];
        u16[j] = temp;
        ++i; --j;
      }
      break;
    case U32:
      while (i < j) {
        int temp = u32[i];
        u32[i] = u32[j];
        u32[j] = temp;
        ++i; --j;
      }
      break;
    case U64:
      while (i < j) {
        long temp = u64[i];
        u64[i] = u64[j];
        u64[j] = temp;
        ++i; --j;
      }
      break;
    }
    return this;
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                    LispObject initialElement,
                                    LispObject initialContents)
  {
    if (initialContents != null) {
      if (initialContents.listp()) {
        LispObject list = initialContents;
        switch (specializedOn) {
        case U8:
          byte[] bytes = new byte[newCapacity];
          for (int i = 0; i < newCapacity; i++) {
            bytes[i] = coerceToJavaByte(list.car());
            list = list.cdr();
          }
          return new BasicVectorPrimitive(bytes);
        case U16:
          short[] shorts = new short[newCapacity];
          for (int i = 0; i < newCapacity; i++) {
            shorts[i] = coerceToJavaUnsignedShort(list.car());
            list = list.cdr();
          }
          return new BasicVectorPrimitive(shorts);
        case U32:
          int[] ints = new int[newCapacity];
          for (int i = 0; i < newCapacity; i++) {
            ints[i] = coerceToJavaUnsignedInt(list.car());
            list = list.cdr();
          }
          return new BasicVectorPrimitive(ints);
        case U64:
          program_error("Unimplemented adjustment of u64 array");
        }
      } else if (initialContents.vectorp()) {
        if (initialContents instanceof BasicVectorPrimitive) {
          switch(specializedOn) {
          case U8:
            byte[] bytes = Arrays.copyOfRange(u8, 0, newCapacity);
            return new BasicVectorPrimitive(bytes);
          case U16:
            short[] shorts = Arrays.copyOfRange(u16, 0, newCapacity);
            return new BasicVectorPrimitive(shorts);
          case U32:
            int[] ints = Arrays.copyOfRange(u32, 0, newCapacity);
            return new BasicVectorPrimitive(ints);
          case U64:
            long[] longs = Arrays.copyOfRange(u64, 0, newCapacity);
            return new BasicVectorPrimitive(longs);
          }
        } else {
          program_error("Unimplmented adjust array for non BasicVectorPrimitive");
        }
      } else {
        type_error(initialContents, Symbol.SEQUENCE);
      }
    }
    if (capacity != newCapacity) {
      if (initialElement == null) {
        switch(specializedOn) {
        case U8:
          byte[] bytes = new byte[newCapacity];
          bytes = Arrays.copyOfRange(u8, 0, Math.min(capacity, newCapacity));
          return new BasicVectorPrimitive(bytes);
        case U16:
          short[] shorts = new short[newCapacity];
          shorts = Arrays.copyOfRange(u16, 0, Math.min(capacity, newCapacity));
          return new BasicVectorPrimitive(shorts);
        case U32:
          int[] ints = new int[newCapacity];
          ints = Arrays.copyOfRange(u32, 0, Math.min(capacity, newCapacity));
          return new BasicVectorPrimitive(ints);
        case U64:
          long[] longs = new long[newCapacity];
          longs = Arrays.copyOfRange(u64, 0, Math.min(capacity, newCapacity));
          return new BasicVectorPrimitive(longs);
        }
      }

      BasicVectorPrimitive result = null;
      switch(specializedOn) {
      case U8:
        result = new BasicVectorPrimitive(Byte.class, newCapacity);
        break;
      case U16:
        result = new BasicVectorPrimitive(Short.class, newCapacity);
        break;
      case U32:
        result = new BasicVectorPrimitive(Integer.class, newCapacity);
        break;
      case U64:
        result = new BasicVectorPrimitive(Long.class, newCapacity);
        break;
      }
      result.fill(initialElement);
      return result;
    }

    // No change.
    return this;
  }
  // public AbstractVector adjustArray(int newCapacity, AbstractArray displacedTo, int displacement)

}
