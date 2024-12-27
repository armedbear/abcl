package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

public class Atomic
{
  static MethodHandles.Lookup Lookup
    = MethodHandles.lookup();
  
  static boolean compareAndSwap(LispObject place, LispObject expectedValue, LispObject newValue) {
    VarHandle vh = null; 
    if (place instanceof Fixnum) {
      try {
        vh = Lookup.findVarHandle(Fixnum.class, "value", int.class);
      } catch (ReflectiveOperationException e) {
        java_error(e);
        return false;  // unreached
      }
    }
    if (vh == null) {
      simple_error("Unable to find VarHandle for place ~a", place);
      return false; // unreached
    }
    return vh.compareAndSet(expectedValue, newValue);
  }

  public static final Primitive _CAS = new pf_cas();
  @DocString(name="%cas",
             args="place expected-value new-value",
             returns="generalized boolean")
  private static final class pf_cas extends Primitive { // Maybe a special operator?  Or do that Lisp-side?
    pf_cas() {
      super(Symbol._CAS);
    }
    @Override
    public LispObject execute(LispObject place, LispObject expectedValue, LispObject newValue) {
      boolean result = compareAndSwap (place, expectedValue, newValue);
      return (result == false) ? NIL : T; // TODO this has to exist somewhere as a static method
    }
  }
}
