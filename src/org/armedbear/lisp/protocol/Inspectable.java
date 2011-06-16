package org.armedbear.lisp.protocol;

/** Object implements a protocol for dynamic introspection. */
public interface Inspectable {
    public org.armedbear.lisp.LispObject getParts();
}

