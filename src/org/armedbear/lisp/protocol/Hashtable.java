package org.armedbear.lisp.protocol;

/** Mark object as implementing the Hashtable protocol. */
public interface Hashtable
  extends org.armedbear.lisp.protocol.LispObject 
{
    public org.armedbear.lisp.LispObject getEntries();
    
    @Deprecated
    public org.armedbear.lisp.LispObject ENTRIES();
}