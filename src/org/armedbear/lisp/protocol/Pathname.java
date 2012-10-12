package org.armedbear.lisp.protocol;

// TODO: transcribe CL:PATHNAME, hook org.armedbear.lisp.Pathname up to use a proxied version of the ANSI contract.
/** Mark object as implementing the Hashtable protocol. */
public interface Pathname
  extends org.armedbear.lisp.protocol.LispObject 
{
  public Pathname coerce();
}
