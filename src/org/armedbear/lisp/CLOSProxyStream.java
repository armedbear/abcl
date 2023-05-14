package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

public class CLOSProxyStream
  extends Stream
{
  LispObject clos = null;

  public CLOSProxyStream(LispObject clos) {
    super(Symbol.CLOS_STREAM);
    this.clos = clos;
  }
}
