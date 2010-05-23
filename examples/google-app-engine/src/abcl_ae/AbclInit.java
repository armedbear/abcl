package abcl_ae;

import java.io.FileInputStream;
import java.io.IOException;

import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Pathname;
import org.armedbear.lisp.ConditionThrowable;

public final class AbclInit {
	static private Object lock = new Object();
	static private boolean initialized = false;

	// package access level
	static void init() {
		if (initialized)
			return;
			
		synchronized (lock) {
			if (initialized)
				return;
				
			Interpreter.initializeLisp();
			Load.load("fasls/first-servlet.abcl");
			
			initialized = true;
		}
	}

}