/*package abcl_ae;

import java.io.FileInputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.File;
//import java.io.IOException;
//import java.io.FileNotFoundException;

import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Pathname;
import org.armedbear.lisp.Stream;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.ConditionThrowable;

public final class AbclInit {
	static Symbol doGet = null;
	static boolean hasErrors = false;
	static String message = "Success";
	
	static {
		FileInputStream in = null;
		
		try {
			in = new FileInputStream("fasls/first-servlet.abcl");
			Load.load("fasls/first-servlet.abcl");
			
			doGet = Lisp.internInPackage("FIRST-SERVLET", "DO-GET");
		} catch (ConditionThrowable condition) {
			// How do we handle exceptions?
			hasErrors = true;
			message = condition.toString();
		} catch (Exception e) {
			// How do we handle exceptions?
			hasErrors = true;
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw, true);
			e.printStackTrace(pw);
			pw.flush();
			sw.flush();
			message = sw.toString();
		} finally {
			try {
				in.close();
			} catch (Exception e) {
				hasErrors = true;
				StringWriter sw = new StringWriter();
				PrintWriter pw = new PrintWriter(sw, true);
				e.printStackTrace(pw);
				pw.flush();
				sw.flush();
				message = sw.toString();
			}
		}
	}
}*/

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
				
			try {
				Interpreter.initializeLisp();
				Load.load("fasls/first-servlet.abcl");
			}
			catch (ConditionThrowable ct) { }
			
			initialized = true;
		}
	}

}