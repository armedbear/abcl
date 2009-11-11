package abcl_ae;

import java.io.IOException;
import javax.servlet.http.*;
import javax.servlet.*;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.SpecialBinding;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.Stream;

public class HelloWorldServlet extends HttpServlet {

	static private Symbol doGet = null;

	public void init() throws ServletException {
		AbclInit.init();
		doGet = Lisp.internInPackage("DO-GET", "FIRST-SERVLET");
	}


	public void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws IOException {

		LispThread currentThread = LispThread.currentThread();

		SpecialBindingsMark mark = currentThread.markSpecialBindings();
		currentThread.bindSpecial(
			Symbol.STANDARD_OUTPUT, 
			new Stream(resp.getOutputStream(), Symbol.CHARACTER, false));

		try {
			currentThread.execute(doGet);
		} finally {
			currentThread.resetSpecialBindings(mark);
		}
	}
}