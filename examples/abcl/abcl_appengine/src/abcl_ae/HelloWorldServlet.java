/*package abcl_ae;

import java.io.IOException;
import javax.servlet.http.*;

import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Stream;
import org.armedbear.lisp.SpecialBinding;
import org.armedbear.lisp.ConditionThrowable;

public class HelloWorldServlet extends HttpServlet {
	public void doGet(HttpServletRequest req, HttpServletResponse resp)
		throws IOException {
		
		if (AbclInit.hasErrors)
		{
			resp.setContentType("text/plain");
			resp.getWriter().println(AbclInit.message);
			return;
		}
		
		// Set the default Lisp output stream to the servlet's output stream.
		LispThread currentThread = LispThread.currentThread();
		SpecialBinding lastSpecialBinding = currentThread.lastSpecialBinding;
		Stream out = new Stream(resp.getOutputStream(), Symbol.CHARACTER, false);
		
		currentThread.bindSpecial(Symbol.STANDARD_OUTPUT, out);
		
		try {
			if (AbclInit.doGet == null)
			{
				resp.setContentType("text/plain");
				resp.getWriter().println(AbclInit.message);
				return;
			}
			
			// Run the Lisp handler.
			currentThread.execute(AbclInit.doGet);
		} catch (ConditionThrowable condition) {
			resp.setContentType("text/plain");
			resp.getWriter().println(condition.toString());
		} finally {
			// Restore the default Lisp output stream.
			currentThread.lastSpecialBinding = lastSpecialBinding;
		}
	}
}*/

package abcl_ae;

import java.io.IOException;
import javax.servlet.http.*;
import javax.servlet.*;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.SpecialBinding;
import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.Load;

public class HelloWorldServlet extends HttpServlet {

	static private Symbol doGet = null;

	public void init() throws ServletException {
		AbclInit.init();
		try {
			doGet = Lisp.internInPackage("DO-GET", "FIRST-SERVLET");
		}
		catch (ConditionThrowable ct) { }
	}


	public void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws IOException {

		LispThread currentThread = LispThread.currentThread();

		SpecialBinding lastSpecialBinding = currentThread.lastSpecialBinding;
		currentThread.bindSpecial(
			Symbol.STANDARD_OUTPUT, 
			new org.armedbear.lisp.Stream(resp.getOutputStream(), Symbol.CHARACTER, false));

		try {
			currentThread.execute(doGet);
		} catch (ConditionThrowable condition) {
			resp.setContentType("text/plain");
			resp.getWriter().println(condition.toString());
		} finally {
			currentThread.lastSpecialBinding = lastSpecialBinding;
		}
	}
}