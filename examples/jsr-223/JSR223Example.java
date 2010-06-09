import javax.script.*;

public class JSR223Example {

	public static void main(String[] args) {
	    //Script Engine instantiation using ServiceProvider - this will
	    //look in the classpath for a file
	    //  /META-INF/services/javax.script.ScriptEngineFactory
	    //where the AbclScriptEngineFactory is registered
	    ScriptEngine lispEngine = new ScriptEngineManager().getEngineByExtension("lisp");

	    //Alternatively, you can directly instantiate the script engine:
	    
	    //ScriptEngineManager scriptManager = new ScriptEngineManager();
	    //scriptManager.registerEngineExtension("lisp", new AbclScriptEngineFactory());
	    //ScriptEngine lispEngine = scriptManager.getEngineByExtension("lisp");

	    //(thanks to Peter Tsenter for suggesting this)
				
		//Accessing variables
		System.out.println();
		System.out.println("*package* = " + lispEngine.get("*package*"));
		Object someValue = new Object();
		lispEngine.put("someVariable", someValue);
		System.out.println("someVariable = " + lispEngine.get("someVariable"));
		try {
			//Interpretation (also from streams)
			lispEngine.eval("(defun hello (arg) (print (list arg someVariable)) (terpri))");
			
			//Direct function invocation
			((Invocable) lispEngine).invokeFunction("hello", "world");
			
			//Implementing a Java interface in Lisp
			lispEngine.eval("(defun compare-to (&rest args) 42)");
			Comparable c = ((Invocable) lispEngine).getInterface(java.lang.Comparable.class);
			System.out.println("compareTo: " + c.compareTo(null));
			
			//Compilation!
			lispEngine.eval("(defmacro slow-compiling-macro (arg) (dotimes (i 1000000) (incf i)) `(print ,arg))");
			
			long millis = System.currentTimeMillis();
			lispEngine.eval("(slow-compiling-macro 42)");
			millis = System.currentTimeMillis() - millis;
			System.out.println("interpretation took " + millis);
			
			millis = System.currentTimeMillis();
			CompiledScript cs = ((Compilable) lispEngine).compile("(slow-compiling-macro 42)");
			millis = System.currentTimeMillis() - millis;
			System.out.println("compilation took " + millis);
			
			millis = System.currentTimeMillis();
			cs.eval();
			millis = System.currentTimeMillis() - millis;
			System.out.println("evaluation took " + millis);

			millis = System.currentTimeMillis();
			cs.eval();
			millis = System.currentTimeMillis() - millis;
			System.out.println("evaluation took " + millis);

			//Ecc. ecc.
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		} catch (ScriptException e) {
			e.printStackTrace();
		}
	}
	
}
