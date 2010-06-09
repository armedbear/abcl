import org.armedbear.lisp.*;

public class hello
{
  public static void main(String[] args)
  {
    try
      {
        Interpreter interpreter = Interpreter.createInstance();
        interpreter.eval("(format t \"Hello, world!~%\")");
      }
    catch (Throwable t)
      {
        t.printStackTrace();
      }
  }
}

// cd ~/j/examples
// cp hello.java ../src
// cd ../src
// javac hello.java
// java hello
