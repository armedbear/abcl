import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;

public class BankExampleMain 
{
  static public void main(String argv[]) {
    Interpreter interpreter = Interpreter.createInstance();
    interpreter.eval("(load \"bank-account.lisp\")");
    org.armedbear.lisp.Package defaultPackage
      = Packages.findPackage("CL-USER");
    Symbol bankAccountImplSymbol
      = defaultPackage.findAccessibleSymbol("*BANK-ACCOUNT-IMPL*");
    LispObject value = bankAccountImplSymbol.symbolValue();
    Object object =  ((JavaObject) value).getObject();
    BankAccount account = (BankAccount) object;
    System.out.println("Initial balance: " + account.getBalance());
    account.withdraw(500);
    System.out.println("After withdrawing 500: " + account.getBalance());
  }
}