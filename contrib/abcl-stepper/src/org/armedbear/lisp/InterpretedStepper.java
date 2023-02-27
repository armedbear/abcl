package org.armedbear.lisp;

import org.armedbear.lisp.Binding;
import org.armedbear.lisp.Environment;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.LispCharacter;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Operator;
import org.armedbear.lisp.Package;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.Primitive;
import org.armedbear.lisp.Return;
import org.armedbear.lisp.SimpleString;
import org.armedbear.lisp.Symbol;

public class InterpretedStepper
{
  // TODO replace with Lisp-side values where appropiate
  public static boolean stepping = false;
  public static boolean delimitedStepping = false;
  public static boolean firstStepping = false;
  public static Binding stepperBlock = null;

  public static boolean stepInSymbolP (LispObject function) {
    Package stepper;
    Symbol symbol;
    LispThread currentThread = LispThread.currentThread();
    LispObject stopAtSymbolFunction;
    LispObject[] args = new LispObject[1];
    LispObject result;

    if (stepping && firstStepping) {
      firstStepping = false;
      stepperBlock = null;
      return false;
    }

    if (!stepping && !delimitedStepping) {
      return false;
    }

    if (stepping && !delimitedStepping) {
      return true;
    }

    if (stepping && delimitedStepping) {
      //  TODO replace with the presence of a value in the symbols PLIST
      // we analyze if the symbol of the function is one of the
      // symbols exported in the packages specified in sys::*stop-packages*
      // or one of the symbols in sys::*stop-symbols*
      // in that case we trigger the stepper here
      setSteppingOff();
      stepper = Packages.findPackageGlobally("ABCL-STEPPER");
      symbol = stepper.findAccessibleSymbol("STOP-AT-SYMBOL-P");
      stopAtSymbolFunction = Lisp.coerceToFunction(symbol);
      args[0] = ((Operator)function).getLambdaName();
      result = Lisp.funcall(stopAtSymbolFunction, args, currentThread);
      setSteppingOnAfterInit();
      if (result != Lisp.NIL) {
        setDelimitedSteppingOff();
        return true;
      }
      return false;
    }
    return false;
  }

  public static void setDelimitedSteppingOn () {
    delimitedStepping = true;
  }

  public static void setDelimitedSteppingOff () {
    delimitedStepping = false;
  }

  public static void setSteppingOn () {
    stepping = true;
    firstStepping = true;
  }

  public static void setSteppingOnAfterInit () {
    stepping = true;
  }

  public static void setSteppingOff () {
    stepping = false;
  }

  public static synchronized final void handleStepping (LispObject function, LispObject args,
                                                        Environment env) {
    LispThread currentThread = LispThread.currentThread();
    Package stepper = Packages.findPackageGlobally("ABCL-STEPPER");
    Package system = Packages.findPackageGlobally("SYSTEM");
    Symbol symbolPrintStepperStr = stepper.findAccessibleSymbol("PRINT-STEPPER-STR");
    LispObject functionPrintStepperStr = Lisp.coerceToFunction(symbolPrintStepperStr);
    if (stepperBlock == null) {
      stepperBlock = env.getOuterMostBlock();
    }
    printForStepping(functionPrintStepperStr, new SimpleString("We are in the stepper mode"), currentThread, true);
    printForStepping(functionPrintStepperStr, new SimpleString("Evaluating :"), currentThread, true);
    LispObject closureName = ((Operator)function).getLambdaName();
    if (closureName != null ) {
      printForStepping(functionPrintStepperStr, new SimpleString(closureName.printObject()), currentThread, true);
    }
    else {
      printForStepping(functionPrintStepperStr, new SimpleString(((Operator)function).printObject()), currentThread, true);
    }
    printForStepping(functionPrintStepperStr, new SimpleString("With args: "), currentThread, true);
    if (args == Lisp.NIL) {
      printForStepping(functionPrintStepperStr, new SimpleString("NIL"), currentThread, true);
    }
    else {
      printForStepping(functionPrintStepperStr, new SimpleString(args.printObject()), currentThread, true);
    }
    boolean leavePrompt = false;
    boolean unexpectedInputUser = false;
    while (!leavePrompt) {
      if (!unexpectedInputUser) {
        printForStepping(functionPrintStepperStr, new SimpleString("Type '?' for a list of options"), currentThread, true);
      }
      unexpectedInputUser = false;
      LispObject charInputUser = Lisp.funcall(Lisp.coerceToFunction(Symbol.READ_CHAR), new LispObject[]{}, currentThread);
      Lisp.funcall(Lisp.coerceToFunction(Symbol.CLEAR_INPUT), new LispObject[]{}, currentThread);
      char inputUser = ((LispCharacter)charInputUser).value;
      String variableName;
      String packageName;
      Package pkg;
      switch (inputUser) {
      case '?':
        setSteppingOff();
        Symbol symbolPrintStepperHelp = stepper.findAccessibleSymbol("PRINT-STEPPER-HELP");
        LispObject functionPrintStepperHelp = Lisp.coerceToFunction(symbolPrintStepperHelp);
        Lisp.funcall(functionPrintStepperHelp, new LispObject[]{}, currentThread);
        setSteppingOnAfterInit();
        break;
      case 'l':
        setSteppingOff();
        Symbol symbolVariables = system.findAccessibleSymbol("ENVIRONMENT-ALL-VARIABLES");
        Symbol symbolFunctions = system.findAccessibleSymbol("ENVIRONMENT-ALL-FUNCTIONS");
        LispObject environmentAllVariables = Lisp.coerceToFunction(symbolVariables);
        LispObject environmentAllFunctions = Lisp.coerceToFunction(symbolFunctions);
        LispObject[] argsEnv = new LispObject[1];
        argsEnv[0] = env;
        LispObject resultVars = Lisp.funcall(environmentAllVariables, argsEnv, currentThread);
        Symbol symbolPprintListLocals = stepper.findAccessibleSymbol("PPRINT-LIST-LOCALS");
        LispObject functionPprintListLocals = Lisp.coerceToFunction(symbolPprintListLocals);
        printForStepping(functionPrintStepperStr, new SimpleString("Showing the values of variable bindings."), currentThread, true);
        printForStepping(functionPrintStepperStr, new SimpleString("From inner to outer scopes:"), currentThread, true);
        argsEnv[0] = resultVars;
        Lisp.funcall(functionPprintListLocals, argsEnv, currentThread);
        argsEnv[0] = env;
        LispObject resultFuncs = Lisp.funcall(environmentAllFunctions, argsEnv, currentThread);
        printForStepping(functionPrintStepperStr, new SimpleString("Showing the values of functions bindings."), currentThread, true);
        printForStepping(functionPrintStepperStr, new SimpleString("From inner to outer scopes:"), currentThread, true);
        argsEnv[0] = resultFuncs;
        Lisp.funcall(functionPprintListLocals, argsEnv, currentThread);
        setSteppingOnAfterInit();
        break;
      case 'c':
        setSteppingOff();
        leavePrompt = true;
        break;
      case 'n':
        setDelimitedSteppingOn();
        leavePrompt = true;
        break;
      case 's':
        leavePrompt = true;
        break;
      case 'q':
        setSteppingOff();
        setDelimitedSteppingOff();
        throw new Return(stepperBlock.symbol, stepperBlock.value, Lisp.NIL);
      case 'i':
        printForStepping(functionPrintStepperStr, new SimpleString("Type the name of the symbol: "), currentThread, false);
        LispObject variableNameLisp = Lisp.funcall(Lisp.coerceToFunction(Symbol.READ_LINE), new LispObject[]{}, currentThread);
        variableName = variableNameLisp.toString().toUpperCase();
        printForStepping(functionPrintStepperStr, new SimpleString("Type the name of the package ('-' for current package): "), currentThread, false);
        LispObject packageNameLisp = Lisp.funcall(Lisp.coerceToFunction(Symbol.READ_LINE), new LispObject[]{}, currentThread);
        packageName = packageNameLisp.toString().toUpperCase();
        if (packageName.equals("-")) {
          pkg = Lisp.getCurrentPackage();
        }
        else {
          pkg = Packages.findPackageGlobally(packageName);
        }
        if (pkg == null) {
          printForStepping(functionPrintStepperStr, new SimpleString(String.format("Couldn't find the package: %s", packageName)), currentThread, true);
        }
        else {
          Symbol symbol = pkg.findAccessibleSymbol(variableName);
          if (symbol == null) {
            String message = String.format("Couldn't find the symbol %s in the package %s", variableName, pkg.getName());
            printForStepping(functionPrintStepperStr, new SimpleString(message), currentThread, true);
          }
          else {
            LispObject variableValue = env.lookup(symbol);
            if (variableValue == null) {
              LispObject symbolValue = ((Symbol)symbol).symbolValueNoThrow();
              if (symbolValue != null) {
                printForStepping(functionPrintStepperStr, new SimpleString(symbolValue.printObject()), currentThread, true);
              }
              else {
                printForStepping(functionPrintStepperStr,
                                 new SimpleString(String.format("Couldn't find the value for: %s", variableName)),
                                 currentThread, true);
              }
            }
            else {
              printForStepping(functionPrintStepperStr, new SimpleString(variableValue.printObject()), currentThread, true);
            }
          }
        }
        break;
      default:
        unexpectedInputUser = true;
      }
    }
  }

  public static final void printForStepping(LispObject printFunction, SimpleString str, LispThread currentThread, boolean newline) {
    LispObject [] argsPrint = new LispObject[2];
    argsPrint[0] = str;
    argsPrint[1] = newline ? Lisp.T : Lisp.NIL;
    Lisp.funcall(printFunction, argsPrint, currentThread);
  }
    // // ### %set-stepper-on
  public static final Primitive SET_STEPPER_ON =
    new Primitive("%set-stepper-on", Lisp.PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute()

      {
        setSteppingOn();
        return Lisp.NIL;
      }
    };

  // // ### %set-stepper-off
  public static final Primitive SET_STEPPER_OFF =
    new Primitive("%set-stepper-off", Lisp.PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute()

      {
        setSteppingOff();
        return Lisp.NIL;
      }
    };

  // // ### %set-delimited-stepping-off
  public static final Primitive SET_DELIMITED_STEPPING_OFF =
    new Primitive("%set-delimited-stepping-off", Lisp.PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute()

      {
        setDelimitedSteppingOff();
        return Lisp.NIL;
      }
    };

}
