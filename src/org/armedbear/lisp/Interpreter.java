/*
 * Interpreter.java
 *
 * Copyright (C) 2002-2006 Peter Graves
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

public final class Interpreter
{
    // There can only be one interpreter.
    public static Interpreter interpreter;

    private final boolean jlisp;
    private final InputStream inputStream;
    private final OutputStream outputStream;

    private static boolean noinit = false;
    private static boolean nosystem = false;
    private static boolean noinform = false;

    public static synchronized Interpreter getInstance()
    {
        return interpreter;
    }

    // Interface.
    public static synchronized Interpreter createInstance()
    {
        if (interpreter != null)
            return null;
        interpreter = new Interpreter();
        _NOINFORM_.setSymbolValue(T);
        initializeLisp();
        return interpreter;
    }

    public static synchronized Interpreter createDefaultInstance(String[] args)
    {
        if (interpreter != null)
            return null;
        interpreter = new Interpreter();

        if (args != null)
            preprocessCommandLineArguments(args);
        if (!noinform) {
            Stream out = getStandardOutput();
            out._writeString(banner());
            out._finishOutput();
        }
        if (noinform)
            _NOINFORM_.setSymbolValue(T);
        else {
            double uptime = (System.currentTimeMillis() - Main.startTimeMillis) / 1000.0;
            getStandardOutput()._writeString("Low-level initialization completed in " +
                                             uptime + " seconds.\n");
        }
        initializeLisp();
        initializeTopLevel();
        if (!nosystem) 
            initializeSystem();
        if (!noinit)
            processInitializationFile();
        if (args != null)
            postprocessCommandLineArguments(args);

        return interpreter;
    }

    public static synchronized Interpreter createJLispInstance(
        InputStream in,
        OutputStream out,
        String initialDirectory,
        String version)
    {
        if (interpreter != null)
            return null;
        interpreter = new Interpreter(in, out, initialDirectory);

        Stream stdout = getStandardOutput();
        stdout._writeLine(version);
        stdout._writeString(banner());
        stdout._finishOutput();

        initializeJLisp();
        initializeTopLevel();
        initializeSystem();
        processInitializationFile();
        return interpreter;
    }

    public static boolean initialized() {
        return initialized;
    }

    private Interpreter()
    {
        jlisp = false;
        inputStream = null;
        outputStream = null;
    }

    private Interpreter(InputStream inputStream, OutputStream outputStream,
                        String initialDirectory)
    {
        jlisp = true;
        this.inputStream = inputStream;
        this.outputStream = outputStream;
        resetIO(new Stream(Symbol.SYSTEM_STREAM, inputStream, Symbol.CHARACTER),
                new Stream(Symbol.SYSTEM_STREAM, outputStream, Symbol.CHARACTER));
        if (!initialDirectory.endsWith(File.separator))
            initialDirectory = initialDirectory.concat(File.separator);
        Symbol.DEFAULT_PATHNAME_DEFAULTS.setSymbolValue(new Pathname(initialDirectory));
    }

    // Interface.
    public LispObject eval(String s)
    {
        return Lisp.eval(new StringInputStream(s).read(true, NIL, false,
                                                  LispThread.currentThread(),
                                                  Stream.currentReadtable));
    }

    public static synchronized void initializeLisp()
    {
        if (!initialized) {
            Load.loadSystemFile("boot.lisp", false, false, false);
            initialized = true;
        }
    }

    public static synchronized void initializeJLisp()
    {
        if (!initialized) {
            Symbol.FEATURES.setSymbolValue(new Cons(Keyword.J,
                                               Symbol.FEATURES.getSymbolValue()));
            Load.loadSystemFile("boot.lisp", false, false, false);

            try {
                Class.forName("org.armedbear.j.LispAPI");
            }
            catch (ClassNotFoundException e) { } // FIXME: what to do?

            Load.loadSystemFile("j.lisp", false); // not being autoloaded

            initialized = true;
        }
    }

    private static boolean topLevelInitialized;

    private static synchronized void initializeTopLevel()
    {
        if (!topLevelInitialized) {
            // Resolve top-level-loop autoload.
            Symbol TOP_LEVEL_LOOP = intern("TOP-LEVEL-LOOP", PACKAGE_TPL);
            LispObject tplFun = TOP_LEVEL_LOOP.getSymbolFunction();
            if (tplFun instanceof Autoload) {
                Autoload autoload = (Autoload) tplFun;
                autoload.load();
            }

            topLevelInitialized = true;
        }
    }

    private static synchronized void processInitializationFile()
    {
        try {
            String userHome = System.getProperty("user.home");
            File file = new File(userHome, ".abclrc");
            if (file.isFile()) {
                Load.load(file.getCanonicalPath());
                return;
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static synchronized void initializeSystem() 
    {
        Load.loadSystemFile("system", false); // not being autoloaded
    }

    // Check for --noinit; verify that arguments are supplied for --load and
    // --eval options.  Copy all unrecognized arguments into
    // ext:*command-line-argument-list*
    private static void preprocessCommandLineArguments(String[] args)

    {
        LispObject arglist = NIL;

        if (args != null) {
            for (int i = 0; i < args.length; ++i) {
                String arg = args[i];
                if (arg.equals("--noinit")) {
                    noinit = true;
                } else if (arg.equals("--nosystem")) {
                    nosystem = true;
                } else if (arg.equals("--noinform")) {
                    noinform = true;
                } else if (arg.equals("--batch")) {
                    _BATCH_MODE_.setSymbolValue(T);
                } else if (arg.equals("--eval")) {
                    if (i + 1 < args.length) {
                        ++i;
                    } else {
                        System.err.println("No argument supplied to --eval");
                        exit(1);
                    }
                } else if (arg.equals("--load") ||
                           arg.equals("--load-system-file")) {
                    if (i + 1 < args.length) {
                        ++i;
                    } else {
                        System.err.println("No argument supplied to --load");
                        exit(1);
                    }
                } else {
                    arglist = new Cons(args[i], arglist);
                }
            }
        }
        arglist.nreverse();

        _COMMAND_LINE_ARGUMENT_LIST_.setSymbolValue(arglist);
    }

    // Do the --load and --eval actions.
    private static void postprocessCommandLineArguments(String[] args)

    {
        if (args != null) {
            for (int i = 0; i < args.length; ++i) {
                String arg = args[i];
                if (arg.equals("--eval")) {
                    if (i + 1 < args.length) {
                        try {
                            evaluate(args[i + 1]);
                        }
                        catch (UnhandledCondition c) {
                            final String separator =
                                System.getProperty("line.separator");
                            StringBuilder sb = new StringBuilder();
                            sb.append(separator);
                            sb.append("Caught ");
                            sb.append(c.getCondition().typeOf().writeToString());
                            sb.append(" while processing --eval option \"" +
                                      args[i + 1] + "\":");
                            sb.append(separator);
                            sb.append("  ");
                            final LispThread thread = LispThread.currentThread();
                            thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                            sb.append(c.getCondition().writeToString());
                            sb.append(separator);
                            System.err.print(sb.toString());
                            exit(2);
                        }
                        ++i;
                    } else {
                        // Shouldn't happen.
                        System.err.println("No argument supplied to --eval");
                        exit(1);
                    }
                } else if (arg.equals("--load") ||
                           arg.equals("--load-system-file")) {
                    if (i + 1 < args.length) {
                        if (arg.equals("--load"))
                            Load.load(new Pathname(args[i + 1]),
                                      false, false, true);

                        else
                            Load.loadSystemFile(args[i + 1], false); // not being autoloaded
                        ++i;
                    } else {
                        // Shouldn't happen.
                        System.err.println("No argument supplied to --load");
                        exit(1);
                    }
                }
            }
        }
        if (_BATCH_MODE_.getSymbolValue() == T) {
            exit(0);
        }
    }

    public void run()
    {
        final LispThread thread = LispThread.currentThread();
        try {
            Symbol TOP_LEVEL_LOOP = intern("TOP-LEVEL-LOOP", PACKAGE_TPL);
            LispObject tplFun = TOP_LEVEL_LOOP.getSymbolFunction();
            if (tplFun instanceof Function) {
                thread.execute(tplFun);
                return;
            }
            // We only arrive here if something went wrong and we weren't able
            // to load top-level.lisp and run the normal top-level loop.
            Stream out = getStandardOutput();
            while (true) {
                try {
                    thread.resetStack();
                    thread.clearSpecialBindings();
                    out._writeString("* ");
                    out._finishOutput();
                    LispObject object =
                        getStandardInput().read(false, EOF, false, thread,
                                                Stream.currentReadtable);
                    if (object == EOF)
                        break;
                    out.setCharPos(0);
                    Symbol.MINUS.setSymbolValue(object);
                    LispObject result = Lisp.eval(object, new Environment(), thread);
                    Debug.assertTrue(result != null);
                    Symbol.STAR_STAR_STAR.setSymbolValue(Symbol.STAR_STAR.getSymbolValue());
                    Symbol.STAR_STAR.setSymbolValue(Symbol.STAR.getSymbolValue());
                    Symbol.STAR.setSymbolValue(result);
                    Symbol.PLUS_PLUS_PLUS.setSymbolValue(Symbol.PLUS_PLUS.getSymbolValue());
                    Symbol.PLUS_PLUS.setSymbolValue(Symbol.PLUS.getSymbolValue());
                    Symbol.PLUS.setSymbolValue(Symbol.MINUS.getSymbolValue());
                    out = getStandardOutput();
                    out.freshLine();
                    LispObject[] values = thread.getValues();
                    Symbol.SLASH_SLASH_SLASH.setSymbolValue(Symbol.SLASH_SLASH.getSymbolValue());
                    Symbol.SLASH_SLASH.setSymbolValue(Symbol.SLASH.getSymbolValue());
                    if (values != null) {
                        LispObject slash = NIL;
                        for (int i = values.length; i-- > 0;)
                            slash = new Cons(values[i], slash);
                        Symbol.SLASH.setSymbolValue(slash);
                        for (int i = 0; i < values.length; i++)
                            out._writeLine(values[i].writeToString());
                    } else {
                        Symbol.SLASH.setSymbolValue(new Cons(result));
                        out._writeLine(result.writeToString());
                    }
                    out._finishOutput();
                }
                catch (StackOverflowError e) {
                    getStandardInput().clearInput();
                    out._writeLine("Stack overflow");
                }
                catch (ControlTransfer c) {
                    // We're on the toplevel, if this occurs,
                    // we're toast...
                    reportError(c, thread);
                }
                catch (Throwable t) {
                    getStandardInput().clearInput();
                    out.printStackTrace(t);
                    thread.printBacktrace();
                }
            }
        }
        catch (Throwable t) {
            t.printStackTrace();
        }
    }

    private static void reportError(ControlTransfer c, LispThread thread)
    {
        getStandardInput().clearInput();
        Stream out = getStandardOutput();
        out.freshLine();
        Condition condition = (Condition) c.getCondition();
        out._writeLine("Error: unhandled condition: " +
                       condition.writeToString());
        if (thread != null)
            thread.printBacktrace();
    }

    private static void reportError(UnhandledCondition c, LispThread thread)
    {
        getStandardInput().clearInput();
        Stream out = getStandardOutput();
        out.freshLine();
        Condition condition = (Condition) c.getCondition();
        out._writeLine("Error: unhandled condition: " +
                       condition.writeToString());
        if (thread != null)
            thread.printBacktrace();
    }

    public void kill()
    {
        kill(0);
    }

    public void kill(int status)
    {
        if (jlisp) {
            try {
                inputStream.close();
            }
            catch (IOException e) {
                Debug.trace(e);
            }
            try {
                outputStream.close();
            }
            catch (IOException e) {
                Debug.trace(e);
            }
        } else {
            ((Stream)Symbol.STANDARD_OUTPUT.getSymbolValue())._finishOutput();
            ((Stream)Symbol.ERROR_OUTPUT.getSymbolValue())._finishOutput();
            System.exit(status);
        }
    }

    public synchronized void dispose()
    {
        Debug.trace("Interpreter.dispose");
        Debug.assertTrue(interpreter == this);
        interpreter = null;
    }

    @Override
    protected void finalize() throws Throwable
    {
        System.err.println("Interpreter.finalize");
    }

    public static final class UnhandledCondition extends Error
    {
        LispObject condition;

        UnhandledCondition(LispObject condition) {
            this.condition = condition;
        }

        public LispObject getCondition() {
            return condition;
        }

        @Override
        public String getMessage() {
            String conditionText;
            try {
                conditionText = getCondition().writeToString();
            } catch (Throwable t) {
                conditionText = "<error printing Lisp condition>";
            }

            return "Unhandled lisp condition: " + conditionText;
        }


    };

    private static final Primitive _DEBUGGER_HOOK_FUNCTION =
        new Primitive("%debugger-hook-function", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            final LispObject condition = first;
            if (interpreter == null) {
                final LispThread thread = LispThread.currentThread();
                final SpecialBindingsMark mark = thread.markSpecialBindings();
                thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                try {
                    final LispObject truename =
                        Symbol.LOAD_TRUENAME.symbolValue(thread);
                    if (truename != NIL) {
                        final LispObject stream =
                            _LOAD_STREAM_.symbolValue(thread);
                        if (stream instanceof Stream) {
                            final int lineNumber =
                                ((Stream)stream).getLineNumber() + 1;
                            final int offset =
                                ((Stream)stream).getOffset();
                            Debug.trace("Error loading " +
                                        truename.writeToString() +
                                        " at line " + lineNumber +
                                        " (offset " + offset + ")");
                        }
                    }
                    Debug.trace("Encountered unhandled condition of type " +
                                condition.typeOf().writeToString() + ':');
                    Debug.trace("  " + condition.writeToString());
                }
                catch (Throwable t) {} // catch any exception to throw below
                finally {
                    thread.resetSpecialBindings(mark);
                }
            }
            UnhandledCondition uc = new UnhandledCondition(condition);
            if (condition.typep(Symbol.JAVA_EXCEPTION) != NIL)
                uc.initCause((Throwable)JavaException
                        .JAVA_EXCEPTION_CAUSE.execute(condition).javaInstance());
            throw uc;
        }
    };

    public static final LispObject readFromString(String s)
    {
        return new StringInputStream(s).read(true, NIL, false,
                                             LispThread.currentThread(),
                                             Stream.currentReadtable);
    }

    // For j.
    /** Runs its input string through the lisp reader and evaluates the result.
     *
     * @param s A string with a valid Common Lisp expression
     * @return The result of the evaluation
     * @exception UnhandledCondition in case the an error occurs which
     *      should be passed to the Lisp debugger
     */
    public static LispObject evaluate(String s)
    {
        if (!initialized)
            initializeJLisp();
        StringInputStream stream = new StringInputStream(s);
        final LispThread thread = LispThread.currentThread();
        LispObject obj = stream.read(false, EOF, false, thread,
                                     Stream.currentReadtable);
        if (obj == EOF)
            return error(new EndOfFile(stream));
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(Symbol.DEBUGGER_HOOK, _DEBUGGER_HOOK_FUNCTION);
        try {
            return Lisp.eval(obj, new Environment(), thread);
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
    }

    private static final String build;

    static {
        String s = null;
        InputStream in = Interpreter.class.getResourceAsStream("build");
        if (in != null) {
            try {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(in));
                s = reader.readLine();
                reader.close();
            }
            catch (IOException e) {}
        }
        build = s;
    }

    private static String banner()
    {
        final String sep = System.getProperty("line.separator");
        StringBuilder sb = new StringBuilder("Armed Bear Common Lisp ");
        sb.append(Version.getVersion());
        if (build != null) {
            sb.append(" (built ");
            sb.append(build);
            sb.append(')');
        }
        sb.append(sep);
        sb.append("Java ");
        sb.append(System.getProperty("java.version"));
        sb.append(' ');
        sb.append(System.getProperty("java.vendor"));
        sb.append(sep);
        String vm = System.getProperty("java.vm.name");
        if (vm != null) {
            sb.append(vm);
            sb.append(sep);
        }
        return sb.toString();
    }
}
