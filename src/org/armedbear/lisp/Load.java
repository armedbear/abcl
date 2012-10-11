/*
 * Load.java
 *
 * Copyright (C) 2002-2007 Peter Graves
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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/* This file holds ABCL's (FASL and non-FASL) loading behaviours.
 *
 * The loading process works like this:
 *   The loader associates the input filename with a special variable
 *   and starts evaluating the forms in the file.
 *
 *   If one of the forms is (INIT-FASL :VERSION <version>), from that
 *   point the file is taken to be a FASL.
 *   The FASL loader takes over and retrieves the file being loaded
 *   from the special variable and continues loading from there.
 *
 */
public final class Load
{
    public static final LispObject load(String filename)
    {
        final LispThread thread = LispThread.currentThread();
        return load(new Pathname(filename),
                    Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                    Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                    true);
    }
  
    /** @return Pathname of loadable file based on NAME, or null if
     * none can be determined. */
    private static final Pathname findLoadableFile(Pathname name) {
        LispObject truename  = Pathname.truename(name, false);
        if (truename instanceof Pathname) {
            Pathname t = (Pathname)truename;
            if (t.name != NIL
                && t.name != null) {
                return t;
            }
        }
        final String COMPILE_FILE_TYPE = Lisp._COMPILE_FILE_TYPE_.symbolValue().getStringValue();
        if (name.type == NIL
            && (name.name != NIL || name.name != null)) {
            Pathname lispPathname = new Pathname(name);
            lispPathname.type = new SimpleString("lisp");
            lispPathname.invalidateNamestring();
            LispObject lisp = Pathname.truename(lispPathname, false);
            Pathname abclPathname = new Pathname(name);
            abclPathname.type = new SimpleString(COMPILE_FILE_TYPE);
            abclPathname.invalidateNamestring();
            LispObject abcl = Pathname.truename(abclPathname, false);
            if (lisp instanceof Pathname && abcl instanceof Pathname) {
              lispPathname = (Pathname)lisp;
              abclPathname = (Pathname)abcl;
              long lispLastModified = lispPathname.getLastModified();
              long abclLastModified = abclPathname.getLastModified();
              if (abclLastModified > lispLastModified) {
                  return abclPathname;  // fasl file is newer
              } else {
                  return lispPathname;
              }
            } else if (abcl instanceof Pathname) {
                return (Pathname) abcl;
            } else if (lisp instanceof Pathname) { 
                return (Pathname) lisp;
            }
        }
        if (name.isJar()) {
            if (name.type.equals(NIL)) {
                name.type = COMPILE_FILE_INIT_FASL_TYPE;
                name.invalidateNamestring();
                Pathname result = findLoadableFile(name);
                if (result != null) {
                    return result;
                }
                name.type = new SimpleString(COMPILE_FILE_TYPE);
                name.invalidateNamestring();
                result = findLoadableFile(name);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }
  
    public static final LispObject load(Pathname pathname,
                                        boolean verbose,
                                        boolean print,
                                        boolean ifDoesNotExist)
    {
        return load(pathname, verbose, print, ifDoesNotExist, false);
    }

    public static final LispObject load(final Pathname pathname,
                                        boolean verbose,
                                        boolean print,
                                        boolean ifDoesNotExist,
                                        boolean returnLastResult)

    {
        Pathname mergedPathname = null;
        if (!pathname.isAbsolute() && !pathname.isJar()) {
            Pathname pathnameDefaults 
                = coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue());
            mergedPathname = Pathname.mergePathnames(pathname, pathnameDefaults);
        }
        Pathname loadableFile = findLoadableFile(mergedPathname != null ? mergedPathname : pathname);
        Pathname truename = coercePathnameOrNull(Pathname.truename(loadableFile));

        if (truename == null || truename.equals(NIL)) {
            if (ifDoesNotExist) {
                return error(new FileError("File not found: " + pathname.princToString(), pathname));
            } else {
                Debug.warn("Failed to load " + pathname.getNamestring());
                return NIL;
            }
        }

        if (Utilities.checkZipFile(truename)) {
            String n = truename.getNamestring();
            String name = Pathname.uriEncode(truename.name.getStringValue());
            if (n.startsWith("jar:")) {
                n = "jar:" + n + "!/" + name + "."
                    + COMPILE_FILE_INIT_FASL_TYPE;
	    } else if (n.startsWith("zip:")) {
                n = "zip:" + n + "!/" + name + "."
                    + COMPILE_FILE_INIT_FASL_TYPE;
            } else {
                n = "jar:file:" + Pathname.uriEncode(n) + "!/" + name + "."
                    + COMPILE_FILE_INIT_FASL_TYPE;
            }
            mergedPathname = new Pathname(n);
            LispObject initTruename = Pathname.truename(mergedPathname);
            if (initTruename == null || initTruename.equals(NIL)) {
                // Maybe the enclosing JAR has been renamed?
                Pathname p = new Pathname(mergedPathname);
                p.name = Keyword.WILD;
                p.invalidateNamestring();
                LispObject result = Pathname.MATCH_WILD_JAR_PATHNAME.execute(p);

                if      (result instanceof Cons
                    && ((Cons)result).length() == 1
                    && ((Cons)result).car() instanceof Pathname) {
                    initTruename = (Pathname)result.car();
                } else {
                  String errorMessage
                      = "Loadable FASL not found for "
                      + "'" + pathname.printObject() + "'"
                      + " in "
                      + "'" + mergedPathname.printObject() + "'";
                  if (ifDoesNotExist) {
                      return error(new FileError(errorMessage, mergedPathname));
                  } else {
                      Debug.trace(errorMessage);
                      return NIL;
                  }
                }
            }
            truename = (Pathname)initTruename;
        } 
				
        InputStream in = truename.getInputStream();
        Debug.assertTrue(in != null);
    
        try {
            return loadFileFromStream(pathname, truename,
                                      new Stream(Symbol.SYSTEM_STREAM, in, Symbol.CHARACTER),
                                      verbose, print, false, returnLastResult);
        }
        finally {
            if (in != null) {
                try {
                   in.close();
                }
                catch (IOException e) {
                    return error(new LispError(e.getMessage()));
                }
            }
        }
    }

    public static LispObject loadSystemFile(String filename, boolean auto)
    {
        LispThread thread = LispThread.currentThread();
        if (auto) {
            final SpecialBindingsMark mark = thread.markSpecialBindings();
            // Due to autoloading, we're not sure about the loader state.
            // Make sure that all reader relevant variables have known state.
            thread.bindSpecial(Symbol.CURRENT_READTABLE,
                               STANDARD_READTABLE.symbolValue(thread));
            thread.bindSpecial(Symbol.READ_BASE, Fixnum.constants[10]);
            thread.bindSpecial(Symbol.READ_SUPPRESS, NIL);
            thread.bindSpecial(Symbol.READ_EVAL, T);
            thread.bindSpecial(Symbol.READ_DEFAULT_FLOAT_FORMAT, Symbol.SINGLE_FLOAT);
            thread.bindSpecial(Symbol._PACKAGE_, PACKAGE_CL_USER);
            try {
                return loadSystemFile(filename,
                                      _AUTOLOAD_VERBOSE_.symbolValue(thread) != NIL,
                                      Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                      auto);
            }
            finally {
                thread.resetSpecialBindings(mark);
            }
        } else {
            return loadSystemFile(filename,
                                  Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                                  Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                  auto);
        }
    }

    private static final Symbol FASL_LOADER = PACKAGE_SYS.intern("*FASL-LOADER*");
    static final LispObject COMPILE_FILE_INIT_FASL_TYPE = new SimpleString("_");

    private static final Pathname coercePathnameOrNull(LispObject p) {
        if (p == null) {
            return null;
        }
        Pathname result = null;
        try {
            result = (Pathname)p;
        } catch (Throwable t) { // XXX narrow me!
            return null;
        }
        return result;
    }
        

    public static final LispObject loadSystemFile(final String filename,
                                                  boolean verbose,
                                                  boolean print,
                                                  boolean auto)

    {
        InputStream in = null;
        Pathname pathname = null;
        Pathname truename = null;
        pathname = new Pathname(filename);
        LispObject bootPath = Site.getLispHome();
        Pathname mergedPathname;
        if (bootPath instanceof Pathname) {
            mergedPathname = Pathname.mergePathnames(pathname, (Pathname)bootPath);
        } else {
            mergedPathname = pathname;
        }
        URL url = null;
        Pathname loadableFile = findLoadableFile(mergedPathname);
        truename = coercePathnameOrNull(Pathname.truename(loadableFile));
        
        final String COMPILE_FILE_TYPE 
          = Lisp._COMPILE_FILE_TYPE_.symbolValue().getStringValue();

        if (truename == null || truename.equals(NIL) || bootPath.equals(NIL)) {
            // Make an attempt to use the boot classpath
            String path = pathname.asEntryPath();
            url = Lisp.class.getResource(path);            
            if (url == null || url.toString().endsWith("/")) {
                url = Lisp.class.getResource(path.replace('-', '_') + "." + COMPILE_FILE_TYPE);
                if (url == null) {
                    url = Lisp.class.getResource(path + ".lisp");
                }
            }
            if (url == null) {
                return error(new LispError("Failed to find loadable system file "
                                           + "'" + path + "'"
                                           + " in boot classpath."));
            }                
            if (!bootPath.equals(NIL)) {
                Pathname urlPathname = new Pathname(url);
							  loadableFile = findLoadableFile(urlPathname);
								truename = (Pathname)Pathname.truename(loadableFile);
                if (truename == null) {
                    return error(new LispError("Failed to find loadable system file in boot classpath "
                                               + "'" + url + "'"));
                }
            } else {
                truename = null; // We can't represent the FASL in a Pathname (q.v. OSGi)
            }
        }

        // Look for a init FASL inside a packed FASL
        if (truename != null
            && truename.type.princToString().equals(COMPILE_FILE_TYPE) && Utilities.checkZipFile(truename))  {
            Pathname init = new Pathname(truename.getNamestring());
            init.type = COMPILE_FILE_INIT_FASL_TYPE;
            LispObject t = Pathname.truename(init);
            if (t instanceof Pathname) {
                truename = (Pathname)t;
            } else {
                return error (new LispError("Failed to find loadable init FASL in "
                                            + "'" + init.getNamestring() + "'"));
            }
        }

        if (truename != null) {
            in = truename.getInputStream();
        } else { 
            try {
                Debug.assertTrue(url != null);
                in = url.openStream();
            } catch (IOException e) {
                error(new FileError("Failed to load system file: " 
                                    + "'" + filename + "'"
                                    + " from URL: " 
                                    + "'" + url + "'"));
            } 
        }

        if (in != null) {
            final LispThread thread = LispThread.currentThread();
            final SpecialBindingsMark mark = thread.markSpecialBindings();
            thread.bindSpecial(_WARN_ON_REDEFINITION_, NIL);
	    thread.bindSpecial(FASL_LOADER, NIL);
            try {
                Stream stream = new Stream(Symbol.SYSTEM_STREAM, in, Symbol.CHARACTER);
                return loadFileFromStream(pathname, truename, stream,
                                          verbose, print, auto);
            } finally {
                thread.resetSpecialBindings(mark);
                try {
                    in.close();
                }
                catch (IOException e) {
                    return error(new LispError(e.getMessage()));
                }
            }
        }
        return error(new FileError("Failed to load system file: " 
                                   + "'" + filename + "'"
                                   + " resolved as " 
                                   + "'" + mergedPathname + "'" , 
                                   truename));
    }

    // ### *fasl-version*
    // internal symbol
    static final Symbol _FASL_VERSION_ =
        exportConstant("*FASL-VERSION*", PACKAGE_SYS, Fixnum.getInstance(39));

    // ### *fasl-external-format*
    // internal symbol
    private static final Symbol _FASL_EXTERNAL_FORMAT_ =
        internConstant("*FASL-EXTERNAL-FORMAT*", PACKAGE_SYS,
                       new SimpleString("UTF-8"));

    // ### *fasl-uninterned-symbols*
    // internal symbol
    /**
     * This variable gets bound to NIL upon loading a FASL, but
     * gets set to a vector of symbols as one of the first actions
     * by the FASL itself.
     *
     */
    public static final Symbol _FASL_UNINTERNED_SYMBOLS_ =
        internSpecial("*FASL-UNINTERNED-SYMBOLS*", PACKAGE_SYS, NIL);

    // Function to access the uninterned symbols "array"
    public final static LispObject getUninternedSymbol(int n) {
        LispThread thread = LispThread.currentThread();
        LispObject uninternedSymbols =
            Load._FASL_UNINTERNED_SYMBOLS_.symbolValue(thread);

        if (! (uninternedSymbols instanceof Cons)) // it must be a vector
            return uninternedSymbols.AREF(n);

        // During normal loading, we won't get to this bit, however,
        // with eval-when processing, we may need to fall back to
        // *FASL-UNINTERNED-SYMBOLS* being an alist structure
        LispObject label = LispInteger.getInstance(n);
        while (uninternedSymbols != NIL)
            {
                LispObject item = uninternedSymbols.car();
                if (label.eql(item.cdr()))
                  return item.car();

                uninternedSymbols = uninternedSymbols.cdr();
            }
        return error(new LispError("No entry for uninterned symbol."));
    }


    // ### init-fasl &key version
    private static final Primitive INIT_FASL = new init_fasl();
    private static class init_fasl extends Primitive {
        init_fasl() {
            super("init-fasl", PACKAGE_SYS, true, "&key version");
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final LispThread thread = LispThread.currentThread();
            if (first == Keyword.VERSION) {
                if (second.eql(_FASL_VERSION_.getSymbolValue())) {
                    // OK
                    thread.bindSpecial(_FASL_UNINTERNED_SYMBOLS_, NIL);
                    thread.bindSpecial(_SOURCE_, NIL);
                    return faslLoadStream(thread);
                }
            }
            return
                error(new SimpleError("FASL version mismatch; found '"
                        + second.princToString() + "' but expected '"
                        + _FASL_VERSION_.getSymbolValue().princToString()
                        + "' in "
                        + Symbol.LOAD_PATHNAME.symbolValue(thread).princToString()));
        }
    }

    private static final LispObject loadFileFromStream(Pathname pathname,
                                                       Pathname truename,
                                                       Stream in,
                                                       boolean verbose,
                                                       boolean print,
                                                       boolean auto)
        {
            return loadFileFromStream(pathname == null ? NIL : pathname,
                                      truename == null ? NIL : truename,
                                      in, verbose, print, auto, false);
    }

    private static Symbol[] savedSpecials =
        new Symbol[] { // CLHS Specified
                       Symbol.CURRENT_READTABLE, Symbol._PACKAGE_,
                       // Compiler policy
                       _SPEED_, _SPACE_, _SAFETY_, _DEBUG_, _EXPLAIN_ };

    // A nil TRUENAME signals a load from stream which has no possible path
    private static final LispObject loadFileFromStream(LispObject pathname,
                                                       LispObject truename,
                                                       Stream in,
                                                       boolean verbose,
                                                       boolean print,
                                                       boolean auto,
                                                       boolean returnLastResult)

    {
        long start = System.currentTimeMillis();
        final LispThread thread = LispThread.currentThread();
        final SpecialBindingsMark mark = thread.markSpecialBindings();

        for (Symbol special : savedSpecials)
            thread.bindSpecialToCurrentValue(special);

        int loadDepth = Fixnum.getValue(_LOAD_DEPTH_.symbolValue(thread));
        thread.bindSpecial(_LOAD_DEPTH_, Fixnum.getInstance(++loadDepth));
        final String prefix = getLoadVerbosePrefix(loadDepth);
        try {
            thread.bindSpecial(Symbol.LOAD_PATHNAME, pathname);

            // The motivation behind the following piece of complexity
            // is the need to preserve the semantics of
            // *LOAD-TRUENAME* as always containing the truename of
            // the current "Lisp file".  Since an ABCL packed FASL
            // actually has a Lisp file (aka "the init FASL") and one
            // or more Java classes from the compiler, we endeavor to
            // make *LOAD-TRUENAME* refer to the "overall" truename so
            // that a (LOAD *LOAD-TRUENAME*) would be equivalent to
            // reloading the complete current "Lisp file".  If this
            // value diverges from the "true" truename, we set the
            // symbol SYS::*LOAD-TRUENAME-FASL* to that divergent
            // value.  Currently the only code that uses this value is
            // Lisp.readFunctionBytes().
            Pathname truePathname = null;
            if (!truename.equals(NIL)) {
                if (truename instanceof Pathname) {
                    truePathname = new Pathname((Pathname)truename);
                } else if (truename instanceof AbstractString) {
                    truePathname = new Pathname(truename.getStringValue());
                } else {
                    Debug.assertTrue(false);
                }
                String type = truePathname.type.getStringValue();
                if (type.equals(Lisp._COMPILE_FILE_TYPE_.symbolValue(thread).getStringValue())
                    || type.equals(COMPILE_FILE_INIT_FASL_TYPE.toString())) {
                    Pathname truenameFasl = new Pathname(truePathname);
                    thread.bindSpecial(Symbol.LOAD_TRUENAME_FASL, truenameFasl);
                }
                if (truePathname.type.getStringValue()
                    .equals(COMPILE_FILE_INIT_FASL_TYPE.getStringValue())
                    && truePathname.isJar()) {
                    if (truePathname.device.cdr() != NIL ) {
                        // We set *LOAD-TRUENAME* to the argument that
                        // a user would pass to LOAD.
                        Pathname enclosingJar = (Pathname)truePathname.device.cdr().car();
                        truePathname.device = new Cons(truePathname.device.car(), NIL);
                        truePathname.host = NIL;
                        truePathname.directory = enclosingJar.directory;
                        if (truePathname.directory.car().equals(Keyword.RELATIVE)) {
                            truePathname.directory.setCar(Keyword.ABSOLUTE);
                        }
                        truePathname.name = enclosingJar.name;
                        truePathname.type = enclosingJar.type;
                        truePathname.invalidateNamestring();
                    } else {
                        // XXX There is something fishy in the asymmetry
                        // between the "jar:jar:http:" and "jar:jar:file:"
                        // cases but this currently passes the tests.
                        if (!(truePathname.device.car() instanceof AbstractString)) {
                            truePathname = (Pathname)truePathname.device.car();
                            truePathname.invalidateNamestring();
                        }
                    }
                    thread.bindSpecial(Symbol.LOAD_TRUENAME, truePathname);
                } else {
                    thread.bindSpecial(Symbol.LOAD_TRUENAME, truename);
                }
            } else {
                thread.bindSpecial(Symbol.LOAD_TRUENAME, truename);
            }
            thread.bindSpecial(_SOURCE_,
                               pathname != null ? pathname : NIL);
            if (verbose) {
                Stream out = getStandardOutput();
                out.freshLine();
                out._writeString(prefix);
                out._writeString(auto ? " Autoloading " : " Loading ");
                out._writeString(!truename.equals(NIL) ? truePathname.princToString() : "stream");
                out._writeLine(" ...");
                out._finishOutput();
                LispObject result = loadStream(in, print, thread, returnLastResult);
                long elapsed = System.currentTimeMillis() - start;
                out.freshLine();
                out._writeString(prefix);
                out._writeString(auto ? " Autoloaded " : " Loaded ");
                out._writeString(!truename.equals(NIL) ? truePathname.princToString() : "stream");
                out._writeString(" (");
                out._writeString(String.valueOf(((float)elapsed)/1000));
                out._writeLine(" seconds)");
                out._finishOutput();
                return result;
            } else
                return loadStream(in, print, thread, returnLastResult);
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
    }

    public static String getLoadVerbosePrefix(int loadDepth)
    {
        StringBuilder sb = new StringBuilder(";");
        for (int i = loadDepth - 1; i-- > 0;)
            sb.append(' ');
        return sb.toString();
    }

    private static final LispObject loadStream(Stream in, boolean print,
                                               LispThread thread, boolean returnLastResult)

    {
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(_LOAD_STREAM_, in);
        SpecialBinding sourcePositionBinding =
            thread.bindSpecial(_SOURCE_POSITION_, Fixnum.ZERO);
        try {
            final Environment env = new Environment();
            LispObject result = NIL;
            while (true) {
                sourcePositionBinding.value = Fixnum.getInstance(in.getOffset());
                LispObject obj = in.read(false, EOF, false,
                                         thread, Stream.currentReadtable);
                if (obj == EOF)
                    break;
		result = eval(obj, env, thread);
                if (print) {
                    Stream out =
                        checkCharacterOutputStream(Symbol.STANDARD_OUTPUT.symbolValue(thread));
                    out._writeLine(result.printObject());
                    out._finishOutput();
                }
            }
            if(returnLastResult) {
                return result;
            } else {
                return T;
            }
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
    }

    static final LispObject faslLoadStream(LispThread thread)
    {
        Stream in = (Stream) _LOAD_STREAM_.symbolValue(thread);
        final Environment env = new Environment();
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        LispObject result = NIL;
        try {
            // Same bindings are established in Lisp.readObjectFromString()
            thread.bindSpecial(Symbol.READ_BASE, LispInteger.getInstance(10));
            thread.bindSpecial(Symbol.READ_EVAL, Symbol.T);
            thread.bindSpecial(Symbol.READ_SUPPRESS, Nil.NIL);

            in.setExternalFormat(_FASL_EXTERNAL_FORMAT_.symbolValue(thread));
            while (true) {
                LispObject obj = in.read(false, EOF, false,  // should be 'true' once we
                                                             // have a FASL wide object table
                                         thread, Stream.faslReadtable);
                if (obj == EOF)
                    break;
                result = eval(obj, env, thread);
            }
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
        return result;
        //There's no point in using here the returnLastResult flag like in
        //loadStream(): this function is only called from init-fasl, which is
        //only called from load, which already has its own policy for choosing
        //whether to return T or the last value.
    }


    // ### %load filespec verbose print if-does-not-exist => generalized-boolean
    private static final Primitive _LOAD = new _load();
    private static class _load extends Primitive {
        _load() {
            super("%load", PACKAGE_SYS, false,
                  "filespec verbose print if-does-not-exist");
        }
        @Override
        public LispObject execute(LispObject filespec, LispObject verbose,
                                  LispObject print, LispObject ifDoesNotExist)
        {
            return load(filespec, verbose, print, ifDoesNotExist, NIL);
        }
    }

    // ### %load-returning-last-result filespec verbose print if-does-not-exist => object
    private static final Primitive _LOAD_RETURNING_LAST_RESULT = new _load_returning_last_result();
    private static class _load_returning_last_result extends Primitive {
        _load_returning_last_result() {
            super("%load-returning-last-result", PACKAGE_SYS, false,
                  "filespec verbose print if-does-not-exist");
        }
        @Override
        public LispObject execute(LispObject filespec, LispObject verbose,
                                  LispObject print, LispObject ifDoesNotExist)
            {
            return load(filespec, verbose, print, ifDoesNotExist, T);
        }
    }

    static final LispObject load(LispObject filespec,
                                         LispObject verbose,
                                         LispObject print,
                                         LispObject ifDoesNotExist,
                                         LispObject returnLastResult)
        {
        if (filespec instanceof Stream) {
            if (((Stream)filespec).isOpen()) {
                LispObject pathname;
                if (filespec instanceof FileStream)
                    pathname = ((FileStream)filespec).getPathname();
                else
                    pathname = NIL;
                LispObject truename;
                if (pathname instanceof Pathname)
                    truename = pathname;
                else
                    truename = NIL;
                return loadFileFromStream(pathname,
                                          truename,
                                          (Stream) filespec,
                                          verbose != NIL,
                                          print != NIL,
                                          false,
                                          returnLastResult != NIL);
            }
            // If stream is closed, fall through...
        }
        Pathname pathname = coerceToPathname(filespec);
        if (pathname instanceof LogicalPathname)
            pathname = LogicalPathname.translateLogicalPathname((LogicalPathname)pathname);
        return load(pathname,
                    verbose != NIL,
                    print != NIL,
                    ifDoesNotExist != NIL,
                    returnLastResult != NIL);
    }

    // ### load-system-file
    private static final Primitive LOAD_SYSTEM_FILE = new load_system_file();
    private static class load_system_file extends Primitive {
        load_system_file () {
            super("load-system-file", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            final LispThread thread = LispThread.currentThread();
            return loadSystemFile(arg.getStringValue(),
                                  Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL
                                  || System.getProperty("abcl.autoload.verbose") != null,
                                  Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                  false);
        }
    }
}
