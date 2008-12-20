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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

public final class Load extends Lisp
{
    public static final LispObject load(String filename)
        throws ConditionThrowable
    {
        final LispThread thread = LispThread.currentThread();
        return load(new Pathname(filename),
                    filename,
                    Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                    Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                    true);
    }

    private static final File findLoadableFile(final String filename,
                                               final String dir)
    {
        File file = new File(dir, filename);
	if (!file.isFile()) {
	    String extension = getExtension(filename);
	    if (extension == null) {
		// No extension specified. Try appending ".lisp" or ".abcl".
		File lispFile = new File(dir, filename.concat(".lisp"));
		File abclFile = new File(dir, filename.concat(".abcl"));
		if (lispFile.isFile() && abclFile.isFile()) {
		    if (abclFile.lastModified() > lispFile.lastModified()) {
			return abclFile;
		    } else {
			return lispFile;
		    }
		} else if (abclFile.isFile()) {
		    return abclFile;
		} else if (lispFile.isFile()) {
		    return lispFile;
                }
            }
        } else
            return file; // the file exists
        return null; // this is the error case: the file does not exist
                     // no need to check again at the caller
    }

    public static final LispObject load(Pathname pathname,
                                        String filename,
                                        boolean verbose,
                                        boolean print,
                                        boolean ifDoesNotExist)
        throws ConditionThrowable
    {
	String dir = null;
        if (!Utilities.isFilenameAbsolute(filename)) {
	    dir =
                coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()).getNamestring();
        }

	File file = findLoadableFile(filename, dir);
        if (file == null) {
            if (ifDoesNotExist)
                return error(new FileError("File not found: " + filename,
                                            pathname));
            else
                return NIL;
        }

	filename = file.getPath();
        ZipFile zipfile = null;
        if (checkZipFile(file))
        {
            try {
                zipfile = new ZipFile(file);
            }
            catch (Throwable t) {
                // Fall through.
            }
        }
        String truename = filename;
        InputStream in = null;
        if (zipfile != null) {
            String name = file.getName();
            int index = name.lastIndexOf('.');
            Debug.assertTrue(index >= 0);
            name = name.substring(0, index).concat("._");
            ZipEntry entry = zipfile.getEntry(name);
            if (entry != null) {
                try {
                    in = zipfile.getInputStream(entry);
                }
                catch (IOException e) {
                    return error(new LispError(e.getMessage()));
                }
            }
        } else {
            try {
                in = new FileInputStream(file);
                truename = file.getCanonicalPath();
            }
            catch (FileNotFoundException e) {
                if (ifDoesNotExist)
                    return error(new FileError("File not found: " + filename,
                                                pathname));
                else
                    return NIL;
            }
            catch (IOException e) {
                return error(new LispError(e.getMessage()));
            }
        }
        try {
            return loadFileFromStream(null, truename,
                                      new Stream(in, Symbol.CHARACTER),
                                      verbose, print, false);
        }
        catch (FaslVersionMismatch e) {
            FastStringBuffer sb =
                new FastStringBuffer("Incorrect fasl version: ");
            sb.append(truename);
            return error(new SimpleError(sb.toString()));
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
            if (zipfile != null) {
                try {
                    zipfile.close();
                }
                catch (IOException e) {
                    return error(new LispError(e.getMessage()));
                }
            }
        }
    }

    public static final LispObject loadSystemFile(String filename)
        throws ConditionThrowable
    {
        final LispThread thread = LispThread.currentThread();
        return loadSystemFile(filename,
                              Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                              Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                              false);
    }

    public static final LispObject loadSystemFile(String filename, boolean auto)
        throws ConditionThrowable
    {
        LispThread thread = LispThread.currentThread();
        if (auto) {
            SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
            thread.bindSpecial(Symbol.CURRENT_READTABLE,
                               STANDARD_READTABLE.symbolValue(thread));
            thread.bindSpecial(Symbol._PACKAGE_, PACKAGE_CL_USER);
            try {
                return loadSystemFile(filename,
                                      _AUTOLOAD_VERBOSE_.symbolValue(thread) != NIL,
                                      Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                      auto);
            }
            finally {
                thread.lastSpecialBinding = lastSpecialBinding;
            }
        } else {
            return loadSystemFile(filename,
                                  Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                                  Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                  auto);
        }
    }

    public static final LispObject loadSystemFile(final String filename,
                                                  boolean verbose,
                                                  boolean print,
                                                  boolean auto)
        throws ConditionThrowable
    {
        final int ARRAY_SIZE = 2;
        String[] candidates = new String[ARRAY_SIZE];
        final String extension = getExtension(filename);
        if (extension == null) {
            // No extension specified.
            candidates[0] = filename + '.' + COMPILE_FILE_TYPE;
            candidates[1] = filename.concat(".lisp");
        } else if (extension.equals(".abcl")) {
            candidates[0] = filename;
            candidates[1] =
                filename.substring(0, filename.length() - 5).concat(".lisp");
        } else
            candidates[0] = filename;
        InputStream in = null;
        Pathname pathname = null;
        String truename = null;
        for (int i = 0; i < ARRAY_SIZE; i++) {
            String s = candidates[i];
            if (s == null)
                break;
            ZipFile zipfile = null;
            final String dir = Site.getLispHome();
            try {
                if (dir != null) {
                    File file = new File(dir, s);
                    if (file.isFile()) {
                        // File exists. For system files, we know the extension
                        // will be .abcl if it is a compiled file.
                        String ext = getExtension(s);
                        if (ext.equalsIgnoreCase(".abcl")) {
                            try {
                                zipfile = new ZipFile(file);
                                String name = file.getName();
                                int index = name.lastIndexOf('.');
                                Debug.assertTrue(index >= 0);
                                name = name.substring(0, index).concat("._");
                                ZipEntry entry = zipfile.getEntry(name);
                                if (entry != null) {
                                    in = zipfile.getInputStream(entry);
                                    truename = file.getCanonicalPath();
                                }
                            }
                            catch (ZipException e) {
                                // Fall through.
                            }
                            catch (Throwable t) {
                                Debug.trace(t);
                                in = null;
                                // Fall through.
                            }
                        }
                        if (in == null) {
                            try {
                                in = new FileInputStream(file);
                                truename = file.getCanonicalPath();
                            }
                            catch (IOException e) {
                                in = null;
                            }
                        }
                    }
                } else {
                    URL url = Lisp.class.getResource(s);
                    if (url != null) {
                        try {
                            in = url.openStream();
                            if ("jar".equals(url.getProtocol()))
                                pathname = new Pathname(url);
                            truename = getPath(url);
                        }
                        catch (IOException e) {
                            in = null;
                        }
                    }
                }
                if (in != null) {
                    final LispThread thread = LispThread.currentThread();
                    final SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                    thread.bindSpecial(_WARN_ON_REDEFINITION_, NIL);
                    try {
                        return loadFileFromStream(pathname, truename,
                                                  new Stream(in, Symbol.CHARACTER),
                                                  verbose, print, auto);
                    }
                    catch (FaslVersionMismatch e) {
                        FastStringBuffer sb =
                            new FastStringBuffer("; Incorrect fasl version: ");
                        sb.append(truename);
                        System.err.println(sb.toString());
                    }
                    finally {
                        thread.lastSpecialBinding = lastSpecialBinding;
                        try {
                            in.close();
                        }
                        catch (IOException e) {
                            return error(new LispError(e.getMessage()));
                        }
                    }
                }
            }
            finally {
                if (zipfile != null) {
                    try {
                        zipfile.close();
                    }
                    catch (IOException e) {
                        return error(new LispError(e.getMessage()));
                    }
                }
            }
        }
        return error(new LispError("File not found: " + filename));
    }

    // ### *fasl-version*
    // internal symbol
    private static final Symbol _FASL_VERSION_ =
        exportConstant("*FASL-VERSION*", PACKAGE_SYS, new Fixnum(28));

    // ### *fasl-anonymous-package*
    // internal symbol
    public static final Symbol _FASL_ANONYMOUS_PACKAGE_ =
        internSpecial("*FASL-ANONYMOUS-PACKAGE*", PACKAGE_SYS, NIL);

    // ### init-fasl
    private static final Primitive INIT_FASL =
        new Primitive("init-fasl", PACKAGE_SYS, true, "&key version")
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first == Keyword.VERSION) {
                if (second.eql(_FASL_VERSION_.getSymbolValue())) {
                    // OK
                    final LispThread thread = LispThread.currentThread();
                    thread.bindSpecial(_FASL_ANONYMOUS_PACKAGE_, NIL);
                    thread.bindSpecial(_SOURCE_, NIL);
                    return faslLoadStream(thread);
                }
            }
            throw new FaslVersionMismatch(second);
        }
    };

    private static final LispObject loadFileFromStream(LispObject pathname,
                                                       String truename,
                                                       Stream in,
                                                       boolean verbose,
                                                       boolean print,
                                                       boolean auto)
        throws ConditionThrowable
    {
        long start = System.currentTimeMillis();
        final LispThread thread = LispThread.currentThread();
        final SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        // "LOAD binds *READTABLE* and *PACKAGE* to the values they held before
        // loading the file."
        thread.bindSpecialToCurrentValue(Symbol.CURRENT_READTABLE);
        thread.bindSpecialToCurrentValue(Symbol._PACKAGE_);
        int loadDepth = Fixnum.getValue(_LOAD_DEPTH_.symbolValue(thread));
        thread.bindSpecial(_LOAD_DEPTH_, new Fixnum(++loadDepth));
        // Compiler policy.
        thread.bindSpecialToCurrentValue(_SPEED_);
        thread.bindSpecialToCurrentValue(_SPACE_);
        thread.bindSpecialToCurrentValue(_SAFETY_);
        thread.bindSpecialToCurrentValue(_DEBUG_);
        thread.bindSpecialToCurrentValue(_EXPLAIN_);
        final String prefix = getLoadVerbosePrefix(loadDepth);
        try {
            if (pathname == null && truename != null)
                pathname = Pathname.parseNamestring(truename);
            thread.bindSpecial(Symbol.LOAD_PATHNAME,
                               pathname != null ? pathname : NIL);
            thread.bindSpecial(Symbol.LOAD_TRUENAME,
                               pathname != null ? pathname : NIL);
            thread.bindSpecial(_SOURCE_,
                               pathname != null ? pathname : NIL);
            if (verbose) {
                Stream out = getStandardOutput();
                out.freshLine();
                out._writeString(prefix);
                out._writeString(auto ? " Autoloading " : " Loading ");
                out._writeString(truename != null ? truename : "stream");
                out._writeLine(" ...");
                out._finishOutput();
                LispObject result = loadStream(in, print, thread);
                long elapsed = System.currentTimeMillis() - start;
                out.freshLine();
                out._writeString(prefix);
                out._writeString(auto ? " Autoloaded " : " Loaded ");
                out._writeString(truename != null ? truename : "stream");
                out._writeString(" (");
                out._writeString(String.valueOf(((float)elapsed)/1000));
                out._writeLine(" seconds)");
                out._finishOutput();
                return result;
            } else
                return loadStream(in, print, thread);
        }
        finally {
            thread.lastSpecialBinding = lastSpecialBinding;
        }
    }

    public static String getLoadVerbosePrefix(int loadDepth)
    {
        FastStringBuffer sb = new FastStringBuffer(";");
        for (int i = loadDepth - 1; i-- > 0;)
            sb.append(' ');
        return sb.toString();
    }

    private static final LispObject loadStream(Stream in, boolean print,
                                               LispThread thread)
        throws ConditionThrowable
    {
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        thread.bindSpecial(_LOAD_STREAM_, in);
        SpecialBinding sourcePositionBinding =
            new SpecialBinding(_SOURCE_POSITION_, Fixnum.ZERO,
                               thread.lastSpecialBinding);
        thread.lastSpecialBinding = sourcePositionBinding;
        try {
            final Environment env = new Environment();
            while (true) {
                sourcePositionBinding.value = new Fixnum(in.getOffset());
                LispObject obj = in.read(false, EOF, false, thread);
                if (obj == EOF)
                    break;
                LispObject result = eval(obj, env, thread);
                if (print) {
                    Stream out =
                        checkCharacterOutputStream(Symbol.STANDARD_OUTPUT.symbolValue(thread));
                    out._writeLine(result.writeToString());
                    out._finishOutput();
                }
            }
            return T;
        }
        finally {
            thread.lastSpecialBinding = lastSpecialBinding;
        }
    }

    private static final LispObject faslLoadStream(LispThread thread)
        throws ConditionThrowable
    {
        Stream in = (Stream) _LOAD_STREAM_.symbolValue(thread);
        final Environment env = new Environment();
        while (true) {
            LispObject obj = in.faslRead(false, EOF, true, thread);
            if (obj == EOF)
                break;
            eval(obj, env, thread);
        }
        return T;
    }

    // Returns extension including leading '.'
    private static final String getExtension(String filename)
    {
        int index = filename.lastIndexOf('.');
        if (index < 0)
            return null;
        if (index < filename.lastIndexOf(File.separatorChar))
            return null; // Last dot was in path part of filename.
        return filename.substring(index);
    }

    private static final String getPath(URL url)
    {
        if (url != null) {
            String path = url.getPath();
            if (path != null) {
                if (Utilities.isPlatformWindows) {
                    if (path.length() > 0 && path.charAt(0) == '/')
                        path = path.substring(1);
                }
                return path;
            }
        }
        return null;
    }

    private static final boolean checkZipFile(File file)
    {
        InputStream in = null;
        try {
            in = new FileInputStream(file);
            byte[] bytes = new byte[4];
            int bytesRead = in.read(bytes);
            return (bytesRead == 4
                    && bytes[0] == 0x50
                    && bytes[1] == 0x4b
                    && bytes[2] == 0x03
                    && bytes[3] == 0x04);
        }
        catch (Throwable t) {
            return false;
        }
        finally {
            if (in != null) {
                try {
                    in.close();
                }
                catch (Throwable t) {}
            }
        }
    }

    // ### %load filespec verbose print if-does-not-exist => generalized-boolean
    private static final Primitive _LOAD =
        new Primitive("%load", PACKAGE_SYS, false,
                      "filespec verbose print if-does-not-exist")
    {
        public LispObject execute(LispObject filespec, LispObject verbose,
                                  LispObject print, LispObject ifDoesNotExist)
            throws ConditionThrowable
        {
            if (filespec instanceof Stream) {
                if (((Stream)filespec).isOpen()) {
                    LispObject pathname;
                    if (filespec instanceof FileStream)
                        pathname = ((FileStream)filespec).getPathname();
                    else
                        pathname = NIL;
                    String truename;
                    if (pathname instanceof Pathname)
                        truename = ((Pathname)pathname).getNamestring();
                    else
                        truename = null;
                    return loadFileFromStream(pathname,
                                              truename,
                                              (Stream) filespec,
                                              verbose != NIL,
                                              print != NIL,
                                              false);
                }
                // If stream is closed, fall through...
            }
            Pathname pathname = coerceToPathname(filespec);
            if (pathname instanceof LogicalPathname)
                pathname = LogicalPathname.translateLogicalPathname((LogicalPathname)pathname);
            return load(pathname,
                        pathname.getNamestring(),
                        verbose != NIL,
                        print != NIL,
                        ifDoesNotExist != NIL);
        }
    };

    // ### load-system-file
    private static final Primitive LOAD_SYSTEM_FILE =
        new Primitive("load-system-file", PACKAGE_SYS, true)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            final LispThread thread = LispThread.currentThread();
            return loadSystemFile(arg.getStringValue(),
                                  Symbol.LOAD_VERBOSE.symbolValue(thread) != NIL,
                                  Symbol.LOAD_PRINT.symbolValue(thread) != NIL,
                                  false);
        }
    };

    private static class FaslVersionMismatch extends Error
    {
        private final LispObject version;

        public FaslVersionMismatch(LispObject version)
        {
            this.version = version;
        }

        public LispObject getVersion()
        {
            return version;
        }
    }
}
