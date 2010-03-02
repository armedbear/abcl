/*
 * Profiler.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public class Profiler
{
    static int sleep = 1;

    // ### %start-profiler
    // %start-profiler type granularity
    public static final Primitive _START_PROFILER =
        new Primitive("%start-profiler", PACKAGE_PROF, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final LispThread thread = LispThread.currentThread();
            Stream out = getStandardOutput();
            out.freshLine();
            if (profiling) {
                out._writeLine("; Profiler already started.");
            } else {
                if (first == Keyword.TIME)
                    sampling = true;
                else if (first == Keyword.COUNT_ONLY)
                    sampling = false;
                else
                    return error(new LispError(
                        "%START-PROFILER: argument must be either :TIME or :COUNT-ONLY"));
                Package[] packages = Packages.getAllPackages();
                for (int i = 0; i < packages.length; i++) {
                    Package pkg = packages[i];
                    Symbol[] symbols = pkg.symbols();
                    for (int j = 0; j < symbols.length; j++) {
                        Symbol symbol = symbols[j];
                        LispObject object = symbol.getSymbolFunction();
                        if (object != null) {
                            object.setCallCount(0);
                            object.setHotCount(0);
                            if (object instanceof StandardGenericFunction) {
                                LispObject methods =
                                    PACKAGE_MOP.intern("GENERIC-FUNCTION-METHODS").execute(object);
                                while (methods != NIL) {
                                    StandardMethod method = (StandardMethod) methods.car();
                                    method.getFunction().setCallCount(0);
                                    method.getFunction().setHotCount(0);
                                    methods = methods.cdr();
                                }
                            }
                        }
                    }
                }
                if (sampling) {
                    sleep = Fixnum.getValue(second);
                    Runnable profilerRunnable = new Runnable() {
                        public void run()
                        {
                            profiling = true; // make sure we don't fall through on the first iteration
                            while (profiling) {
                                try {
                                    thread.incrementCallCounts();
                                    Thread.sleep(sleep);
                                }
                                //### FIXME exception
                                catch (InterruptedException e) {
                                    Debug.trace(e);
                                }
                            }
                        }
                    };
                    Thread t = new Thread(profilerRunnable);
                    // Maximum priority doesn't hurt:
                    // we're sleeping all the time anyway
                    t.setPriority(Thread.MAX_PRIORITY);
                    new Thread(profilerRunnable).start();
                }
                out._writeLine("; Profiler started.");
            }
            return thread.nothing();
        }
    };

    // ### stop-profiler
    public static final Primitive STOP_PROFILER =
        new Primitive("stop-profiler", PACKAGE_PROF, true)
    {
        @Override
        public LispObject execute()
        {
            Stream out = getStandardOutput();
            out.freshLine();
            if (profiling) {
                profiling = false;
                out._writeLine("; Profiler stopped.");
            } else
                out._writeLine("; Profiler was not started.");
            out._finishOutput();
            return LispThread.currentThread().nothing();
        }
    };
}
