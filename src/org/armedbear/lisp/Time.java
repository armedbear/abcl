/*
 * Time.java
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

import java.lang.reflect.Method;
import java.util.Date;
import java.util.TimeZone;

public final class Time
{
  // ### %time
  public static final Primitive _TIME = new pf__time();
  private static final class pf__time extends Primitive {
    pf__time() {
      super("%time", PACKAGE_SYS, false);
    }
    @Override
    public LispObject execute(LispObject arg) {
      long bytesStart = threadAllocatedBytes();
      long realStart = System.currentTimeMillis();
      try {
        return arg.execute();
      } finally {
        long realElapsed = System.currentTimeMillis() - realStart;
        long bytesEnd = threadAllocatedBytes();
        Stream out 
          = checkCharacterOutputStream(Symbol.TRACE_OUTPUT.symbolValue());
            out.freshLine();
            StringBuilder sb = new StringBuilder();
            sb.append(String.valueOf((float)realElapsed / 1000));
            sb.append(" seconds real time");
            sb.append(System.getProperty("line.separator"));
            if (bytesStart >= 0 && bytesEnd >= 0) {
              sb.append(bytesEnd - bytesStart);
              sb.append(" bytes consed");
              sb.append(System.getProperty("line.separator"));
            }
            out._writeString(sb.toString());
            out._finishOutput();
      }
    }
  };

  /** The bytes the current thread has allocated so far, or -1 where
   * the JVM does not count them.  TIME used to count cons cells in a
   * static field incremented by every Cons constructor, which every
   * consing thread then wrote to.
   */
  static long threadAllocatedBytes() {
    try {
      return AllocationCounter.bytes();
    } catch (Throwable t) { // no com.sun.management in this JVM
      return -1;
    }
  }

  private static final class AllocationCounter {
    static final com.sun.management.ThreadMXBean bean
      = (com.sun.management.ThreadMXBean)
        java.lang.management.ManagementFactory.getThreadMXBean();

    static long bytes() {
      if (!bean.isThreadAllocatedMemoryEnabled())
        return -1;
      return bean.getThreadAllocatedBytes(Thread.currentThread().getId());
    }
  }

  // ### get-internal-real-time
  private static final Primitive GET_INTERNAL_REAL_TIME = new pf_get_internal_real_time();
  private static final class pf_get_internal_real_time extends Primitive {
    pf_get_internal_real_time() {
      super("get-internal-real-time", "");
    }
    @Override
    public LispObject execute() {
      return number(System.currentTimeMillis());
    }
  };

  // ### get-internal-run-time
  private static final Primitive GET_INTERNAL_RUN_TIME = new pf_get_internal_run_time();
  private static final class pf_get_internal_run_time extends Primitive {
    pf_get_internal_run_time() {
      super("get-internal-run-time", "");
    }
    @Override
    public LispObject execute() {
      return number(System.currentTimeMillis());
    }
  };

  // ### get-universal-time
  private static final Primitive GET_UNIVERSAL_TIME = new pf_get_universal_time();
  private static final class pf_get_universal_time extends Primitive {
    pf_get_universal_time() {
      super("get-universal-time", "");
    }
    @Override
    public LispObject execute() {
      return number(System.currentTimeMillis() / 1000 + 2208988800L);
    }
  };

  // ### default-time-zone => offset daylight-p
  private static final Primitive DEFAULT_TIME_ZONE = new pf_default_time_zone();
  private static final class pf_default_time_zone extends Primitive {
    pf_default_time_zone() {
      super("default-time-zone", PACKAGE_SYS, false);
    }
    @Override
    public LispObject execute() {
      return getTimeZone(System.currentTimeMillis());
    }
  };

  private static final LispObject getTimeZone(long unixTimeMillis) {
    TimeZone tz = TimeZone.getDefault();
    //int offset = tz.getOffset(System.currentTimeMillis());
    // Classpath hasn't implemented TimeZone.getOffset(long).
    int rawOffset = tz.getRawOffset();
    final boolean inDaylightTime =
      tz.inDaylightTime(new Date(unixTimeMillis));
    if (inDaylightTime)
      rawOffset += tz.getDSTSavings();
    // "Time zone values increase with motion to the west..."
    // Convert milliseconds to hours.
    return LispThread.currentThread()
      .setValues(Fixnum.getInstance(- rawOffset).divideBy(Fixnum.getInstance(3600000)),
                 inDaylightTime ? T : NIL);
  }

  // ### get-time-zone universal-time => hours-west daylight-p
  private static final Primitive GET_TIME_ZONE = new pf_get_time_zone();
  @DocString(name="get-time-zone",
             args="time-in-millis",
             returns="hours-west daylight-p",
             doc= "Returns as the first value the timezone difference in hours from the Greenwich meridian for TIME-IN-MILLIS via the Daylight Savings Time assumptions that were in place at the instant's occurance.  Returns as the second value a boolean as to whether daylight savings time was in effect at the occurance.")
  private static final class pf_get_time_zone extends Primitive {
    pf_get_time_zone() {
      super("get-time-zone", PACKAGE_EXT, true);
    }
    @Override
    public LispObject execute(LispObject arg) {
      return getTimeZone((arg.longValue() - 2208988800L) * 1000);
    }
  };
}
