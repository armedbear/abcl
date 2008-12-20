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

import java.lang.reflect.Method;
import java.util.Date;
import java.util.TimeZone;

public final class Time extends Lisp
{
  private static final long getCurrentThreadUserTime()
  {
    try
      {
        Class c = Class.forName("org.armedbear.lisp.Native");
        Method m = c.getMethod("getCurrentThreadUserTime", (Class[]) null);
        Object result = m.invoke((Object) null, (Object[]) null);
        if (result instanceof Long)
          return ((Long)result).longValue();
      }
    catch (Throwable t) {}
    return -1;
  }

  private static final long getCurrentThreadSystemTime()
  {
    try
      {
        Class c = Class.forName("org.armedbear.lisp.Native");
        Method m = c.getMethod("getCurrentThreadSystemTime", (Class[]) null);
        Object result = m.invoke((Object) null, (Object[]) null);
        if (result instanceof Long)
          return ((Long)result).longValue();
      }
    catch (Throwable t) {}
    return -1;
  }

  // ### %time
  private static final Primitive _TIME =
    new Primitive("%time", PACKAGE_SYS, false)
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Cons.setCount(0);
        long userStart = -1;
        long systemStart = -1;
        try
          {
            userStart = getCurrentThreadUserTime();
            systemStart = getCurrentThreadSystemTime();
          }
        catch (Throwable t) {}
        long realStart = System.currentTimeMillis();
        try
          {
            return arg.execute();
          }
        finally
          {
            long realElapsed = System.currentTimeMillis() - realStart;
            final long userStop;
            final long systemStop;
            if (userStart > 0)
              {
                userStop = getCurrentThreadUserTime();
                systemStop = getCurrentThreadSystemTime();
              }
            else
              {
                userStop = -1;
                systemStop = -1;
              }
            long count = Cons.getCount();
            Stream out =
              checkCharacterOutputStream(Symbol.TRACE_OUTPUT.symbolValue());
            out.freshLine();
            FastStringBuffer sb = new FastStringBuffer();
            sb.append(String.valueOf((float)realElapsed / 1000));
            sb.append(" seconds real time");
            sb.append(System.getProperty("line.separator"));
            if (userStart > 0)
              {
                sb.append(String.valueOf((float)(userStop - userStart) / 100));
                sb.append(" seconds user run time");
                sb.append(System.getProperty("line.separator"));
                sb.append(String.valueOf((float)(systemStop - systemStart) / 100));
                sb.append(" seconds system run time");
                sb.append(System.getProperty("line.separator"));
              }
            sb.append(count);
            sb.append(" cons cell");
            if (count != 1)
              sb.append('s');
            sb.append(System.getProperty("line.separator"));
            out._writeString(sb.toString());
            out._finishOutput();
          }
      }
    };

  // ### get-internal-real-time
  private static final Primitive GET_INTERNAL_REAL_TIME =
    new Primitive("get-internal-real-time", "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        return number(System.currentTimeMillis());
      }
    };

  // ### get-internal-run-time
  private static final Primitive GET_INTERNAL_RUN_TIME =
    new Primitive("get-internal-run-time", "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        if (Utilities.isPlatformUnix)
          {
            long userTime = -1;
            long systemTime = -1;
            try
              {
                userTime = getCurrentThreadUserTime();
                systemTime = getCurrentThreadSystemTime();
              }
            catch (Throwable t) {}
            if (userTime >= 0 && systemTime >= 0)
              return number((userTime + systemTime) * 10);
          }
        return number(System.currentTimeMillis());
      }
    };

  // ### get-universal-time
  private static final Primitive GET_UNIVERSAL_TIME =
    new Primitive("get-universal-time", "")
    {
      public LispObject execute()
      {
        return number(System.currentTimeMillis() / 1000 + 2208988800L);
      }
    };

  // ### default-time-zone => offset daylight-p
  private static final Primitive DEFAULT_TIME_ZONE =
    new Primitive("default-time-zone", PACKAGE_SYS, false)
    {
      public LispObject execute() throws ConditionThrowable
      {
        TimeZone tz = TimeZone.getDefault();
        //int offset = tz.getOffset(System.currentTimeMillis());
        // Classpath hasn't implemented TimeZone.getOffset(long).
        int rawOffset = tz.getRawOffset();
        final boolean inDaylightTime =
          tz.inDaylightTime(new Date(System.currentTimeMillis()));
        if (inDaylightTime)
          rawOffset += tz.getDSTSavings();
        // "Time zone values increase with motion to the west..."
        // Convert milliseconds to hours.
        return LispThread.currentThread().setValues(
          new Fixnum(- rawOffset).divideBy(new Fixnum(3600000)),
          inDaylightTime ? T : NIL);
      }
    };
}
