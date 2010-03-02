/*
 * JHandler.java
 *
 * Copyright (C) 2003-2005 Andras Simon, Peter Graves
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

import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;

public final class JHandler
{
    static final Map<Object,Map<String,Entry>> table =
       new WeakHashMap<Object,Map<String,Entry>>();

    public static void callLisp (String s, Object o)
    {
        callLisp(s, o, "");
    }

    public static void callLisp (String s, Object o, String s1)
    {
        callLisp(s, o, s1, new int[] {});
    }

    public static void callLisp (String s, Object o, String s1, int ai[]) {
        callLisp(s, o, new String[] { s1 }, ai);
    }

    public static void callLisp (String s, Object o, String as[])
    {
        callLisp(s, o, as, new int[] {});
    }

    public static void callLisp (String s, Object o, String as[], int ai[])
    {
        if (table.containsKey(o)) {
            Map<String,Entry> entryTable =  (Map<String,Entry>)table.get(o);
            if (entryTable.containsKey(s)) {
                Function f = ((Entry)entryTable.get(s)).getHandler();
                LispObject data = ((Entry)entryTable.get(s)).getData();
                Fixnum count = ((Entry)entryTable.get(s)).getCount();
                Fixnum[] lispAi = new Fixnum[ai.length];
                for (int i = 0; i < ai.length; i++) {
                    lispAi[i] = Fixnum.getInstance(ai[i]);
                }
                LispObject lispAiVector = new SimpleVector(lispAi);
                SimpleString[] lispAs = new SimpleString[as.length];
                for (int i = 0; i < as.length; i++) {
                    lispAs[i] = new SimpleString(as[i]);
                }
                LispObject lispAsVector = new SimpleVector(lispAs);
                LispObject[] args = new LispObject[] //FIXME: count -> seq_num
                { data, new JavaObject(o), lispAiVector, lispAsVector, internKeyword(s), count };
                f.execute(args);
            }
        }
    }

    // jregister-handler1 object event handler data count
    private static final Primitive _JREGISTER_HANDLER =
        new Primitive("%jregister-handler", PACKAGE_JAVA)
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length != 5)
                return error(new WrongNumberOfArgumentsException(this));
            Map<String,Entry> entryTable = null;
            Object object = args[0].javaInstance();
            String event = ((Symbol)args[1]).getName();
            if (!table.containsKey(object)) {
                entryTable = new HashMap<String,Entry>();
                table.put(object,entryTable);
            } else {
                entryTable = (Map<String,Entry>)table.get(object);
            }
            Entry entry = new Entry((Function) args[2], args[3], event, entryTable);
            if (args[4] != NIL)
                entry.addCount(((Fixnum)args[4]).value);
            entryTable.put(event,entry);
            return T;
        }
    };

    private static class Entry
    {
        Function handler;
        LispObject data;
        int count = -1;
        Map<String,Entry> entryTable;
        String event;

        public Entry (Function handler, LispObject data, String event,
                      Map<String,Entry> entryTable)
        {
            this.entryTable = entryTable;
            this.event = event;
            this.handler = handler;
            this.data = data;
        }

        public Function getHandler ()
        {
            return handler;
        }

        public void addData (LispObject data)
        {
            this.data = data;
        }

        public LispObject getData ()
        {
            return data;
        }

        public void addCount (int count)
        {
            this.count = count;
        }

        public Fixnum getCount ()
        {
            if (count == 0)
                entryTable.remove(event);
            return (Fixnum.getInstance (count--));
        }
    }
}
