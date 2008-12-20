/*
 * JdbCommands.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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
 */

package org.armedbear.j.jdb;

import com.sun.jdi.Bootstrap;
import org.armedbear.j.Help;
import org.armedbear.j.JavaMode;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;

public final class JdbCommands implements JdbConstants
{
    public static int findCommand(String cmd)
    {
        // Single-letter abbreviations for the most common commands.
        if (cmd.equals("b"))
            return JDB_BREAK;
        if (cmd.equals("c"))
            return JDB_CONTINUE;
        if (cmd.equals("f"))
            return JDB_FINISH;
        if (cmd.equals("n"))
            return JDB_NEXT;
        if (cmd.equals("q"))
            return JDB_QUIT;
        if (cmd.equals("s"))
            return JDB_STEP;
        if (cmd.equals("p"))
            return JDB_PRINT;

        if ("break".startsWith(cmd))
            return JDB_BREAK;
        if (cmd.startsWith("ca"))
            if ("catch".startsWith(cmd))
                return JDB_CATCH;
        if (cmd.startsWith("co"))
            if ("continue".startsWith(cmd))
                return JDB_CONTINUE;
        if (cmd.startsWith("cl"))
            if ("clear".startsWith(cmd))
                return JDB_CLEAR;
        if ("finish".startsWith(cmd))
            return JDB_FINISH;
        if ("go".startsWith(cmd))
            return JDB_CONTINUE;
        if ("locals".startsWith(cmd))
            return JDB_LOCALS;
        if ("next".startsWith(cmd))
            return JDB_NEXT;
        if ("print".startsWith(cmd))
            return JDB_PRINT;
        if ("quit".startsWith(cmd))
            return JDB_QUIT;
        if (cmd.startsWith("rest"))
            if ("restart".startsWith(cmd))
                return JDB_RESTART;
        if (cmd.startsWith("resu"))
            if ("resume".startsWith(cmd))
                return JDB_CONTINUE;
        if (cmd.startsWith("std"))
            if ("stdin".startsWith(cmd))
                return JDB_STDIN;
        if (cmd.startsWith("ste"))
            if ("step".startsWith(cmd))
                return JDB_STEP;
        if (cmd.startsWith("sto"))
            if ("stop".startsWith(cmd))
                return JDB_BREAK;
        if (cmd.startsWith("su"))
            if ("suspend".startsWith(cmd))
                return JDB_SUSPEND;
        if ("tbreak".startsWith(cmd))
            return JDB_TBREAK;

        return -1;
    }

    public static void jdb()
    {
        if (JavaMode.getJdb() == null) {
            try {
                Bootstrap.virtualMachineManager();
            }
            catch (NoClassDefFoundError e) {
                Log.error("unable to load com.sun.jdi.Bootstrap");
                String classpath = System.getProperty("java.class.path");
                Log.error("classpath = " + classpath);
                Help.help("jdb.html");
                MessageDialog.showMessageDialog(
                    "Unable to find tools.jar. " +
                    "See doc/jdb.html for installation instructions.",
                    "Jdb");
                return;
            }
        }
        Jdb.jdb();
    }

    public static void jdb(String s)
    {
        if (JavaMode.getJdb() != null)
            command(s);
        else
            MessageDialog.showMessageDialog("The debugger is not running.",
                "Error");
    }

    public static void jdbContinue()
    {
        command("continue");
    }

    public static void jdbFinish()
    {
        command("finish");
    }

    public static void jdbLocals()
    {
        command("locals");
    }

    public static void jdbNext()
    {
        command("next");
    }

    public static void jdbQuit()
    {
        command("quit");
    }

    public static void jdbRestart()
    {
        command("restart");
    }

    public static void jdbStep()
    {
        command("step");
    }

    public static void jdbSuspend()
    {
        command("suspend");
    }

    public static void command(String s)
    {
        Jdb jdb = Jdb.findJdb();
        if (jdb != null)
            jdb.doCommand(s);
    }
}
