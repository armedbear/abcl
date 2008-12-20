/*
 * ProcessTable.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

package org.armedbear.j;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public final class ProcessTable
{
    private ArrayList entries = new ArrayList();

    private ProcessTable()
    {
    }

    public static ProcessTable getProcessTable()
    {
        if (!Platform.isPlatformUnix())
            return null;
        ProcessTable table = new ProcessTable();
        try {
            String[] cmdarray = {"/bin/sh", "-c", "ps -o pid,ppid,command"};
            Process process = Runtime.getRuntime().exec(cmdarray);
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(process.getInputStream()));
            if (reader != null) {
                String s;
                while ((s = reader.readLine()) != null) {
                    if (s.length() > 0)
                        table.addEntry(s);
                }
            }
        }
        catch (Throwable t) {
            Log.error(t);
        }
        return table;
    }

    public List findMatchingEntries(String command)
    {
        ArrayList results = new ArrayList();
        if (command != null && command.length() > 0){
            // First char of command might be replaced with '-', so we look
            // for that pattern too.
            String alternate = "-".concat(command.substring(1));
            for (int i = 0; i < entries.size(); i++) {
                ProcessTableEntry entry = (ProcessTableEntry) entries.get(i);
                if (entry.command.indexOf(command) >= 0 ||
                    entry.command.indexOf(alternate) >= 0)
                    results.add(entry);
            }
        }
        return results;
    }

    public List findChildren(int pid)
    {
        ArrayList results = new ArrayList();
        for (int i = 0; i < entries.size(); i++) {
            ProcessTableEntry entry = (ProcessTableEntry) entries.get(i);
            if (entry.ppid == pid)
                results.add(entry);
        }
        return results;
    }

    private void addEntry(String s)
    {
        StringTokenizer st = new StringTokenizer(s);
        if (st.countTokens() >= 3) {
            String pidString = st.nextToken();
            String ppidString = st.nextToken();
            String command = st.nextToken("\n").trim();
            int pid, ppid;
            try {
                pid  = Integer.parseInt(pidString);
                ppid = Integer.parseInt(ppidString);
                entries.add(new ProcessTableEntry(pid, ppid, command));
            }
            catch (NumberFormatException e) {}
        }
    }
}
