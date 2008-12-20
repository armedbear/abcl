/*
 * VMConnection.java
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
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.VirtualMachineManager;
import com.sun.jdi.connect.Connector.Argument;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.LaunchingConnector;
import java.util.Map;
import org.armedbear.j.Debug;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;

public final class VMConnection
{
    private final Connector connector;
    private final Map map;

    public VMConnection(Connector connector, Map map)
    {
        this.connector = connector;
        this.map = map;
    }

    public static VMConnection getConnection(Jdb jdb)
    {
        VirtualMachineManager vmm = Bootstrap.virtualMachineManager();
        LaunchingConnector connector = vmm.defaultConnector();
        Map map = connector.defaultArguments();
        String javaHome = jdb.getJavaHome();
        ((Connector.Argument)map.get("home")).setValue(javaHome);
        String javaExecutable = jdb.getJavaExecutable();
        ((Connector.Argument)map.get("vmexec")).setValue(javaExecutable);

        // Command line.
        FastStringBuffer sb = new FastStringBuffer(jdb.getMainClass());
        String mainClassArgs = jdb.getMainClassArgs();
        if (mainClassArgs != null && mainClassArgs.length() > 0) {
            sb.append(' ');
            sb.append(mainClassArgs);
        }
        ((Connector.Argument)map.get("main")).setValue(sb.toString());

        // CLASSPATH and VM options.
        sb.setLength(0);
        String vmArgs = jdb.getVMArgs();
        if (vmArgs != null) {
            vmArgs = vmArgs.trim();
            if (vmArgs.length() > 0) {
                sb.append(vmArgs);
                sb.append(' ');
            }
        }
        String classPath = jdb.getClassPath();
        if (classPath != null) {
            classPath = classPath.trim();
            if (classPath.length() > 0) {
                sb.append("-classpath ");
                sb.append(classPath);
            }
        }
        ((Connector.Argument)map.get("options")).setValue(sb.toString());

        ((Connector.Argument)map.get("suspend")).setValue("true");
        return new VMConnection(connector, map);
    }

    public VirtualMachine open(Jdb jdb)
    {
        if (connector instanceof LaunchingConnector)
            return launchTarget(jdb);
        // Otherwise...
        Debug.bug();
        return null;
    }

    private VirtualMachine launchTarget(Jdb jdb)
    {
        VirtualMachine vm = null;
        try {
            vm = ((LaunchingConnector)connector).launch(map);
        }
        catch (VMDisconnectedException disconnected) {
            return null;
        }
        catch (Exception e) {
            Log.error(e);
            return null;
        }
        Process process = vm.process();
        jdb.displayRemoteOutput(process.getErrorStream());
        jdb.displayRemoteOutput(process.getInputStream());
        vm.suspend();
        jdb.setSuspended(true);
        jdb.setVM(vm);
        new EventHandler(jdb);
        jdb.fireContextChanged();
        return vm;
    }
}
