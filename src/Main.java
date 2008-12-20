/*
 * Main.java
 *
 * Copyright (C) 1998-2003 Peter Graves
 * $Id: Main.java,v 1.3 2003-07-04 14:25:26 piso Exp $
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

import java.lang.reflect.Method;

public final class Main
{
    public static void main(String[] args)
    {
        final String version = System.getProperty("java.version");
        if (version.startsWith("1.0") || version.startsWith("1.1") ||
            version.startsWith("1.2") || version.startsWith("1.3")) {
            System.err.println("");
            System.err.print("J requires Java 1.4 or later.");
            System.err.println(" (Java 1.4.2 is recommended.)");
            System.err.println("");
            System.exit(1);
        }
        try {
            Class c = Class.forName("org.armedbear.j.Editor");
            Class[] parameterTypes = new Class[1];
            parameterTypes[0] = String[].class;
            Method method = c.getMethod("main", parameterTypes);
            Object[] parameters = new Object[1];
            parameters[0] = args;
            method.invoke(null, parameters);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}
