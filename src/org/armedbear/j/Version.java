/*
 * Version.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public final class Version
{
    private static String version;
    private static String build;
    private static String hostName;
    private static String snapshot;

    private static boolean initialized;

    // "0.16.0+"
    public static String getVersion()
    {
        initialize();
        return version;
    }

    // "J 0.16.0+"
    public static String getShortVersionString()
    {
        initialize();
        FastStringBuffer sb = new FastStringBuffer("J");
        if (version != null && version.length() > 0) {
            sb.append(' ');
            sb.append(version);
        }
        return sb.toString();
    }

    // "J 0.16.0+ (built Fri Jul 26 2002 07:03:12 PDT on merlin)"
    public static String getLongVersionString()
    {
        FastStringBuffer sb = new FastStringBuffer(getShortVersionString());
        if (build != null && build.length() > 0) {
            sb.append(" (built ");
            sb.append(build);
            if (hostName != null && hostName.length() > 0) {
                sb.append(" on ");
                sb.append(hostName);
            }
            sb.append(")");
        }
        return sb.toString();
    }

    /**
     * Returns a string describing the source snapshot from which the running
     * instance of j was built, if applicable.
     *
     * @return  a string describing the source snapshot, or <code>null</code>
     *          if not applicable.
     * @since   0.16.1
     */
    public static String getSnapshotInformation()
    {
        if (version != null && version.endsWith("+") && snapshot != null) {
            if (!snapshot.equals(build)) {
                FastStringBuffer sb =
                    new FastStringBuffer("(built from development snapshot created ");
                sb.append(snapshot);
                sb.append(')');
                return sb.toString();
            }
        }
        return null;
    }

    private static void initialize()
    {
        if (!initialized) {
            InputStream inputStream =
                Editor.class.getResourceAsStream("version");
            if (inputStream != null) {
                try {
                    BufferedReader reader =
                        new BufferedReader(new InputStreamReader(inputStream));
                    version = reader.readLine();
                    reader.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            inputStream = Editor.class.getResourceAsStream("build");
            if (inputStream != null) {
                try {
                    BufferedReader reader =
                        new BufferedReader(new InputStreamReader(inputStream));
                    build = reader.readLine();
                    hostName = reader.readLine();
                    reader.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            inputStream = Editor.class.getResourceAsStream("snapshot");
            if (inputStream != null) {
                try {
                    BufferedReader reader =
                        new BufferedReader(new InputStreamReader(inputStream));
                    snapshot = reader.readLine();
                    reader.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            initialized = true;
        }
    }
}
