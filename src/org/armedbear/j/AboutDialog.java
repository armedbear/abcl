/*
 * AboutDialog.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.Box;
import javax.swing.JPanel;

public class AboutDialog extends AbstractDialog
{
  private long totalMemory;
  private long freeMemory;

  public AboutDialog()
  {
    super(Editor.getCurrentFrame(), "About J", true);
    Editor.getCurrentFrame().setWaitCursor();
    memory();
    JPanel panel = Utilities.createPanel("J");
    mainPanel.add(panel);
    String longVersionString = Version.getLongVersionString();
    if (longVersionString != null)
      addStaticText(panel, longVersionString);
    String snapshotInformation = Version.getSnapshotInformation();
    if (snapshotInformation != null)
      addStaticText(panel, snapshotInformation);
    addStaticText(panel,
                  "Copyright (C) 1998-2005 Peter Graves (peter@armedbear.org)");
    addStaticText(panel,
                  "J is free software; see the source for copying conditions.");
    addStaticText(panel, "There is ABSOLUTELY NO WARRANTY.");
    addStaticText(panel, "The latest version of j is available from:");
    addStaticText(panel, "    http://armedbear.org");
    addStaticText(panel, "Please report bugs to:");
    addStaticText(panel, "    armedbear-j-devel@lists.sourceforge.net");
    addStaticText(panel, getUptimeString());
    panel = Utilities.createPanel("System Information");
    addVerticalStrut();
    mainPanel.add(panel);
    FastStringBuffer sb = new FastStringBuffer("Java ");
    sb.append(System.getProperty("java.version"));
    sb.append(' ');
    sb.append(System.getProperty("java.vendor"));
    addStaticText(panel, sb.toString());
    // Additional information if available.
    String fullversion = System.getProperty("java.fullversion");
    if (fullversion != null)
      addStaticText(panel, fullversion);
    String vm = System.getProperty("java.vm.name");
    if (vm != null)
      addStaticText(panel, vm);
    Log.debug("total memory " + totalMemory);
    Log.debug("used " + (totalMemory - freeMemory));
    Log.debug("free " + freeMemory);
    sb.setLength(0);
    sb.append(formatMemory(totalMemory));
    sb.append(" total Java memory (");
    sb.append(formatMemory(totalMemory - freeMemory));
    sb.append(" used, ");
    sb.append(formatMemory(freeMemory));
    sb.append(" free)");
    addStaticText(panel, sb.toString());
    sb.setLength(0);
    sb.append(System.getProperty("os.name"));
    sb.append(' ');
    sb.append(System.getProperty("os.version"));
    addStaticText(panel, sb.toString());
    addVerticalStrut();
    addOK();
    pack();
    okButton.requestFocus();
    Editor.getCurrentFrame().setDefaultCursor();
  }

  private static String getUptimeString()
  {
    final int millisecondsPerMinute = 60 * 1000;
    final int millisecondsPerHour = 60 * millisecondsPerMinute;
    final int millisecondsPerDay = 24 * millisecondsPerHour;

    long now = System.currentTimeMillis();
    SimpleDateFormat dateFormatter = new SimpleDateFormat ("EEEE MMM d yyyy h:mm a");
    String dateString = dateFormatter.format(new Date(now));
    long uptime = now - Editor.getStartTimeMillis();

    // Don't show uptime if less than 1 minute.
    if (uptime < millisecondsPerMinute)
      return dateString;

    int days = (int) (uptime / millisecondsPerDay);
    int remainder = (int) (uptime % millisecondsPerDay);
    int hours = remainder / millisecondsPerHour;
    remainder = remainder % millisecondsPerHour;
    int minutes = remainder / millisecondsPerMinute;

    FastStringBuffer sb = new FastStringBuffer(dateString);
    sb.append("   up ");
    if (uptime < millisecondsPerHour)
      {
        sb.append(minutes);
        sb.append(" minute");
        if (minutes > 1)
          sb.append('s');
      }
    else
      {
        if (days > 0)
          {
            sb.append(days);
            sb.append(" day");
            if (days > 1)
              sb.append('s');
            sb.append(", ");
          }
        sb.append(hours);
        sb.append(':');
        if (minutes < 10)
          sb.append('0');
        sb.append(minutes);
      }
    return sb.toString();
  }

  private void addStaticText(JPanel panel, String s)
  {
    panel.add(Box.createVerticalStrut(6));
    panel.add(new StaticTextField(s));
  }

  private void memory()
  {
    Runtime runtime = Runtime.getRuntime();
    try
      {
        runtime.gc();
        Thread.currentThread().sleep(100);
        runtime.runFinalization();
        Thread.currentThread().sleep(100);
        runtime.gc();
        Thread.currentThread().sleep(100);
      }
    catch (InterruptedException e)
      {
        Log.error(e);
      }
    totalMemory = runtime.totalMemory();
    freeMemory = runtime.freeMemory();
  }

  private String formatMemory(long value)
  {
    if (value < 1000)
      return String.valueOf(value) + " bytes";
    if (value < 1000 * 1024)
      {
        double k = Math.round(value * 10 / (float) 1024) / 10.0;
        return String.valueOf(k) + "K";
      }
    if (value < 1000 * 1024 * 1024)
      {
        double m = Math.round(value * 10 / (float) (1024 * 1024)) / 10.0;
        return String.valueOf(m) + "M";
      }
    double g = Math.round(value * 10 / (float) (1024 * 1024 * 1024)) / 10.0;
    return String.valueOf(g) + "G";
  }

  private int parseInteger(String s, String caption) throws Exception
  {
    int index = s.indexOf(caption);
    if (index < 0)
      throw new Exception();
    index += caption.length();
    while (Character.isWhitespace(s.charAt(index)))
      ++index;
    return Utilities.parseInt(s.substring(index));
  }

  public static void about()
  {
    AboutDialog d = new AboutDialog();
    Editor.currentEditor().centerDialog(d);
    d.show();
  }
}
