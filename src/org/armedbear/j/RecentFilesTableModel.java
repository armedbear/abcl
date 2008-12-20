/*
 * RecentFilesTableModel.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;

public final class RecentFilesTableModel extends AbstractTableModel
{
    private static final int NAME        = 0;
    private static final int LOCATION    = 1;
    private static final int LAST_VISIT  = 2;
    private static final int FIRST_VISIT = 3;

    private static final int ASCENDING   = 0;
    private static final int DESCENDING  = 1;

    private final String[] columnNames =
        {"Name", "Location", "Last Visit", "First Visit"};

    private static SimpleDateFormat timeFormat = new SimpleDateFormat("h:mm a");
    private static SimpleDateFormat shortDateFormat =
        new SimpleDateFormat("EEE h:mm a");
    private static SimpleDateFormat fullDateFormat =
        new SimpleDateFormat("MMM d yyyy h:mm a");

    private final List data = RecentFiles.getInstance().getEntries();

    private int[] indexes;

    private Calendar startOfDay;
    private Calendar startOfWeek;

    private int sortColumn = LAST_VISIT;
    private int sortOrder = DESCENDING;

    public RecentFilesTableModel()
    {
        indexes = new int[data.size()];

        for (int i = 0; i < indexes.length; i++)
            indexes[i] = i;

        startOfDay = Calendar.getInstance();
        startOfDay.setTime(new Date(System.currentTimeMillis()));
        startOfDay.set(Calendar.HOUR_OF_DAY, 0);
        startOfDay.set(Calendar.HOUR, 0);
        startOfDay.set(Calendar.MINUTE, 0);
        startOfDay.set(Calendar.SECOND, 0);
        startOfDay.set(Calendar.MILLISECOND, 0);
        startOfDay.set(Calendar.AM_PM, 0);
        startOfWeek = Calendar.getInstance();
        startOfWeek.setTime(startOfDay.getTime());
        startOfWeek.add(Calendar.DATE, -6);
    }

    public int getColumnCount()
    {
        return columnNames.length;
    }

    public int getRowCount()
    {
        return data.size();
    }

    public String getColumnName(int col)
    {
        return columnNames[col];
    }

    private String format(long date)
    {
        Date d = new Date(date);

        Calendar c = Calendar.getInstance();

        c.setTime(d);

        if (c.before(startOfWeek))
            return fullDateFormat.format(d);

        if (c.before(startOfDay))
            return shortDateFormat.format(d);

        return timeFormat.format(d);
    }

    public RecentFilesEntry getEntryAtRow(int row)
    {
        int i = indexes[row];
        return (RecentFilesEntry) data.get(i);
    }

    public int getRowForEntry(RecentFilesEntry entry)
    {
        for (int row = 0; row < indexes.length; row ++) {
            int i = indexes[row];
            if (entry == data.get(i))
                return row;
        }
        return -1;
    }

    public Object getValueAt(int row, int col)
    {
        int i = indexes[row];
        RecentFilesEntry entry = (RecentFilesEntry) data.get(i);
        if (entry != null) {
            switch (col) {
                case NAME:
                    return entry.name;
                case LOCATION:
                    return entry.location;
                case FIRST_VISIT:
                    return format(entry.firstVisit);
                case LAST_VISIT:
                    return format(entry.lastVisit);
            }
        }
        return null;
    }

    public void sortByColumn(int column)
    {
        if (column == sortColumn) {
            // Sorting on same column.  Reverse sort order.
            sortByColumn(column, sortOrder == DESCENDING ? ASCENDING : DESCENDING);
        } else {
            // Sorting on a different column.  Use default sort order for that column.
            switch (column) {
                case NAME:
                    sortByColumn(NAME, ASCENDING);
                    break;
                case LOCATION:
                    sortByColumn(LOCATION, ASCENDING);
                    break;
                case FIRST_VISIT:
                    sortByColumn(FIRST_VISIT, DESCENDING);
                    break;
                case LAST_VISIT:
                    sortByColumn(LAST_VISIT, DESCENDING);
                    break;
            }
        }
        fireTableChanged(new TableModelEvent(this));
    }

    private void sortByColumn(int column, int order)
    {
        if (order == DESCENDING) {
            for (int i = 0; i < getRowCount(); i++) {
                for (int j = i + 1; j < getRowCount(); j++) {
                    if (compareByColumn(indexes[i], indexes[j], column) < 0)
                        swap(i, j);
                }
            }
        } else {
            for (int i = 0; i < getRowCount(); i++) {
                for (int j = i + 1; j < getRowCount(); j++) {
                    if (compareByColumn(indexes[i], indexes[j], column) > 0)
                        swap(i, j);
                }
            }
        }
        sortColumn = column;
        sortOrder = order;
    }

    private void sortByName()
    {
        for (int i = 0; i < getRowCount(); i++) {
            for (int j = i + 1; j < getRowCount(); j++) {
                if (compareByColumn(indexes[i], indexes[j], 0) > 0)
                    swap(i, j);
            }
        }
    }

    private void sortByLocation()
    {
        for (int i = 0; i < getRowCount(); i++) {
            for (int j = i + 1; j < getRowCount(); j++) {
                if (compareByColumn(indexes[i], indexes[j], 1) > 0)
                    swap(i, j);
            }
        }
    }

    private void sortByFirstVisit()
    {
        for (int i = 0; i < getRowCount(); i++) {
            for (int j = i + 1; j < getRowCount(); j++) {
                if (compareByColumn(indexes[i], indexes[j], 2) < 0)
                    swap(i, j);
            }
        }
    }

    private void sortByLastVisit()
    {
        for (int i = 0; i < getRowCount(); i++) {
            for (int j = i + 1; j < getRowCount(); j++) {
                if (compareByColumn(indexes[i], indexes[j], 3) < 0)
                    swap(i, j);
            }
        }
    }

    private int compareByColumn(int i, int j, int column)
    {
        RecentFilesEntry entry1 = (RecentFilesEntry) data.get(i);
        RecentFilesEntry entry2 = (RecentFilesEntry) data.get(j);
        switch (column) {
            case NAME:
                return entry1.name.compareTo(entry2.name);
            case LOCATION:
                return entry1.location.compareTo(entry2.location);
            case FIRST_VISIT:
                if (entry1.firstVisit < entry2.firstVisit)
                    return -1;
                if (entry1.firstVisit == entry2.firstVisit)
                    return 0;
                return 1;
            case LAST_VISIT:
                if (entry1.lastVisit < entry2.lastVisit)
                    return -1;
                if (entry1.lastVisit == entry2.lastVisit)
                    return 0;
                return 1;
            default:
                Debug.assertTrue(false);
        }
        return 0;
    }

    private void swap(int i, int j)
    {
        int tmp = indexes[i];
        indexes[i] = indexes[j];
        indexes[j] = tmp;
    }

    public Class getColumnClass(int col)
    {
        return String.class;
    }
}
