/*
 * IncomingFilter.java
 *
 * Copyright (C) 2002 Peter Graves
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

package org.armedbear.j.mail;

import java.util.ArrayList;
import java.util.List;
import org.armedbear.j.Log;

public final class IncomingFilter
{
    public static final int NOTHING           = 0;
    public static final int MOVE              = 1;
    public static final int BOUNCE            = 2;
    public static final int BOUNCE_AND_DELETE = 3;

    private static ArrayList filterList;

    private final String mailbox;
    private final String pattern;
    private final MailboxFilter filter;
    private final int action;
    private final String parameter;

    private IncomingFilter(String mailbox, String pattern, MailboxFilter filter,
        int action, String parameter)
    {
        this.mailbox = mailbox;
        this.pattern = pattern;
        this.filter = filter;
        this.action = action;
        this.parameter = parameter;
    }

    public final String getPattern()
    {
        return pattern;
    }

    public final MailboxFilter getFilter()
    {
        return filter;
    }

    public final int getAction()
    {
        return action;
    }

    public final String getParameter()
    {
        return parameter;
    }

    public static synchronized void addIncomingFilter(
        String mailbox,
        String pattern,
        String actionName,
        String parameter)
    {
        if (mailbox == null) return;
        if (pattern == null) return;
        if (actionName == null) return;
        if (parameter == null) return;
        if (!mailbox.equals("inbox")) {
            Log.error("addIncomingFilter - only \"inbox\" is supported");
            return;
        }
        MailboxFilter filter = MailboxFilter.getMailboxFilter(pattern);
        if (filter == null) {
            Log.error("addIncomingFilter - bad pattern |" + pattern + "|");
            return;
        }
        int action = NOTHING;
        if (actionName.equalsIgnoreCase("move"))
            action = MOVE;
        else if (actionName.equalsIgnoreCase("bounce"))
            action = BOUNCE;
        else if (actionName.equalsIgnoreCase("bounce_and_delete"))
            action = BOUNCE_AND_DELETE;
        else {
            Log.error("addIncomingFilter - action \"" + actionName +
                "\" is not supported");
            return;
        }
        if (filterList == null)
            filterList = new ArrayList();
        filterList.add(new IncomingFilter(mailbox, pattern, filter, action,
            parameter));
    }

    public static synchronized final void resetIncomingFilters()
    {
        filterList = null;
    }

    public static synchronized final List getFilterList()
    {
        return filterList == null ? null : new ArrayList(filterList);
    }
}