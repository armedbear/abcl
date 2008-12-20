/*
 * NewsCommands.java
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

package org.armedbear.j.mail;

import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.InputDialog;

public final class NewsCommands
{
    public static void news()
    {
        if (!Editor.checkExperimental())
            return;
        final Editor editor = Editor.currentEditor();
        NntpSession session = NntpSession.getSession();
        if (session != null) {
            News news = new News(session);
            editor.makeNext(news);
            editor.activate(news);
        }
    }

    public static void news(String host)
    {
        if (!Editor.checkExperimental())
            return;
        final Editor editor = Editor.currentEditor();
        NntpSession session = NntpSession.getSession(host);
        if (session != null) {
            News news = new News(session);
            editor.makeNext(news);
            editor.activate(news);
        }
    }

    public static void openGroupAtDot()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof News && editor.getDot() != null) {
            String groupName = editor.getDotLine().getText();
            NntpSession session =
                NntpSession.getSession(((News)buffer).getHost());
            NewsGroupSummary summary = new NewsGroupSummary(session, groupName);
            editor.makeNext(summary);
            editor.activate(summary);
        }
    }

    public static void openGroup()
    {
        if (!Editor.checkExperimental())
            return;
        final Editor editor = Editor.currentEditor();
        String groupName =
            InputDialog.showInputDialog(editor, "Group:","Open Group", null);
        if (groupName == null || groupName.length() == 0)
            return;
        editor.repaintNow();
        NntpSession session = NntpSession.getSession();
        NewsGroupSummary summary = new NewsGroupSummary(session, groupName);
        editor.makeNext(summary);
        editor.activate(summary);
    }

    public static void readArticle()
    {
        readArticle(false);
    }

    public static void readArticleOtherWindow()
    {
        readArticle(true);
    }

    private static void readArticle(boolean useOtherWindow)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof NewsGroupSummary && editor.getDot() != null)
            ((NewsGroupSummary)buffer).readArticle(editor,
                editor.getDotLine(), useOtherWindow);
    }
}
