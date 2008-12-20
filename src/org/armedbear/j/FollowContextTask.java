/*
 * FollowContextTask.java
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

package org.armedbear.j;

import java.util.ArrayList;
import java.util.List;
import javax.swing.SwingUtilities;

public class FollowContextTask extends IdleThreadTask implements Constants
{
    private Expression lastExpression;
    private Position lastPos;

    public FollowContextTask()
    {
        setIdle(500); // 500 ms
        setRunnable(runnable);
    }

    private final Runnable runnable = new Runnable() {
        public void run()
        {
            final Editor editor = Editor.currentEditor();
            if (editor == null)
                return;
            if (editor.getBuffer() == null)
                return;
            if (editor.getMark() != null)
                return;
            if (editor.getDot() == null || editor.getDot().equals(lastPos))
                return;
            lastPos = new Position(editor.getDot());
            Expression expression =
                editor.getMode().getExpressionAtDot(editor, false);
            if (expression == null)
                return;
            if (!expression.equals(lastExpression)) {
                final Tag tag = findMatchingTag(editor, expression);
                if (tag != null) {
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            if (tag instanceof LocalTag)
                                TagCommands.gotoLocalTag(editor, (LocalTag)tag, true);
                            else if (tag instanceof GlobalTag)
                                TagCommands.gotoGlobalTag(editor, (GlobalTag)tag, true);
                            editor.updateDisplay();
                        }
                    };
                    SwingUtilities.invokeLater(r);
                }
                lastExpression = expression;
            }
        }
    };

    private static Tag findMatchingTag(Editor editor, Expression expression)
    {
        List list =
            TagCommands.findMatchingTags(editor.getBuffer(), expression);
        if (list != null && list.size() == 1) {
            // Exactly one match.
            return (Tag) list.get(0);
        }
        return null;
    }

    public static void followContext()
    {
        if (!Editor.checkExperimental())
            return;
        IdleThread.runFollowContextTask(new FollowContextTask());
    }
}
