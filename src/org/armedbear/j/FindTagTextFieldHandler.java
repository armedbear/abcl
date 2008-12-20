/*
 * FindTagTextFieldHandler.java
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class FindTagTextFieldHandler extends DefaultTextFieldHandler
{
    public FindTagTextFieldHandler(Editor editor, HistoryTextField textField)
    {
        super(editor, textField);
    }

    public boolean wantTab()
    {
        return true;
    }

    public void enter()
    {
        String pattern = textField.getText();
        if (pattern == null)
            return;
        pattern = pattern.trim();
        if (pattern.length() == 0)
            return;
        // Save history.
        History history = textField.getHistory();
        if (history != null) {
            history.append(pattern);
            history.save();
        }
        editor.ensureActive();
        editor.setFocusToDisplay();
        editor.updateLocation();
        findTag(pattern);
        editor.getDispatcher().eventHandled();
    }

    private void findTag(String pattern)
    {
        final Buffer buffer = editor.getBuffer();
        List tags = findMatchingTags(buffer, pattern);
        if (tags != null) {
            if (tags.size() > 1) {
                // Can we get a unique match if we just consider defuns etc.
                // and explicit tags?
                ArrayList shortList = new ArrayList();
                for (Iterator it = tags.iterator(); it.hasNext();) {
                    Tag tag = (Tag) it.next();
                    if (tag instanceof LocalTag) {
                        int type = ((LocalTag)tag).getType();
                        // Java etc.
                        if (type == TAG_METHOD || type == TAG_EXPLICIT)
                            shortList.add(tag);
                        // Lisp.
                        if (type == TAG_DEFUN || type == TAG_GENERIC_FUNCTION ||
                            type == TAG_MACRO)
                            shortList.add(tag);
                    }
                }
                if (shortList.size() == 1)
                    tags = shortList;
            }
            if (tags.size() > 1) {
                editor.setDefaultCursor();
                ListTagsBuffer buf =
                    new ListTagsBuffer(editor, "findTag", pattern, tags);
                editor.makeNext(buf);
                Editor otherEditor = editor.getOtherEditor();
                Editor ed = editor.activateInOtherWindow(buf);
                if (otherEditor == null)
                    ed.shrinkWindowIfLargerThanBuffer();
                ed.setDot(buf.getInitialDotPos());
                ed.moveCaretToDotCol();
                ed.updateDisplay();
            } else if (tags.size() == 1) {
                // Exactly one match.
                Tag tag = (Tag) tags.get(0);
                editor.pushPosition();
                if (tag instanceof LocalTag)
                    TagCommands.gotoLocalTag(editor, (LocalTag) tag, false);
                else if (tag instanceof GlobalTag)
                    TagCommands.gotoGlobalTag(editor, (GlobalTag) tag, false);
                else
                    Debug.bug();
            }
        } else
            editor.status("Tag \"".concat(pattern).concat("\" not found"));
    }

    private static List findMatchingTags(Buffer buffer, String pattern)
    {
        boolean ignoreCase = Utilities.isLowerCase(pattern);
        final Mode mode = buffer.getMode();
        // We'll start by looking in the current buffer. If we find an exact
        // match there, we're done.
        List list = findMatchingTagsInBuffer(buffer, pattern, ignoreCase);
        if (list == null) {
            // No exact match in the current buffer. Look in the current
            // directory.
            list =
                TagCommands.findMatchingTagsInDirectory(pattern,
                    buffer.getCurrentDirectory(), mode, -1, ignoreCase);
            if (list == null) {
                // Look at all the directories in the buffer's tag path.
                List dirs = TagCommands.getDirectoriesInTagPath(buffer);
                if (dirs != null) {
                    for (int i = 0; i < dirs.size(); i++) {
                        String dir = (String) dirs.get(i);
                        File directory = File.getInstance(dir);
                        if (directory.equals(buffer.getCurrentDirectory()))
                            continue;
                        List tagsInDir =
                            TagCommands.findMatchingTagsInDirectory(pattern,
                                directory, mode, -1, ignoreCase);
                        if (tagsInDir != null) {
                            if (list == null)
                                list = new ArrayList();
                            list.addAll(tagsInDir);
                        }
                    }
                }
            }
        }
        return (list != null && list.size() > 0) ? list : null;
    }

    private static List findMatchingTagsInBuffer(Buffer buffer, String pattern,
        boolean ignoreCase)
    {
        if (buffer.getTags() == null) {
            Tagger tagger = buffer.getMode().getTagger(buffer);
            if (tagger != null)
                tagger.run();
        }
        boolean isQualified = buffer.getMode().isQualifiedName(pattern);
        List list = new ArrayList();
        final List localTags = buffer.getTags();
        if (localTags != null) {
            // Look through all the local tags.
            Iterator iter = localTags.iterator();
            while (iter.hasNext()) {
                LocalTag localTag = (LocalTag) iter.next();
                if (isQualified) {
                    String tagName = localTag.getName();
                    if ((ignoreCase && tagName.equalsIgnoreCase(pattern)) || tagName.equals(pattern)) {
                        list.add(localTag);
                    }
                } else {
                    // Not qualified.
                    String methodName = localTag.getMethodName();
                    if (methodName != null) {
                        if ((ignoreCase && methodName.equalsIgnoreCase(pattern)) || methodName.equals(pattern)) {
                            list.add(localTag);
                        }
                    }
                }
            }
        }
        return (list != null && list.size() > 0) ? list : null;
    }

    public List getCompletions(final String prefix)
    {
        List list = getCompletionsInCurrentBuffer(prefix);
        Mode mode = editor.getMode();
        List tags =
            Editor.getTagFileManager().getTags(editor.getCurrentDirectory(),
                mode);
        if (tags != null) {
            boolean prefixIsQualified = mode.isQualifiedName(prefix);
            boolean ignoreCase = Utilities.isLowerCase(prefix);
            int prefixLength = prefix.length();
            for (int i = 0; i < tags.size(); i++) {
                GlobalTag tag = (GlobalTag) tags.get(i);
                if (tag.getName().regionMatches(ignoreCase, 0, prefix, 0, prefixLength)) {
                    String toBeAdded;
                    if (prefixIsQualified)
                        toBeAdded = tag.getName();
                    else {
                        toBeAdded = tag.getClassName();
                        if (toBeAdded == null)
                            toBeAdded = tag.getName();
                    }
                    maybeAdd(list, toBeAdded);
                    continue;
                }
                if (!prefixIsQualified && mode.hasQualifiedNames()) {
                    // The name we're looking for does not have a class prefix.
                    // Look for a match on the method name of the tag.
                    String methodName = tag.getMethodName();
                    if (methodName != null) {
                        if (methodName.regionMatches(ignoreCase, 0, prefix, 0, prefixLength))
                            maybeAdd(list, tag.getMethodName());
                    }
                }
            }
        }
        return list;
    }

    private List getCompletionsInCurrentBuffer(String prefix)
    {
        List list = new ArrayList();
        List tags = editor.getBuffer().getTags();
        if (tags != null) {
            boolean ignoreCase = Utilities.isLowerCase(prefix);
            int prefixLength = prefix.length();
            for (int i = 0; i < tags.size(); i++) {
                Tag tag = (Tag) tags.get(i);
                if (tag.getMethodName().regionMatches(ignoreCase, 0, prefix, 0, prefixLength))
                    maybeAdd(list, tag.getMethodName());
            }
        }
        return list;
    }

    // Add name if it's not already in the list.
    private void maybeAdd(List list, String name)
    {
        if (name != null) {
            for (int i = list.size(); i-- > 0;)
                if (name.equals(list.get(i)))
                    return; // It's already there.
            list.add(name);
        }
    }
}
