/*
 * CommandTable.java
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public class CommandTable
{
    // The default load factor is 0.75, so an initial capacity of 600 will
    // accommodate 450 entries without rehashing.
    private static final int INITIAL_CAPACITY = 600;

    private static HashMap map;

    public static final Command getCommand(String name)
    {
        if (name == null)
            return null;
        if (map == null)
            init();
        return (Command) map.get(name.toLowerCase());
    }

    private static synchronized void init()
    {
        if (map == null) {
            map = new HashMap(INITIAL_CAPACITY);

            // Commands implemented in Editor.java.
            addCommand("backspace");
            addCommand("bob");
            addCommand("bol");
            addCommand("bottom");
            addCommand("cancelBackgroundProcess");
            addCommand("closeAll");
            addCommand("closeOthers");
            addCommand("closeParen");
            addCommand("commentRegion");
            addCommand("copyAppend");
            addCommand("copyPath");
            addCommand("copyRegion");
            addCommand("cppFindMatch");
            addCommand("cycleIndentSize");
            addCommand("cyclePaste");
            addCommand("cycleTabWidth");
            addCommand("defaultMode");
            addCommand("delete");
            addCommand("deleteWordLeft");
            addCommand("deleteWordRight");
            addCommand("dirBrowseFile");
            addCommand("dirCopyFile");
            addCommand("dirDeleteFiles");
            addCommand("dirDoShellCommand");
            addCommand("dirGetFile");
            addCommand("dirHome");
            addCommand("dirHomeDir");
            addCommand("dirMoveFile");
            addCommand("dirRescan");
            addCommand("dirTagFile");
            addCommand("dirUpDir");
            addCommand("doShellCommandOnRegion");
            addCommand("down");
            addCommand("dropBookmark");
            addCommand("dropTemporaryMarker");
            addCommand("electricCloseAngleBracket");
            addCommand("electricCloseBrace");
            addCommand("electricColon");
            addCommand("electricOpenBrace");
            addCommand("electricPound");
            addCommand("electricQuote");
            addCommand("electricSemi");
            addCommand("electricStar");
            addCommand("end");
            addCommand("enlargeWindow");
            addCommand("eob");
            addCommand("eol");
            addCommand("escape");
            addCommand("executeCommand");
            addCommand("findFirstOccurrence");
            addCommand("findMatchingChar");
            addCommand("findNext");
            addCommand("findNextWord");
            addCommand("findPrev");
            addCommand("findPrevWord");
            addCommand("fold");
            addCommand("foldMethods");
            addCommand("foldRegion");
            addCommand("gotoBookmark");
            addCommand("gotoTemporaryMarker");
            addCommand("help");
            addCommand("home");
            addCommand("httpDeleteCookies");
            addCommand("incrementalFind");
            addCommand("indentLine");
            addCommand("indentLineOrRegion");
            addCommand("indentRegion");
            addCommand("insertBraces");
            addCommand("insertByte");
            addCommand("insertChar");
            addCommand("insertKeyText");
            addCommand("insertParentheses");
            addCommand("insertString");
            addCommand("insertTab");
            addCommand("jmips");
            addCommand("justOneSpace");
            addCommand("killAppend");
            addCommand("killBuffer");
            addCommand("killFrame");
            addCommand("killLine");
            addCommand("killRegion");
            addCommand("killWordLeft");
            addCommand("killWordRight");
            addCommand("left");
            addCommand("mode");
            addCommand("movePastCloseAndReindent");
            addCommand("newBuffer");
            addCommand("newFrame");
            addCommand("newline");
            addCommand("newlineAndIndent");
            addCommand("nextBuffer");
            addCommand("nextFrame");
            addCommand("offset");
            addCommand("openFile");
            addCommand("openFileInOtherWindow");
            addCommand("otherWindow");
            addCommand("pageDown");
            addCommand("pageDownOtherWindow");
            addCommand("pageUp");
            addCommand("pageUpOtherWindow");
            addCommand("paste");
            addCommand("pasteColumn");
            addCommand("popPosition");
            addCommand("prevBuffer");
            addCommand("pushPosition");
            addCommand("quit");
            addCommand("redo");
            addCommand("resetDisplay");
            addCommand("revertBuffer");
            addCommand("right");
            addCommand("save");
            addCommand("saveAll");
            addCommand("saveAllExit");
            addCommand("saveAs");
            addCommand("saveCopy");
            addCommand("selectAll");
            addCommand("selectBob");
            addCommand("selectDown");
            addCommand("selectEnd");
            addCommand("selectEob");
            addCommand("selectHome");
            addCommand("selectLeft");
            addCommand("selectPageDown");
            addCommand("selectPageUp");
            addCommand("selectRight");
            addCommand("selectSyntax");
            addCommand("selectUp");
            addCommand("selectWord");
            addCommand("selectWordLeft");
            addCommand("selectWordRight");
            addCommand("setEncoding");
            addCommand("shrinkWindowIfLargerThanBuffer");
            addCommand("sidebarListBuffers");
            addCommand("sidebarListTags");
            addCommand("slideIn");
            addCommand("slideOut");
            addCommand("splitWindow");
            addCommand("stamp");
            addCommand("tab");
            addCommand("tempBufferQuit");
            addCommand("textMode");
            addCommand("toCenter");
            addCommand("toTop");
            addCommand("toggleSidebar");
            addCommand("top");
            addCommand("uncommentRegion");
            addCommand("undo");
            addCommand("unfold");
            addCommand("unfoldAll");
            addCommand("unsplitWindow");
            addCommand("unwrapParagraph");
            addCommand("up");
            addCommand("visibleTabs");
            addCommand("whatChar");
            addCommand("windowDown");
            addCommand("windowUp");
            addCommand("wordLeft");
            addCommand("wordRight");
            addCommand("wrapParagraph");
            addCommand("wrapParagraphsInRegion");
            addCommand("wrapRegion");

            // Commands implemented in other classes.
            addCommand("about", "AboutDialog");
            addCommand("alias", "AliasDialog");
            addCommand("alignStrings",  "AlignStrings");
            addCommand("apropos", "Help");
            addCommand("archiveOpenFile", "ArchiveMode");
            addCommand("backwardSexp", "LispMode");
            addCommand("backwardUpList", "LispMode");
            addCommand("binaryMode", "BinaryMode");
            addCommand("browseFileAtDot", "BrowseFile");
            addCommand("centerTag", "TagCommands");
            addCommand("changes", "ChangeMarks");
            addCommand("checkPath", "CheckPath");
            addCommand("chmod", "Directory");
            addCommand("clearRegister", "Registers");
            addCommand("compile", "CompilationCommands");
            addCommand("compileAndLoadLispFile", "LispMode");
            addCommand("compileLispFile", "LispMode");
            addCommand("copyLink", "WebBuffer");
            addCommand("copyXPath", "XmlMode");
            addCommand("cvs", "CVS", "cvs");
            addCommand("cvsAdd", "CVS", "add");
            addCommand("cvsCommit", "CVS", "commit");
            addCommand("cvsDiff", "CVS", "diff");
            addCommand("cvsDiffDir", "CVS", "diffDir");
            addCommand("cvsLog", "CVS", "log");
            addCommand("darcs", "Darcs");
            addCommand("decodeRegion", "RegionCommands");
            addCommand("defaultKeyMaps", "KeyMap");
            addCommand("describe", "LispShellMode");
            addCommand("describeBindings", "Help");
            addCommand("describeKey", "DescribeKeyDialog");
            addCommand("detabRegion", "RegionCommands");
            addCommand("diff", "DiffMode");
            addCommand("diffGotoFile", "DiffMode", "gotoFile");
            addCommand("dir", "Directory");
            addCommand("dirBack", "Directory");
            addCommand("dirCycleSortBy", "Directory");
            addCommand("dirDoShellCommand", "Directory");
            addCommand("dirForward", "Directory");
            addCommand("dirLimit", "Directory");
            addCommand("dirOpenFile", "Directory");
            addCommand("dirOpenFileAndKillDirectory", "Directory");
            addCommand("dirUnlimit", "Directory");
            addCommand("doShellCommandOnRegion", "RegionCommands");
            addCommand("downList", "LispMode");
            addCommand("editRegister", "Registers");
            addCommand("endMacro", "Macro");
            addCommand("entabRegion", "RegionCommands");
            addCommand("evalDefunLisp", "LispMode");
            addCommand("compileDefunLisp", "LispMode");
            addCommand("electricCloseParen", "LispShellMode");
            addCommand("evalRegionLisp", "LispMode");
            addCommand("expand", "Expansion");
            addCommand("find", "FindDialog");
            addCommand("findInFiles", "FindInFiles");
            addCommand("findOccurrenceAtDot", "ListOccurrences");
            addCommand("findOccurrenceAtDotAndKillList", "ListOccurrences");
            addCommand("findTag", "TagCommands");
            addCommand("findTagAtDot", "TagCommands");
            addCommand("findTagAtDotOtherWindow", "TagCommands");
            addCommand("finish", "CheckinBuffer");
            addCommand("followContext", "FollowContextTask");
            addCommand("forwardSexp", "LispMode");
            addCommand("google", "WebMode");
            addCommand("gotoFile", "GotoFile");
            addCommand("help", "Help");
            addCommand("htmlBold", "HtmlMode");
            addCommand("htmlElectricEquals", "HtmlMode");
            addCommand("htmlEndTag", "HtmlMode");
            addCommand("htmlFindMatch", "HtmlMode");
            addCommand("htmlInsertMatchingEndTag", "HtmlMode");
            addCommand("htmlInsertTag", "HtmlMode");
            addCommand("htmlStartTag", "HtmlMode");
            addCommand("httpShowHeaders", "HttpLoadProcess");
            addCommand("hyperspec", "LispMode");
            addCommand("iList", "IList");
            addCommand("imageCycleBackground", "ImageMode");
            addCommand("imageFit", "ImageMode");
            addCommand("imageRestore", "ImageMode");
            addCommand("imageZoomIn", "ImageMode");
            addCommand("imageZoomOut", "ImageMode");
            addCommand("insertRegister", "Registers");
            addCommand("jdkHelp", "JDKHelp");
            addCommand("jlisp", "JLisp");
            addCommand("jumpToColumn", "JumpCommands");
            addCommand("jumpToLine", "JumpCommands");
            addCommand("jumpToOffset", "JumpCommands");
            addCommand("jumpToTag", "ListTagsMode");
            addCommand("jumpToTagAndKillList", "ListTagsMode");
            addCommand("killCompilation", "CompilationBuffer");
            addCommand("lisp", "LispShell");
            addCommand("listFiles", "FindInFiles");
            addCommand("lispFindMatchingChar", "LispMode");
            addCommand("lispSelectSyntax", "LispMode");
            addCommand("listIncludes", "CheckPath");
            addCommand("listMatchingTags", "TagCommands");
            addCommand("listMatchingTagsAtDot", "TagCommands");
            addCommand("listOccurrences", "ListOccurrences");
            addCommand("listOccurrencesOfPatternAtDot", "ListOccurrences");
            addCommand("listProperties", "PropertiesDialog");
            addCommand("listRegisters", "Registers");
            addCommand("listTags", "ListTagsDialog");
            addCommand("listThreads", "Debug");
            addCommand("loadLispFile", "LispMode");
            addCommand("loadSession", "Session");
            addCommand("lowerCaseRegion", "RegionCommands");
            addCommand("makeTagFile", "TagCommands");
            addCommand("man", "ManMode");
            addCommand("manFollowLink", "ManMode");
            addCommand("markSexp", "LispMode");
            addCommand("mouseCopyToInput", "LispShellMode");
            addCommand("mouseFindOccurrence", "ListOccurrences");
            addCommand("mouseFindTag", "TagCommands");
            addCommand("mouseJumpToTag", "ListTagsMode");
            addCommand("nextChange", "ChangeMarks");
            addCommand("nextComment", "CheckinBuffer");
            addCommand("nextError", "CompilationCommands");
            addCommand("nextTag", "TagCommands");
            addCommand("openFileInOtherFrame", "OpenFileDialog");
            addCommand("p4", "P4");
            addCommand("p4Add", "P4", "add");
            addCommand("p4Change", "P4", "change");
            addCommand("p4Diff", "P4", "diff");
            addCommand("p4DiffDir", "P4", "diffDir");
            addCommand("p4Edit", "P4", "edit");
            addCommand("p4Log", "P4", "log");
            addCommand("p4Revert", "P4", "revert");
            addCommand("p4Submit", "P4", "submit");
            addCommand("pastePrimarySelection", "SystemSelection");
            addCommand("phpHelp", "PHPMode");
            addCommand("playbackMacro", "Macro");
            addCommand("previousChange", "ChangeMarks");
            addCommand("previousComment", "CheckinBuffer");
            addCommand("previousError", "CompilationCommands");
            addCommand("previousTag", "TagCommands");
            addCommand("print", "PrintCommands");
            addCommand("printBuffer", "PrintCommands");
            addCommand("printRegion", "PrintCommands");
            addCommand("properties", "PropertiesDialog");
            addCommand("recentFiles", "RecentFilesDialog");
            addCommand("recompile", "CompilationCommands");
            addCommand("recordMacro", "Macro");
            addCommand("reloadKeyMaps", "KeyMap");
            addCommand("renumberRegion", "RegionCommands");
            addCommand("replace", "ReplaceDialog");
            addCommand("replaceInFiles", "FindInFiles");
            addCommand("resetLisp", "LispShellMode");
            addCommand("saveSession", "Session");
            addCommand("saveToRegister", "Registers");
            addCommand("selectToMarker", "Marker");
            addCommand("selectToTemporaryMarker", "Marker");
            addCommand("shellCommand", "ShellCommand");
            addCommand("shell", "Shell");
            addCommand("shellBackspace", "CommandInterpreter");
            addCommand("shellEnter", "CommandInterpreter");
            addCommand("shellEscape", "CommandInterpreter");
            addCommand("shellHome", "CommandInterpreter");
            addCommand("shellInterrupt", "Shell");
            addCommand("shellNextInput", "CommandInterpreter");
            addCommand("shellNextPrompt", "CommandInterpreter");
            addCommand("shellPreviousInput", "CommandInterpreter");
            addCommand("shellPreviousPrompt", "CommandInterpreter");
            addCommand("shellTab", "Shell");
            addCommand("showMessage", "CompilationCommands");
            addCommand("slime", "LispShell");
            addCommand("sortLines", "Sort");
            addCommand("source", "JDKHelp");
            addCommand("startMacro", "Macro");
            addCommand("ssh", "RemoteShell");
            addCommand("tagDown", "ListTagsMode");
            addCommand("tagUp", "ListTagsMode");
            addCommand("telnet", "RemoteShell");
            addCommand("thisError", "CompilationCommands");
            addCommand("toggleWrap", "WrapText");
            addCommand("upperCaseRegion", "RegionCommands");
            addCommand("whereIs", "ExecuteCommandDialog");
            addCommand("wrapComment", "WrapText");
            addCommand("writeGlobalKeyMap", "SaveFileDialog");
            addCommand("writeLocalKeyMap", "SaveFileDialog");
            addCommand("xmlElectricEquals", "XmlMode");
            addCommand("xmlElectricSlash", "XmlMode");
            addCommand("xmlFindCurrentNode", "XmlMode");
            addCommand("xmlFindMatch", "XmlMode");
            addCommand("xmlInsertEmptyElementTag", "XmlMode");
            addCommand("xmlInsertMatchingEndTag", "XmlMode");
            addCommand("xmlInsertTag", "XmlMode");
            addCommand("xmlParseBuffer", "XmlMode");
            addCommand("xmlValidateBuffer", "XmlMode");

            // Mail commands.
            addCommand("attachFile", "mail.MailCommands");
            addCommand("bounce", "mail.MailCommands");
            addCommand("ccGroup", "mail.MailCommands");
            addCommand("compose", "mail.MailCommands");
            addCommand("foldThread", "mail.MailCommands");
            addCommand("foldThreads", "mail.MailCommands");
            addCommand("inbox", "mail.MailCommands");
            addCommand("mailboxCreateFolder", "mail.MailCommands");
            addCommand("mailboxDelete", "mail.MailCommands");
            addCommand("mailboxDeleteFolder", "mail.MailCommands");
            addCommand("mailboxExpunge", "mail.MailCommands");
            addCommand("mailboxFlag", "mail.MailCommands");
            addCommand("mailboxGetNewMessages", "mail.MailCommands");
            addCommand("mailboxLastMessage", "mail.MailCommands");
            addCommand("mailboxLimit", "mail.MailCommands");
            addCommand("mailboxMarkRead", "mail.MailCommands");
            addCommand("mailboxMarkUnread", "mail.MailCommands");
            addCommand("mailboxMoveToFolder", "mail.MailCommands");
            addCommand("mailboxReadMessage", "mail.MailCommands");
            addCommand("mailboxReadMessageOtherWindow", "mail.MailCommands");
            addCommand("mailboxSaveToFolder", "mail.MailCommands");
            addCommand("mailboxStop", "mail.MailCommands");
            addCommand("mailboxTag", "mail.MailCommands");
            addCommand("mailboxTagPattern", "mail.MailCommands");
            addCommand("mailboxToggleRaw", "mail.MailCommands");
            addCommand("mailboxUndelete", "mail.MailCommands");
            addCommand("mailboxUnlimit", "mail.MailCommands");
            addCommand("mailboxUntagAll", "mail.MailCommands");
            addCommand("messageDelete", "mail.MailCommands");
            addCommand("messageFlag", "mail.MailCommands");
            addCommand("messageForward", "mail.MailCommands");
            addCommand("messageIndex", "mail.MailCommands");
            addCommand("messageMoveToFolder", "mail.MailCommands");
            addCommand("messageNext", "mail.MailCommands");
            addCommand("messageNextInThread", "mail.MailCommands");
            addCommand("messageParent", "mail.MailCommands");
            addCommand("messagePrevious", "mail.MailCommands");
            addCommand("messagePreviousInThread", "mail.MailCommands");
            addCommand("messageReplyToGroup", "mail.MailCommands");
            addCommand("messageReplyToSender", "mail.MailCommands");
            addCommand("messageSaveAttachment", "mail.MailCommands");
            addCommand("messageToggleHeaders", "mail.MailCommands");
            addCommand("messageToggleRaw", "mail.MailCommands");
            addCommand("messageToggleWrap", "mail.MailCommands");
            addCommand("messageViewAttachment", "mail.MailCommands");
            addCommand("openMailbox", "mail.MailCommands");
            addCommand("send", "mail.MailCommands");
            addCommand("sendMailBackTab", "mail.MailCommands");
            addCommand("sendMailElectricColon", "mail.MailCommands");
            addCommand("sendMailTab", "mail.MailCommands");
            addCommand("toggleGroupByThread", "mail.MailCommands");

            // News commands.
            addCommand("news", "mail.NewsCommands");
            addCommand("openGroup", "mail.NewsCommands");
            addCommand("openGroupAtDot", "mail.NewsCommands");
            addCommand("readArticle", "mail.NewsCommands");
            addCommand("readArticleOtherWindow", "mail.NewsCommands");

            // jdb commands.
            addCommand("jdb", "jdb.JdbCommands");
            addCommand("jdbContinue", "jdb.JdbCommands");
            addCommand("jdbDeleteBreakpoint", "jdb.Jdb");
            addCommand("jdbFinish", "jdb.JdbCommands");
            addCommand("jdbLocals", "jdb.JdbCommands");
            addCommand("jdbNext", "jdb.JdbCommands");
            addCommand("jdbQuit", "jdb.JdbCommands");
            addCommand("jdbRestart", "jdb.JdbCommands");
            addCommand("jdbRunToCurrentLine", "jdb.Jdb");
            addCommand("jdbSetBreakpoint", "jdb.Jdb");
            addCommand("jdbStep", "jdb.JdbCommands");
            addCommand("jdbSuspend", "jdb.JdbCommands");
            addCommand("jdbToggleBreakpoint", "jdb.Jdb");

            // Web browser commands.
            addCommand("webBack", "WebBuffer", "back");
            addCommand("webForward", "WebBuffer", "forward");
            addCommand("webReload", "WebBuffer", "refresh");
            addCommand("followLink", "WebBuffer");
            addCommand("mouseFollowLink", "WebBuffer");
            addCommand("viewPage", "WebBuffer");
            addCommand("viewSource", "WebBuffer");

            // Abbreviations.
            addCommand("sr", "Registers", "saveToRegister");
            addCommand("ir", "Registers", "insertRegister");
            addCommand("lr", "Registers", "listRegisters");
            addCommand("hs", "LispMode", "hyperspec");
            addCommand("clhs", "LispMode", "hyperspec");
            addCommand("abcl", "LispShell", "lisp");

            if (Editor.isDebugEnabled() && map.size() > INITIAL_CAPACITY * 0.75) {
                Log.error("CommandTable.init need to increase initial capacity!");
                Log.error("CommandTable.init size = " + map.size());
            }
        }
    }

    // For commands that are implemented by a method of the same name in the
    // org.armedbear.j.Editor class.
    private static final void addCommand(String commandName)
    {
        map.put(commandName.toLowerCase(), new Command(commandName));
    }

    // For commands that are implemented by a method of the same name in the
    // specified class.
    private static final void addCommand(String commandName, String className)
    {
        map.put(commandName.toLowerCase(), new Command(commandName, className, commandName));
    }

    private static final void addCommand(String commandName, String className, String methodName)
    {
        map.put(commandName.toLowerCase(), new Command(commandName, className, methodName));
    }

    public static List getCompletionsForPrefix(String prefix)
    {
        String lower = prefix.toLowerCase();
        Iterator it = map.values().iterator();
        ArrayList list = new ArrayList();
        while (it.hasNext()) {
            Command command = (Command) it.next();
            if (command.getName().toLowerCase().startsWith(lower))
                list.add(command.getName());
        }
        return list;
    }

    public static List apropos(String s)
    {
        String lower = s.toLowerCase();
        Iterator it = map.values().iterator();
        ArrayList list = new ArrayList();
        while (it.hasNext()) {
            String name = ((Command)it.next()).getName();
            if (name.toLowerCase().indexOf(lower) >= 0)
                list.add(name);
        }
        return list;
    }
}
