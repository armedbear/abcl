/*
 * AbclScriptEngineFactory.java
 *
 * Copyright (C) 2008 Alessio Stalla
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

package org.armedbear.lisp.scripting;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

public class AbclScriptEngineFactory implements ScriptEngineFactory {

    private static AbclScriptEngine THE_ONLY_ONE_ENGINE = null;
	
    public String getEngineName() {
	return "ABCL Script";
    }

    public String getEngineVersion() {
	return "0.1";
    }

    public List<String> getExtensions() {
	List<String> extensions = new ArrayList<String>(1);
	extensions.add("lisp");
	return Collections.unmodifiableList(extensions);
    }

    public String getLanguageName() {
	return "ANSI Common Lisp";
    }

    public String getLanguageVersion() {
	return "ANSI X3.226:1994";
    }

    public static String escape(String raw) {
	StringBuilder sb = new StringBuilder();
	int len = raw.length();
	char c;
	for(int i = 0; i < len; ++i) {
	    c = raw.charAt(i);
	    if(c != '"') {
		sb.append(c);
	    } else {
		sb.append("\\\"");
	    }
	}
	return sb.toString();
    }
	
    public String getMethodCallSyntax(String obj, String method, String... args) {
	StringBuilder sb = new StringBuilder();
	sb.append("(jcall \"");
	sb.append(method);
	sb.append("\" ");
	sb.append(obj);
	for(String arg : args) {
	    sb.append(" ");
	    sb.append(arg);
	}
	sb.append(")");
	return sb.toString();
    }
    
    public List<String> getMimeTypes() {
	return Collections.unmodifiableList(new ArrayList<String>());
    }

    public List<String> getNames() {
	List<String> names = new ArrayList<String>(1);
	names.add("ABCL");
	names.add("cl");
	names.add("Lisp");
	names.add("Common Lisp");
	return Collections.unmodifiableList(names);
    }

    public String getOutputStatement(String str) {
	return "(cl:print \"" + str + "\")";
    }

    public Object getParameter(String key) {
	// TODO Auto-generated method stub
	return null;
    }

    public String getProgram(String... statements) {
	StringBuilder sb = new StringBuilder();
	sb.append("(cl:progn");
	for(String stmt : statements) {
	    sb.append("\n\t");
	    sb.append(stmt);
	}
	sb.append(")");
	return sb.toString();
    }
    
    public ScriptEngine getScriptEngine() {
        if (THE_ONLY_ONE_ENGINE == null) {
            THE_ONLY_ONE_ENGINE = new AbclScriptEngine();
        }
	return THE_ONLY_ONE_ENGINE;
    }

}
