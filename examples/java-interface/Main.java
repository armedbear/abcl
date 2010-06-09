/*
 * Main.java
 *
 * Copyright (C) 2008 Ville Voutilainen
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

import org.armedbear.lisp.*;

public class Main
{
    /**
     * This example loads a lisp file and gets two function symbols
     * from it. The functions return implementations of MyInterface.
     * The example gets two separate implementations and invokes
     * the functions in the interface for both implementations.
     */
    public static void main(String[] argv)
    {
	try
	    {
		Interpreter interpreter = Interpreter.createInstance();
		interpreter.eval("(load \"interface_implementation.lisp\")");
		// the function is not in a separate package, thus the
		// correct package is CL-USER. Symbol names are
		// upper case. Package needs the prefix, because java
		// also has a class named Package.
		org.armedbear.lisp.Package defaultPackage = 
		    Packages.findPackage("CL-USER");
		Symbol interfacesym = 
		    defaultPackage.findAccessibleSymbol("GET-INTERFACE");
		Function interfaceFunction = 
		    (Function) interfacesym.getSymbolFunction();
		LispObject myinterface = interfaceFunction.execute();
		MyInterface x = 
		    (MyInterface) JavaObject.getObject(myinterface);
		x.firstFunction();
		x.secondFunction();
		Symbol interfacesym2 = 
		    defaultPackage.
		    findAccessibleSymbol("GET-ANOTHER-INTERFACE");
		Function interfaceFunction2 = 
		    (Function) interfacesym2.getSymbolFunction();
		LispObject myInterface2 = interfaceFunction2.execute();
		MyInterface y = 
		    (MyInterface) JavaObject.getObject(myInterface2);
		y.firstFunction();
		y.secondFunction();
	    }
	catch (Throwable t)
	    {
		System.out.println("exception!");
		t.printStackTrace();
	    }
    }
}