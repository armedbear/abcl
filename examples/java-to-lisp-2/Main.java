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
     * This example creates an Interpreter instance, loads our
     * lisp code from a file and then looks up two functions defined
     * in the loaded lisp file and executes the functions. 
     *
     * The first function takes a single parameter and prints its value, 
     * so we can provide any Object, so we use a String.
     *
     * The second function takes two numbers, adds them together, prints
     * the parameters and the result, and returns the result. 
     * We use two integers as parameters and just print the result
     * from java side.
     */
    public static void main(String[] argv)
    {
	try
	    {
		Interpreter interpreter = Interpreter.createInstance();
		interpreter.eval("(load \"lispfunctions.lisp\")");
		// the function is not in a separate package, thus the
		// correct package is CL-USER. Symbol names are
		// upper case. Package needs the prefix, because java
		// also has a class named Package.
		org.armedbear.lisp.Package defaultPackage = 
		    Packages.findPackage("CL-USER");

		Symbol voidsym = 
		    defaultPackage.findAccessibleSymbol("VOID-FUNCTION");
		Function voidFunction = (Function) voidsym.getSymbolFunction();
		voidFunction.execute(new JavaObject("String given from java"));

		Symbol intsym = 
		    defaultPackage.findAccessibleSymbol("INT-FUNCTION");
		Function intFunction = (Function) intsym.getSymbolFunction();
		LispObject result = 
		    intFunction.execute(new JavaObject(1), 
					new JavaObject(6));
		System.out.print("The result on the java side: ");
		System.out.println(result.intValue());
	    }
	catch (Throwable t)
	    {
		System.out.println("exception!");
		t.printStackTrace();
	    }
    }
}