/*
 * Java.java
 *
 * Copyright (C) 2002-2006 Peter Graves, Andras Simon
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;

public final class JavaBeans {

    private static final Primitive JGET_PROPERTY_VALUE = new pf__jget_property_value();
    @DocString(name="%jget-propety-value", args="java-object property-name",
    doc="Gets a JavaBeans property on JAVA-OBJECT.\n" +
        "SYSTEM-INTERNAL: Use jproperty-value instead.")
    private static final class pf__jget_property_value extends Primitive
    {
        pf__jget_property_value() 
        {
	    super("%jget-property-value", PACKAGE_JAVA, false,
                  "java-object property-name");
        }
    	
        @Override
        public LispObject execute(LispObject javaObject, LispObject propertyName) {
			try {
				Object obj = javaObject.javaInstance();
				PropertyDescriptor pd = getPropertyDescriptor(obj, propertyName);
				Object value = pd.getReadMethod().invoke(obj);
				if(value instanceof LispObject) {
				    return (LispObject) value;
				} else if(value != null) {
				    return JavaObject.getInstance(value, true);
				} else {
				    return NIL;
				}
			} catch (Exception e) {
                return error(new JavaException(e));
			}
        }
    };
    
    private static final Primitive JSET_PROPERTY_VALUE = new pf__jset_property_value();
    @DocString(name="%jset-propety-value", args="java-object property-name value",
    doc="Sets a JavaBean property on JAVA-OBJECT.\n" +
        "SYSTEM-INTERNAL: Use (setf jproperty-value) instead.")
    private static final class pf__jset_property_value extends Primitive
    {
        pf__jset_property_value()
        {
	    super("%jset-property-value", PACKAGE_JAVA, false,
                  "java-object property-name value");
        }
    	
        @Override
        public LispObject execute(LispObject javaObject, LispObject propertyName, LispObject value) {
	    Object obj = null;
	    try {
		obj = javaObject.javaInstance();
		PropertyDescriptor pd = getPropertyDescriptor(obj, propertyName);
		Object jValue;
		//TODO maybe we should do this in javaInstance(Class)
		if(value instanceof JavaObject) {
		    jValue = value.javaInstance();
		} else {
		    if(Boolean.TYPE.equals(pd.getPropertyType()) ||
		       Boolean.class.equals(pd.getPropertyType())) {
			jValue = value != NIL;
		    } else {
			jValue = value != NIL ? value.javaInstance() : null;
		    }
		}
		pd.getWriteMethod().invoke(obj, jValue);
		return value;
	    } catch (Exception e) {
            return error(new JavaException(e));
	    }
        }
    };

    static PropertyDescriptor getPropertyDescriptor(Object obj, LispObject propertyName) throws IntrospectionException {
        String prop = ((AbstractString) propertyName).getStringValue();
        BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
        for(PropertyDescriptor pd : beanInfo.getPropertyDescriptors()) {
        	if(pd.getName().equals(prop)) {
        		return pd;
        	}
        }
        error(new LispError("Property " + prop + " not found in " + obj));

        return null; // not reached
    }
}    
