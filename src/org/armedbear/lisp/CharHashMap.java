package org.armedbear.lisp;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CharHashMap<T> {

	final public T[] constants;
	final public T NULL;
	final static int CACHE_SIZE = 256; 
	final HashMap<Character, T> backing;

        @SuppressWarnings("unchecked")
	public CharHashMap(Class componentType, T def) {
		NULL = def;
		constants = (T[]) Array.newInstance(componentType, CACHE_SIZE);
		Arrays.fill(constants, NULL);
		backing = new HashMap<Character, T>();
	}
	
	@Override
	public Object clone() {
		CharHashMap<T> n = new CharHashMap<T>(constants.getClass().getComponentType(),NULL);
		System.arraycopy(constants,0, n.constants,0,CACHE_SIZE);
		n.backing.putAll(backing);
		return n;
	}
	
	public T get(char key) {
		if (key<CACHE_SIZE) return constants[key];
		T value = backing.get(key);
		return (value==null) ? NULL:value;
	}

	public void clear() {
		Arrays.fill(constants,NULL);
		backing.clear();
	}

	public T put(char key, T value) {
		if (key<CACHE_SIZE) {
			T old = constants[key];
			constants[key] = value;
			return old;
		}
		else return backing.put(key, value);
	}

	public Iterator<Character> getCharIterator() {
		return new Iterator<Character>() {			
			final Iterator<Character> carIt =  backing.keySet().iterator();
			int charNum = -1;
			public boolean hasNext() {
				if ( charNum<CACHE_SIZE) return true;
				return carIt.hasNext();
			}
			public Character next() {
				if ( charNum<CACHE_SIZE) return (char)++charNum;
				return carIt.next();
			}
			public void remove() {
				throw new UnsupportedOperationException();			
			}
			
		};
	}
}
