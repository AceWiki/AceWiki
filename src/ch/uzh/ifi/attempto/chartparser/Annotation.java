// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.chartparser;

import java.util.HashMap;
import java.util.Map;

/**
 * This class represents a grammar annotation object.
 * 
 * @author Tobias Kuhn
 */
public class Annotation {
	
	private Map<String, Object> items = new HashMap<String, Object>();
	
	/**
	 * Creates a new annotation object.
	 */
	public Annotation() {
	}
	
	/**
	 * Sets an annotation item.
	 * 
	 * @param name The name of the annotation item.
	 * @param value The value of the annotation item.
	 */
	public void setItem(String name, Object value) {
		items.put(name, value);
	}
	
	/**
	 * Returns the value of an annotation item.
	 * 
	 * @param name The name of the annotation item.
	 * @return The value.
	 */
	public Object getItem(String name) {
		return items.get(name);
	}
	
	/**
	 * Creates a deep copy of this annotation object.
	 * 
	 * @return A deep copy.
	 */
	public Annotation deepCopy() {
		return deepCopy(new HashMap<Integer, StringObject>());
	}
	
	/**
	 * Creates a deep copy of this annotation object using the given string objects. This method is
	 * usually called form another deepCopy-method.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	Annotation deepCopy(HashMap<Integer, StringObject> stringObjs) {
		Annotation a = new Annotation();
		for (String n : items.keySet()) {
			a.setItem(n, copyStructure(items.get(n), stringObjs));
		}
		return a;
	}
	
	private Object copyStructure(Object structure, HashMap<Integer, StringObject> stringObjs) {
		if (structure instanceof Object[]) {
			Object[] array = (Object[]) structure;
			Object[] arrayC = new Object[array.length];
			for (int i=0 ; i < array.length ; i++) {
				Object o = array[i];
				arrayC[i] = copyStructure(o, stringObjs);
			}
			return arrayC;
		} else if (structure instanceof String) {
			return structure;
		} else if (structure instanceof StringRef) {
			StringRef s = (StringRef) structure;
			StringObject so = stringObjs.get(s.getID());
			if (so == null) {
				StringRef sr = new StringRef(s.getString());
				stringObjs.put(s.getID(), sr.getStringObject());
				return sr;
			} else {
				return so.newStringRef();
			}
		}
		return null;
	}

}
