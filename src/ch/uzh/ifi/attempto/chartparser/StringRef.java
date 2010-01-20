// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.chartparser;

/**
 * This class represents a reference to a string object that can unify with other string objects.
 * Uninstantiated values are represented by the null value. Such uninstantiated string objects can
 * unify with other string objects (instantiated or uninstantiated ones).
 * 
 * @author Tobias Kuhn
 */
public class StringRef {
	
	private StringObject stringObject;
	
	/**
	 * Creates a reference to a new string object with null as value.
	 */
	public StringRef() {
		this((String) null);
	}
	
	/**
	 * Creates a reference to a new string object with the given value.
	 * 
	 * @param string The value of the new string object.
	 */
	public StringRef(String string) {
		stringObject = new StringObject(string);
		stringObject.addReference(this);
	}
	
	/**
	 * Creates a new reference to an existing string object.
	 * 
	 * @param stringObject The existing string object.
	 */
	StringRef(StringObject stringObject) {
		this.stringObject = stringObject;
		stringObject.addReference(this);
	}
	
	void setStringObject(StringObject stringObject) {
		this.stringObject = stringObject;
	}
	
	/**
	 * Unifies the string object of this reference with the string object of another reference.
	 * If unification is not possible, an exception is thrown.
	 * 
	 * @param stringRef The reference to the string object to be unified with the string object of
	 *     this object.
	 * @throws UnificationFailedException If unification fails.
	 */
	public void unify(StringRef stringRef) throws UnificationFailedException {
		stringObject.unify(stringRef.stringObject);
	}
	
	/**
	 * Returns the value of the string object of this reference.
	 * 
	 * @return The value of the string object.
	 */
	public String getString() {
		return stringObject.getString();
	}
	
	int getID() {
		return stringObject.getID();
	}
	
	StringObject getStringObject() {
		return stringObject;
	}

}
