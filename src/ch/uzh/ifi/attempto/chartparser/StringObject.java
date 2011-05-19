// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

/**
 * This class represents a string object to which string references can point and which can unify
 * with other string objects.
 * 
 * @author Tobias Kuhn
 */
class StringObject {
	
	private static int nextID = 0;
	
	private String string;
	private final int id;
	
	// This should be a set, but a list is used for perfomance reasons and duplicates are removed
	// from time to time:
	private List<StringRef> stringRefs = new ArrayList<StringRef>(3);
	
	// This field counts the number of new string references since the last duplicates removal.
	private int newStringRefs = 0;
	
	public StringObject(String string) {
		this.string = string;
		id = nextID++;
	}
	
	public String getString() {
		return string;
	}
	
	void addReference(StringRef r) {
		stringRefs.add(r);
		newStringRefs++;
		r.setStringObject(this);
	}
	
	public void unify(StringObject e) throws UnificationFailedException {
		if (string == null) {
			string = e.string;
		} else if (e.string != null && !string.equals(e.string)) {
			throw new UnificationFailedException();
		}
		stringRefs.addAll(e.stringRefs);
		newStringRefs += e.stringRefs.size();
		for (StringRef r : e.stringRefs) {
			r.setStringObject(this);
		}
		if (newStringRefs > 100) {
			// Time to remove duplicates from the list of string references.
			HashSet<StringRef> set = new HashSet<StringRef>(stringRefs);
			stringRefs.clear();
			stringRefs.addAll(set);
			newStringRefs = 0;
		}
	}
	
	public int getID() {
		return id;
	}
	
	public StringRef newStringRef() {
		return new StringRef(this);
	}

}
