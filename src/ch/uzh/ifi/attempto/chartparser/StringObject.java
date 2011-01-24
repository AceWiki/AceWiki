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
	private List<StringRef> stringRefs = new ArrayList<StringRef>(3);
	
	public StringObject(String string) {
		this.string = string;
		id = nextID++;
	}
	
	public String getString() {
		return string;
	}
	
	void addReference(StringRef r) {
		stringRefs.add(r);
		r.setStringObject(this);
	}
	
	public void unify(StringObject e) throws UnificationFailedException {
		if (string == null) {
			string = e.string;
		} else if (e.string != null && !string.equals(e.string)) {
			throw new UnificationFailedException();
		}
		stringRefs.addAll(e.stringRefs);
		for (StringRef r : e.stringRefs) {
			r.setStringObject(this);
		}
	}
	
	public int getID() {
		return id;
	}
	
	public StringRef newStringRef() {
		return new StringRef(this);
	}

}
