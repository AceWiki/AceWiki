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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

/**
 * This class stands for a terminal category. Terminal categories consists only of a name and
 * cannot have features. The names of terminal categories correspond to the tokens in the text
 * to be parsed.
 * 
 * @author Tobias Kuhn
 */
public class Terminal extends Category {

	/**
	 * Creates a new terminal category.
	 * 
	 * @param name The name of the terminal category.
	 */
	public Terminal(String name) {
		this.name = name;
	}
	
	protected String getType() {
		return "term";
	}
	
	public void unify(Category c) throws UnificationFailedException {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			throw new UnificationFailedException();
		}
	}
	
	public void tryToUnify(Category c) throws UnificationFailedException {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			throw new UnificationFailedException();
		}
	}
	
	public boolean canUnify(Category c) {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			return false;
		} else {
			return true;
		}
	}
	
	public boolean isSimilar(Category c) {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			return false;
		} else {
			return true;
		}
	}
	
	public boolean subsumes(Category c) {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			return false;
		} else {
			return true;
		}
	}
	
	public void skolemize() {
	}
	
	public Set<String> getFeatureNames() {
		return null;
	}
	
	public Collection<StringRef> getFeatureValues() {
		return null;
	}
	
	String getIdentifier(List<Integer> mvars, String[] usedFeatureNames) {
		return toString();
	}
	
	public Category deepCopy() {
		return new Terminal(name);
	}
	
	Category deepCopy(HashMap<Integer, StringObject> stringObjs) {
		return new Terminal(name);
	}
	
	public int hashCode() {
		return name.hashCode();
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof Terminal)) return false;
		return toString().equals(obj.toString());
	}
	
	public String toString() {
		return "'" + name + "'";
	}

}
