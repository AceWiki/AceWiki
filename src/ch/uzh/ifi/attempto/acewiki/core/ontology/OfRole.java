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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.ape.Gender;
import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class stands for roles that are represented by of-constructs. Of-constructs
 * consist of a noun plus the word "of". They have only one word form.
 *<p>
 * 0: word form consisting of a noun plus the word "of".
 *<p>
 * Examples: "father of"; "part of".
 * 
 * @author Tobias Kuhn
 */
public class OfRole extends Role {
	
	private String word;
	
	/**
	 * Creates a new role that is represented by an of-construct.
	 */
	public OfRole() {
	}
	
	public String[] getWords() {
		return new String[] {word};
	}

	protected void changeWords(String... words) {
		if (words[0] == null || words[0].equals("")) {
			word = null;
		} else if (words[0].endsWith(" of")) {
			word = words[0];
		} else {
			word = words[0] + " of";
		}
	}
	
	/**
	 * Returns the noun of the of-construct.
	 * 
	 * @return The noun.
	 */
	public String getNoun() {
		if (word == null) {
			return null;
		} else if (word.endsWith(" of")) {
			return word.substring(0, word.length()-3);
		} else {
			throw new RuntimeException("Illegal of-construct: " + word);
		}
	}
	
	/**
	 * Returns the pretty-printed noun of the of-construct. Pretty-printing replaces
	 * underscores by blanks.
	 * 
	 * @return The pretty-printed noun.
	 */
	public String getPrettyNoun() {
		String n = getPrettyWord(0);
		if (n == null) {
			return null;
		} else if (n.endsWith(" of")) {
			return n.substring(0, n.length()-3);
		} else {
			throw new RuntimeException("Illegal of-construct: " + n);
		}
	}

	List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		if (word != null) {
			entries.add(LexiconEntry.createNounSgEntry(getNoun(), getNoun(), Gender.UNDEF));
		}
		return entries;
	}
	
	public String getType() {
		return "Of-Construct";
	}
	
	public String getInternalType() {
		return "nounof";
	}

}
