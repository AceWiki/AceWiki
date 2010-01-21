// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.ape.Gender;
import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class stands for concepts that are represented by nouns. This is the only way how
 * concepts can be represented at the moment, but it is planned to support also adjectives.
 * Noun concepts have two word forms.
 *<p>
 * 0: singular form.
 * 1: plural form.
 *<p>
 * Examples: ["country", "countries"]; ["woman", "women"].
 * 
 * @author Tobias Kuhn
 */
public class NounConcept extends Concept {
	
	private String singular, plural;
	
	/**
	 * Creates a new noun concept.
	 */
	public NounConcept() {
		singular = "";
		plural = "";
	}
	
	public String[] getWords() {
		return new String[] {singular, plural};
	}
	
	protected void changeWords(String... words) {
		singular = words[0];
		plural = words[1];
	}

	List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		entries.add(LexiconEntry.createNounSgEntry(getWord(0), getWord(0), Gender.UNDEF));
		entries.add(LexiconEntry.createNounPlEntry(getWord(1), getWord(0), Gender.UNDEF));
		return entries;
	}
	
	public String getType() {
		return "Noun";
	}
	
	public String getInternalType() {
		return "noun";
	}
	
}
