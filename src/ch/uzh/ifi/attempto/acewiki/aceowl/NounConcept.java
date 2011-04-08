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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.ape.Gender;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;
import ch.uzh.ifi.attempto.chartparser.Preterminal;

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
	
	public void changeWords(String... words) {
		singular = words[0];
		plural = words[1];
	}
	
	public String getIRISuffix() {
		return getWord(0);
	}

	// TODO: move and make package visible
	public List<LexiconEntry> getLexiconEntries() {
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

	// TODO: move an make package visible
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("noun")) {
			lexRules.add(new LexicalRule("noun", getWord(0)));
		}
		if (catName == null || catName.equals("nounpl")) {
			lexRules.add(new LexicalRule("nounpl", getWord(1)));
		}
		if (catName == null || catName.equals("defnoun")) {
			Preterminal cat = new Preterminal("defnoun");
			cat.setFeature("noun", getWord(0));
			lexRules.add(new LexicalRule(cat, "the " + getWord(0)));
		}
	}
	
}
