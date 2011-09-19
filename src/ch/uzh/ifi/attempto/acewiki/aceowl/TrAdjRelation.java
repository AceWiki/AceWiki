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

import ch.uzh.ifi.attempto.acewiki.owl.OWLRelation;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class stands for relations that are represented by transitive adjectives in ACE and object
 * properties in OWL. Transitive adjectives consist of an adjective plus a preposition that is
 * connected to the adjective by a hyphen "-" or an underscore "_". Underscores are replaced by
 * blanks in the case of pretty-printing. Transitive adjectives have just one word form.
 *<p>
 * 0: word form consisting of an adjective plus a preposition.
 *<p>
 * Examples: "located-in"; "used_for".
 * 
 * @author Tobias Kuhn
 */
public class TrAdjRelation extends OWLRelation implements ACEOWLOntoElement {
	
	private String word;
	
	/**
	 * Creates a new relation that is represented by a transitive adjective.
	 */
	public TrAdjRelation() {
		word = "";
	}
	
	public String[] getWords() {
		return new String[] {word};
	}

	public void setWords(String... words) {
		word = words[0];
	}
	
	public String getIRISuffix() {
		return getWord(0);
	}
	
	public List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		entries.add(LexiconEntry.createTrAdjEntry(word, word, ""));
		return entries;
	}
	
	public String getType() {
		return "Transitive Adjective";
	}
	
	public String getInternalType() {
		return "tradj";
	}
	
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("tradj")) {
			lexRules.add(new LexicalRule("tradj", getWord(0)));
		}
	}

}
