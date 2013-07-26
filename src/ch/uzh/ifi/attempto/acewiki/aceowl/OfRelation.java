// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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
import ch.uzh.ifi.attempto.ape.Gender;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class stands for relations that are represented by of-constructs in ACE and object
 * properties in OWL. Of-constructs consist of a noun plus the word "of". They have only one word
 * form.
 *<p>
 * 0: word form consisting of a noun plus the word "of".
 *<p>
 * Examples: "father of"; "part of".
 * 
 * @author Tobias Kuhn
 */
public class OfRelation extends OWLRelation implements ACEOWLOntoElement {
	
	private String word;
	
	/**
	 * Creates a new relation that is represented by an of-construct.
	 */
	public OfRelation() {
	}
	
	public String[] getWords() {
		return new String[] {word};
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		word = words[0];
		if (!words[0].endsWith(" of")) {
			word = word + " of";
		}
	}
	
	public String serializeWords() {
		return word + ";";
	}
	
	/**
	 * Returns the noun of the of-construct.
	 * 
	 * @return The noun.
	 */
	public String getNoun() {
		if (word == null) {
			return null;
		} else {
			return word.substring(0, word.length()-3);
		}
	}
	
	public String getIRISuffix() {
		return getNoun();
	}
	
	public List<LexiconEntry> getLexiconEntries() {
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
	
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("nounof")) {
			lexRules.add(new LexicalRule("nounof", getWord(0)));
		}
	}

}
