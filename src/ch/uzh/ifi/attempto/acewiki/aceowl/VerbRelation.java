// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
 * This class stands for relations that are represented by transitive verbs in ACE and object
 * properties in OWL. Transitive verbs have three word forms: a third singular form, a bare
 * infinitive form, and a passive form. The bare infinitive form is used in the case of negation
 * and plural. The passive form always ends with the word "by".
 *<p>
 * 0: third singular form.
 * 1: bare infinitive form.
 * 2: passive form.
 *<p>
 * Examples: ["gives", "give", "given by"]; ["knows", "know", "known by"].
 * 
 * @author Tobias Kuhn
 */
public class VerbRelation extends OWLRelation implements ACEOWLOntoElement {
	
	private String thirdSg, inf, passive;
	
	/**
	 * Creates a new verb relation.
	 */
	public VerbRelation() {
	}
	
	public String[] getWords() {
		return new String[] {thirdSg, inf, passive};
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		thirdSg = words[0];
		inf = words[1];
		if (words.length < 3 || words[2] == null) {
			passive = null;
		} else if (words[2].endsWith(" by")) {
			passive = words[2];
		} else {
			passive = words[2] + " by";
		}
		if (passive != null && passive.length() == 0) {
			passive = null;
		}
	}
	
	public String serializeWords() {
		return thirdSg + ";" + inf + ";" + (passive == null ? "" : passive) + ";";
	}
	
	/**
	 * Returns the past participle which is the passive form without the "by".
	 * E.g. for the passive form "given by", the past participle is "given".
	 * 
	 * @return The past participle form.
	 */
	public String getPastPart() {
		String s = getWord(2);
		if (s == null) {
			return null;
		} else if (s.endsWith(" by")) {
			return s.substring(0, s.length()-3);
		} else {
			throw new RuntimeException("Illegal passive word form: " + s);
		}
	}
	
	public String getIRISuffix() {
		return getWord(1);
	}
	
	public List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		entries.add(LexiconEntry.createTrVerbThirdEntry(thirdSg, inf));
		entries.add(LexiconEntry.createTrVerbInfEntry(inf, inf));
		if (passive != null) {
			entries.add(LexiconEntry.createTrVerbPPEntry(getPastPart(), inf));
		}
		return entries;
	}
	
	public String getType() {
		return "Verb";
	}
	
	public String getInternalType() {
		return "trverb";
	}
	
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("verbsg")) {
			lexRules.add(new LexicalRule("verbsg", getWord(0)));
		}
		if (catName == null || catName.equals("verbinf")) {
			lexRules.add(new LexicalRule("verbinf", getWord(1)));
		}
		if (catName == null || catName.equals("pverb")) {
			if (getWord(2) != null) {
				lexRules.add(new LexicalRule("pverb", getWord(2)));
			}
		}
	}

}
