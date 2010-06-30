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

import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class stands for roles that are represented by a transitive verb. Transitive
 * verbs have three word forms: a third singular form, a bare infinitive form ,
 * and a passive form. The bare infinitive form is used in the case of negation and plural.
 * The passive form always ends with the word "by".
 *<p>
 * 0: third singular form.
 * 1: bare infinitive form.
 * 2: passive form.
 *<p>
 * Examples: ["gives", "give", "given by"]; ["knows", "know", "known by"].
 * 
 * @author Tobias Kuhn
 */
public class VerbRole extends Role {
	
	private String thirdSg, inf, passive;
	
	/**
	 * Creates a new verb role.
	 */
	public VerbRole() {
	}
	
	public String[] getWords() {
		return new String[] {thirdSg, inf, passive};
	}
	
	protected void changeWords(String... words) {
		thirdSg = words[0];
		inf = words[1];
		if (words.length < 3 || words[2] == null) {
			passive = null;
		} else if (words[2].endsWith(" by")) {
			passive = words[2];
		} else {
			passive = words[2] + " by";
		}
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
	
	/**
	 * Returns the pretty-printed past participle. The pretty printing replaces
	 * underscores by blanks.
	 * 
	 * @return The pretty-printed past participle form.
	 * @see #getPastPart()
	 */
	public String getPrettyPastPart() {
		String s = getWord(2);
		if (s == null) {
			return null;
		} else if (s.endsWith(" by")) {
			return s.substring(0, s.length()-3);
		} else {
			throw new RuntimeException("Illegal passive word form: " + s);
		}
	}
	
	public String getURISuffix() {
		return getWord(1);
	}

	List<LexiconEntry> getLexiconEntries() {
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

}
