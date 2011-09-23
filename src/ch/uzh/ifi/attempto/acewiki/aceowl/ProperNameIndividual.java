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

import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.owl.OWLIndividual;
import ch.uzh.ifi.attempto.ape.Gender;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class stands for individuals that are represented by ACE proper names and OWL individuals.
 * Proper names can be used either with a definite article (e.g. "the United Nations") or without
 * (e.g. "Switzerland"). Furthermore, proper names can have an abbreviation that is a shorter
 * name with exactly the same meaning. This abbreviation can aswell be used either with a definite
 * article (e.g. "the UN") or without (e.g. "ACE").
 *<p>
 * Proper names have four word forms. The first one is the proper name with the definite
 * article or just the proper name if no definite article is used for this proper name. The second
 * one is in each case just the bare proper name. The third form is the abbreviation with the
 * definite article if there is one. The fourth form, finally, is just the bare abbreviation. If
 * there is no abbreviation then the third and fourth form are identical to the first and second
 * form, respectively. For proper names that do not use a definite article and that have no
 * abbreviation, all four forms are identical.
 *<p>
 * 0: proper name, preceded by "the" if used with definite article.
 * 1: bare proper name.
 * 2: abbreviation, preceded by "the" if used with definite article; or the same as 0 if there is
 *    no abbreviation.
 * 3: bare abbreviation; or the same as 1 if there is no abbreviation.
 *<p>
 * Examples: ["the United Nations", "United Nations", "the UN", "UN"];
 *           ["the Nile", "Nile", "Nile", "Nile];
 *           ["Switzerland", "Switzerland", "Switzerland", "Switzerland"];
 *           ["Attempto Controlled English", "Attempto Controlled English", "ACE", "ACE"].
 * 
 * @author Tobias Kuhn
 */
public class ProperNameIndividual extends OWLIndividual implements ACEOWLOntoElement {
	
	private String word, abbrev;
	private boolean wordDefArt, abbrevDefArt;
	
	/**
	 * Creates a new individual that has no name yet and is not registered to an ontology.
	 */
	public ProperNameIndividual() {
		this.word = "";
		this.abbrev = null;
		this.wordDefArt = false;
		this.abbrevDefArt = false;
	}
	
	public String[] getWords() {
		if (abbrev == null) {
			if (wordDefArt) {
				return new String[] {"the " + word, word, "the " + word, word};
			} else {
				return new String[] {word, word, word, word};
			}
		} else {
			if (wordDefArt) {
				if (abbrevDefArt) {
					return new String[] {"the " + word, word, "the " + abbrev, abbrev};
				} else {
					return new String[] {"the " + word, word, abbrev, abbrev};
				}
			} else {
				if (abbrevDefArt) {
					return new String[] {word, word, "the " + abbrev, abbrev};
				} else {
					return new String[] {word, word, abbrev, abbrev};
				}
			}
		}
	}
	
	public String[] getHeadwords() {
		if (abbrev == null) {
			return new String[] {getWord(1)};
		} else {
			return new String[] {getWord(1), getWord(3)};
		}
	}
	
	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		if (words.length == 1) {
			word = words[0];
			wordDefArt = false;
			abbrev = null;
			abbrevDefArt = false;
		} else if (words.length == 2) {
			word = words[1];
			wordDefArt = words[0].startsWith("the ");
			abbrev = null;
			abbrevDefArt = false;
		} else if (words[2] == null || words[0].equals(words[2])) {
			word = words[1];
			wordDefArt = words[0].startsWith("the ");
			abbrev = null;
			abbrevDefArt = false;
		} else {
			word = words[1];
			wordDefArt = words[0].startsWith("the ");
			abbrev = words[3];
			abbrevDefArt = words[2].startsWith("the ");
		}
		if (abbrev != null && abbrev.length() == 0) {
			abbrev = null;
		}
	}
	
	public List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		if (wordDefArt) {
			entries.add(LexiconEntry.createPropernameDefSgEntry(word, word, Gender.UNDEF));
		} else {
			entries.add(LexiconEntry.createPropernameSgEntry(word, word, Gender.UNDEF));
		}
		if (abbrev != null) {
			if (abbrevDefArt) {
				entries.add(LexiconEntry.createPropernameDefSgEntry(abbrev, word, Gender.UNDEF));
			} else {
				entries.add(LexiconEntry.createPropernameSgEntry(abbrev, word, Gender.UNDEF));
			}
		}
		return entries;
	}
	
	public String getType() {
		return "Proper Name";
	}
	
	public String getInternalType() {
		return "propername";
	}
	
	/**
	 * Returns true if the proper name has to be used with the definite article "the".
	 * 
	 * @return true if the definite article "the" has to be used.
	 */
	public boolean hasDefiniteArticle() {
		return wordDefArt;
	}
	
	/**
	 * Returns true if the given word form uses the definite article "the". This returns
	 * always false for 1 and 3.
	 * 
	 * @param wordNumber the word number
	 * @return true if the definite article "the" is used for the word form of the
	 *         given word number
	 */
	public boolean hasDefiniteArticle(int wordNumber) {
		if (wordNumber == 0) {
			return wordDefArt;
		} else if (wordNumber == 2) {
			return abbrevDefArt;
		} else {
			return false;
		}
	}
	
	/**
	 * Returns the abbreviation (without definite article) or null if there is no abbreviation.
	 * 
	 * @return the abbreviation
	 */
	public String getAbbreviation() {
		if (abbrev == null) return null;
		return abbrev;
	}
	
	public String getIRISuffix() {
		return word;
	}
	
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("propername")) {
			lexRules.add(new LexicalRule("propername", getWord(0)));
			if (getAbbreviation() != null) {
				lexRules.add(new LexicalRule("propername", getWord(2)));
			}
		}
	}
	
	public TextContainer getAnswerText() {
		return new TextContainer(new OntologyTextElement(this, 1));
	}

}
