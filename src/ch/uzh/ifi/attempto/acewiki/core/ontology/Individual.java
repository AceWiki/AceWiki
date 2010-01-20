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
 * This class represents an individual (in logic called "constant"). AceWiki supports only
 * proper names to represent individuals (and no extensions are planned in this respect).
 * For that reason, this class is proper name specific.
 *<p>
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
public class Individual extends OntologyElement {
	
	private String word, abbrev;
	private boolean wordDefArt, abbrevDefArt;
	
	private List<Concept> conceptsCache;
	private long conceptsCacheStateID = -1;
	
	/**
	 * Creates a new individual that has no name yet and is not registered to an ontology.
	 */
	public Individual() {
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
	
	public String getHeadword() {
		if (abbrev == null) {
			return getPrettyWord(1);
		} else {
			return getPrettyWord(1) + " (" + getPrettyWord(3) + ")";
		}
	}
	
	public String[] getIndexEntries() {
		return new String[] {getHeadword(), getPrettyWord(3)};
	}
	
	protected void changeWords(String... words) {
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
	}

	List<LexiconEntry> getLexiconEntries() {
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
		return abbrev.replace("_", " ");
	}
	
	public String getURISuffix() {
		return "#" + word;
	}
	
	/**
	 * Calculates all concepts this individual belongs to.
	 * 
	 * @return A list of all concepts of this individual.
	 * @see Ontology#getConcepts(Individual)
	 */
	public synchronized List<Concept> getConcepts() {
		Ontology o = getOntology();
		if (conceptsCacheStateID != o.getStateID()) {
			conceptsCache = o.getConcepts(this);
			conceptsCacheStateID = o.getStateID();
		}
		return new ArrayList<Concept>(conceptsCache);
	}
	
	/**
	 * Returns the cached concepts or null if there are no cached concepts. The returned
	 * concepts might not be up-to-date.
	 * 
	 * @return A list of the cached concepts of this individual.
	 */
	public List<Concept> getCachedConcepts() {
		if (conceptsCache == null) return null;
		return new ArrayList<Concept>(conceptsCache);
	}

	/**
	 * Returns true if the concepts of this individual are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the concepts are cached.
	 */
	public boolean areConceptsCached() {
		return conceptsCacheStateID == getOntology().getStateID();
	}

}
