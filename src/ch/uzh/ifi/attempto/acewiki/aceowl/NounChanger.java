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
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;

/**
 * This class is used to modify or create nouns.
 * 
 * @author Tobias Kuhn
 */
public class NounChanger implements LexiconChanger {
	
	public String getDescription() {
		return "Every noun represents a certain type of things. " +
			"For example, the noun \"city\" stands for all things that are cities.";
	}
	
	public List<LexiconDetail> getDetails(OntologyElement el) {
		NounConcept concept = (NounConcept) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				"singular",
				"examples: woman, city, process",
				concept.getPrettyWord(0)
			));
		l.add(new LexiconDetail(
				"plural",
				"examples: women, cities, processes",
				concept.getPrettyWord(1)
			));
		return l;
	}
	
	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		NounConcept concept = (NounConcept) el;
		
		String singular = Ontology.normalize((String) newValues.get(0));
		String plural = Ontology.normalize((String) newValues.get(1));
		String singularP = singular.replace("_", " ");
		String pluralP = plural.replace("_", " ");
		
		if (singular.equals(plural)) {
			throw new InvalidWordException("Singular and plural form have to be distinct.");
		}
		if (singular.equals("")) {
			throw new InvalidWordException("No singular form defined: Please specify the " +
				"singular form.");
		}
		if (!Ontology.isValidWordOrEmpty(singular)) {
			throw new InvalidWordException("Invalid character: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		if (FunctionWords.isFunctionWord(singular)) {
			throw new InvalidWordException("'" + singularP + "' is a predefined word and cannot " +
				"be used here.");
		}
		OntologyElement oe = ontology.getElement(singular);
		if (oe != null && oe != concept) {
			throw new InvalidWordException("The word '" + singularP + "' is already used. " +
				"Please use a different one.");
		}
		if (plural.equals("")) {
			throw new InvalidWordException("No plural form defined: Please specify the plural " +
				"form.");
		}
		if (!Ontology.isValidWordOrEmpty(plural)) {
			throw new InvalidWordException("Invalid character: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		if (FunctionWords.isFunctionWord(plural)) {
			throw new InvalidWordException("'" + pluralP + "' is a predefined word and cannot " +
				"be used here.");
		}
		oe = ontology.getElement(plural);
		if (oe != null && oe != concept) {
			throw new InvalidWordException("The word '" + pluralP + "' is already used. Please " +
				"use a different one.");
		}
		concept.setWords(singular, plural);
	}

}
