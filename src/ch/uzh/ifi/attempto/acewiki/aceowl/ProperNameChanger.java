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
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;

/**
 * This class is used to modify or create proper names.
 * 
 * @author Tobias Kuhn
 */
public class ProperNameChanger implements LexiconChanger {
	
	public String getDescription() {
		return "Every proper name represents a certain individual. " +
			"The country \"Switzerland\" and the person \"Bob Dylan\" are typical " +
			"examples. Proper names can have an abbreviation that has the same meaning " +
			"as the longer proper name.";
	}
	
	public List<LexiconDetail> getDetails(OntologyElement el) {
		ProperNameIndividual ind = (ProperNameIndividual) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				"proper name",
				"examples: Switzerland, Bob Dylan, Nile, United Nations",
				ind.getWord(1)
			));
		l.add(new LexiconDetail(
				"... used with \"the\"",
				"examples: the Nile, the United Nations",
				ind.hasDefiniteArticle(0)
			));
		l.add(new LexiconDetail(
				"abbreviation",
				"example: HTML, UN",
				ind.getAbbreviation()
			));
		l.add(new LexiconDetail(
				"... used with \"the\"",
				"example: the UN",
				ind.hasDefiniteArticle(2)
			));
		return l;
	}
	
	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		ProperNameIndividual ind = (ProperNameIndividual) el;
		
		String name = ACEOWLLexicon.normalize((String) newValues.get(0));
		String abbrev = ACEOWLLexicon.normalize((String) newValues.get(2));
		boolean nameDefArt = (Boolean) newValues.get(1);
		boolean abbrevDefArt = (Boolean) newValues.get(3);
		
		if (name.toLowerCase().startsWith("the_")) {
			name = name.substring(4);
			nameDefArt = true;
		}
		if (abbrev.toLowerCase().startsWith("the_")) {
			abbrev = abbrev.substring(4);
			abbrevDefArt = true;
		}
		String nameP = LanguageUtils.getPrettyPrinted(name);
		String abbrevP = LanguageUtils.getPrettyPrinted(abbrev);
		
		if (name.equals("")) {
			throw new InvalidWordException("No proper name defined: Please specify a name.");
		}
		if (!ACEOWLLexicon.isValidWordOrEmpty(name) || !ACEOWLLexicon.isValidWordOrEmpty(abbrev)) {
			throw new InvalidWordException("Invalid character: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		if (FunctionWords.isFunctionWord(name)) {
			throw new InvalidWordException("'" + nameP + "' is a predefined word and cannot be " +
				"used here.");
		}
		if (FunctionWords.isFunctionWord(abbrev)) {
			throw new InvalidWordException("'" + abbrevP + "' is a predefined word and cannot " +
				"be used here.");
		}
		if (abbrev.length() >= name.length()) {
			throw new InvalidWordException("The abbreviation has to be shorter than the full " +
				"proper name.");
		}
		OntologyElement oe = ontology.getElement(name);
		if (oe != null && oe != ind) {
			throw new InvalidWordException("The word '" + nameP + "' is already used. Please " +
				"use a different one.");
		}
		oe = ontology.getElement(abbrev);
		if (oe != null && oe != ind) {
			throw new InvalidWordException("The word '" + abbrevP + "' is already used. Please " +
				"use a different one.");
		}
		String word = name;
		if (nameDefArt) {
			word = "the " + name;
		}
		String abbrevWord = abbrev;
		if (!abbrev.equals("") && abbrevDefArt) {
			abbrevWord = "the " + abbrev;
		}
		ontology.change(ind, word + ";" + name + ";" + abbrevWord + ";" + abbrev);
	}

}
