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
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class is used to modify or create verbs.
 * 
 * @author Tobias Kuhn
 */
public class VerbChanger implements LexiconChanger {
	
	public String getDescription() {
		return "Every verb represents a certain relation between things. " +
			"For example, the verb \"owns\" relates persons to their possessions.";
	}
	
	public List<LexiconDetail> getDetails(OntologyElement el) {
		VerbRelation relation = (VerbRelation) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				"third singular",
				"examples: owns, applies to, touches",
				relation.getWord(0)
			));
		l.add(new LexiconDetail(
				"bare infinitive",
				"examples: own, apply to, touch",
				relation.getWord(1)
			));
		l.add(new LexiconDetail(
				"past participle",
				"examples: owned, applied to, touched",
				relation.getPastPart(),
				false
			));
		return l;
	}
	
	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		VerbRelation relation = (VerbRelation) el;
		
		String thirdSg = ACEOWLLexicon.normalize((String) newValues.get(0));
		String inf = ACEOWLLexicon.normalize((String) newValues.get(1));
		String pastPart = ACEOWLLexicon.normalize((String) newValues.get(2));
		if (pastPart.toLowerCase().endsWith("_by")) {
			pastPart = pastPart.substring(0, pastPart.length()-3);
		}
		String thirdSgP = LanguageUtils.getPrettyPrinted(thirdSg);
		String infP = LanguageUtils.getPrettyPrinted(inf);
		String pastPartP = LanguageUtils.getPrettyPrinted(pastPart);
		
		// check whether all necessary fields are filled-in
		if (thirdSg.equals("")) {
			throw new InvalidWordException("No third singular defined: Please define the third " +
				"singular form.");
		}
		if (inf.equals("")) {
			throw new InvalidWordException("No bare infinitive defined: Please define the bare " +
				"infinitive form.");
		}
		if (pastPart.equals("") && wordNumber == 2) {
			throw new InvalidWordException("No past participle defined: Please define the past " +
				"participle form.");
		}
		if (pastPart.equals("") && isPassiveUsed(ontology, relation)) {
			throw new InvalidWordException("The past participle form cannot be removed because " +
				"there are sentences that are using it.");
		}
		
		// check whether the words contain only valid characters
		if (!ACEOWLLexicon.isValidWordOrEmpty(thirdSg) || !ACEOWLLexicon.isValidWordOrEmpty(inf) ||
				!ACEOWLLexicon.isValidWordOrEmpty(pastPart)) {
			throw new InvalidWordException("Invalid character used: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		
		// check whether a word is a predefined function word
		if (FunctionWords.isFunctionWord(thirdSg)) {
			throw new InvalidWordException("'" + thirdSgP + "' is a predefined word and cannot " +
				"be used here.");
		}
		if (FunctionWords.isFunctionWord(inf)) {
			throw new InvalidWordException("'" + infP + "' is a predefined word and cannot be " +
				"used here.");
		}
		if (FunctionWords.isFunctionWord(pastPart)) {
			throw new InvalidWordException("'" + pastPartP + "' is a predefined word and cannot " +
				"be used here.");
		}
		
		// check whether all word forms are distinct
		if (thirdSg.equals(inf)) {
			throw new InvalidWordException("The singular and plural forms have to be distinct.");
		}
		
		// check whether a word is already defined
		OntologyElement oe1 = ontology.getElement(thirdSg);
		if (oe1 != null && oe1 != relation) {
			throw new InvalidWordException("The word '" + thirdSgP + "' is already used. Please " +
				"use a different one.");
		}
		OntologyElement oe2 = ontology.getElement(inf);
		if (oe2 != null && oe2 != relation) {
			throw new InvalidWordException("The word '" + infP + "' is already used. Please use " +
				"a different one.");
		}
		OntologyElement oe3 = ontology.getElement(pastPart);
		if (oe3 != null && oe3 != relation) {
			throw new InvalidWordException("The word '" + pastPartP + "' is already used. " +
				"Please use a different one.");
		}
		
		ontology.change(relation, thirdSg + ";" + inf + ";" + pastPart);
	}
	
	private static boolean isPassiveUsed(Ontology o, VerbRelation vr) {
		for (Sentence sentence : o.getReferences(vr)) {
			ACESentence s = (ACESentence) sentence;
			for (TextElement t : s.getTextElements()) {
				if (t instanceof OntologyTextElement) {
					OntologyTextElement ot = (OntologyTextElement) t;
					if (vr == ot.getOntologyElement() && ot.getWordNumber() == 2) return true;
				}
			}
		}
		return false;
	}

}
