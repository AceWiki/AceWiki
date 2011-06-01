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

import ch.uzh.ifi.attempto.acewiki.core.AbstractLexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;

/**
 * This class represents a form to create or modify of-constructs.
 * 
 * @author Tobias Kuhn
 */
public class NounOfChanger extends AbstractLexiconChanger {
	
	public NounOfChanger() {
		setTitle("Of-Construct");
		setDescription("Every of-construct represents a certain relation between things. " +
			"For example, the of-construct \"child of\" relates persons to their " +
			"parents. Every of-construct consists of a noun plus the preposition " +
			"\"of\".");
	}
	
	public List<LexiconDetail> getDetails(OntologyElement el) {
		OfRelation relation = (OfRelation) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				"noun",
				"examples: part, child, owner",
				relation.getPrettyNoun()
			));
		return l;
	}
	
	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		OfRelation relation = (OfRelation) el;
		String name = Ontology.normalize((String) newValues.get(0));
		if (name.toLowerCase().endsWith("_of")) {
			name = name.substring(0, name.length()-3);
		}
		String nameP = name.replace("_", " ");
		
		if (name.equals("")) {
			throw new InvalidWordException("No noun defined: Please specify the singular form " +
				"of a noun.");
		}
		if (!Ontology.isValidWordOrEmpty(name)) {
			throw new InvalidWordException("Invalid character: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		if (FunctionWords.isFunctionWord(name)) {
			throw new InvalidWordException("'" + nameP + "' is a predefined word and cannot be " +
				"used here.");
		}
		OntologyElement oe = ontology.getElement(name + " of");
		if (oe != null && oe != relation) {
			throw new InvalidWordException("The word '" + nameP + "' is already used. Please " +
				"use a different one.");
		}
		relation.setWords(name);
	}

}
