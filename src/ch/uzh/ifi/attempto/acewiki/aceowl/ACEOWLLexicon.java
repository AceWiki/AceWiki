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

import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.base.AbstractOption;
import ch.uzh.ifi.attempto.chartparser.DynamicLexicon;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class manages the lexical entries for AceWiki.
 * 
 * @author Tobias Kuhn
 */
public class ACEOWLLexicon implements DynamicLexicon {
	
	private Ontology ontology;
	
	/**
	 * Creates a new lexicon manager.
	 */
	public ACEOWLLexicon() {
	}

	/**
	 * This is the first method to be called and provides the ontology object.
	 * 
	 * @param ontology The ontology object.
	 */
	public void init(Ontology ontology) {
		this.ontology = ontology;
	}

	public Collection<LexicalRule> getLexRules(AbstractOption option) {
		String catName = option.getCategoryName();
		Collection<LexicalRule> lexRules = new ArrayList<LexicalRule>();
		if (catName.equals("variable")) {
			addVariableEntries(lexRules, "variable");
		} else if (catName.equals("reference")) {
			addVariableEntries(lexRules, "reference");
		} else if (catName.equals("number")) {
			for (int i = 2 ; i < 100 ; i++) {
				lexRules.add(new LexicalRule("number", i + ""));
			}
		} else {
			for (OntologyElement el : ontology.getOntologyElements()) {
				if (el instanceof ACEOWLOntoElement) {
					((ACEOWLOntoElement) el).collectLexicalRules(catName, lexRules);
				}
			}
		}
		for (LexicalRule r : lexRules) {
			r.getCategory().setFeature("text", r.getWord().getName());
		}
		return lexRules;
	}

	public Collection<LexicalRule> getLexRules(String word) {
		Collection<LexicalRule> lexRules = new ArrayList<LexicalRule>();
		if (word.matches("[XYZ][0-9]*")) {
			lexRules.add(new LexicalRule("variable", word));
			lexRules.add(new LexicalRule("reference", word));
		} else if (word.matches("[1-9][0-9]+|[2-9]")) {
			lexRules.add(new LexicalRule("number", word));
		} else {
			OntologyElement oe = ontology.getElement(word);
			if (word.startsWith("the ")) {
				oe = ontology.getElement(word.substring(4));
			}
			if (oe != null && oe instanceof ACEOWLOntoElement) {
				((ACEOWLOntoElement) oe).collectLexicalRules(null, lexRules);
			}
		}
		for (LexicalRule r : lexRules) {
			r.getCategory().setFeature("text", r.getWord().getName());
		}
		return lexRules;
	}
	
	private static void addVariableEntries(Collection<LexicalRule> entries, String cat) {
		for (String s : new String[] {"X", "Y", "Z"}) {
			String t = s;
			entries.add(new LexicalRule(cat, t));
		}
	}
	
	/**
	 * Returns true if the string represents a valid word form.
	 * 
	 * @param s The string.
	 * @return true if the string represents a valid word form.
	 */
	public static boolean isValidWordOrEmpty(String s) {
		return s.matches("([a-zA-Z][a-zA-Z0-9_-]*)?");
	}
	
	/**
	 * Normalizes the string. White space characters are replaced by underscores.
	 * 
	 * @param s The input string.
	 * @return The normalized string.
	 */
	public static String normalize(String s) {
		return s.replaceAll("(\\s|_)+", "_").replaceAll("(^_|_$)", "");
	}

}
