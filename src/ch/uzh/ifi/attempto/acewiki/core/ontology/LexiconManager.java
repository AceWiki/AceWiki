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
import java.util.Collection;

import ch.uzh.ifi.attempto.chartparser.DynamicLexicon;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class manages the lexical entries for AceWiki.
 * 
 * @author Tobias Kuhn
 */
public class LexiconManager implements DynamicLexicon {
	
	private Ontology ontology;
	
	/**
	 * Creates a new lexicon manager for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	public LexiconManager(Ontology ontology) {
		this.ontology = ontology;
	}

	public Collection<LexicalRule> getLexRulesByCatName(String catName) {
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
				el.collectLexicalRules(catName, lexRules);
			}
		}
		for (LexicalRule r : lexRules) {
			r.getCategory().setFeature("text", r.getWord().getName());
		}
		return lexRules;
	}

	public Collection<LexicalRule> getLexRulesByWord(String word) {
		Collection<LexicalRule> lexRules = new ArrayList<LexicalRule>();
		if (word.matches("[XYZ][0-9]*")) {
			lexRules.add(new LexicalRule("variable", word));
			lexRules.add(new LexicalRule("reference", word));
		} else if (word.matches("[0-9]+")) {
			lexRules.add(new LexicalRule("number", word));
		} else {
			OntologyElement oe = ontology.get(word);
			if (word.startsWith("the ")) {
				oe = ontology.get(word.substring(4));
			}
			if (oe != null) {
				oe.collectLexicalRules(null, lexRules);
			}
		}
		for (LexicalRule r : lexRules) {
			r.getCategory().setFeature("text", r.getWord().getName());
		}
		return lexRules;
	}
	
	private static void addVariableEntries(Collection<LexicalRule> entries, String cat) {
		for (String s : new String[] {"X", "Y", "Z"}) {
			for (int i = 0 ; i <= 3 ; i++) {
				String t = s;
				if (i != 0) t += i;
				entries.add(new LexicalRule(cat, t));
			}
		}
	}

}
