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

package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * This class represents a grammar that is needed to run the chart parser. A grammar can be created
 * either directly in Java or on the basis of a file in the Codeco notation. See the package
 * description of {@link ch.uzh.ifi.attempto.codeco} for more information about the Codeco notation.
 *<p>
 * For performance and simplicity reasons, the methods of this class return internal lists and not
 * copies thereof. However, these lists should not be changed outside this class.
 * 
 * @author Tobias Kuhn
 */
public class Grammar {
	
	private List<GrammarRule> rules = new ArrayList<GrammarRule>();
	private List<LexicalRule> lexRules = new ArrayList<LexicalRule>();
	private Map<String, List<GrammarRule>> rulesByHeadName = new HashMap<String, List<GrammarRule>>();
	private Map<String, List<LexicalRule>> lexRulesByCatName = new HashMap<String, List<LexicalRule>>();
	private Map<String, List<LexicalRule>> lexRulesByWord = new HashMap<String, List<LexicalRule>>();
	
	private final TreeSet<String> usedFeatureNames = new TreeSet<String>();
	
	/**
	 * Creates an empty grammar.
	 */
	public Grammar() {
	}
	
	/**
	 * Adds a grammar rule.
	 * 
	 * @param rule The grammar rule to be added.
	 */
	public void addGrammarRule(GrammarRule rule) {
		rules.add(rule);
		getRulesByHeadName(rule.getHead().getName()).add(rule);
		collectFeatureNames(rule.getHead());
		for (Category c : rule.getBody()) {
			collectFeatureNames(c);
		}
	}
	
	/**
	 * Adds a lexical rule. Lexical rules could also be called "lexicon entries".
	 * 
	 * @param lexRule The lexical rule to be added.
	 */
	public void addLexicalRule(LexicalRule lexRule) {
		lexRules.add(lexRule);
		getLexRulesByCatName(lexRule.getCategory().getName()).add(lexRule);
		getLexRulesByWord(lexRule.getWord().getName()).add(lexRule);
		collectFeatureNames(lexRule.getCategory());
	}
	
	/**
	 * Returns the grammar rules with a head category of the specified name.
	 * 
	 * @param name The name of the head category.
	 * @return A list of grammar rules.
	 */
	public List<GrammarRule> getRulesByHeadName(String name) {
		List<GrammarRule> l = rulesByHeadName.get(name);
		if (l == null) {
			l = new ArrayList<GrammarRule>();
			rulesByHeadName.put(name, l);
		}
		return l;
	}
	
	/**
	 * Returns the lexical rules with a pre-terminal category of the specified name.
	 * 
	 * @param name The name of the pre-terminal category.
	 * @return A list of lexical rules.
	 */
	public List<LexicalRule> getLexRulesByCatName(String name) {
		List<LexicalRule> l = lexRulesByCatName.get(name);
		if (l == null) {
			l = new ArrayList<LexicalRule>();
			lexRulesByCatName.put(name, l);
		}
		return l;
	}
	
	/**
	 * Returns the lexical rules for the specified word. The word corresponds to a terminal
	 * category.
	 * 
	 * @param word The word.
	 * @return A list of lexical rules.
	 */
	public List<LexicalRule> getLexRulesByWord(String word) {
		List<LexicalRule> l = lexRulesByWord.get(word);
		if (l == null) {
			l = new ArrayList<LexicalRule>();
			lexRulesByWord.put(word, l);
		}
		return l;
	}
	
	/**
	 * Returns all names of features used in feature structures of categories contained in this
	 * grammar. The list contains no duplicates and the elements are sorted alphabetically.
	 * 
	 * @return All used feature names in alphabetical order.
	 */
	public String[] getUsedFeatureNames() {
		return usedFeatureNames.toArray(new String[]{});
	}
	
	private void collectFeatureNames(Category c) {
		Set<String> fnames = c.getFeatureNames();
		if (fnames != null) usedFeatureNames.addAll(fnames);
	}
	
	public String toString() {
		String s = "";
		for (GrammarRule r : rules) {
			s += r + "\n";
		}
		s += "\n";
		for (LexicalRule le : lexRules) {
			s += le + "\n";
		}
		return s;
	}

}
