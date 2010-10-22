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
 * 
 * @author Tobias Kuhn
 */
public class Grammar {
	
	private List<GrammarRule> rules = new ArrayList<GrammarRule>();
	private List<LexicalRule> lexRules = new ArrayList<LexicalRule>();
	private Map<String, List<GrammarRule>> rulesByHeadName = new HashMap<String, List<GrammarRule>>();
	private Map<String, List<LexicalRule>> lexRulesByCat = new HashMap<String, List<LexicalRule>>();
	private Map<String, List<LexicalRule>> lexRulesByWord = new HashMap<String, List<LexicalRule>>();
	private Set<String> terminalSymbols = new TreeSet<String>();
	private Set<String> preterminalSymbols = new TreeSet<String>();
	private Set<String> nonterminalSymbols = new TreeSet<String>();
	private Set<String> featureNames = new TreeSet<String>();
	
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
		rulesByHeadName(rule.getHead().getName()).add(rule);
		processCategory(rule.getHead());
		for (Category c : rule.getBody()) {
			processCategory(c);
		}
	}
	
	/**
	 * Adds a lexical rule. Lexical rules could also be called "lexicon entries".
	 * 
	 * @param lexRule The lexical rule to be added.
	 */
	public void addLexicalRule(LexicalRule lexRule) {
		lexRules.add(lexRule);
		lexRulesByCat(lexRule.getCategory().getName()).add(lexRule);
		lexRulesByWord(lexRule.getWord().getName()).add(lexRule);
		processCategory(lexRule.getWord());
		processCategory(lexRule.getCategory());
	}
	
	/**
	 * Returns the internal list of grammar rules with a head category of the specified name.
	 * 
	 * @param name The name of the head category.
	 * @return The internal list of grammar rules.
	 */
	List<GrammarRule> rulesByHeadName(String name) {
		List<GrammarRule> l = rulesByHeadName.get(name);
		if (l == null) {
			l = new ArrayList<GrammarRule>();
			rulesByHeadName.put(name, l);
		}
		return l;
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
	 * Returns the internal list of lexical rules with a pre-terminal category of the specified
	 * name.
	 * 
	 * @param categoryName The name of the pre-terminal category.
	 * @return The internal list of lexical rules.
	 */
	List<LexicalRule> lexRulesByCat(String categoryName) {
		List<LexicalRule> l = lexRulesByCat.get(categoryName);
		if (l == null) {
			l = new ArrayList<LexicalRule>();
			lexRulesByCat.put(categoryName, l);
		}
		return l;
	}

	/**
	 * Returns the list of lexical rules with a pre-terminal category of the specified name.
	 * 
	 * @param categoryName The name of the pre-terminal category.
	 * @return A list of lexical rules.
	 */
	public List<LexicalRule> getLexicalRulesByCategory(String categoryName) {
		return new ArrayList<LexicalRule>(lexRulesByCat(categoryName));
	}
	
	/**
	 * Returns the internal list of lexical rules for the specified word. The word corresponds to a
	 * terminal category.
	 * 
	 * @param word The word.
	 * @return The internal list of lexical rules.
	 */
	List<LexicalRule> lexRulesByWord(String word) {
		List<LexicalRule> l = lexRulesByWord.get(word);
		if (l == null) {
			l = new ArrayList<LexicalRule>();
			lexRulesByWord.put(word, l);
		}
		return l;
	}

	/**
	 * Returns the list of lexical rules for the specified word. The word corresponds to a terminal
	 * category.
	 * 
	 * @param word The word.
	 * @return A list of lexical rules.
	 */
	public List<LexicalRule> getLexicalRulesByWord(String word) {
		return new ArrayList<LexicalRule>(lexRulesByWord(word));
	}
	
	/**
	 * Returns an array of all names of features used in feature structures of categories contained
	 * in this grammar. The list contains no duplicates and the elements are sorted alphabetically.
	 * 
	 * @return An array of all used feature names in alphabetical order.
	 */
	String[] getFeatureNamesArray() {
		return featureNames.toArray(new String[]{});
	}
	
	/**
	 * Returns a set of all names of features used in feature structures of categories contained in
	 * this grammar.
	 * 
	 * @return A set of all used feature names.
	 */
	public Set<String> getFeatureNames() {
		return new TreeSet<String>(featureNames);
	}
	
	/**
	 * Returns whether the given feature name is used in this grammar.
	 * 
	 * @param featureName The feature name.
	 * @return true if the feature name is used.
	 */
	public boolean containsFeatureName(String featureName) {
		return featureNames.contains(featureName);
	}
	
	/**
	 * Returns a set of all terminal symbols used in this grammar.
	 * 
	 * @return A set of all terminal symbols.
	 */
	public Set<String> getTerminalSymbols() {
		return new TreeSet<String>(terminalSymbols);
	}
	
	/**
	 * Returns whether the given terminal symbol is used in this grammar.
	 * 
	 * @param terminalSymbol The terminal symbol.
	 * @return true if the symbol is used.
	 */
	public boolean containsTerminalSymbol(String terminalSymbol) {
		return terminalSymbols.contains(terminalSymbol);
	}

	/**
	 * Returns a set of all preterminal symbols used in this grammar.
	 * 
	 * @return A set of all preterminal symbols.
	 */
	public Set<String> getPreterminalSymbols() {
		return new TreeSet<String>(preterminalSymbols);
	}

	/**
	 * Returns whether the given preterminal symbol is used in this grammar.
	 * 
	 * @param preterminalSymbol The preterminal symbol.
	 * @return true if the symbol is used.
	 */
	public boolean containsPreterminalSymbol(String preterminalSymbol) {
		return preterminalSymbols.contains(preterminalSymbol);
	}

	/**
	 * Returns a set of all nonterminal symbols used in this grammar.
	 * 
	 * @return A set of all nonterminal symbols.
	 */
	public Set<String> getNonterminalSymbols() {
		return new TreeSet<String>(nonterminalSymbols);
	}
	
	/**
	 * Returns whether the given nonterminal symbol is used in this grammar.
	 * 
	 * @param nonterminalSymbol The nonterminal symbol.
	 * @return true if the symbol is used.
	 */
	public boolean containsNonterminalSymbol(String nonterminalSymbol) {
		return nonterminalSymbols.contains(nonterminalSymbol);
	}
	
	/**
	 * This is an auxiliary method for grammar classes that are automatically generated out of a
	 * Codeco representation. It sets a feature of a feature map to a certain unbound variable.
	 * 
	 * @param fm The feature map for which a feature should be set.
	 * @param featureName The name of the feature to be set.
	 * @param varID The identifier of the unbound variable to which the feature should be set.
	 * @param featureHash A hash map with variable identiers as keys and the string reference
	 *   objects that represent the respective variables as values.
	 */
	protected static void setFeature(FeatureMap fm, String featureName, int varID,
			HashMap<Integer, StringRef> featureHash) {
		if (featureHash.get(varID) == null) {
			StringRef stringRef = new StringRef();
			fm.setFeature(featureName, stringRef);
			featureHash.put(varID, stringRef);
		} else {
			fm.setFeature(featureName, featureHash.get(varID));
		}
	}
	
	/**
	 * This is an auxiliary method for grammar classes that are automatically generated out of a
	 * Codeco representation. It returns a string reference object that represents a certain
	 * unbound variable.
	 * 
	 * @param varID The identifier of the unbound variable for which a string reference object
	 *   should be returned.
	 * @param featureHash A hash map with variable identiers as keys and the string reference
	 *   objects that represent the respective variables as values.
	 * @return A string reference object.
	 */
	protected static StringRef getStringRef(int varID, HashMap<Integer, StringRef> featureHash) {
		StringRef stringRef = featureHash.get(varID);
		if (stringRef == null) {
			stringRef = new StringRef();
			featureHash.put(varID, stringRef);
		}
		return stringRef;
	}
	
	private void processCategory(Category c) {
		if (c instanceof Terminal) {
			terminalSymbols.add(c.getName());
		} else if (c instanceof Preterminal) {
			preterminalSymbols.add(c.getName());
		} else if (c instanceof Nonterminal) {
			nonterminalSymbols.add(c.getName());
		}
		Set<String> fnames = c.getFeatureNames();
		if (fnames != null) featureNames.addAll(fnames);
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
