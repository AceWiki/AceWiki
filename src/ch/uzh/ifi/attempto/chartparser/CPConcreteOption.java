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

package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.base.ConcreteOption;

/**
 * This class represents a concrete option for the chart parser. Such a concrete option consists of
 * a word in the form of a terminal category standing for a possible next token, and optionally of
 * a pre-terminal category from which the terminal category was derived.
 * 
 * @author Tobias Kuhn
 */
public class CPConcreteOption implements ConcreteOption {
	
	private final Terminal terminal;
	private final Preterminal category;
	private final String identifier;
	
	CPConcreteOption(Grammar grammar, Terminal word, Preterminal category) {
		this.terminal = word;
		this.category = category;
		identifier = calculateIdentifier(grammar.getFeatureNamesArray());
	}
	
	CPConcreteOption(Grammar grammar, LexicalRule lexRule) {
		this(grammar, lexRule.getWord(), lexRule.getCategory());
	}
	
	public String getWord() {
		return terminal.getName();
	}
	
	/**
	 * Returns the pre-terminal category of this concrete option, or null if no pre-terminal
	 * category was involved.
	 * 
	 * @return The pre-terminal category.
	 */
	public Preterminal getCategory() {
		return category;
	}
	
	public String getCategoryName() {
		if (category == null) {
			return null;
		} else {
			return category.getName();
		}
	}
	
	String calculateIdentifier(String[] usedFeatureNames) {
		if (category == null) {
			return terminal + " <-";
		} else {
			List<Integer> vars = new ArrayList<Integer>();
			List<Integer> mvars = new ArrayList<Integer>();
			
			vars.clear();
			mvars.clear();
			category.collectVars(vars, mvars);
			return terminal + " <- " + category.getIdentifier(mvars, usedFeatureNames);
		}
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof CPConcreteOption)) return false;
		CPConcreteOption other = (CPConcreteOption) obj;
		return this.identifier.equals(other.identifier);
	}
	
	public int hashCode() {
		return identifier.hashCode();
	}
	
	public String toString() {
		if (category == null) {
			return terminal + " <-";
		} else {
			return terminal + " <- " + category;
		}
	}

}
