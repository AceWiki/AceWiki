// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import ch.uzh.ifi.attempto.base.AbstractOption;

/**
 * This class represents an abstract option for the chart parser. Such an abstract option consists
 * of a category that represents possible next tokens and of a set of zero or more exceptions, also
 * in the form of categories. The categories can be terminal or pre-terminal, but not non-terminal.
 * 
 * @author Tobias Kuhn
 */
public class CPAbstractOption implements AbstractOption {
	
	private final Category category;
	private final Category[] exceptions;
	private final String identifier;
	
	CPAbstractOption(Grammar grammar, Category category, Category[] exceptions) {
		this.category = category;
		if (exceptions != null) {
			this.exceptions = exceptions;
		} else {
			this.exceptions = new Category[] {};
		}
		identifier = calculateIdentifier(grammar.getFeatureNamesArray());
	}
	
	CPAbstractOption(Grammar grammar, Category category, List<Category> exceptions) {
		this(grammar, category, exceptions.toArray(new Category[] {}));
	}
	
	CPAbstractOption(Grammar grammar, Category category) {
		this(grammar, category, (Category[]) null);
	}
	
	/**
	 * Returns the terminal or pre-terminal category that represents possible next tokens.
	 * 
	 * @return The terminal or pre-terminal category.
	 */
	public Category getCategory() {
		return category;
	}
	
	public String getCategoryName() {
		return category.getName();
	}
	
	/**
	 * This method returns the exceptions.
	 * 
	 * @return The exceptions in the form of categories.
	 */
	public Category[] getExceptions() {
		return exceptions;
	}
	
	/**
	 * Returns true if the given category is fulfilled by this option.
	 * 
	 * @param c The category.
	 * @return true if it is fulfilled by this option.
	 */
	public boolean isFulfilledBy(Category c) {
		if (!category.subsumes(c)) return false;
		if (exceptions == null) return true;
		for (Category x : exceptions) {
			if (x.subsumes(c)) return false;
		}
		return true;
	}
	
	String calculateIdentifier(String[] usedFeatureNames) {
		List<Integer> vars = new ArrayList<Integer>();
		List<Integer> mvars = new ArrayList<Integer>();
		String id = "";
		
		vars.clear();
		mvars.clear();
		category.collectVars(vars, mvars);
		id += category.getIdentifier(mvars, usedFeatureNames) + " / ";
		
		if (exceptions != null) {
			for (Category x : exceptions) {
				vars.clear();
				mvars.clear();
				x.collectVars(vars, mvars);
				id += x.getIdentifier(mvars, usedFeatureNames) + " ";
			}
		}
		return id;
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof CPAbstractOption)) return false;
		CPAbstractOption other = (CPAbstractOption) obj;
		return this.identifier.equals(other.identifier);
	}
	
	public int hashCode() {
		return identifier.hashCode();
	}
	
	public String toString() {
		String s = "";
		s += category + "";
		if (exceptions.length > 0) {
			s += " except";
			for (Category x : exceptions) {
				s += " " + x;
			}
		}
		return s;
	}

}
