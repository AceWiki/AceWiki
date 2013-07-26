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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import ch.uzh.ifi.attempto.base.NextTokenOptions;

/**
 * This class represents a set of options that describe how a partial text can be continued
 * according to a given grammar, and contains specific code for its use in the chart parser.
 * 
 * @author Tobias Kuhn
 */
public class CPNextTokenOptions implements NextTokenOptions {
	
	private Set<CPAbstractOption> aOptions;
	private Set<CPConcreteOption> cOptions;
	private Map<String,Set<CPAbstractOption>> categoryMap;
	private Set<String> tokens;
	
	/**
	 * Generates a new object with the given abstract and concrete options.
	 * 
	 * @param aOptions The set of abstract options.
	 * @param cOptions The set of concrete options.
	 */
	CPNextTokenOptions(Set<CPAbstractOption> aOptions, Set<CPConcreteOption> cOptions) {
		this.aOptions = aOptions;
		this.cOptions = cOptions;
	}
	
	public Set<CPAbstractOption> getAbstractOptions() {
		return aOptions;
	}
	
	/**
	 * Returns the abstract options that have a pre-terminal category with the specified name.
	 * 
	 * @param categoryName The name of the category.
	 * @return The set of abstract options with the respective category name.
	 */
	public Set<CPAbstractOption> getAbstractOptions(String categoryName) {
		createCategoryCache();
		Set<CPAbstractOption> s = categoryMap.get(categoryName);
		if (s == null) {
			s = new HashSet<CPAbstractOption>();
			categoryMap.put(categoryName, s);
		}
		return s;
	}
	
	public Set<CPConcreteOption> getConcreteOptions() {
		return cOptions;
	}
	
	public boolean containsToken(String token) {
		createTokenCache();
		return tokens.contains(token);
	}
	
	public boolean containsCategory(String categoryName) {
		createCategoryCache();
		return categoryMap.containsKey(categoryName);
	}
	
	/**
	 * Returns true if the given pre-terminal category represents a possible next token.
	 * 
	 * @param c The pre-terminal category.
	 * @return true if it represents a possible next token.
	 */
	public boolean containsCategory(Preterminal c) {
		createCategoryCache();
		if (!categoryMap.containsKey(c.getName())) return false;
		for (CPAbstractOption o : getAbstractOptions(c.getName())) {
			if (o.isFulfilledBy(c)) {
				return true;
			}
		}
		return false;
	}
	
	private void createTokenCache() {
		if (tokens != null) return;
		
		tokens = new HashSet<String>();
		for (CPConcreteOption o : cOptions) {
			tokens.add(o.getWord());
		}
	}
	
	private void createCategoryCache() {
		if (categoryMap != null) return;
		
		categoryMap = new HashMap<String, Set<CPAbstractOption>>();
		for (CPAbstractOption o : aOptions) {
			if (o.getCategory() instanceof Preterminal) {
				String n = o.getCategory().getName();
				Set<CPAbstractOption> s = categoryMap.get(n);
				if (s == null) {
					s = new HashSet<CPAbstractOption>();
					categoryMap.put(n, s);
				}
				s.add(o);
			}
		}
	}

}
