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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * This class represents a set of options that describe how a partial text can be continued
 * according to a given grammar. The possible next tokens are represented in an abstract way (on
 * the basis of abstract options) and also in a concrete way (on the basis of concrete options).
 * 
 * @see AbstractOption
 * @see ConcreteOption
 * @author Tobias Kuhn
 */
public class NextTokenOptions {
	
	private Set<AbstractOption> aOptions;
	private Set<ConcreteOption> cOptions;
	private Map<String,Set<AbstractOption>> preterminalMap;
	private Set<String> terminalNames;
	
	/**
	 * Generates a new object with the given abstract and concrete options.
	 * 
	 * @param aOptions The set of abstract options.
	 * @param cOptions The set of concrete options.
	 */
	NextTokenOptions(Set<AbstractOption> aOptions, Set<ConcreteOption> cOptions) {
		this.aOptions = aOptions;
		this.cOptions = cOptions;
	}
	
	/**
	 * Returns the abstract options.
	 * 
	 * @return The set of abstract options.
	 */
	public Set<AbstractOption> getAbstractOptions() {
		return aOptions;
	}
	
	/**
	 * Returns the abstract options that have a pre-terminal category with the specified name.
	 * 
	 * @param preterminalName The name of the pre-terminal category.
	 * @return The set of abstract options with the respective pre-terminal name.
	 */
	public Set<AbstractOption> getAbstractOptions(String preterminalName) {
		createPreterminalsCache();
		Set<AbstractOption> s = preterminalMap.get(preterminalName);
		if (s == null) {
			s = new HashSet<AbstractOption>();
			preterminalMap.put(preterminalName, s);
		}
		return s;
	}
	
	/**
	 * Returns the concrete options.
	 * 
	 * @return The set of concrete options.
	 */
	public Set<ConcreteOption> getConcreteOptions() {
		return cOptions;
	}
	
	/**
	 * Returns true if the specified terminal category is a possible next token.
	 * 
	 * @param terminalName The name of the terminal category.
	 * @return true if it is a possible next token.
	 */
	public boolean containsTerminal(String terminalName) {
		createTerminalsCache();
		return terminalNames.contains(terminalName);
	}
	
	/**
	 * Returns true if the specifed pre-terminal category represents a possible next token.
	 * 
	 * @param preterminalName The name of the pre-terminal category.
	 * @return true if it represents a possible next token.
	 */
	public boolean containsPreterminal(String preterminalName) {
		createPreterminalsCache();
		return preterminalMap.containsKey(preterminalName);
	}
	
	/**
	 * Returns true if the given pre-terminal category represents a possible next token.
	 * 
	 * @param c The pre-terminal category.
	 * @return true if it represents a possible next token.
	 */
	public boolean allowsForCategory(Preterminal c) {
		createPreterminalsCache();
		if (!preterminalMap.containsKey(c.getName())) return false;
		for (AbstractOption o : getAbstractOptions(c.getName())) {
			if (o.isFulfilledBy(c)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns the terminal categories of all concrete options.
	 * 
	 * @return A set of terminal categories.
	 */
	public Set<String> getTerminals() {
		createTerminalsCache();
		return terminalNames;
	}
	
	/**
	 * Returns the pre-terminal category names of all abstract options.
	 * 
	 * @return A set of pre-terminal category names.
	 */
	public Set<String> getPreterminals() {
		createPreterminalsCache();
		return preterminalMap.keySet();
	}
	
	private void createTerminalsCache() {
		if (terminalNames != null) return;
		
		terminalNames = new HashSet<String>();
		for (ConcreteOption o : cOptions) {
			terminalNames.add(o.getWord());
		}
	}
	
	private void createPreterminalsCache() {
		if (preterminalMap != null) return;
		
		preterminalMap = new HashMap<String, Set<AbstractOption>>();
		for (AbstractOption o : aOptions) {
			if (o.getCategory() instanceof Preterminal) {
				String n = o.getCategory().getName();
				Set<AbstractOption> s = preterminalMap.get(n);
				if (s == null) {
					s = new HashSet<AbstractOption>();
					preterminalMap.put(n, s);
				}
				s.add(o);
			}
		}
	}

}
