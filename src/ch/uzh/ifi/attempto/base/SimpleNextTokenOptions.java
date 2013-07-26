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

package ch.uzh.ifi.attempto.base;

import java.util.HashSet;
import java.util.Set;

/**
 * This is a simple implementation of NextTokenOptions.
 * 
 * @author Tobias Kuhn
 */
public class SimpleNextTokenOptions implements NextTokenOptions {

	private Set<ConcreteOption> cOptions = new HashSet<ConcreteOption>();
	private Set<AbstractOption> aOptions = new HashSet<AbstractOption>();
	private Set<String> tokens = new HashSet<String>();
	private Set<String> categories = new HashSet<String>();
	
	/**
	 * Creates a new object for the given concrete and abstract options.
	 * 
	 * @param cOptions The concrete options.
	 * @param aOptions The abstract options.
	 */
	public SimpleNextTokenOptions(Set<ConcreteOption> cOptions, Set<AbstractOption> aOptions) {
		if (cOptions != null) {
			this.cOptions = cOptions;
			for (ConcreteOption o : cOptions) {
				tokens.add(o.getWord());
			}
		}
		if (aOptions != null) {
			this.aOptions = aOptions;
			for (AbstractOption o : aOptions) {
				categories.add(o.getCategoryName());
			}
		}
	}
	
	/**
	 * Creates a new object for the given concrete, without any abstract options.
	 * 
	 * @param cOptions The concrete options.
	 */
	public SimpleNextTokenOptions(Set<ConcreteOption> cOptions) {
		this(cOptions, null);
	}

	public Set<AbstractOption> getAbstractOptions() {
		return aOptions;
	}

	public Set<ConcreteOption> getConcreteOptions() {
		return cOptions;
	}

	public boolean containsToken(String token) {
		return tokens.contains(token);
	}

	public boolean containsCategory(String categoryName) {
		return categories.contains(categoryName);
	}

}
