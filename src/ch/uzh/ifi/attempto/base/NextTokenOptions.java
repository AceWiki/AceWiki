// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import java.util.Set;

/**
 * This class represents a set of options that describe how a partial text can be continued
 * according. The possible next tokens are represented in an abstract way (on the basis of abstract
 * options) and also in a concrete way (on the basis of concrete options).
 * 
 * @see AbstractOption
 * @see ConcreteOption
 * @author Tobias Kuhn
 */
public interface NextTokenOptions {

	/**
	 * Returns the abstract options.
	 * 
	 * @return The set of abstract options.
	 */
	public Set<? extends AbstractOption> getAbstractOptions();

	/**
	 * Returns the concrete options.
	 * 
	 * @return The set of concrete options.
	 */
	public Set<? extends ConcreteOption> getConcreteOptions();
	
	/**
	 * Returns true if the specified token is a possible next token.
	 * 
	 * @param token The token text.
	 * @return true if it is a possible next token.
	 */
	public boolean containsToken(String token);
	
	/**
	 * Returns true if the specifed category represents a possible next token.
	 * 
	 * @param categoryName The name of the category.
	 * @return true if it represents a possible next token.
	 */
	public boolean containsCategory(String categoryName);

}
