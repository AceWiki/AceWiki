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

package ch.uzh.ifi.attempto.base;

/**
 * This class represents an option (in an abstract way) how a partial sentence can be continued.
 * Such an abstract option must at least have a category name.
 * 
 * @see NextTokenOptions
 * @see ConcreteOption
 * @author Tobias Kuhn
 */
public interface AbstractOption {

	/**
	 * Returns the name of the category that represents possible next tokens.
	 * 
	 * @return The category name.
	 */
	public String getCategoryName();
	
}
