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

/**
 * This class represents an option (in a concrete way) how a partial sentence can be continued.
 * Such a concrete option contains the specific word and may contain its category.
 * 
 * @see AbstractOption
 * @see NextTokenOptions
 * @author Tobias Kuhn
 */
public interface ConcreteOption {

	/**
	 * Returns the word of this concrete option.
	 * 
	 * @return The word.
	 */
	public String getWord();

	/**
	 * Returns the name of the category of this concrete option, or null if there is no such
	 * category.
	 * 
	 * @return The category name.
	 */
	public String getCategoryName();
	
}
