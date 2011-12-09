// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
 * This is a simple implementation of a concrete option.
 * 
 * @author Tobias Kuhn
 */
public class SimpleConcreteOption implements ConcreteOption {
	
	private String word;
	private String categoryName;
	
	/**
	 * Creates a new concrete option for the given word and category name.
	 * 
	 * @param word The word.
	 * @param categoryName The category name.
	 */
	public SimpleConcreteOption(String word, String categoryName) {
		this.word = word;
		this.categoryName = categoryName;
	}
	
	/**
	 * Creates a new concrete option for the given word, without a category.
	 * 
	 * @param word The word.
	 */
	public SimpleConcreteOption(String word) {
		this.word = word;
	}

	public String getWord() {
		return word;
	}

	public String getCategoryName() {
		return categoryName;
	}

}
