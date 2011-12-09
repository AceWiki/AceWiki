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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This class represents a detail of a sentence like a syntax tree or a logical formula.
 * 
 * @author Tobias Kuhn
 */
public class SentenceDetail {
	
	private final String name;
	private final String richText;
	
	/**
	 * Creates a new sentence detail.
	 * 
	 * @param name The name of the detail, as shown to the user.
	 * @param richText The content of the detail, possibly containing HTML tags.
	 */
	public SentenceDetail(String name, String richText) {
		this.name = name;
		this.richText = richText;
	}
	
	/**
	 * Returns the name of the sentence detail, as shown to the user.
	 * 
	 * @return The name.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns the content of the sentence detail. It can be plain text or contain HTML tags.
	 * 
	 * @return The content as rich text.
	 */
	public String getRichText() {
		return richText;
	}

}
