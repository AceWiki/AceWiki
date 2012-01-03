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

import java.util.List;

/**
 * This interface defines text operations that are needed by the predictive editor.
 * 
 * @author Tobias Kuhn
 */
public interface TextOperator {
	
	/**
	 * This method should split a text into its tokens.
	 * 
	 * @param text The input text.
	 * @return A list of strings representing the tokens.
	 */
	public List<String> splitIntoTokens(String text);
	
	/**
	 * This method checks the context of a text element and can do small surface adaptations of a
	 * token according to the surrounding text. E.g. in English "a" should become "an" in front
	 * of "apple".
	 * 
	 * @param textElement The text element whose text should be adapted to the context.
	 * @param preceding The preceding text.
	 * @param following The following text.
	 * @return The adapted text.
	 */
	public String getTextInContext(TextElement textElement, String preceding, String following);
	
	/**
	 * This method should create a text element for the given text.
	 * 
	 * @param text The text.
	 * @return The text element.
	 */
	// TODO This method should be able to return multiple possibilities.
	public TextElement createTextElement(String text);
	
	/**
	 * This method should return the "glue" to connect two text elements. Mostly, this should be a
	 * single blank space or the empty string.
	 * 
	 * @param left The left-hand side text element.
	 * @param right The right-hand side text element.
	 * @return The glue string.
	 */
	public String getGlue(TextElement left, TextElement right);

}
