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
 * This class describes a text element (a word or a phrase) to be used by the predictive editor.
 * Every text element contains a text and one or more grammatical categories. If a text element
 * contains more than one category then this means that it can stand for any of these categories.
 * 
 * @author Tobias Kuhn
 */
public class TextElement {
	
	private String text;
	private TextContainer textContainer;
	
	/**
	 * Creates a new text element.
	 * 
	 * @param text The text of the new text element.
	 */
	public TextElement(String text) {
		this.text = text;
	}
	
	/**
	 * Creates a new text element without initializing the fields of the object.
	 */
	protected TextElement() {
	}
	
	/**
	 * Returns the original text of this text element. This is the unchanged text before the
	 * context is checked by the text operator.
	 * 
	 * @return The original text (before context checking).
	 */
	public String getOriginalText() {
		return text;
	}
	
	/**
	 * Returns the text of this text element. The text might have been changed by the context
	 * checking of the text operator.
	 * 
	 * @return The text (after context checking).
	 */
	public String getText() {
		if (textContainer != null) {
			return textContainer.getTextElementText(this);
		}
		return getOriginalText();
	}
	
	/**
	 * Sets the text container that contains this text element.
	 * 
	 * @param textContainer The text container.
	 */
	void setTextContainer(TextContainer textContainer) {
		this.textContainer = textContainer;
	}
	
	/**
	 * Removes the text container.
	 */
	void removeTextContainer() {
		this.textContainer = null;
	}
	
	/**
	 * Two text elements are equals if they share the same text.
	 */
	public boolean equals(Object obj) {
		if (!(obj instanceof TextElement)) return false;
		TextElement other = (TextElement) obj;
		if (!toString().equals(other.toString())) return false;
		return true;
	}
	
	public String toString() {
		return text;
	}

}
