// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.preditor;

import java.util.HashSet;
import java.util.Set;

import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.chartparser.Terminal;

/**
 * This class describes a text element (a word or a phrase) to be used by the predictive editor.
 * Every text element contains a text and one or more grammatical categories. If a text element
 * contains more than one category then this means that it can stand for any of these categories.
 * 
 * @author Tobias Kuhn
 */
public class TextElement {
	
	private String text;
	private Set<Preterminal> categories;
	private TextContainer textContainer;
	
	/**
	 * Creates a new text element.
	 * 
	 * @param text The text of the new text element.
	 * @param category The category.
	 */
	public TextElement(String text, Preterminal category) {
		this.text = text;
		categories = new HashSet<Preterminal>();
		if (category == null) {
			categories.add(null);
		} else {
			categories.add((Preterminal) category.deepCopy());
		}
	}
	
	/**
	 * Creates a new text element.
	 * 
	 * @param text The text of the new text element.
	 * @param categoryName The name of the category.
	 */
	public TextElement(String text, String categoryName) {
		this(text, new Preterminal(categoryName));
	}
	
	/**
	 * Returns a new text element.
	 * 
	 * @param text The text of the new text element.
	 */
	public TextElement(String text) {
		this(text, (Preterminal) null);
	}
	
	/**
	 * Creates a new text element without initializing the fields of the object.
	 */
	protected TextElement() {
	}
	
	/**
	 * Returns a terminal category with the text of this text element.
	 * 
	 * @return A terminal category.
	 */
	public Terminal getTerminal() {
		return new Terminal(text);
	}
	
	/**
	 * Returns the preterminal categories of this text element. If this text element can be derived
	 * without a preterminal category then null is an element of the set. If it cannot be derived
	 * from a preterminal category then null is the only element. Text elements can have more then
	 * one category if another text element is included through the <code>include</code>-method.
	 * 
	 * @return The list of categories or null.
	 */
	public Set<Preterminal> getCategories() {
		return categories;
	}
	
	/**
	 * Returns the original text of this text element. This is the unchanged text before it is given
	 * to the context checker.
	 * 
	 * @return The original text (before the context checker).
	 */
	public String getOriginalText() {
		return text;
	}
	
	/**
	 * Returns the text of this text element. The text might have been changed by the context checker.
	 * 
	 * @return The text (after the context checker).
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
	 * Includes the categories of the given text element. This is only allowed if the two
	 * text elements are equal (accoring to the equals-method).
	 * 
	 * @param textElement The text element whose categories should be included.
	 */
	public void include(TextElement textElement) {
		if (!equals(textElement)) {
			throw new RuntimeException("Only equal text elements can be included");
		}
		categories.addAll(textElement.categories);
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
