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

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents a text container that stores a sequence of text elements that represent a
 * (partial) text or sentence.
 * 
 * @author Tobias Kuhn
 */
public class TextContainer {
	
	private List<TextElement> elements = new ArrayList<TextElement>();
	private TextOperator textOperator = new DefaultTextOperator();
	
	/**
	 * Creates a new text container.
	 */
	public TextContainer() {
	}
	
	/**
	 * Creates a new text container using the given text operator.
	 * 
	 * @param textOperator The text operator to be used.
	 */
	public TextContainer(TextOperator textOperator) {
		setTextOperator(textOperator);
	}
	
	/**
	 * Creates a new text container that contains the given text elements.
	 * 
	 * @param elements The elements to be added to the new text container.
	 */
	public TextContainer(TextElement... elements) {
		for (TextElement el : elements) {
			addElement(el);
		}
	}
	
	/**
	 * Creates a new text container that uses the given text operator and that contains the
	 * given text elements.
	 * 
	 * @param textOperator The text operator to be used.
	 * @param elements The elements to be added to the new text container.
	 */
	public TextContainer(TextOperator textOperator, TextElement... elements) {
		for (TextElement el : elements) {
			addElement(el);
		}
		setTextOperator(textOperator);
	}
	
	/**
	 * Returns the number of text elements of this text container.
	 * 
	 * @return The number of text elements.
	 */
	public int getTextElementsCount() {
		return elements.size();
	}
	
	/**
	 * Returns the text element with the given index.
	 * @param index The index of the text element to be returned.
	 * @return The text element.
	 */
	public TextElement getTextElement(int index) {
		return elements.get(index);
	}
	
	/**
	 * Returns the sequence of text elements.
	 * 
	 * @return A list containing the text elements.
	 */
	public List<TextElement> getTextElements() {
		return new ArrayList<TextElement>(elements);
	}
	
	/**
	 * Sets the text elements.
	 * @param elements A list of text elements.
	 */
	public void setTextElements(List<TextElement> elements) {
		this.elements = new ArrayList<TextElement>(elements);
	}
	
	/**
	 * Adds the text element to the end of the sequence.
	 * 
	 * @param el The text element to be added.
	 */
	public void addElement(TextElement el) {
		el.setTextContainer(this);
		elements.add(el);
	}
	
	/**
	 * Removes all text elements.
	 */
	public void removeAllElements() {
		for (TextElement te : elements) te.removeTextContainer();
		elements.clear();
	}
	
	/**
	 * Removes the last text element of the sequence if it is not empty.
	 */
	public void removeLastElement() {
		if (elements.size() > 0) {
			int last = elements.size() - 1;
			elements.get(last).removeTextContainer();
			elements.remove(last);
		}
	}
	
	/**
	 * Returns the text that is represented by the sequence of text element as a string.
	 * 
	 * @return The text.
	 */
	public String getText() {
		String text = "";
		TextElement prev = null;
		for (TextElement e : elements) {
			if (prev != null) {
				text += textOperator.getGlue(prev, e) + e.getText();
			} else {
				text += e.getText();
			}
			prev = e;
		}
		return text;
	}
	
	/**
	 * Sets the text operator.
	 * 
	 * @param textOperator The new text operator.
	 */
	public void setTextOperator(TextOperator textOperator) {
		this.textOperator = textOperator;
	}
	
	/**
	 * Returns the text operator of this text container.
	 * 
	 * @return The text operator.
	 */
	public TextOperator getTextOperator() {
		return textOperator;
	}
	
	/**
	 * Returns the position of the given text element within this text container or -1 if the
	 * text element is not contained by this text container. Note that the elements are checked for
	 * identity, not for equality.
	 * 
	 * @param textElement The text element.
	 * @return The index of the text element.
	 */
	public int getIndexOf(TextElement textElement) {
		// indexOf(...) does not work, because it uses the equals-method, but we need to check for
		// identity:
		int index = -1;
		for (int i = 0 ; i < elements.size() ; i++) {
			if (elements.get(i) == textElement) {
				index = i;
				break;
			}
		}
		return index;
	}
	
	String getTextElementText(TextElement te) {
		if (textOperator == null) {
			return te.getOriginalText();
		} else {
			String preceding = null;
			String following = null;
			int pos = getIndexOf(te);
			if (pos > 0) {
				preceding = elements.get(pos-1).getOriginalText();
			}
			if (pos < elements.size()-1) {
				following = elements.get(pos+1).getOriginalText();
			}
			return textOperator.getTextInContext(te, preceding, following);
		}
	}
	
	/**
	 * Returns a new text container containing a subsequence of the elements of this text
	 * container.
	 * 
	 * @param startPos The position of the first element.
	 * @param endPos The position after the last element.
	 * @return The new text container.
	 */
	public TextContainer getSubTextContainer(int startPos, int endPos) {
		TextContainer subtc = new TextContainer(textOperator);
		for (int i = startPos; i < endPos; i++) {
			subtc.addElement(elements.get(i));
		}
		return subtc;
	}
	
	public TextContainer clone() {
		TextContainer clone = new TextContainer();
		clone.elements = new ArrayList<TextElement>(this.elements);
		clone.textOperator = this.textOperator;
		return clone;
	}

}
