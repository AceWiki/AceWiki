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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

import ch.uzh.ifi.attempto.base.TextContainerSet;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This class represents a sentence for a monolingual AceWiki engine.
 * 
 * @author Tobias Kuhn
 */
public abstract class MonolingualSentence extends AbstractSentence {

	/**
	 * Initializes a new sentence.
	 */
	protected MonolingualSentence() {
	}

	/**
	 * Returns a list of text elements that represent the tokens of this sentence.
	 * 
	 * @return A token list.
	 */
	public abstract List<TextElement> getTextElements();

	public final List<TextElement> getTextElements(String language) {
		return getTextElements();
	}

	/**
	 * Returns a text container with the text of this sentence in the only available language.
	 * 
	 * @return The text container.
	 */
	public abstract TextContainerSet getTextContainerSet();

	public final TextContainerSet getTextContainerSet(String language) {
		return getTextContainerSet();
	}

	/**
	 * Returns the text of this sentence in the only available language.
	 * 
	 * @return The sentence text.
	 */
	public String getText() {
		return getText("Default");
	}

	/**
	 * Returns a list of sentence details to be shown to the user.
	 * 
	 * @return A list of sentence details.
	 */
	public abstract List<SentenceDetail> getDetails();

	public final List<SentenceDetail> getDetails(String language) {
		return getDetails();
	}

	/**
	 * Returns the text operator for the given language.
	 * 
	 * @return The text operator.
	 */
	protected TextOperator getTextOperator() {
		return getTextOperator("Default");
	}

}
