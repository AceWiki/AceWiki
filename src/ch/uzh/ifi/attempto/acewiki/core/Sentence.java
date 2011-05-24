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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

import ch.uzh.ifi.attempto.preditor.TextElement;

public interface Sentence extends Statement {
	
	/**
	 * Returns a list of text elements that represent the tokens of this sentence.
	 * 
	 * @return A token list.
	 */
	public List<TextElement> getTextElements();
	
	/**
	 * Returns the sentence text as a string. Underscores are used for compound words,
	 * e.g. "credit_card".
	 * 
	 * @return The sentence text as a string.
	 */
	public String getText();
	
	/**
	 * Returns the sentence text as a string with underscores displayed as blanks. Compound
	 * words containing underscores like "credit_cards" are pretty-printed with blank characters:
	 * "credit card".
	 * 
	 * @return The sentence text as a pretty-printed string.
	 */
	public String getPrettyText();
	
	// TODO move isReasonable/isIntegrated/setIntegrated to ontology or reasoner class?
	
	/**
	 * Returns true if this sentence can participate in reasoning.
	 * 
	 * @return true if this sentence can participate in reasoning.
	 */
	public boolean isReasonable();
	
	/**
	 * Returns true if the sentence is integrated into the ontology.
	 * 
	 * @return true if the sentence is integrated into the ontology.
	 */
	public boolean isIntegrated();
	
	// TODO cleanup/clarify!
	public void setIntegrated(boolean integrated);
	
	/**
	 * Checks whether the sentence contains the given word form (by word number) of the
	 * given ontology element.
	 * 
	 * @param e The ontology element.
	 * @param wordNumber The word number.
	 * @return true if the word form occurs in this sentence.
	 */
	public abstract boolean contains(OntologyElement e, int wordNumber);

	/**
	 * Checks whether the sentence contains the given ontology element (no matter which
	 * word form).
	 * 
	 * @param e The ontology element.
	 * @return true if the ontology element occurs in this sentence.
	 */
	public abstract boolean contains(OntologyElement e);
	
	public void parse();
	
	public boolean isReadOnly();
	
	public List<SentenceDetail> getDetails();

}
