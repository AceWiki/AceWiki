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

import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This interface represents a sentence in the given controlled natural language. It includes
 * declarative sentences as well as questions.
 * 
 * @author Tobias Kuhn
 */
public interface Sentence extends Statement {
	
	/**
	 * Returns a list of text elements that represent the tokens of this sentence in the given
	 * language.
	 * 
	 * @param language The language.
	 * @return A token list.
	 */
	public List<TextElement> getTextElements(String language);
	
	/**
	 * Returns true if this sentence can participate in reasoning.
	 * 
	 * @return true if this sentence can participate in reasoning.
	 */
	// TODO move to ontology or reasoner class?
	public boolean isReasonable();
	
	/**
	 * Returns true if the sentence is integrated into the ontology.
	 * 
	 * @return true if the sentence is integrated into the ontology.
	 */
	public boolean isIntegrated();
	
	/**
	 * Informs the sentence object whether it is integrated into the ontology or not. This
	 * method should only be called from the ontology or an ontology loader.
	 * 
	 * @param integrated true if the sentence is integrated into the ontology.
	 */
	public void setIntegrated(boolean integrated);

	/**
	 * Checks whether the sentence contains the given ontology element (no matter which
	 * word form).
	 * 
	 * @param e The ontology element.
	 * @return true if the ontology element occurs in this sentence.
	 */
	public boolean contains(OntologyElement e);
	
	/**
	 * This method is called whenever some words of the sentence are modified.
	 */
	public void update();
	
	/**
	 * Returns whether the sentence can be changed or is immutable.
	 * 
	 * @return true if the sentence cannot be changed.
	 */
	public boolean isImmutable();
	
	/**
	 * Returns a list of sentence details in the given language.
	 * 
	 * @param language The language.
	 * @return A list of sentence details.
	 */
	public List<SentenceDetail> getDetails(String language);

}
