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

import java.util.List;

/**
 * This interface represents an index for the word forms of ontology elements.
 * 
 * @author Tobias Kuhn
 */
public interface WordIndex {
	
	/**
	 * This method is called by the ontology object when a new ontology element is added.
	 * 
	 * @param element A new ontology element.
	 */
	public void elementAdded(OntologyElement element);

	/**
	 * This method is called by the ontology object when an ontology element is removed.
	 * 
	 * @param element The ontology element to be removed.
	 */
	public void elementRemoved(OntologyElement element);

	/**
	 * This method is called just before the word forms of an ontology element are changed.
	 * 
	 * @param element The ontology element to be changed.
	 */
	public void elementBeforeChange(OntologyElement element);

	/**
	 * This method is called just after the word forms of an ontology element have been changed.
	 * 
	 * @param element The changed ontology element.
	 */
	public void elementAfterChange(OntologyElement element);
	
	/**
	 * This method should return the ontology element with the given word form, or null if there is
	 * no such element.
	 * 
	 * @param word The word form.
	 * @return The ontology element or null.
	 */
	public OntologyElement getElement(String word);
	
	/**
	 * This method should return a list of ontology elements that match the given search text.
	 * 
	 * @param searchText The text to search for.
	 * @return The list of ontology elements.
	 */
	public List<OntologyElement> searchForElements(String searchText);

}
