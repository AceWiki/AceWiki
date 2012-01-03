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

/**
 * This interface represents an object that knows how to create and change words in the form of
 * ontology elements and how this should be presented to the user.
 * 
 * @author Tobias Kuhn
 */
public interface LexiconChanger {
	
	/**
	 * Returns the description of the given word type to be shown to the user.
	 * 
	 * @return The description.
	 */
	public String getDescription();
	
	/**
	 * Returns a list of lexical details for the given ontology element.
	 * 
	 * @param el The ontology element.
	 * @return A list of lexical details.
	 */
	public List<LexiconDetail> getDetails(OntologyElement el);
	
	/**
	 * Tries to save a modification on an ontology element. An exception is thrown if the
	 * modification is not possible, e.g. because of name conflicts.
	 * 
	 * @param el The ontology element to be modified.
	 * @param wordNumber The word number to be used right after a successful modification.
	 * @param newValues The new values corresponding to the list of lexical details.
	 * @param ontology The ontology.
	 * @throws InvalidWordException If the modification is not possible.
	 */
	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException;

}
