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

public interface Statement {
	
	public void init(Ontology ontology, Article article);

	/**
	 * This method returns the ontology this statement belongs to.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology();

	/**
	 * This method returns the owner ontology element of this statement.
	 * 
	 * @return The owner ontology element.
	 */
	public Article getArticle();
	
	/**
	 * This method returns the text of this statement.
	 * 
	 * @return The text.
	 */
	public String getText();
	
	/**
	 * This method returns a serialization of the statement.
	 * 
	 * @param encodeWords defines whether words should be encoded (for the internal "database") or
	 *   not (for export in the AceWiki data format.
	 * @return The serialized representation of the statement.
	 */
	public String serialize(boolean encodeWords);

}
