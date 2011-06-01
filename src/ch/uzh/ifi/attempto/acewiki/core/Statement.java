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

/**
 * This interface represents a statement of a wiki article. This can be sentences in the
 * controlled natural language or informal comments.
 * 
 * @author Tobias Kuhn
 */
public interface Statement {
	
	/**
	 * This method is the first one to be called. It provides the ontology and the article object.
	 * The article can be null if the statement is not assigned to an article.
	 * 
	 * @param ontology The ontology.
	 * @param article The article.
	 */
	public void init(Ontology ontology, Article article);

	/**
	 * Returns the ontology this statement belongs to.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology();

	/**
	 * Returns the owner ontology element of this statement.
	 * 
	 * @return The owner ontology element.
	 */
	public Article getArticle();
	
	/**
	 * Returns the text of this statement.
	 * 
	 * @return The text.
	 */
	public String getText();
	
	/**
	 * Returns a serialization of the statement.
	 * 
	 * @param encodeWords defines whether words should be encoded (for the internal "database") or
	 *   not (for export in the AceWiki data format).
	 * @return The serialized representation of the statement.
	 */
	public String serialize(boolean encodeWords);

}
