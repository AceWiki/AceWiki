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
	 * Returns the text of this statement in the given language.
	 * 
	 * @param language The language.
	 * @return The text.
	 */
	public String getText(String language);
	
	/**
	 * Returns a serialization of the statement.
	 * 
	 * @return The serialized representation of the statement.
	 */
	public String serialize();

}
