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
 * This class represents a statement that can be either an ACE sentence or a comment. A
 * statement can either be part of an article or it can be an independent statement that has no
 * article.
 * 
 * @author Tobias Kuhn
 */
public abstract class Statement {
	
	private Ontology ontology;
	private Article article;
	
	/**
	 * Initializes a new independent statement.
	 */
	protected Statement() {
	}
	
	void init(Ontology ontology, Article article) {
		this.ontology = ontology;
		this.article = article;
	}
	
	/**
	 * This method returns the ontology this statement belongs to.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	/**
	 * This method returns the owner ontology element of this statement.
	 * 
	 * @return The owner ontology element.
	 */
	public Article getArticle() {
		return article;
	}
	
	/**
	 * This method returns the text of this statement.
	 * 
	 * @return The text.
	 */
	public abstract String getText();
	
	/**
	 * This method returns a string that denotes the type of this statement.
	 * 
	 * @return The type of this statement.
	 */
	public abstract String getType();
	
	/**
	 * This method returns a serialization of the statement.
	 * 
	 * @param encodeWords defines whether words should be encoded (for the internal "database") or
	 *   not (for export in the AceWiki data format.
	 * @return The serialized representation of the statement.
	 */
	// TODO: move? and make package visible
	public abstract String serialize(boolean encodeWords);
	
	public String toString() {
		return getText();
	}

}
