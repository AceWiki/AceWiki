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

import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This class represents a statement that can be either an ACE sentence or a comment. A
 * statement can either be part of an article or it can be an independent statement that has no
 * article.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractStatement implements Statement {
	
	private Ontology ontology;
	private Article article;
	
	/**
	 * Initializes a new independent statement.
	 */
	protected AbstractStatement() {
	}
	
	public void init(Ontology ontology, Article article) {
		this.ontology = ontology;
		this.article = article;
	}
	
	public Ontology getOntology() {
		return ontology;
	}
	
	public Article getArticle() {
		return article;
	}
	
	public String toString() {
		return getText("Default");
	}
	
	/**
	 * Returns the AceWiki engine.
	 * 
	 * @return The engine.
	 */
	protected AceWikiEngine getEngine() {
		return getOntology().getEngine();
	}
	
	/**
	 * Returns the language handler for the given language.
	 * 
	 * @param language The language.
	 * @return The language handler.
	 */
	protected LanguageHandler getLanguageHandler(String language) {
		return getOntology().getEngine().getLanguageHandler(language);
	}
	
	/**
	 * Returns the text operator for the given language.
	 * 
	 * @param language The language.
	 * @return The text operator.
	 */
	protected TextOperator getTextOperator(String language) {
		return getOntology().getEngine().getLanguageHandler(language).getTextOperator();
	}

}
