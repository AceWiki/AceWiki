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
 * This is a partial implementation of an ontology element.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractOntologyElement implements OntologyElement {
	
	private Ontology ontology;
	private Article article;
	
	private long id = -1;
	
	public void initId(long id) {
		this.id = id;
	}
	
	public void initOntology(Ontology ontology) {
		if (this.ontology != null && this.ontology != ontology) {
			throw new RuntimeException("Cannot change the ontology for element " + toString());
		}
		this.ontology = ontology;
	}
	
	public void initArticle(Article article) {
		this.article = article;
	}
	
	public String getWord(int n) {
		return getWords()[n];
	}
	
	public String getWord() {
		return getWord(0);
	}
	
	public String[] getHeadwords() {
		return new String[] {getWord(0)};
	}
	
	public long getId() {
		return id;
	}
	
	public Ontology getOntology() {
		return ontology;
	}
	
	public Article getArticle() {
		if (article == null) {
			article = new Article(this);
		}
		return article;
	}
	
	/**
	 * Writes the text to the log file.
	 * 
	 * @param text The text to be written to the log file.
	 */
	protected void log(String text) {
		if (ontology != null) {
			ontology.log(text);
		}
	}
	
	public String toString() {
		return getInternalType() + ":" + serializeWords();
	}

}
