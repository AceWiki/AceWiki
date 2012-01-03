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

/**
 * This class represents an ontology element. Such ontology elements include individuals, concepts
 * and relations. Each ontology element corresponds to a word which has one or more word forms.
 * Word forms are identified by a number (the word form id).
 * 
 * @author Tobias Kuhn
 */
public interface OntologyElement {
	
	/**
	 * Initializes the id of this ontology element.
	 * 
	 * @param id The id.
	 */
	public void initId(long id);
	
	/**
	 * Initializes the ontology of this ontology element.
	 * 
	 * @param ontology The ontology.
	 */
	public void initOntology(Ontology ontology);
	
	/**
	 * Initializes the article of this ontology element.
	 * 
	 * @param article The article.
	 */
	public void initArticle(Article article);
	
	/**
	 * Returns the numerical id of this ontology element.
	 * 
	 * @return The id.
	 */
	public long getId();
	
	/**
	 * Returns the ontology of this ontology element.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology();
	
	/**
	 * Returns the article of this ontology element.
	 * 
	 * @return The article.
	 */
	public Article getArticle();
	
	/**
	 * Returns an array of all word forms.
	 * 
	 * @return An array containing all word forms.
	 */
	public String[] getWords();

	/**
	 * Returns the word form for the given word form id.
	 * 
	 * @param wordFormID The word form id.
	 * @return The word form.
	 */
	public String getWord(int wordFormID);
	
	/**
	 * Returns the main word form.
	 * 
	 * @return The word form.
	 */
	public String getWord();
	
	/**
	 * Returns the headwords that are used in the GUI (title, index, etc) to refer to this ontology
	 * element. At least one headword is required.
	 * 
	 * @return The headwords.
	 */
	public String[] getHeadwords();
	
	/**
	 * Returns the word type as it is used internally.
	 * 
	 * @return The internal word type.
	 */
	public String getInternalType();
	
	/**
	 * Sets the word forms.
	 * 
	 * @param serializedWords The serialized word forms to be set.
	 */
	public void setWords(String serializedWords);
	
	/**
	 * Returns the word forms of this ontology element in a serialized form.
	 * 
	 * @return The serialized word forms.
	 */
	public String serializeWords();
	
	/**
	 * Returns the word type as it is shown to the user.
	 * 
	 * @return The word type.
	 */
	public String getType();

}
