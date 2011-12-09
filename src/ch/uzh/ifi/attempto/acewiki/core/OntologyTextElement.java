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

import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class represents a text element that links to an ontology element. The text of
 * the text elements corresponds to one of the word forms of the ontology element.
 * 
 * @author Tobias Kuhn
 */
// TODO Get rid of pre and post texts.
public class OntologyTextElement extends TextElement {
	
	private OntologyElement ontologyElement;
	private int wordNumber;
	private String pre = "";
	private String post = "";
	
	/**
	 * Creates a new ontology text element.
	 * 
	 * @param element The ontology element.
	 * @param wordNumber The word number.
	 */
	public OntologyTextElement(OntologyElement element, int wordNumber) {
		if (element.getWord(wordNumber) == null) {
			throw new RuntimeException(element + " has no word number " + wordNumber);
		}
		this.ontologyElement = element;
		this.wordNumber = wordNumber;
	}

	/**
	 * Returns the text of this text element in its plain form where underscores are not
	 * replaces by blanks.
	 * 
	 * @return The plain text.
	 */
	public String getUnderscoredText() {
		return super.getText();
	}
	
	public String getText() {
		String t = super.getText();
		if (t == null) return null;
		return LanguageUtils.getPrettyPrinted(t);
	}
	
	public String getOriginalText() {
		return pre + ontologyElement.getWord(wordNumber) + post;
	}
	
	/**
	 * This method adds a text to the front of the word of the ontology element.
	 * 
	 * @param pre The text to be added to the front.
	 */
	public void setPreText(String pre) {
		if (pre != null) this.pre = pre;
	}
	
	/**
	 * Returns the text added to the front.
	 * 
	 * @return The text to be added to the front.
	 */
	public String getPreText() {
		return pre;
	}
	
	/**
	 * This method adds a text to the end of the word of the ontology element.
	 * 
	 * @param post The text to be added to the end.
	 */
	public void setPostText(String post) {
		if (post != null) this.post = post;
	}
	
	/**
	 * Returns the text added to the end.
	 * 
	 * @return The text to be added to the end.
	 */
	public String getPostText() {
		return post;
	}

	/**
	 * Returns the id of the word form of the ontology element that is used for this
	 * text element.
	 * 
	 * @return The word form id.
	 */
	public int getWordNumber() {
		return wordNumber;
	}
	
	/**
	 * Returns the ontology element to which this text element is linked.
	 * 
	 * @return The ontology element.
	 */
	public OntologyElement getOntologyElement() {
		return ontologyElement;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof OntologyTextElement) {
			OntologyTextElement other = ((OntologyTextElement) obj);
			return (
					ontologyElement == other.ontologyElement &&
					wordNumber == other.wordNumber
				);
		}
		return false;
	}

}
