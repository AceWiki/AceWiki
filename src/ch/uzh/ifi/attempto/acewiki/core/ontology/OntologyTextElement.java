// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import ch.uzh.ifi.attempto.chartparser.Terminal;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class represents a text element that links to an ontology element. The text of
 * the text elements corresponds to one of the word forms of the ontology element.
 * 
 * @author Tobias Kuhn
 */
public class OntologyTextElement extends TextElement {
	
	private OntologyElement ontologyElement;
	private int wordNumber;
	private String pre = "";
	private String post = "";
	
	/**
	 * Creates a new ontology text element.
	 * 
	 * @param oe The ontology element.
	 * @param wn The word number.
	 */
	public OntologyTextElement(OntologyElement oe, int wn) {
		if (oe.getWord(wn) == null) {
			throw new RuntimeException(oe + " has no word number " + wn);
		}
		this.ontologyElement = oe;
		this.wordNumber = wn;
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
		if (t != null) t = t.replace("_", " ");
		return t;
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
	 * This method adds a text to the end of the word of the ontology element.
	 * 
	 * @param post The text to be added to the end.
	 */
	public void setPostText(String post) {
		if (post != null) this.post = post;
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
	
	public Terminal getTerminal() {
		return new Terminal(getOriginalText());
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
