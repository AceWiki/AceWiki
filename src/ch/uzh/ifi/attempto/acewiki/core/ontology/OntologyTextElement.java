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

import java.util.HashSet;
import java.util.Set;

import ch.uzh.ifi.attempto.chartparser.Preterminal;
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
	private Preterminal category;
	private String pre = "";
	private String post = "";
	
	/**
	 * Creates a new text element for the given word form (by word form id) of the given
	 * ontology element.
	 * 
	 * @param el The ontology element.
	 * @param wordNumber The word form id.
	 * @return The newly created text element.
	 */
	public static OntologyTextElement createTextElement(OntologyElement el, int wordNumber) {
		if (el instanceof NounConcept) {
			if (wordNumber == 0) {
				return new OntologyTextElement(el, 0, "noun");
			} else if (wordNumber == 1) {
				return new OntologyTextElement(el, 1, "nounpl");
			}
		} else if (el instanceof Individual) {
			return new OntologyTextElement(el, wordNumber, "propername");
		} else if (el instanceof VerbRole) {
			if (wordNumber == 0) {
				return new OntologyTextElement(el, 0, "verbsg");
			} else if (wordNumber == 1) {
				return new OntologyTextElement(el, 1, "verbinf");
			} else if (wordNumber == 2) {
				return new OntologyTextElement(el, 2, "pverb");
			}
		} else if (el instanceof OfRole) {
			if (wordNumber == 0) return new OntologyTextElement(el, 0, "nounof");
		} else if (el instanceof TrAdjRole) {
			if (wordNumber == 0) return new OntologyTextElement(el, 0, "tradj");
		}
		return null;
	}
	
	/**
	 * Creates a new text element for the default word form of the given ontology element.
	 * 
	 * @param el The ontology element.
	 * @return The newly created text element.
	 */
	public static OntologyTextElement createTextElement(OntologyElement el) {
		return createTextElement(el, 0);
	}
	
	/**
	 * Creates a new ontology text element.
	 * 
	 * @param oe The ontology element.
	 * @param wn The word number.
	 * @param cat The preterminal category.
	 */
	public OntologyTextElement(OntologyElement oe, int wn, Preterminal cat) {
		if (oe.getWord(wn) == null) {
			throw new RuntimeException(oe + " has no word number " + wn);
		}
		this.ontologyElement = oe;
		this.wordNumber = wn;
		this.category = cat;
	}
	
	/**
	 * Creates a new ontology text element.
	 * 
	 * @param oe The ontology element.
	 * @param wn The word number.
	 * @param catname The category name.
	 */
	public OntologyTextElement(OntologyElement oe, int wn, String catname) {
		this(oe, wn, new Preterminal(catname));
		category.setFeature("text", getOriginalText());
		if (category.getName().equals("propername")) {
			if (((Individual) ontologyElement).hasDefiniteArticle(wordNumber)) {
				category.setFeature("capitalize", "true");
			} else {
				category.setFeature("capitalize", "false");
			}
		}
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

	public Set<Preterminal> getCategories() {
		Set<Preterminal> set = new HashSet<Preterminal>();
		set.add(category);
		return set;
	}
	
	/**
	 * Returns the ontology element to which this text element is linked.
	 * 
	 * @return The ontology element.
	 */
	public OntologyElement getOntologyElement() {
		return ontologyElement;
	}
	
	public void include(TextElement textElement) {
		if (!equals(textElement)) {
			throw new RuntimeException("Only equal text elements can be included");
		}
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof OntologyTextElement) {
			OntologyTextElement other = ((OntologyTextElement) obj);
			return (
					ontologyElement == other.ontologyElement &&
					wordNumber == other.wordNumber &&
					category.equals(other.category)
				);
		}
		return false;
	}

}
