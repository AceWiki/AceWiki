// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;

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
	private String category;
	
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
	 * @param ontologyElement The ontology element.
	 * @param wordNumber The word number.
	 * @param category The category name.
	 */
	public OntologyTextElement(OntologyElement ontologyElement, int wordNumber, String category) {
		if (ontologyElement.getWord(wordNumber) == null) {
			throw new RuntimeException(ontologyElement + " has no word number " + wordNumber);
		}
		this.ontologyElement = ontologyElement;
		this.wordNumber = wordNumber;
		this.category = category;
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
		return ontologyElement.getWord(wordNumber);
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
		return new Terminal(ontologyElement.getWord(wordNumber));
	}

	public List<Preterminal> getCategories() {
		Preterminal cat = new Preterminal(category);
		cat.setFeature("text", ontologyElement.getWord(wordNumber));
		if (category.equals("propername")) {
			if (((Individual) ontologyElement).hasDefiniteArticle(wordNumber)) {
				cat.setFeature("capitalize", "true");
			} else {
				cat.setFeature("capitalize", "false");
			}
		}
		List<Preterminal> list = new ArrayList<Preterminal>();
		list.add(cat);
		return list;
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
