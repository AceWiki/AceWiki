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

import java.util.List;

import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextOperator;

// TODO split this interface into several interfaces:
// - LanguageFactory
// - AssignmentSentenceFactory
// - HierarchySentenceFactory
/**
 * This interface represents a factory to construct language elements such as sentences and
 * words (ontology elements).
 * 
 * @author Tobias Kuhn
 */
public interface LanguageFactory {

	/**
	 * This is the first method to be called and provides the ontology object.
	 * 
	 * @param ontology The ontology object.
	 */
	public void init(Ontology ontology);
	
	/**
	 * Returns the text operator.
	 * 
	 * @return The text operator.
	 */
	public TextOperator getTextOperator();
	
	/**
	 * Creates a new sentence object based on the given serialization.
	 * 
	 * @param serialized The serialized representation of the sentence.
	 * @return A new sentence object.
	 */
	public Sentence createSentence(String serialized);
	
	/**
	 * Creates a list of new sentences from a text container and/or a parser state.
	 * 
	 * @param tc The text container.
	 * @param parser The parser object with the parsed text.
	 * @return A list of new sentences.
	 */
	public List<Sentence> createSentences(TextContainer tc, PredictiveParser parser);
	
	/**
	 * Creates a new ontology element for the given lexical type.
	 * 
	 * @param type The lexical type.
	 * @return A new ontology element.
	 */
	public OntologyElement createOntologyElement(String type);
	
	/**
	 * Creates a new assignement sentence that assigns a given individual to a given concept.
	 * 
	 * @param ind The individual.
	 * @param concept The concept.
	 * @return A new sentence representing the assignment.
	 */
	public Sentence createAssignmentSentence(Individual ind, Concept concept);

	/**
	 * Creates a new hierarchy sentence that states that a certain concept is a sub-concept of
	 * another concept.
	 * 
	 * @param subConcept The sub-concept.
	 * @param superConcept The super-concept.
	 * @return A new sentence representing the assignment.
	 */
	public Sentence createHierarchySentence(Concept subConcept, Concept superConcept);
	
	/**
	 * Creates a text that represents the given ontology element as an answer to a question.
	 * 
	 * @param el The ontology element.
	 * @return The text representing an answer.
	 */
	public TextContainer createAnswerItem(OntologyElement el);

}
