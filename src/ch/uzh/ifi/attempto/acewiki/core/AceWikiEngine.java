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

/**
 * This is the main interface for the AceWiki behavior.
 * 
 * @author Tobias Kuhn
 */
public interface AceWikiEngine {
	
	/**
	 * This is the first method to be called and provides the ontology object.
	 * 
	 * @param ontology The ontology object.
	 */
	public void init(Ontology ontology);
	
	/**
	 * Returns the language handler.
	 * 
	 * @return The language handler.
	 */
	public LanguageHandler getLanguageHandler();
	
	/**
	 * Returns the lexical types, as defined by the respective ontology element types.
	 * 
	 * @return The lexical types.
	 */
	public String[] getLexicalTypes();
	
	/**
	 * Returns the reasoner.
	 * 
	 * @return The reasoner.
	 */
	public AceWikiReasoner getReasoner();
	
	/**
	 * Returns the word index.
	 * 
	 * @return The word index.
	 */
	public WordIndex getWordIndex();
	
	/**
	 * Returns a list of exporters to export the wiki content in different formats.
	 * 
	 * @return A list of ontology exporters.
	 */
	public List<OntologyExporter> getExporters();
	
	/**
	 * Creates a new ontology element for the given lexical type.
	 * 
	 * @param type The lexical type.
	 * @return A new ontology element.
	 */
	public OntologyElement createOntologyElement(String type);
	
	/**
	 * Creates a new sentence object based on the given serialization.
	 * 
	 * @param serialized The serialized representation of the sentence.
	 * @return A new sentence object.
	 */
	public Sentence createSentence(String serialized);
	
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

}
