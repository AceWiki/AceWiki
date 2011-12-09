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

import java.util.List;
import java.util.Map;

// TODO split this interface into several interfaces:
// - AceWikiReasoner
// - AssignmentsReasoner
// - HierarchyReasoner
// - ConceptSatisfiabilityReasoner
// - QuestionReasoner
/**
 * This is the reasoner interface for AceWiki.
 * 
 * @author Tobias Kuhn
 */
public interface AceWikiReasoner {

	/**
	 * This is the first method to be called and provides the ontology object.
	 * 
	 * @param ontology The ontology object.
	 */
	public void init(Ontology ontology);

	/**
	 * Should return the name of the reasoner.
	 * 
	 * @return The name of the reasoner.
	 */
	public String getReasonerName();
	
	/**
	 * Should return the version of the reasoner.
	 * 
	 * @return The version of the reasoner.
	 */
	public String getReasonerVersion();

	/**
	 * Should return the type of the reasoner.
	 * 
	 * @return The reasoner type.
	 */
	public String getReasonerType();
	
	/**
	 * This method can return a map of name/value pairs with information about the reasoner.
	 * To retain the insertion order, a map implementation like LinkedHashMap should be used.
	 * It is allowed to return null for no information.
	 * 
	 * @return A map of name/value pairs with reasoner information.
	 */
	public Map<String, String> getInfo();

	/**
	 * Loads the reasoner or reasoner interface.
	 */
	public void load();
	
	/**
	 * Loads the given ontology element. The method flushElements is called after each
	 * sequence of one or more loadElement-calls.
	 * 
	 * @param element The ontology element.
	 */
	public void loadElement(OntologyElement element);
	
	/**
	 * Unloads the given ontology element. The method flushElements is called after each
	 * sequence of one or more unloadElement-calls.
	 * 
	 * @param element The ontology element.
	 */
	public void unloadElement(OntologyElement element);
	
	/**
	 * This method can finalize the loading or unloading of elements. It is always called
	 * after a sequence of one or more loadElement- or unloadElement-calls.
	 */
	public void flushElements();

	/**
	 * Should return all concepts the given individual belongs to.
	 * 
	 * @param ind The individual.
	 * @return A list of all concepts of the individual.
	 */
	public List<Concept> getConcepts(Individual ind);

	/**
	 * Should return all individuals that belong to the given concept.
	 * 
	 * @param concept The concept.
	 * @return A list of all individuals of the concept.
	 */
	public List<Individual> getIndividuals(Concept concept);

	/**
	 * Should return all super-concepts of the given concept.
	 * 
	 * @param concept The concept for which all super-concepts should be returned.
	 * @return A list of all super-concepts.
	 */
	public List<Concept> getSuperConcepts(Concept concept);

	/**
	 * Should return all the sub-concepts of the given concept.
	 * 
	 * @param concept The concept for which all sub-concepts should be returned.
	 * @return A list of all sub-concepts.
	 */
	public List<Concept> getSubConcepts(Concept concept);

	/**
	 * Should return a list of ontology elements that answer the given question.
	 * 
	 * @param question The question to be answered.
	 * @return A list of ontology elements that are the answer to the question.
	 */
	public List<AnswerElement> getAnswer(Question question);

	/**
	 * Should return true if the ontology is consistent.
	 * 
	 * @return true if the ontology is consistent.
	 */
	public boolean isConsistent();

	/**
	 * Should check if the given concept is satisfiable.
	 * 
	 * @param concept The concept.
	 * @return true if the concept is satisfiable.
	 */
	public boolean isSatisfiable(Concept concept);
	
	/**
	 * Loads the given sentence.
	 * 
	 * @param sentence The sentence.
	 */
	public void loadSentence(Sentence sentence);
	
	/**
	 * Unloads the given sentence.
	 * 
	 * @param sentence The sentence.
	 */
	public void unloadSentence(Sentence sentence);

}
