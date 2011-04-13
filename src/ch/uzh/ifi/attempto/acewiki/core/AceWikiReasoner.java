package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

public interface AceWikiReasoner {
	
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
	
	public String[] getInfo();

	/**
	 * Loads the reasoner or reasoner interface.
	 */
	public void load();
	
	public void loadElement(OntologyElement element);
	
	public void unloadElement(OntologyElement element);

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
	 * @return A list of ontology elements that are the answer for the question.
	 */
	public List<OntologyElement> getAnswer(Question question);

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
	
	public void loadSentence(Sentence sentence);
	
	public void unloadSentence(Sentence sentence);
	
	public void flushReasoner();
	
	public Ontology getOntology();

}
