package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.List;

public interface AceWikiReasoner {

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
	
	public List<Concept> getConcepts(Individual ind);
	
	public List<Individual> getIndividuals(Concept concept);
	
	public List<Concept> getSuperConcepts(Concept concept);
	
	public List<Concept> getSubConcepts(Concept concept);
	
	public List<OntologyElement> getAnswer(Question question);
	
	public boolean isConsistent();
	
	public boolean isSatisfiable(Concept concept);
	
	public void loadSentence(Sentence sentence);
	
	public void unloadSentence(Sentence sentence);
	
	public void flushReasoner();

}
