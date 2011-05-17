package ch.uzh.ifi.attempto.acewiki.core;

import ch.uzh.ifi.attempto.preditor.TextOperator;

// TODO split this interface into several interfaces:
// - LanguageFactory
// - AssignmentSentenceFactory
// - HierarchySentenceFactory
public interface LanguageFactory {
	
	public void init(Ontology ontology);
	
	public TextOperator getTextOperator();
	
	public Sentence createSentence(String text);
	
	public OntologyElement createOntologyElement(String type);
	
	public Sentence createAssignmentSentence(Individual ind, Concept concept);
	
	public Sentence createHierarchySentence(Concept subConcept, Concept superConcept);

}
