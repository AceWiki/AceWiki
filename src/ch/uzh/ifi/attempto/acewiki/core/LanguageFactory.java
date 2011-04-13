package ch.uzh.ifi.attempto.acewiki.core;

import ch.uzh.ifi.attempto.preditor.TextOperator;

public interface LanguageFactory {
	
	public void init(Ontology ontology);
	
	public TextOperator getTextOperator();
	
	public Sentence createSentence(String text);
	
	public OntologyElement createOntologyElement(String type);

}
