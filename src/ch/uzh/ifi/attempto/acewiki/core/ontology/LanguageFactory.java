package ch.uzh.ifi.attempto.acewiki.core.ontology;

public interface LanguageFactory {
	
	public Sentence createSentence(String text);
	
	public OntologyElement createOntologyElement(String type);

}
