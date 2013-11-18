package ch.uzh.ifi.attempto.acewiki.core;

public interface AceWikiGrammarEditor {
	boolean isEditable();
	GrammarEditorResult update(Ontology ontology);
}