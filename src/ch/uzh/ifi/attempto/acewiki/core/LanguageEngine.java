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

import ch.uzh.ifi.attempto.chartparser.Grammar;

/**
 * This is the main interface to define language-specific behavior.
 * 
 * @author Tobias Kuhn
 */
public interface LanguageEngine {
	
	/**
	 * This is the first method to be called and provides the ontology object.
	 * 
	 * @param ontology The ontology object.
	 */
	public void init(Ontology ontology);
	
	/**
	 * Returns the grammar that describes the language.
	 * 
	 * @return The grammar.
	 */
	public Grammar getGrammar();
	
	/**
	 * Returns the name of the grammatical category used to write texts in the predictive editor.
	 * 
	 * @return The name of the grammatical category of a text.
	 */
	public String getTextCategory();
	
	/**
	 * Returns the name of the grammatical category used for a single sentence. This is used to
	 * split a text into sentences.
	 * 
	 * @return The name of the grammatical category of a sentence.
	 */
	public String getSentenceCategory();
	
	/**
	 * Returns the dynamic lexicon.
	 * 
	 * @return The dynamic lexicon.
	 */
	public AceWikiLexicon getLexicon();
	
	/**
	 * Returns the language factory object.
	 * 
	 * @return The language factory.
	 */
	public LanguageFactory getLanguageFactory();
	
	/**
	 * Returns the lexical types, as defined by the respective ontology element types.
	 * 
	 * @return The lexical types.
	 */
	public String[] getLexicalTypes();
	
	/**
	 * Returns the controller object for the predictive editor.
	 * 
	 * @return The editor controller.
	 */
	public EditorController getEditorController();
	
	/**
	 * Returns a lexicon changer object for the given lexical type.
	 * 
	 * @param type The lexical type.
	 * @return A lexicon changer object.
	 */
	public LexiconChanger getLexiconChanger(String type);
	
	/**
	 * Returns the reasoner.
	 * 
	 * @return The reasoner.
	 */
	public AceWikiReasoner getReasoner();
	
	/**
	 * Returns a list of exporters to export the wiki content in different formats.
	 * 
	 * @return A list of ontology exporters.
	 */
	public List<OntologyExporter> getExporters();
	
	/**
	 * Returns a suggestion to change a newly created sentence, or null (no suggestion).
	 * 
	 * @param sentence The newly created sentence.
	 * @return A suggestion to change the sentence or null.
	 */
	public SentenceSuggestion getSuggestion(Sentence sentence);

}
