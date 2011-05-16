package ch.uzh.ifi.attempto.acewiki.core;

import java.util.Collection;
import java.util.List;

import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

public interface OntologyElement extends Comparable<OntologyElement> {
	
	public void initId(long id);
	
	public void initOntology(Ontology ontology);
	
	public void initArticle(Article article);
	
	/**
	 * Returns the word forms. The position in the array corresponds to the word form id.
	 * 
	 * @return An array containing the word forms.
	 */
	public String[] getWords();
	
	/**
	 * This method returns a list that contains the word forms for external representations,
	 * for example for exports.
	 * 
	 * @return The word forms.
	 */
	public String[] getExternalWordList();

	/**
	 * Returns the word form for the given word form id.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getWord(int n);
	
	/**
	 * Returns the word form with the id 0 (the default word form).
	 * 
	 * @return The word form.
	 */
	public String getWord();
	
	/**
	 * Returns the index of the given word form or -1 if this ontology element has no such word
	 * form.
	 * 
	 * @param word The word form.
	 * @return The index.
	 */
	public int getIndexOfWord(String word);
	
	/**
	 * Returns the pretty-printed word form for the given word form id. The pretty-printing
	 * transforms underscores into blanks.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getPrettyWord(int n);

	/**
	 * Sets the word forms. The order reflects the word form ids. The indexes of the
	 * ontology are automatically updated.
	 * 
	 * @param words The word forms.
	 */
	public void setWords(String... words);
	
	/**
	 * Changes the word forms without updating the ontology indexes. The order reflects
	 * the word form ids.
	 * 
	 * @param words The word forms.
	 */
	public void changeWords(String... words);
	
	/**
	 * Returns the headword that is used in the GUI to refer to this ontology element.
	 * For example, it is used for the title of the article. Unless overridden, it is
	 * the same as the pretty-printed word form with the id 0. 
	 * 
	 * @return The headword.
	 */
	public String getHeadword();
	
	/**
	 * Returns a list of words that should be listed in the index to point to this ontology
	 * element.
	 * 
	 * @return The index words.
	 */
	public String[] getIndexEntries();
	
	/**
	 * Returns the word type as it is shown to the user. Newer versions of AceWiki can
	 * savely change this value.
	 * 
	 * @return The word type.
	 */
	public String getType();
	
	/**
	 * Returns the word type as it is used internally. Changing this value in newer versions
	 * of AceWiki breaks backwards compatibility for loading ontologies.
	 * 
	 * @return The internal word type.
	 */
	public String getInternalType();
	
	public long getId();
	
	/**
	 * Returns the ontology this ontology element is registered at.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology();
	
	public Article getArticle();

}
