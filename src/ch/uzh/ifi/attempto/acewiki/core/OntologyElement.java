package ch.uzh.ifi.attempto.acewiki.core;

import java.util.Collection;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

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
	
	/**
	 * Returns the lexicon entries (one for each word form).
	 * 
	 * @return The lexicon entries.
	 */
	// TODO: move!
	public List<LexiconEntry> getLexiconEntries();
	
	/**
	 * Returns the IRI of the ontology element. This IRI is a concatenation of the
	 * ontology IRI and the IRI suffix of the ontology element.
	 * 
	 * @return The IRI.
	 * @see #getIRISuffix()
	 */
	// TODO: move!
	public IRI getIRI();
	
	/**
	 * Returns the IRI suffix of this ontology element. For example "country".
	 * 
	 * @return The IRI suffix.
	 * @see #getIRI()
	 */
	// TODO: move!
	public String getIRISuffix();
	
	/**
	 * Returns an OWL data factory. Subclasses use this factory to create their OWL
	 * representations.
	 * 
	 * @return OWL data factory.
	 */
	// TODO: move!
	public OWLDataFactory getOWLDataFactory();
	
	/**
	 * This method returns an OWL object for the given ontology element.
	 * 
	 * @return An OWL object.
	 */
	// TODO: move!
	public OWLLogicalEntity getOWLRepresentation();
	
	/**
	 * This method returns an OWL axiom that declares the given ontology element.
	 * 
	 * @return An OWL declaration axiom.
	 */
	// TODO: move!
	public OWLDeclarationAxiom getOWLDeclaration();
	
	/**
	 * This method should collect the lexical rules of this ontology element for the given category
	 * name.
	 * 
	 * @param catName The category name.
	 * @param lexRules The lexical rules should be added to this collection.
	 */
	// TODO: Improve this.
	// TODO: move!
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules);
	
	/**
	 * Registers this ontology element at the given ontology. An ontology element can be
	 * registered only once.
	 * 
	 * @param ontology
	 */
	// TODO: move to Ontology class!
	public void registerAt(Ontology ontology);

}
