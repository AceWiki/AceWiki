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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLDeclarationAxiomImpl;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This class represents an ontology element which can be an individual ("constant"), a concept
 * ("unary relation", "class", "type"), or a role ("binary relation", "property"). See the
 * respective sub-classes.
 *<p>
 * In AceWiki, each ontology element corresponds to a word which has one or more word forms.
 * Word forms are identified by a number (the word form id).
 *<p>
 * Every ontology element has an article which consists of a list of statements.
 * 
 * @author Tobias Kuhn
 */
public abstract class OntologyElement implements Comparable<OntologyElement> {
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	
	private Ontology ontology;
	private Article article;
	
	private long id = -1;
	
	/**
	 * Initializes the ontology element.
	 */
	protected OntologyElement() {
	}
	
	void initOntology(Ontology ontology) {
		this.ontology = ontology;
	}
	
	void initArticle(Article article) {
		this.article = article;
	}
	
	/**
	 * Returns the word forms. The position in the array corresponds to the word form id.
	 * 
	 * @return An array containing the word forms.
	 */
	public abstract String[] getWords();
	
	/**
	 * This method returns a list that contains the word forms for external representations,
	 * for example for exports.
	 * 
	 * @return The word forms.
	 */
	public String[] getExternalWordList() {
		return getWords();
	}
	
	/**
	 * Returns the word form for the given word form id.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getWord(int n) {
		return getWords()[n];
	}
	
	/**
	 * Returns the word form with the id 0 (the default word form).
	 * 
	 * @return The word form.
	 */
	public String getWord() {
		return getWord(0);
	}
	
	/**
	 * Returns the index of the given word form or -1 if this ontology element has no such word
	 * form.
	 * 
	 * @param word The word form.
	 * @return The index.
	 */
	public int getIndexOfWord(String word) {
		String[] words = getWords();
		for (int i = 0 ; i < words.length ; i++) {
			if (word.equals(words[i])) return i;
		}
		return -1;
	}
	
	/**
	 * Returns the pretty-printed word form for the given word form id. The pretty-printing
	 * transforms underscores into blanks.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getPrettyWord(int n) {
		String w = getWord(n);
		if (w == null) return null;
		return w.replace("_", " ");
	}
	
	/**
	 * Sets the word forms. The order reflects the word form ids. The indexes of the
	 * ontology are automatically updated.
	 * 
	 * @param words The word forms.
	 */
	public final void setWords(String... words) {
		if (ontology == null) {
			changeWords(words);
		} else {
			synchronized (ontology) {
				ontology.removeFromWordIndex(this);
				changeWords(words);
				ontology.addToWordIndex(this);
				ontology.refresh(this);
			}
		}
	}
	
	/**
	 * Changes the word forms without updating the ontology indexes. The order reflects
	 * the word form ids.
	 * 
	 * @param words The word forms.
	 */
	protected abstract void changeWords(String... words);
	
	/**
	 * Returns the headword that is used in the GUI to refer to this ontology element.
	 * For example, it is used for the title of the article. Unless overridden, it is
	 * the same as the pretty-printed word form with the id 0. 
	 * 
	 * @return The headword.
	 */
	public String getHeadword() {
		return getPrettyWord(0);
	}
	
	/**
	 * Returns a list of words that should be listed in the index to point to this ontology
	 * element.
	 * 
	 * @return The index words.
	 */
	public String[] getIndexEntries() {
		return new String[] {getHeadword()};
	}
	
	/**
	 * Returns the word type as it is shown to the user. Newer versions of AceWiki can
	 * savely change this value.
	 * 
	 * @return The word type.
	 */
	public abstract String getType();
	
	/**
	 * Returns the word type as it is used internally. Changing this value in newer versions
	 * of AceWiki breaks backwards compatibility for loading ontologies.
	 * 
	 * @return The internal word type.
	 */
	public abstract String getInternalType();
	
	/**
	 * Returns the ontology this ontology element is registered at.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	public Article getArticle() {
		if (article == null) {
			article = new Article(this);
		}
		return article;
	}
	
	/**
	 * Registers this ontology element at the given ontology. An ontology element can be
	 * registered only once.
	 * 
	 * @param ontology
	 */
	public void registerAt(Ontology ontology) {
		if (this.ontology != null) {
			throw new RuntimeException("Cannot change the ontology for element " + toString());
		}
		if (ontology == null) {
			return;
		}
		this.ontology = ontology;
		synchronized (ontology) {
			ontology.register(this);
			ontology.getStorage().save(this);
		}
	}
	
	/**
	 * Returns the lexicon entries (one for each word form).
	 * 
	 * @return The lexicon entries.
	 */
	List<LexiconEntry> getLexiconEntries() {
		return null;
	}
	
	/**
	 * Returns the IRI of the ontology element. This IRI is a concatenation of the
	 * ontology IRI and the IRI suffix of the ontology element.
	 * 
	 * @return The IRI.
	 * @see #getIRISuffix()
	 */
	public final IRI getIRI() {
		String baseIRI = "";
		if (ontology != null) {
			baseIRI = ontology.getURI();
		}
		return IRI.create(baseIRI + "#" + getIRISuffix());
	}
	
	/**
	 * Returns the IRI suffix of this ontology element. For example "country".
	 * 
	 * @return The IRI suffix.
	 * @see #getIRI()
	 */
	public abstract String getIRISuffix();
	
	final void setId(long id) {
		this.id = id;
	}
	
	final long getId() {
		return id;
	}
	
	/**
	 * Returns an OWL data factory. Subclasses use this factory to create their OWL
	 * representations.
	 * 
	 * @return OWL data factory.
	 */
	protected OWLDataFactory getOWLDataFactory() {
		return dataFactory;
	}
	
	/**
	 * Writes the text to the log file.
	 * 
	 * @param text The text to be written to the log file.
	 */
	protected void log(String text) {
		if (ontology != null) {
			ontology.log(text);
		}
	}
	
	/**
	 * This method returns an OWL object for the given ontology element.
	 * 
	 * @return An OWL object.
	 */
	public abstract OWLLogicalEntity getOWLRepresentation();
	
	/**
	 * This method returns an OWL axiom that declares the given ontology element.
	 * 
	 * @return An OWL declaration axiom.
	 */
	public OWLDeclarationAxiom getOWLDeclaration() {
		OWLLogicalEntity owl = getOWLRepresentation();
		if (owl == null) {
			return null;
		} else {
			return new OWLDeclarationAxiomImpl(
					dataFactory,
					getOWLRepresentation(),
					new ArrayList<OWLAnnotation>()
				);
		}
	}
	
	/**
	 * This method should collect the lexical rules of this ontology element for the given category
	 * name.
	 * 
	 * @param catName The category name.
	 * @param lexRules The lexical rules should be added to this collection.
	 */
	// TODO: Improve this.
	abstract void collectLexicalRules(String catName, Collection<LexicalRule> lexRules);

	public int compareTo(OntologyElement e) {
		if (this instanceof DummyOntologyElement && !(e instanceof DummyOntologyElement)) {
			return -1;
		} else if (!(this instanceof DummyOntologyElement) && e instanceof DummyOntologyElement) {
			return 1;
		} else {
			return getHeadword().compareToIgnoreCase(e.getHeadword());
		}
	}
	
	public String toString() {
		String l = "";
		for (String s : getWords()) {
			if (s == null) {
				l += ",";
			} else {
				l += s + ",";
			}
		}
		if (l.length() > 0) l = l.substring(0, l.length()-1);
		return getType() + "{" + l + "}";
	}

}
