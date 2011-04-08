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
public abstract class AbstractOntologyElement implements OntologyElement {
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	
	private Ontology ontology;
	private Article article;
	
	private long id = -1;
	
	/**
	 * Initializes the ontology element.
	 */
	protected AbstractOntologyElement() {
	}
	
	public void initId(long id) {
		this.id = id;
	}
	
	public void initOntology(Ontology ontology) {
		this.ontology = ontology;
	}
	
	public void initArticle(Article article) {
		this.article = article;
	}
	
	public String[] getExternalWordList() {
		return getWords();
	}
	
	public String getWord(int n) {
		return getWords()[n];
	}
	
	public String getWord() {
		return getWord(0);
	}
	
	public int getIndexOfWord(String word) {
		String[] words = getWords();
		for (int i = 0 ; i < words.length ; i++) {
			if (word.equals(words[i])) return i;
		}
		return -1;
	}
	
	public String getPrettyWord(int n) {
		String w = getWord(n);
		if (w == null) return null;
		return w.replace("_", " ");
	}
	
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
	
	public long getId() {
		return id;
	}
	
	public Ontology getOntology() {
		return ontology;
	}
	
	public Article getArticle() {
		if (article == null) {
			article = new Article(this);
		}
		return article;
	}

	// TODO: move to Ontology class!
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

	// TODO: move!
	public List<LexiconEntry> getLexiconEntries() {
		return null;
	}

	// TODO: move!
	public final IRI getIRI() {
		String baseIRI = "";
		if (ontology != null) {
			baseIRI = ontology.getURI();
		}
		return IRI.create(baseIRI + "#" + getIRISuffix());
	}

	// TODO: move!
	public OWLDataFactory getOWLDataFactory() {
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

	// TODO: move!
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
