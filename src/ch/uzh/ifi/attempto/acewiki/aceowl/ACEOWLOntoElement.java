package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.Collection;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This interface represents an ontology element that maps to a lexical entry for ACE and
 * to an OWL representation.
 * 
 * @author Tobias Kuhn
 */
public interface ACEOWLOntoElement extends OntologyElement {
	
	/**
	 * Returns the lexicon entries (one for each word form).
	 * 
	 * @return The lexicon entries.
	 */
	public List<LexiconEntry> getLexiconEntries();
	
	/**
	 * Returns the IRI of the ontology element. This IRI is a concatenation of the
	 * ontology IRI and the IRI suffix of the ontology element.
	 * 
	 * @return The IRI.
	 */
	public IRI getIRI();
	
	/**
	 * This method returns an OWL object for the given ontology element.
	 * 
	 * @return An OWL object.
	 */
	public OWLLogicalEntity getOWLRepresentation();
	
	/**
	 * This method returns an OWL axiom that declares the given ontology element.
	 * 
	 * @return An OWL declaration axiom.
	 */
	public OWLDeclarationAxiom getOWLDeclaration();
	
	/**
	 * This method should collect the lexical rules of this ontology element for the given category
	 * name.
	 * 
	 * @param catName The category name.
	 * @param lexRules The lexical rules should be added to this collection.
	 */
	// TODO: Improve this.
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules);

}
