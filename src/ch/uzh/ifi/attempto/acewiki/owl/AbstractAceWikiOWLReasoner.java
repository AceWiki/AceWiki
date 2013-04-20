package ch.uzh.ifi.attempto.acewiki.owl;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;

/**
 * TODO: move most of the methods from subclasses here
 */
public abstract class AbstractAceWikiOWLReasoner implements AceWikiReasoner {

	public abstract OWLOntology exportOWLOntology(boolean consistent);
	public abstract OWLOntologyManager getOWLOntologyManager();

}