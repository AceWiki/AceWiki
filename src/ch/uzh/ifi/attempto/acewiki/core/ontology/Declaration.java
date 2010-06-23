package ch.uzh.ifi.attempto.acewiki.core.ontology;

/**
 * This class represents ACE declarations (declarative sentences). Some declarative sentences can
 * be translated into OWL and can participate in reasoning. Others have no OWL representation and
 * do not participate in reasoning.
 * 
 * @author Tobias Kuhn
 */
public class Declaration extends Sentence {
	
	/**
	 * Creates a new asserted sentence. Asserted sentences must have an owner.
	 * 
	 * @param text The sentence text.
	 * @param owner The owner ontology element.
	 */
	protected Declaration(String text, OntologyElement owner) {
		super(text, owner);
	}
	
	/**
	 * Creates a new inferred sentence. Inferred sentences have no owner.
	 * 
	 * @param text The sentence text.
	 * @param ontology The ontology.
	 */
	protected Declaration(String text, Ontology ontology) {
		super(text, ontology);
	}
	
	/**
	 * Checks if the sentence is read-only or not. Only inferred sentences are read-only.
	 * 
	 * @return true if the sentence is read-only.
	 */
	public boolean isReadOnly() {
		return getOwner() == null;
	}

}
