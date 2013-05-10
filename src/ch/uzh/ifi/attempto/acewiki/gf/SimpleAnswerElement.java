package ch.uzh.ifi.attempto.acewiki.gf;

import org.semanticweb.owlapi.model.OWLLogicalEntity;

import ch.uzh.ifi.attempto.acewiki.core.AnswerElement;
import ch.uzh.ifi.attempto.acewiki.owl.AbstractOWLOntoElement;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;

// TODO: rename: OWLEntityAnswerElement
public class SimpleAnswerElement extends AbstractOWLOntoElement implements AnswerElement {

	private final OWLLogicalEntity mEntity;

	public SimpleAnswerElement(OWLLogicalEntity entity) {
		mEntity = entity;
	}

	@Override
	public String[] getWords() {
		return new String[] { mEntity.toString() };
	}

	@Override
	public void setWords(String serializedWords) {
		throw new UnsupportedOperationException();
	}

	@Override
	public OWLLogicalEntity getOWLRepresentation() {
		return mEntity;
	}

	@Override
	public String getIRISuffix() {
		return mEntity.getIRI().getFragment();
	}

	public String serializeWords() {
		return getWords() + ";";
	}

	public String getType() {
		return mEntity.getEntityType().toString();
	}

	public String getInternalType() {
		return getType();
	}

	/**
	 * <p>Returns the textual representation of the entity.</p>
	 *
	 * TODO: translate the entity into the given language, and remove the replaceFirst-hack.
	 */
	public TextContainer getAnswerText(String lang) {
		return new TextContainer(new TextElement(getIRISuffix().replaceFirst("_[A-Z0-9]+$", "")));
	}

}