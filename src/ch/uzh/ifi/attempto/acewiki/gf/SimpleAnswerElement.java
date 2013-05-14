package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Map;

import org.semanticweb.owlapi.model.OWLLogicalEntity;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AnswerElement;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.owl.AbstractOWLOntoElement;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;

// TODO: cleanup
// TODO: rename to something like GFOWLEntityAnswerElement
public class SimpleAnswerElement extends AbstractOWLOntoElement implements AnswerElement {

	private final OWLLogicalEntity mEntity;
	private final Ontology mOntology;

	public SimpleAnswerElement(Ontology ontology, OWLLogicalEntity entity) {
		mOntology = ontology;
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
	 */
	public TextContainer getAnswerText(String lang) {
		AceWikiEngine engine = mOntology.getEngine();
		String answerAsStr = null;
		String iriSuffix = getIRISuffix();
		if (engine instanceof GfEngine) {
			Map<String, String> iriToToken = ((GfEngine) engine).getGfGrammar().getIriToToken(lang);
			if (iriToToken != null) {
				answerAsStr = iriToToken.get(iriSuffix);
			}
		}

		// TODO: if mapping failed then we use the entity name as the answer and indicate this with #
		if (answerAsStr == null) {
			answerAsStr = iriSuffix.replaceFirst("_[A-Z0-9]+$", "") + " (#)";
		}
		// TODO: remove hack (replacing of underscores by spaces
		answerAsStr = answerAsStr.replaceAll("_", " ");
		return new TextContainer(new TextElement(answerAsStr));
	}

}