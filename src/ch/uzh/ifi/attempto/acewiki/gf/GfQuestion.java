package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.owl.OWLQuestion;

public class GfQuestion extends GfSentence implements OWLQuestion {

	private OWLClassExpression questionOWLClass;
	private OWLNamedIndividual questionOWLIndividual;
	private boolean recalculateOWLEntities = true;

	public GfQuestion(GfGrammar grammar, GfWikiEntry entry) {
		super(grammar, entry);
	}

	public GfQuestion(GfGrammar grammar, String language, String tokenText) {
		super(grammar, language, tokenText);
	}


	public OWLClassExpression getQuestionOWLClass() {
		calculateQuestionOWLEntities();
		return questionOWLClass;
	}

	public OWLNamedIndividual getQuestionOWLIndividual() {
		calculateQuestionOWLEntities();
		return questionOWLIndividual;
	}

	private void calculateQuestionOWLEntities() {
		if (!recalculateOWLEntities) return;
		questionOWLClass = null;
		questionOWLIndividual = null;

		OWLSubClassOfAxiom questionOWLAxiom = null;
		for (OWLAxiom ax : getOWLAxioms()) {
			if (ax instanceof OWLSubClassOfAxiom) {
				questionOWLAxiom = (OWLSubClassOfAxiom) ax;
				break;
			}
		}
		if (questionOWLAxiom != null) {
			questionOWLClass = questionOWLAxiom.getSubClass();
		}

		if (questionOWLClass instanceof OWLObjectOneOf) {
			OWLObjectOneOf oneof = ((OWLObjectOneOf) questionOWLClass);
			if (oneof != null && oneof.getIndividuals().size() == 1) {
				OWLIndividual owlInd = oneof.getIndividuals().iterator().next();
				if (owlInd instanceof OWLNamedIndividual) {
					questionOWLIndividual = (OWLNamedIndividual) owlInd;
				}
			}
		}

		recalculateOWLEntities = false;
	}

	public void update() {
		recalculateOWLEntities = true;
		super.update();
	}

	public boolean isReasonable() {
		return false;
	}

	// TODO: move to superclass (same for GfDeclaration)
	public GfSentence copyFor(Article article) {
		GfSentence c = new GfQuestion(getGfGrammar(), mGfWikiEntry);
		c.init(getOntology(), article);
		c.setIntegrated(isIntegrated());
		return c;
	}

	public Sentence unambiguousCopyFor(Article article, int index) {
		List<String> trees = new ArrayList<>();
		trees.add(mGfWikiEntry.getTrees().getTrees().get(0));
		GfWikiEntry wikiEntry = new GfWikiEntry(mGfWikiEntry.getLanguage(), mGfWikiEntry.getText(), new TreeList(trees));
		GfSentence d = new GfQuestion(mGfGrammar, wikiEntry);
		d.init(getOntology(), article);
		return d;
	}

}