// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.owl.OWLQuestion;
import ch.uzh.ifi.attempto.base.TextContainer;

/**
 * This class represents ACE questions.
 * 
 * @author Tobias Kuhn
 */
public class ACEQuestion extends ACESentence implements OWLQuestion {
	
	private OWLClassExpression questionOWLClass;
	private OWLNamedIndividual questionOWLIndividual;
	private boolean recalculateOWLEntities = true;

	/**
	 * Creates a new ACE question.
	 * 
	 * @param serialized The serialized representation of the question.
	 */
	public ACEQuestion(String serialized) {
		super(serialized);
	}

	/**
	 * Creates a new ACE question
	 * 
	 * @param textContainer The text container with the question text.
	 */
	public ACEQuestion(TextContainer textContainer) {
		super(textContainer);
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

	public ACEQuestion copyFor(Article article) {
		ACEQuestion c = new ACEQuestion(serialize());
		c.init(getOntology(), article);
		c.setIntegrated(isIntegrated());
		return c;
	}

}
