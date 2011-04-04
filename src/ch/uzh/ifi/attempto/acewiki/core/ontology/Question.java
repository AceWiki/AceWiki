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

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

/**
 * This class represents ACE questions.
 * 
 * @author Tobias Kuhn
 */
public class Question extends Sentence {
	
	private OWLClassExpression questionOWLClass;
	private OWLNamedIndividual questionOWLIndividual;
	private boolean recalculateOWLEntities = true;
	
	/**
	 * Creates a new question.
	 * 
	 * @param text The question text.
	 */
	protected Question(String text) {
		super(text);
	}
	
	/**
	 * Returns the OWL class expression for this question. All individuals that belong to this
	 * class expression can be considered answers to the question.
	 * 
	 * @return The OWL class expression.
	 */
	public OWLClassExpression getQuestionOWLClass() {
		calculateQuestionOWLEntities();
		return questionOWLClass;
	}

	/**
	 * Returns the OWL individual for this question. For questions like "What is Switzerland?", the
	 * respective individual ("Switzerland") is returned. In all other cases, null is returned.
	 * 
	 * @return The OWL individual.
	 */
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
	
	void parse() {
		recalculateOWLEntities = true;
		super.parse();
	}
	
	public boolean isReasonerParticipant() {
		return false;
	}
	
	public boolean isReadOnly() {
		return false;
	}
	
	public String getType() {
		return "question";
	}

}
