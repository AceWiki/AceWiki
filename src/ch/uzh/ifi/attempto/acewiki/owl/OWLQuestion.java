// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.owl;

import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import ch.uzh.ifi.attempto.acewiki.core.Question;

/**
 * This class represents questions with OWL representations.
 * 
 * @author Tobias Kuhn
 */
public interface OWLQuestion extends OWLSentence, Question {

	/**
	 * Returns the OWL class expression for this question. All individuals that belong to this
	 * class expression can be considered answers to the question.
	 * 
	 * @return The OWL class expression.
	 */
	public abstract OWLClassExpression getQuestionOWLClass();

	/**
	 * Returns the OWL individual for this question. For questions like "What is Switzerland?", the
	 * respective individual ("Switzerland") is returned. In all other cases, null is returned.
	 * 
	 * @return The OWL individual.
	 */
	public abstract OWLNamedIndividual getQuestionOWLIndividual();

}
