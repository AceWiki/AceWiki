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

package ch.uzh.ifi.attempto.acewiki.owl;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;

import ch.uzh.ifi.attempto.acewiki.core.Sentence;

/**
 * This class represents a sentence with an OWL representation.
 * 
 * @author Tobias Kuhn
 */
public interface OWLSentence extends Sentence {

	/**
	 * Returns a set of OWL axioms that represent this sentence.
	 * 
	 * @return The OWL axioms.
	 */
	public abstract Set<OWLAxiom> getOWLAxioms();

	/**
	 * Returns true if this sentence has an OWL representation.
	 * 
	 * @return true if this sentence has an OWL representation.
	 */
	public abstract boolean isOWL();

	/**
	 * Returns true if this sentence has an OWL or SWRL representation.
	 * 
	 * @return true if this sentence has an OWL or SWRL representation.
	 */
	public abstract boolean isOWLSWRL();

	/**
	 * Returns a pretty-printed OWL representation of this sentence.
	 * 
	 * @return The pretty-printed OWL representation.
	 */
	public abstract String getPrettyOWL();

}
