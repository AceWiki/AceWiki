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

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

/**
 * This interface represents an ontology element that has a representation in OWL.
 * 
 * @author Tobias Kuhn
 */
public interface OWLOntoElement {

	/**
	 * Returns the IRI of the ontology element. This IRI is a concatenation of the
	 * ontology IRI and the IRI suffix of the ontology element.
	 * 
	 * @return The IRI.
	 */
	public abstract IRI getIRI();

	/**
	 * This method returns an OWL object for the given ontology element.
	 * 
	 * @return An OWL object.
	 */
	public abstract OWLLogicalEntity getOWLRepresentation();

	/**
	 * This method returns an OWL axiom that declares the given ontology element.
	 * 
	 * @return An OWL declaration axiom.
	 */
	public abstract OWLDeclarationAxiom getOWLDeclaration();

}