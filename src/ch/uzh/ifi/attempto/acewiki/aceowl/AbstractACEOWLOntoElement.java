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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLDeclarationAxiomImpl;
import ch.uzh.ifi.attempto.acewiki.core.AbstractOntologyElement;
import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This is a partial implementation of an ontology element that maps to a lexical entry for ACE and
 * to an OWL representation.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractACEOWLOntoElement extends AbstractOntologyElement
		implements ACEOWLOntoElement {
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	
	public List<LexiconEntry> getLexiconEntries() {
		return null;
	}
	
	public final IRI getIRI() {
		String baseIRI = "";
		if (getOntology() != null) {
			baseIRI = getOntology().getURI();
		}
		return IRI.create(baseIRI + "#" + getIRISuffix());
	}
	
	/**
	 * Returns the suffix of the OWL identifier of this ontology element. Such identifiers are IRIs
	 * and the suffix is the part after the hash sign.
	 * 
	 * @return The IRI suffix.
	 */
	public abstract String getIRISuffix();
	
	/**
	 * Returns the OWL data factory object.
	 * 
	 * @return The OWL data factory.
	 */
	public OWLDataFactory getOWLDataFactory() {
		return dataFactory;
	}
	
	public OWLDeclarationAxiom getOWLDeclaration() {
		OWLLogicalEntity owl = getOWLRepresentation();
		if (owl == null) {
			return null;
		} else {
			return new OWLDeclarationAxiomImpl(
					dataFactory,
					getOWLRepresentation(),
					new ArrayList<OWLAnnotation>()
				);
		}
	}

}
