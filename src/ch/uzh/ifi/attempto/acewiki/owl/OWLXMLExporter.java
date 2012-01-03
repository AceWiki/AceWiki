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

import java.io.IOException;

import org.coode.owlapi.owlxml.renderer.OWLXMLRenderer;
import org.semanticweb.owlapi.io.OWLRendererException;
import org.semanticweb.owlapi.model.OWLOntology;

import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;

/**
 * This exporter generates an OWL/XML representation of the ontology.
 * 
 * @author Tobias Kuhn
 */
public class OWLXMLExporter extends OntologyExporter {
	
	private final boolean consistent;

	/**
	 * Creates a new OWL/XML exporter.
	 * 
	 * @param consistent Defines if only the consistent part of the ontology should be considered.
	 */
	public OWLXMLExporter(boolean consistent) {
		this.consistent = consistent;
	}
	
	protected void writeContent() throws IOException {
		AceWikiOWLReasoner owlReasoner = (AceWikiOWLReasoner) getOntology()
				.getReasoner().getWrappedReasoner();
		OWLOntology owlOntology = owlReasoner.exportOWLOntology(consistent);
        try {
            OWLXMLRenderer renderer = new OWLXMLRenderer(owlReasoner.getOWLOntologyManager());
            renderer.render(owlOntology, getOutputStream());
        } catch (OWLRendererException ex) {
            ex.printStackTrace();
        }
	}
	
	public String getName() {
		return "OWL Ontology, " + (consistent ? "consistent" : "full");
	}
	
	public boolean isApplicable() {
		return getOntology().getReasoner().getWrappedReasoner() instanceof AceWikiOWLReasoner;
	}
	
	public String getFileSuffix() {
		return ".owl";
	}
	
	public String getContentType() {
		return "application/owl+xml";
	}

}
