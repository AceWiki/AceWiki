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

import java.io.IOException;

import org.coode.owlapi.owlxml.renderer.OWLXMLRenderer;
import org.semanticweb.owlapi.io.OWLRendererException;
import org.semanticweb.owlapi.model.OWLOntology;

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
	 * @param ontology The ontology.
	 * @param consistent Defines if only the consistent part of the ontology should be considered.
	 */
	public OWLXMLExporter(Ontology ontology, boolean consistent) {
		super(ontology);
		this.consistent = consistent;
	}
	
	protected void writeContent() throws IOException {
		AceWikiOWLReasoner owlReasoner = (AceWikiOWLReasoner) getOntology().getReasoner();
		OWLOntology owlOntology = owlReasoner.exportOWLOntology(consistent);
        try {
            OWLXMLRenderer renderer = new OWLXMLRenderer(owlReasoner.getOWLOntologyManager());
            renderer.render(owlOntology, getOutputStream());
        } catch (OWLRendererException ex) {
            ex.printStackTrace();
        }
	}
	
	public String getText() {
		return "OWL Ontology, " + (consistent ? "consistent" : "full");
	}
	
	public boolean isApplicable() {
		return getOntology().getReasoner() instanceof AceWikiOWLReasoner;
	}
	
	public String getFileSuffix() {
		return ".owl";
	}
	
	public String getContentType() {
		return "application/owl+xml";
	}

}
