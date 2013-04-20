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

import java.io.IOException;

import org.coode.owlapi.functionalrenderer.OWLFunctionalSyntaxRenderer;
import org.semanticweb.owlapi.io.OWLRendererException;
import org.semanticweb.owlapi.model.OWLOntology;

import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;

public class OWLFunctionalSyntaxExporter extends OntologyExporter {

	private final boolean mConsistent;

	public OWLFunctionalSyntaxExporter(boolean consistent) {
		mConsistent = consistent;
	}

	protected void writeContent(String language) throws IOException {
		AbstractAceWikiOWLReasoner owlReasoner = (AbstractAceWikiOWLReasoner) getOntology()
				.getReasoner().getWrappedReasoner();
		OWLOntology owlOntology = owlReasoner.exportOWLOntology(mConsistent);
		try {
			OWLFunctionalSyntaxRenderer renderer = new OWLFunctionalSyntaxRenderer(owlReasoner.getOWLOntologyManager());
			renderer.render(owlOntology, getOutputStream());
		} catch (OWLRendererException ex) {
			ex.printStackTrace();
		}
	}

	public String getName() {
		return "OWL Ontology in functional syntax, " + (mConsistent ? "consistent" : "full");
	}

	public boolean isApplicable() {
		return getOntology().getReasoner().getWrappedReasoner() instanceof AbstractAceWikiOWLReasoner;
	}

	public String getFileSuffix() {
		return ".owl";
	}

	public String getContentType() {
		return "text/plain";
	}

}