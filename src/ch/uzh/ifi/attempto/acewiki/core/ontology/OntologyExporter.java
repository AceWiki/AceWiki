// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.coode.owlapi.owlxml.renderer.OWLXMLRenderer;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologySetProvider;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class can export AceWiki ontologies in different formats.
 * 
 * @author Tobias Kuhn
 */
public class OntologyExporter {
	
	private final Ontology ontology;
	private static int owlOntologyID = 0;
	
	/**
	 * Creates a new exporter for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	public OntologyExporter(Ontology ontology) {
		this.ontology = ontology;
	}
	
	/**
	 * Returns the complete ontology as an OWL/XML formatted string.
	 * 
	 * @param consistent If true then only the consistent part of the ontology is included.
	 * @return A string that contains the complete ontology in OWL/XML format.
	 */
	public synchronized String getOWLOntologyAsXML(boolean consistent) {
        StringWriter sw = new StringWriter();
        try {
            OWLXMLRenderer renderer = new OWLXMLRenderer(ontology.getOWLOntologyManager());
            renderer.render(getOWLOntology(consistent), sw);
            sw.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
		return sw.toString();
	}
	
	/**
	 * Returns an OWL ontology object that contains the complete ontology.
	 * 
	 * @param consistent If true then only the consistent part of the ontology is included.
	 * @return An OWL ontology object containing the complete ontology.
	 */
	public synchronized OWLOntology getOWLOntology(final boolean consistent) {
		OWLOntology owlOntology = null;
		
		synchronized (ontology) {
			OWLOntologySetProvider setProvider = new OWLOntologySetProvider() {
				
				public Set<OWLOntology> getOntologies() {
					HashSet<OWLOntology> ontologies = new HashSet<OWLOntology>();
					for (OntologyElement el : ontology.getOntologyElements()) {
						for (Sentence s : el.getSentences()) {
							if (s instanceof Question || !s.isOWL()) continue;
							if (consistent && (!s.isReasonerParticipant() || !s.isIntegrated())) {
								continue;
							}
							
							OWLOntology o = s.getOWLOntology();
							if (o != null) ontologies.add(o);
						}
					}
					ontologies.add(ontology.getDifferentIndividualsAxiom());
					return ontologies;
				}
				
			};
			
			owlOntologyID++;
			try {
				OWLOntologyMerger ontologyMerger = new OWLOntologyMerger(setProvider);
				owlOntology = ontologyMerger.createMergedOntology(
						ontology.getOWLOntologyManager(),
						IRI.create("http://attempto.ifi.uzh.ch/default/" + owlOntologyID)
					);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		
		return owlOntology;
	}
	
	/**
	 * Returns the complete ontology as one ACE text.
	 * 
	 * @param consistent If true then only the consistent part of the ontology is included.
	 * @return A string that contains the complete ontology as an ACE text.
	 */
	public synchronized String getACEText(boolean consistent) {
		String t = "";
		List<OntologyElement> elements = ontology.getOntologyElements();
		Collections.sort(elements);
		for (OntologyElement oe : elements) {
			String heading = "\n# " + oe.getHeadword() + "\n\n";
			for (Sentence s : oe.getSentences()) {
				if (!consistent || (s.isIntegrated() && s.isReasonerParticipant()) ) {
					if (heading != null) {
						t += heading;
						heading = null;
					}
					t += s.getText() + "\n\n";
				}
			}
		}
		return t;
	}
	
	/**
	 * Returns the lexicon definition for all ontology elements in the ACE lexicon format.
	 * 
	 * @return A string that contains the lexicon definition.
	 */
	public synchronized String getLexiconDef() {
		List<String> lexiconEntries = new ArrayList<String>();
		for (OntologyElement oe : ontology.getOntologyElements()) {
			for (LexiconEntry le : oe.getLexiconEntries()) {
				if (!lexiconEntries.contains(le.toString())) {
					lexiconEntries.add(le.toString());
				}
			}
		}
		Collections.sort(lexiconEntries);
		String t = "";
		for (String s : lexiconEntries) {
			t += s + ".\n";
		}
		return t;
	}

}
