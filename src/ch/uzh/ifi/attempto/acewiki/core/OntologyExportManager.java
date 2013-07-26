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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.List;

/**
 * This class organizes a collection of ontology exporters.
 * 
 * @author Tobias Kuhn
 */
public class OntologyExportManager {
	private final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(this.getClass());
	
	private List<OntologyExporter> exporters = new ArrayList<OntologyExporter>();
	private Ontology ontology;
	
	/**
	 * Creates a new ontology export manager for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	public OntologyExportManager(Ontology ontology) {
		this.ontology = ontology;
	}
	
	/**
	 * Adds an ontology exporter.
	 * 
	 * @param exporter The ontolgy exporter to be added.
	 */
	public void addExporter(OntologyExporter exporter) {
		exporter.init(ontology);
		if (exporter.isApplicable()) {
			exporters.add(exporter);
		} else {
			log.warn("Ignoring non-applicable exporter: {}", exporter.getName());
		}
	}
	
	/**
	 * Removes an ontology exporter.
	 * 
	 * @param exporter The ontolgy exporter to be removed.
	 */
	public void removeExporter(OntologyExporter exporter) {
		exporters.remove(exporter);
	}
	
	/**
	 * Removes all ontology exporters.
	 */
	public void removeAllExporters() {
		exporters.clear();
	}
	
	/**
	 * Returns all ontology exporters.
	 * 
	 * @return All ontology exporters.
	 */
	public List<OntologyExporter> getExporters() {
		return new ArrayList<OntologyExporter>(exporters);
	}

}
