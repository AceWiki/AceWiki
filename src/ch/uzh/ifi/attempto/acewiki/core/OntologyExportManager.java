package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.List;

public class OntologyExportManager {
	
	private List<OntologyExporter> exporters = new ArrayList<OntologyExporter>();
	private Ontology ontology;
	
	public OntologyExportManager(Ontology ontology) {
		this.ontology = ontology;
	}
	
	public void addExporter(OntologyExporter exporter) {
		exporter.init(ontology);
		if (exporter.isApplicable()) {
			exporters.add(exporter);
		} else {
			System.err.println("Ignoring non-applicable exporter: " + exporter.getText());
		}
	}
	
	public void removeExporter(OntologyExporter exporter) {
		exporters.remove(exporter);
	}
	
	public void removeAllExporters() {
		exporters.clear();
	}
	
	public List<OntologyExporter> getExporters() {
		return new ArrayList<OntologyExporter>(exporters);
	}

}
