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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 * This abstract class is used to export AceWiki ontologies in different formats.
 * 
 * @author Tobias Kuhn
 */
public abstract class OntologyExporter {
	
	private final Ontology ontology;
	private OutputStream outputStream;
	private Writer writer;
	
	/**
	 * Initializes a new exporter for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	protected OntologyExporter(Ontology ontology) {
		this.ontology = ontology;
	}
	
	/**
	 * Writes the export content into the given output stream. The stream is closed at the end.
	 * 
	 * @param outputStream The output stream.
	 */
	public void export(OutputStream outputStream) {
		this.outputStream = outputStream;
		try {
			synchronized (ontology) {
				writeContent();
			}
			if (writer != null) writer.close();
			outputStream.close();
			this.writer = null;
			this.outputStream = null;
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * This internal method should write the export content.
	 * 
	 * @throws IOException when an IO problem occurs.
	 */
	protected abstract void writeContent() throws IOException;
	
	/**
	 * Returns the file suffix for the given export type.
	 * 
	 * @return The file suffix.
	 */
	public abstract String getFileSuffix();
	
	/**
	 * Returns the content type for the given export type.
	 * 
	 * @return The content type.
	 */
	public abstract String getContentType();
	
	/**
	 * Returns the AceWiki ontology for this exporter.
	 * 
	 * @return The AceWiki ontology.
	 */
	protected Ontology getOntology()  {
		return ontology;
	}
	
	/**
	 * Returns the OWL ontology manager.
	 * 
	 * @return The OWL ontology manager.
	 */
	protected OWLOntologyManager getOWLOntologyManager() {
		return ontology.getOWLOntologyManager();
	}
	
	/**
	 * Returns the list of all ontology elements.
	 * 
	 * @return The ontology elements.
	 */
	protected List<OntologyElement> getOntologyElements() {
		return ontology.getOntologyElements();
	}
	
	/**
	 * Returns the current output stream.
	 * 
	 * @return The current output stream.
	 */
	protected OutputStream getOutputStream() {
		return outputStream;
	}
	
	/**
	 * Writes the given string into the current output stream.
	 * 
	 * @param str The string to be written.
	 * @throws IOException when an IO problem occurs.
	 */
	protected void write(String str) throws IOException {
		if (writer == null) {
			writer = new BufferedWriter(new OutputStreamWriter(outputStream));
		}
		writer.write(str);
	}

}
