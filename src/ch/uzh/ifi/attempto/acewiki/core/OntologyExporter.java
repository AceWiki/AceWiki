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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

/**
 * This abstract class is used to export AceWiki ontologies in different formats.
 * 
 * @author Tobias Kuhn
 */
public abstract class OntologyExporter {
	
	private Ontology ontology;
	private OutputStream outputStream;
	private Writer writer;
	
	/**
	 * Creates a new exporter.
	 */
	protected OntologyExporter() {
	}
	
	void init(Ontology ontology) {
		this.ontology = ontology;
	}
	
	/**
	 * Writes the export content into the given output stream. The stream is closed at the end.
	 * 
	 * @param outputStream The output stream.
	 * @param language The language used for exporting (ignored by many exporters).
	 */
	public void export(OutputStream outputStream, String language) {
		this.outputStream = outputStream;
		try {
			writeContent(language);
			if (writer != null) writer.close();
			outputStream.close();
			this.writer = null;
			this.outputStream = null;
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * Returns whether this ontology exporter is applicable in the current context.
	 * 
	 * @return true if this ontology exporter is applicable.
	 */
	public abstract boolean isApplicable();
	
	/**
	 * Returns the name of this exporter as shown to the user, or a text key for localization.
	 * 
	 * @return The name of this exporter.
	 */
	public abstract String getName();
	
	/**
	 * This internal method should write the export content.
	 * 
	 * @param language The current language.
	 * @throws IOException when an IO problem occurs.
	 */
	protected abstract void writeContent(String language) throws IOException;
	
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
	 * Returns the list of all ontology elements.
	 * 
	 * @return The ontology elements.
	 */
	protected List<OntologyElement> getOntologyElements() {
		return ontology.getOntologyElements();
	}

	/**
	 * Returns all instances of class <code>type</code> in the current list of ontology elements.
	 * The returned iterable has elements whose class is <code>type</code> or a subclass of <code>type</code>.
	 * The returned iterable's iterator does not support <code>remove()</code>.
	 *
	 * @param type the type of ontology elements desired
	 * @return an unmodifiable iterable containing all the ontology elements that are of the requested type
	 */
	protected <T> Iterable<T> getOntologyElements(Class<T> type) {
		return ontology.getOntologyElements(type);
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
