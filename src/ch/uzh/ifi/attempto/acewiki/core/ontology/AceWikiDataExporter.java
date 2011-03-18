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

/**
 * This exporter generates a file that contains the complete wiki data in a special AceWiki data
 * format.
 * 
 * @author Tobias Kuhn
 */
public class AceWikiDataExporter extends OntologyExporter {
	
	/**
	 * Creates a new AceWiki data exporter.
	 * 
	 * @param ontology The ontology.
	 */
	public AceWikiDataExporter(Ontology ontology) {
		super(ontology);
	}
	
	protected void writeContent() throws IOException {
		for (OntologyElement oe : getOntologyElements()) {
			write(oe.serialize(false));
			write("\n");
		}
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getText() {
		return "AceWiki data file";
	}
	
	public String getFileSuffix() {
		return ".acewikidata";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
