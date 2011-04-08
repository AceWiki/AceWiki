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

package ch.uzh.ifi.attempto.acewiki.core;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

/**
 * This exporter generates a table of the wiki statements in CSV (comma separated values) format.
 * 
 * @author Tobias Kuhn
 */
public class StatementTableExporter extends OntologyExporter {

	/**
	 * Creates a new statement table exporter.
	 * 
	 * @param ontology The ontology.
	 */
	public StatementTableExporter(Ontology ontology) {
		super(ontology);
	}
	
	protected void writeContent() throws IOException {
		write("PAGE,TYPE,TEXT\n");
		List<OntologyElement> elements = getOntologyElements();
		Collections.sort(elements);
		for (OntologyElement oe : elements) {
			for (Statement s : oe.getArticle().getStatements()) {
				// Replace quotes " by two quotes "" and reduce blank spaces
				String t = s.getText().replaceAll("\"", "\"\"").replaceAll("\\s+", " ");
				write(oe.getWord() + "," + s.getType() + ",\"" + t + "\"\n");
			}
		}
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getText() {
		return "Statement Table";
	}
	
	public String getFileSuffix() {
		return "-st.csv";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
