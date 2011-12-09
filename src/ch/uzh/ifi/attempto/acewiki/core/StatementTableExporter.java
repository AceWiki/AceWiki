// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
import java.util.List;

/**
 * This exporter generates a table of the wiki statements in CSV (comma separated values) format.
 * 
 * @author Tobias Kuhn
 */
public class StatementTableExporter extends OntologyExporter {
	
	protected void writeContent() throws IOException {
		write("PAGE,TYPE,TEXT\n");
		List<OntologyElement> elements = getOntologyElements();
		LanguageUtils.sortOntologyElements(elements);
		for (OntologyElement oe : elements) {
			for (Statement s : oe.getArticle().getStatements()) {
				write(LanguageUtils.getHeading(oe) + ",");
				write(getStatementType(s) + ",");
				write("\"" + s.getText().replaceAll("\"", "\"\"").replaceAll("\\s+", " ") + "\"");
				write("\n");
			}
		}
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getName() {
		return "Statement Table";
	}
	
	public String getFileSuffix() {
		return "-st.csv";
	}
	
	public String getContentType() {
		return "text/plain";
	}
	
	private static String getStatementType(Statement statement) {
		if (statement instanceof Comment) {
			return "comment";
		} else if (statement instanceof Question) {
			return "question";
		} else if (statement instanceof Declaration) {
			Declaration declaration = (Declaration) statement;
			if (declaration.getArticle() == null) {
				return "inferred";
			} else if (declaration.isIntegrated()) {
				return "asserted";
			} else {
				return "unasserted";
			}
		} else {
			throw new RuntimeException("Unknown statement class");
		}
	}

}
