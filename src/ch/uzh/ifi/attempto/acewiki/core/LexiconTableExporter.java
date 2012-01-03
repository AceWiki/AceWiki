// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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
 * This exporter generates a table of the lexicon entries in CSV (comma separated values) format.
 * 
 * @author Tobias Kuhn
 */
public class LexiconTableExporter extends OntologyExporter {
	
	protected void writeContent() throws IOException {
		write("TYPE,HEADWORDS,SERIALIZED\n");
		List<OntologyElement> elements = getOntologyElements();
		LanguageUtils.sortOntologyElements(elements);
		for (OntologyElement oe : elements) {
			write(oe.getInternalType() + ",");
			write(LanguageUtils.getHeading(oe) + ",");
			write("\"" + oe.serializeWords().replaceAll("\"", "\"\"") + "\"");
			write("\n");
		}
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getName() {
		return "Lexicon Table";
	}
	
	public String getFileSuffix() {
		return "-lex.csv";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
