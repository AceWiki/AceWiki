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

import java.io.IOException;
import java.util.Collections;
import java.util.List;

/**
 * This exporter generates a table of the lexicon entries in CSV (comma separated values) format.
 * 
 * @author Tobias Kuhn
 */
public class LexiconTableExporter extends OntologyExporter {
	
	/**
	 * Creates a new lexicon table exporter.
	 * 
	 * @param ontology The ontology.
	 */
	public LexiconTableExporter(Ontology ontology) {
		super(ontology);
	}
	
	protected void writeContent() throws IOException {
		write("TYPE,WORD_FORM_1,WORD_FORM_2,WORD_FORM_3\n");
		List<OntologyElement> elements = getOntologyElements();
		Collections.sort(elements);
		for (OntologyElement oe : elements) {
			write(oe.getInternalType());
			for (String word : oe.getExternalWordList()) {
				if (word == null) {
					write(",");
				} else {
					write("," + word);
				}
			}
			write("\n");
		}
	}
	
	public String getFileSuffix() {
		return "-lex.csv";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
