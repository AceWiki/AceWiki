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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This exporter generates a lexicon file in the ACE lexicon format containing all the words used
 * in the knowledge base.
 * 
 * @author Tobias Kuhn
 */
public class ACELexiconExporter extends OntologyExporter {
	
	protected void writeContent() throws IOException {
		List<String> lexiconEntries = new ArrayList<String>();
		for (OntologyElement oe : getOntologyElements()) {
			if (oe instanceof ACEOWLOntoElement) {
				for (LexiconEntry le : ((ACEOWLOntoElement) oe).getLexiconEntries()) {
					if (!lexiconEntries.contains(le.toString())) {
						lexiconEntries.add(le.toString());
					}
				}
			}
		}
		Collections.sort(lexiconEntries, String.CASE_INSENSITIVE_ORDER);
		for (String s : lexiconEntries) {
			write(s + ".\n");
		}
	}
	
	public String getName() {
		return "ACE Lexicon";
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getFileSuffix() {
		return ".lex.pl";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
