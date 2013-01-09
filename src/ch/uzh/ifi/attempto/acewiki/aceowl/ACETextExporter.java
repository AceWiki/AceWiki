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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.io.IOException;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

/**
 * This exporter generates a file that contains the complete ontology as one ACE text.
 * 
 * @author Tobias Kuhn
 */
public class ACETextExporter extends OntologyExporter {

	private final boolean consistent;
	
	/**
	 * Creates a new ACE text exporter.
	 * 
	 * @param consistent Defines if only the consistent part of the ontology should be considered.
	 */
	public ACETextExporter(boolean consistent) {
		this.consistent = consistent;
	}
	
	protected void writeContent(String language) throws IOException {
		List<OntologyElement> elements = getOntologyElements();
		LanguageUtils.sortOntologyElements(elements);
		for (OntologyElement oe : elements) {
			String heading = "\n# " + LanguageUtils.getHeading(oe) + "\n\n";
			for (Sentence sentence : oe.getArticle().getSentences()) {
				ACESentence s = (ACESentence) sentence;
				if (!consistent || (s.isIntegrated() && s.isReasonable()) ) {
					if (heading != null) {
						write(heading);
						heading = null;
					}
					write(s.getText() + "\n\n");
				}
			}
		}
	}
	
	public String getName() {
		return "ACE Text, " + (consistent ? "consistent" : "full");
	}
	
	public boolean isApplicable() {
		return true;
	}
	
	public String getFileSuffix() {
		return ".ace.txt";
	}
	
	public String getContentType() {
		return "text/plain";
	}

}
