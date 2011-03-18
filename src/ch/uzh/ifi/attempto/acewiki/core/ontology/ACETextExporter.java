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
import java.util.Collections;
import java.util.List;

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
	 * @param ontology The ontology.
	 * @param consistent Defines if only the consistent part of the ontology should be considered.
	 */
	public ACETextExporter(Ontology ontology, boolean consistent) {
		super(ontology);
		this.consistent = consistent;
	}
	
	protected void writeContent() throws IOException {
		List<OntologyElement> elements = getOntologyElements();
		Collections.sort(elements);
		for (OntologyElement oe : elements) {
			String heading = "\n# " + oe.getHeadword() + "\n\n";
			for (Sentence s : oe.getSentences()) {
				if (!consistent || (s.isIntegrated() && s.isReasonerParticipant()) ) {
					if (heading != null) {
						write(heading);
						heading = null;
					}
					write(s.getText() + "\n\n");
				}
			}
		}
	}
	
	public String getText() {
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
