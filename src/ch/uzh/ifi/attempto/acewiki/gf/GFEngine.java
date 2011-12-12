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

package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.core.AbstractAceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.DummyReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

// To run AceWiki with this GF engine, add the following lines to web.xml:
//
// <servlet>
//   <servlet-name>MoltoWiki</servlet-name>
//   <servlet-class>ch.uzh.ifi.attempto.acewiki.AceWikiServlet</servlet-class>
//   <init-param>
//     <param-name>engine_class</param-name>
//     <param-value>ch.uzh.ifi.attempto.acewiki.gf.GFOWLEngine</param-value>
//   </init-param>
//   <init-param>
//     <param-name>ontology</param-name>
//     <param-value>molto</param-value>
//   </init-param>
//   <init-param>
//     <param-name>title</param-name>
//     <param-value>Molto Wiki</param-value>
//   </init-param>
// </servlet>
// 
// <servlet-mapping>
//   <servlet-name>MoltoWiki</servlet-name>
//   <url-pattern>/acewikimolto/</url-pattern>
// </servlet-mapping>

/**
 * This is an AceWiki engine using GF (Grammatical Framework).
 * 
 * @author Tobias Kuhn
 */
public class GFEngine extends AbstractAceWikiEngine {
	
	GFHandler languageHandler = new GFHandler();
	AceWikiReasoner reasoner = new DummyReasoner();
	
	public LanguageHandler getLanguageHandler() {
		return languageHandler;
	}
	
	public AceWikiReasoner getReasoner() {
		return reasoner;
	}

	public OntologyElement createOntologyElement(String type) {
		// TODO
		return null;
	}
	
	public Sentence createSentence(String serialized) {
		return new GFDeclaration(serialized);
	}
	
	public Sentence createAssignmentSentence(Individual ind, Concept concept) {
		// TODO
		return null;
	}
	
	public Sentence createHierarchySentence(Concept subConcept, Concept superConcept) {
		// TODO
		return null;
	}

}
