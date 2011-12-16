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

import java.util.HashMap;
import java.util.Map;

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
//   <servlet-name>MoltoWikiEng</servlet-name>
//   <servlet-class>ch.uzh.ifi.attempto.acewiki.AceWikiServlet</servlet-class>
//   <init-param>
//     <param-name>engine_class</param-name>
//     <param-value>ch.uzh.ifi.attempto.acewiki.gf.GFEngine</param-value>
//   </init-param>
//   <init-param>
//     <param-name>language</param-name>
//     <param-value>Eng</param-value>
//   </init-param>
//   <init-param>
//     <param-name>ontology</param-name>
//     <param-value>molto</param-value>
//   </init-param>
//   <init-param>
//     <param-name>title</param-name>
//     <param-value>Molto Wiki in English</param-value>
//   </init-param>
// </servlet>
// 
// <servlet-mapping>
//   <servlet-name>MoltoWikiEng</servlet-name>
//   <url-pattern>/moltoeng/</url-pattern>
// </servlet-mapping>

/**
 * This is an AceWiki engine using GF (Grammatical Framework).
 * 
 * @author Tobias Kuhn
 */
public class GFEngine extends AbstractAceWikiEngine {
	
	private Map<String, GFHandler> languageHandlers = new HashMap<String, GFHandler>();
	private AceWikiReasoner reasoner = new DummyReasoner();
	private GFGrammar gfGrammar;
	
	/**
	 * Creates a new GF-based AceWiki engine.
	 */
	public GFEngine() {
		gfGrammar = new GFGrammar("ch/uzh/ifi/attempto/acewiki/gf/", "Foods", "Eng");
	}
	
	public LanguageHandler getLanguageHandler(String language) {
		GFHandler lh = languageHandlers.get(language);
		if (lh == null) {
			lh = new GFHandler(language, gfGrammar);
			languageHandlers.put(language, lh);
		}
		return lh;
	}

	public String[] getLanguages() {
		return new String[] {"Eng", "Ger", "Ita"};
	}
	
	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	public GFGrammar getGFGrammar() {
		return gfGrammar;
	}
	
	public AceWikiReasoner getReasoner() {
		return reasoner;
	}

	public OntologyElement createOntologyElement(String type) {
		// TODO
		return null;
	}
	
	public Sentence createSentence(String serialized) {
		return new GFDeclaration(gfGrammar.deserialize(serialized), gfGrammar);
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
