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

package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.AbstractAceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.DummyReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

/**
 * This is an AceWiki engine using GF (Grammatical Framework).
 * 
 * @author Kaarel Kaljurand
 */
public class GFEngine extends AbstractAceWikiEngine {

	private Map<String, GFHandler> languageHandlers = new HashMap<String, GFHandler>();
	private AceWikiReasoner reasoner = new DummyReasoner();
	private GFGrammar gfGrammar;

	/**
	 * Creates a new GF-based AceWiki engine.
	 */
	public GFEngine() {
	}

	public void init(Ontology ontology) {

		URI serviceUri;
		try {
			serviceUri = new URI(ontology.getParameter("service_uri"));
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
		gfGrammar = new GFGrammar(
				serviceUri,
				ontology.getParameter("pgf_name")
				);
		super.init(ontology);
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
		return gfGrammar.getLanguages().toArray(new String[0]);
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
		return new GFDeclaration(GFGrammar.deserialize(serialized), gfGrammar);
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
