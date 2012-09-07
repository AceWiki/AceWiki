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

import ch.uzh.ifi.attempto.acewiki.aceowl.ProperNameIndividual;
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

	public static final String TYPE_ARTICLE = "article";
	// TODO: support the creation of dynamic queries
	// public static final String TYPE_QUERY = "query";
	public static final String TYPE_TEST = "test";

	private Map<String, GFHandler> languageHandlers = new HashMap<String, GFHandler>();
	private AceWikiReasoner reasoner = new DummyReasoner();
	private GFGrammar gfGrammar;

	/**
	 * Creates a new GF-based AceWiki engine.
	 */
	public GFEngine() {
		setLexicalTypes(TYPE_ARTICLE, TYPE_TEST);
	}

	public void init(Ontology ontology) {

		URI serviceUri;
		try {
			serviceUri = new URI(ontology.getParameter("service_uri"));
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}

		// Note: start_cat can be null, in this case the default start category is used
		gfGrammar = new GFGrammar(
				serviceUri,
				ontology.getParameter("pgf_name"),
				ontology.getParameter("start_cat")
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

	// TODO: return sensible objects
	public OntologyElement createOntologyElement(String type) {
		if (type.equals(TYPE_ARTICLE)) {
			return new TypeArticle();
		} else if (type.equals(TYPE_TEST)) {
			return new ProperNameIndividual();
		}
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
