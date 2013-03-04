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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.AbstractAceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

/**
 * This AceWiki engine uses a GF (Grammatical Framework) grammar.
 * 
 * @author Kaarel Kaljurand
 */
public class GfEngine extends AbstractAceWikiEngine {

	// TODO: support the creation of dynamic queries
	// public static final String TYPE_QUERY = "query";

	private Map<String, GfHandler> languageHandlers = new HashMap<String, GfHandler>();

	private String[] languages;

	private GfGrammar gfGrammar;

	/**
	 * Creates a new GF-based AceWiki engine.
	 */
	public GfEngine() {
		setLexicalTypes(GeneralTopic.NORMAL_TYPE, TypeGfModule.INTERNAL_TYPE);
	}

	public void init(Ontology ontology) {
		gfGrammar = new GfGrammar(ontology);

		// Sort languages alphabetically according to displayed language name:
		List<String> languageNames = new ArrayList<>();
		Map<String,String> languageMap = new HashMap<>();
		for (String l : gfGrammar.getLanguages()) {
			String n = getLanguageHandler(l).getLanguageName();
			languageNames.add(n);
			languageMap.put(n, l);
		}
		Collections.sort(languageNames);
		languages = new String[languageNames.size()];
		int i = 0;
		for (String l : languageNames) {
			languages[i++] = languageMap.get(l);
		}

		super.init(ontology);
	}


	public LanguageHandler getLanguageHandler(String language) {
		GfHandler lh = languageHandlers.get(language);
		if (lh == null) {
			lh = new GfHandler(language, gfGrammar);
			languageHandlers.put(language, lh);
		}
		return lh;
	}


	public String[] getLanguages() {
		return languages;
	}


	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	public GfGrammar getGfGrammar() {
		return gfGrammar;
	}

	// TODO: implement a reasoner that does ACE reasoning if ACE is
	// one of the languages
	public AceWikiReasoner getReasoner() {
		return null;
	}

	public OntologyElement createOntologyElement(String type) {
		if (GeneralTopic.NORMAL_TYPE.equals(type)) {
			return GeneralTopic.makeNormal("");
		} else if (TypeGfModule.hasType(type)) {
			return new TypeGfModule();
		}
		return null;
	}


	public Sentence createSentence(String serialized) {
		return new GfDeclaration(gfGrammar, GfGrammar.deserialize(serialized));
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
