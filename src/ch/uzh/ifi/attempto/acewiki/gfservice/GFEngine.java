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
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorage;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebStorage;

/**
 * This is an AceWiki engine using GF (Grammatical Framework).
 * 
 * @author Kaarel Kaljurand
 */
public class GFEngine extends AbstractAceWikiEngine {

	private final Logger mLogger = LoggerFactory.getLogger(GFEngine.class);

	// TODO: support the creation of dynamic queries
	// public static final String TYPE_QUERY = "query";

	// TODO: remove this
	public static final String TYPE_TEST = "test";

	private Map<String, GFHandler> languageHandlers = new HashMap<String, GFHandler>();
	private AceWikiReasoner reasoner = new DummyReasoner();

	private GFGrammar gfGrammar;

	// List of languages, i.e. concrete language modules.
	// This is instantiated at init-time.
	// TODO: allow languages to be added and removed during wiki runtime
	private Set<String> mLanguages;
	private GfStorage mStorage;
	private String mDir = null;

	/**
	 * Creates a new GF-based AceWiki engine.
	 */
	public GFEngine() {
		setLexicalTypes(TypeArticle.INTERNAL_TYPE, TypeGfModule.INTERNAL_TYPE, TYPE_TEST);
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
		mLanguages = gfGrammar.getLanguages();
		mStorage = new GfWebStorage(serviceUri);
		mDir = getDir(ontology.getParameter("pgf_name"));
		mLogger.info("GfEngine: init: mDir = '{}'", mDir);

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
		return mLanguages.toArray(new String[0]);
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
		if (TypeArticle.hasType(type)) {
			return new TypeArticle();
		} else if (TypeGfModule.hasType(type)) {
			return new TypeGfModule();
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


	public GfParseResult parseGfModule(GfModule gfModule) throws GfServiceException {
		return mStorage.parse(gfModule);
	}


	/**
	 * Updates the grammar based on the given GF module, which is either
	 * a new component of the grammar or which has undergone modifications
	 * and needs to be reintegrated.
	 *
	 * @param gfModule new or modified grammar module
	 * @return GfStorageResult
	 * @throws GfServiceException
	 */
	public GfStorageResult integrateGfModule(GfModule gfModule) throws GfServiceException {
		// If the module is a concrete syntax module then
		// update it in the context of other concrete modules.
		if (mLanguages.contains(gfModule.getName())) {
			return mStorage.update(mDir, gfModule, mLanguages);
		}
		// Otherwise just upload it and recompile the existing concrete modules.
		mStorage.upload(mDir, gfModule);
		return mStorage.update(mDir, mLanguages);
	}


	public boolean isGrammarEditable() {
		return ! (mDir == null);
	}


	// TODO: we assume that editable directories have a certain form
	private static String getDir(String str) {
		Pattern p = Pattern.compile("(/tmp/.+)/.+");
		Matcher m = p.matcher(str);
		if (m.matches()) {
			return m.group(1);
		}
		return null;
	}

}