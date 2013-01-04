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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Maps;

import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfService;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultBrowse;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultComplete;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultGrammar;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultLinearize;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultParse;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultRandom;
import ch.uzh.ifi.attempto.gfservice.GfStorage;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebService;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebStorage;


/**
 * This class wraps GF features of a particular GF grammar.
 *
 * @author Kaarel Kaljurand
 */
public class GFGrammar {

	// Note that true can remove (always removes?) lins
	// which are not available in all the concretes,
	// i.e. if you add a lin then you need to add it too all the concretes
	// otherwise you cannot use it in a sentence.
	private final static boolean OPTIMIZE_PGF = true;

	private final static char GF_TOKEN_SEPARATOR = ' ';
	private final static char GF_TREE_SEPARATOR = '|';

	private final static int GF_PARSE_LIMIT = 10;

	public final static Splitter GF_TOKEN_SPLITTER = Splitter.on(GF_TOKEN_SEPARATOR);

	private final GfService mGfService;
	private final GfStorage mGfStorage;
	private final String mCat;
	private final String mDir;

	// List of languages, i.e. concrete language modules.
	// This is instantiated/finalized at construction-time.
	// TODO: allow languages to be added and removed during wiki runtime
	private final Set<String> mLanguages;

	private final Map<String, Set<String>> mCacheCatProducers = Maps.newHashMap();
	private final Map<String, Set<String>> mCacheCatConsumers = Maps.newHashMap();


	/**
	 * Creates a new GF grammar object.
	 */
	public GFGrammar(URI serviceUri, String pgfName, String cat) {
		mGfService = new GfWebService(serviceUri, pgfName);
		mGfStorage = new GfWebStorage(serviceUri);
		mCat = cat;
		mDir = getDir(pgfName);
		mLanguages = makeLanguages();
	}


	public GfServiceResultGrammar getGrammar() throws GfServiceException {
		return mGfService.grammar();
	}


	/**
	 * @return set of names of the concrete languages defined in the grammar
	 */
	public Set<String> getLanguages() {
		return mLanguages;
	}


	/**
	 * Parses the given text in the given language.
	 *
	 * @param text The text.
	 * @param language The language.
	 * @return The parse result.
	 * @throws GfServiceException
	 */
	public Set<String> parse(String text, String language) throws GfServiceException {
		GfServiceResultParse result = mGfService.parse(mCat, text, language, GF_PARSE_LIMIT);
		return result.getTrees(language);
	}


	public String random() throws GfServiceException {
		return random(1).iterator().next();
	}


	public List<String> random(int limit) throws GfServiceException {
		GfServiceResultRandom result = mGfService.random(mCat, limit);
		return result.getTrees();
	}


	/**
	 * Deserializes a serialized representation into a parse state.
	 *
	 * @param serialized The serialized representation
	 * @return The parse state.
	 */
	public static TreeSet deserialize(String serialized) {
		return new TreeSet(Splitter.on(GF_TREE_SEPARATOR).split(serialized));
	}


	public Set<String> linearize(String tree, String language) throws GfServiceException {
		GfServiceResultLinearize result = mGfService.linearize(tree, language);
		return result.getTexts(language);
	}


	public Map<String, Set<String>> linearize(String tree) throws GfServiceException {
		GfServiceResultLinearize result = mGfService.linearize(tree, null);
		return result.getTexts();
	}


	public Set<String> complete(List<String> tokens, String language) throws GfServiceException {
		return complete(mCat, tokens, language);
	}


	/**
	 * <p>This method tries to return a set that contains more than one element, i.e.
	 * if there is only one (unambiguous) completion then "complete" is automatically
	 * called again. In this case the result set contains multi-token completions.
	 * There is a limit of 15 tokens to each completion.</p>
	 *
	 * @param cat start category for the parser
	 * @param tokens list of tokens the last of which is to be completed
	 * @param language language of the input tokens
	 * @return list of possible completions
	 * @throws GfServiceException
	 */
	public Set<String> complete(String cat, List<String> tokens, String language) throws GfServiceException {
		// Remove the last argument if this behavior turns out to be confusing
		GfServiceResultComplete result = mGfService.complete(cat, getCompletionInput(tokens), language, null, 15);
		return result.getCompletions(language);
	}

	public String abstrtree(String tree) throws GfServiceException {
		return mGfService.abstrtree(tree).getDataUri();
	}

	public String parsetree(String tree, String from) throws GfServiceException {
		return mGfService.parsetree(tree, from).getDataUri();
	}

	public String alignment(String tree) throws GfServiceException {
		return mGfService.alignment(tree).getDataUri();
	}


	public Set<String> getProducers(String cat) throws GfServiceException {
		Set<String> producers = mCacheCatProducers.get(cat);
		if (producers == null) {
			producers = addResultBrowse(cat, true);
		}
		return producers;
	}


	public Set<String> getConsumers(String cat) throws GfServiceException {
		Set<String> consumers = mCacheCatConsumers.get(cat);
		if (consumers == null) {
			consumers = addResultBrowse(cat, false);
		}
		return consumers;
	}


	/**
	 * Serializes a given parse state.
	 *
	 * @param parseState The parse state.
	 * @return The serialization.
	 */
	public static String serialize(TreeSet parseState) {
		return Joiner.on(GF_TREE_SEPARATOR).join(parseState.getTrees());
	}


	public GfParseResult parseGfModule(GfModule gfModule) throws GfServiceException {
		return mGfStorage.parse(gfModule);
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
			return mGfStorage.update(mDir, mCat, OPTIMIZE_PGF, mLanguages, gfModule);
		}
		// Otherwise just upload it and recompile the existing concrete modules.
		mGfStorage.upload(mDir, gfModule);
		return mGfStorage.update(mDir, mCat, OPTIMIZE_PGF, mLanguages);
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


	private static String getCompletionInput(List<String> tokens) {
		if (tokens.isEmpty()) {
			return "";
		}
		return Joiner.on(GF_TOKEN_SEPARATOR).join(tokens) + GF_TOKEN_SEPARATOR;
	}


	private Set<String> makeLanguages() {
		try {
			return mGfService.grammar().getLanguages().keySet();
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return Collections.emptySet();
	}


	private Set<String> addResultBrowse(String cat, boolean returnProducers) throws GfServiceException {
		GfServiceResultBrowse result = mGfService.browse(cat);
		Set<String> producers = result.getProducers();
		Set<String> consumers = result.getConsumers();
		mCacheCatProducers.put(cat, producers);
		mCacheCatConsumers.put(cat, consumers);
		if (returnProducers) {
			return producers;
		}
		return consumers;
	}
}
