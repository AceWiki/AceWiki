// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Functions;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Ordering;
import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfService;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultBrowseAll;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultComplete;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultGrammar;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultLinearize;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultLinearizeAll;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultParse;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultRandom;
import ch.uzh.ifi.attempto.gfservice.GfStorage;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;
import ch.uzh.ifi.attempto.gfservice.GfStorageResultLs;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebService;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebStorage;


/**
 * This class wraps GF features of a particular GF grammar.
 *
 * @author Kaarel Kaljurand
 */
public class GfGrammar {

	// TODO: let the user configure the size of the ambiguity
	public final static int GF_PARSE_LIMIT = 10;

	private final int LINEARIZE_ALL_QUERY_LIMIT;

	private final Logger mLogger = LoggerFactory.getLogger(GfGrammar.class);

	// Some naming conventions
	public final static String PREFIX_DISAMB = "Disamb";
	public final static String SUFFIX_APE = "Ape";
	public final static String EXTENSION_GF = ".gf";
	public final static String EXTENSION_GFO = ".gfo";

	// Note that true can remove (always removes?) lins
	// which are not available in all the concretes,
	// i.e. if you add a lin then you need to add it too all the concretes
	// otherwise you cannot use it in a sentence.
	private final static boolean OPTIMIZE_PGF = true;

	private final static int GF_APE_FIELD_LOGICAL_SYMBOL = 3;

	private final static char GF_TOKEN_SEPARATOR = ' ';
	private final static char GF_TREE_SEPARATOR = '|';
	private final static char GF_APE_SEPARATOR = '|';
	private final static String GF_SERIALIZATION_SEPARATOR = "||";

	public final static Joiner GF_TREE_JOINER = Joiner.on(GF_TREE_SEPARATOR);
	public final static Joiner GF_SERIALIZATION_JOINER = Joiner.on(GF_SERIALIZATION_SEPARATOR).useForNull("");
	public final static Joiner GF_TOKEN_JOINER = Joiner.on(GF_TOKEN_SEPARATOR);
	public final static Splitter GF_TREE_SPLITTER = Splitter.on(GF_TREE_SEPARATOR).omitEmptyStrings();
	public final static Splitter GF_APE_SPLITTER = Splitter.on(GF_APE_SEPARATOR);
	public final static Splitter GF_SERIALIZATION_SPLITTER = Splitter.on(GF_SERIALIZATION_SEPARATOR);
	public final static Splitter GF_TOKEN_SPLITTER = Splitter.on(GF_TOKEN_SEPARATOR);

	private final GfService mGfService;
	private final GfStorage mGfStorage;
	private final String mCat;
	private final String mDir;

	private GfServiceResultGrammar mGfServiceResultGrammar;
	private GfServiceResultBrowseAll mGfServiceResultBrowseAll;

	private final Map<String, Multimap<String, String>> langToTokenToCats = Maps.newHashMap();
	private final Map<String, Map<String, String>> langToIriToToken = Maps.newHashMap();

	// TODO: could use a Multiset instead but there does not seem to be a
	// short way to get out k-largest elements.
	private final Map<String, Integer> mCatToSize = Maps.newHashMap();

	public GfGrammar(Ontology ontology) {
		URI serviceUri;
		try {
			serviceUri = new URI(ontology.getParameter("service_uri"));
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}

		String pgfName = ontology.getParameter("pgf_name");
		mGfService = new GfWebService(serviceUri, pgfName);
		mGfStorage = new GfWebStorage(serviceUri);
		// Note: start_cat can be null, in this case the default start category is used
		mCat = ontology.getParameter("start_cat");
		mDir = getDir(pgfName);
		LINEARIZE_ALL_QUERY_LIMIT = ontology.getParameterAsInt("linearize_all_query_limit");

		try {
			refreshGrammarInfo();
			refreshLangToTokenToCats();
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	public GfServiceResultGrammar getGrammar() {
		return mGfServiceResultGrammar;
	}


	/**
	 * @return set of names of the concrete languages defined in the grammar
	 */
	public Set<String> getLanguages() {
		if (mGfServiceResultGrammar == null) {
			return Collections.emptySet();
		}
		return mGfServiceResultGrammar.getLanguages().keySet();
	}


	/**
	 * @param grammar GF grammar
	 * @return {@code true} iff the given grammar contains a concrete language with suffix SUFFIX_APE
	 */
	public boolean isAceCompatible() {
		return getLanguages().contains(mGfServiceResultGrammar.getName() + SUFFIX_APE);
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
	 * Serializes the GF wiki entry, given as 3 components:
	 *   - language (e.g. GeographyEng)
	 *   - sentence as string (e.g. "Germany is a country .")
	 *   - set of corresponding trees
	 *
	 * The format is:
	 *
	 *  lang||text||tree1|tree2|...|treeN
	 *
	 * This is more robust, e.g. if the tree cannot be linearized anymore
	 * because grammar was refactored then we could try to parse the
	 * sentence. Also the sentence could be shown if the tree
	 * has multiple variant lins.
	 */
	public static String serialize(GfWikiEntry entry) {
		return GF_SERIALIZATION_JOINER.join(
				entry.getLanguage(),
				entry.getText(),
				GF_TREE_JOINER.join(entry.getTrees().getTrees()));
	}


	/**
	 * Deserializes a GF wiki entry.
	 */
	public static GfWikiEntry deserialize(String serialized) {
		List<String> splitsAsList = ImmutableList.copyOf(GF_SERIALIZATION_SPLITTER.split(serialized));
		if (splitsAsList.size() == 1) {
			// deprecated form, containing just the trees
			return new GfWikiEntry(new TreeList(GF_TREE_SPLITTER.split(serialized)));
		} else if (splitsAsList.size() == 3) {
			Iterable<String> trees = GF_TREE_SPLITTER.split(splitsAsList.get(2));
			return new GfWikiEntry(
					splitsAsList.get(0),
					splitsAsList.get(1),
					new TreeList(trees));
		}
		throw new RuntimeException("Syntax error: " + serialized);
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
		// Removed it (was 15), it seemed to be buggy in some cases.
		GfServiceResultComplete result = mGfService.complete(cat, getCompletionInput(tokens), language, null);
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


	public Set<String> getProducers(String cat) {
		return mGfServiceResultBrowseAll.getProducers(cat);
	}


	public Set<String> getConsumers(String cat) {
		return mGfServiceResultBrowseAll.getConsumers(cat);
	}


	public String getCategoryName(String cat, String language) {
		return mGfServiceResultBrowseAll.getCategoryName(cat, language);
	}


	/**
	 * <p>Returns the {@code k} largest categories in the order of size.
	 * The size is in terms of the number of producer functions that are
	 * not consumer functions.</p>
	 */
	public List<String> getLargestCategories(int k) {
		return Ordering.natural().onResultOf(Functions.forMap(mCatToSize)).greatestOf(mCatToSize.keySet(), k);
	}

	public Multimap<String, String> getTokenToCats(String language) {
		return langToTokenToCats.get(language);
	}

	public Map<String, String> getIriToToken(String language) {
		return langToIriToToken.get(language);
	}

	public GfParseResult parseGfModule(GfModule gfModule) throws GfServiceException {
		return mGfStorage.parse(gfModule);
	}


	/**
	 * Uploads the given GF module to the server.
	 */
	public void upload(GfModule module) throws GfServiceException {
		mGfStorage.upload(mDir, module);
	}


	public Set<String> ls(String extension) throws GfServiceException {
		GfStorageResultLs result = mGfStorage.ls(mDir, extension);
		return result.getFilenames();
	}


	public void rm(String path) throws GfServiceException {
		mGfStorage.rm(mDir, path);
	}


	public int rmGfo() throws GfServiceException {
		int count = 0;
		for (String path : ls(EXTENSION_GFO)) {
			mGfStorage.rm(mDir, path);
			count++;
		}
		return count;
	}


	public String downloadAsString(String filename) throws GfServiceException {
		return mGfStorage.downloadAsString(mDir, filename);
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
		Set<String> languages = getLanguages();
		GfStorageResult result = null;
		if (isToplevelModule(gfModule, languages)) {
			// If the module is a (toplevel) concrete syntax module then
			// update it in the context of other concrete modules.
			result = mGfStorage.update(mDir, mCat, OPTIMIZE_PGF, languages, gfModule);
		} else {
			// Otherwise just upload it and recompile the existing concrete modules.
			mGfStorage.upload(mDir, gfModule);
			result = mGfStorage.update(mDir, mCat, OPTIMIZE_PGF, languages);
		}
		if (result != null && result.isSuccess()) {
			refreshGrammarInfo();
			refreshLangToTokenToCats();
		}
		return result;
	}


	public boolean isGrammarEditable() {
		return ! (mDir == null);
	}


	/**
	 * True if the module is a concrete syntax module which no other
	 * module imports. We check if the module name has the form
	 * {@code GrammarLan}. This covers also modules
	 * which were added after the wiki was started up. The previous
	 * technique {@code languages.contains(gfModule.getName())} did not
	 * cover the new modules.
	 */
	private boolean isToplevelModule(GfModule gfModule, Set<String> languages) {
		String moduleName = gfModule.getName();
		if (languages.contains(moduleName)) {
			return true;
		}
		if (mGfServiceResultGrammar == null) {
			return false;
		}
		String grammarName = mGfServiceResultGrammar.getName();

		return (
				moduleName.startsWith(grammarName) &&
				moduleName.length() >= grammarName.length() + 3 &&
				Character.isUpperCase(moduleName.charAt(grammarName.length()))
				||
				moduleName.startsWith(PREFIX_DISAMB + grammarName) &&
				moduleName.length() >= PREFIX_DISAMB.length() + grammarName.length() + 3 &&
				Character.isUpperCase(moduleName.charAt(PREFIX_DISAMB.length() + grammarName.length()))
				);
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
		return GF_TOKEN_JOINER.join(tokens) + GF_TOKEN_SEPARATOR;
	}


	private void refreshGrammarInfo() throws GfServiceException {
		mGfServiceResultGrammar = mGfService.grammar();
		mGfServiceResultBrowseAll = mGfService.browseAll();
	}


	/**
	 * <p>Creates a structure from which you can look up the categories of tokens.</p>
	 *
	 * <pre>
	 * language -> token -> categories
	 * </pre>
	 */
	private void refreshLangToTokenToCats() throws GfServiceException {
		// Collect together all the consumer functions.
		// TODO We are not interested in their linearizations, at least for the time begin.
		Set<String> funsAllConsumers = Sets.newHashSet();
		Set<String> cats = mGfServiceResultBrowseAll.getCategories();
		for (String cat : cats) {
			funsAllConsumers.addAll(getConsumers(cat));
		}

		int countAllFuns = mGfServiceResultGrammar.getFunctions().size();
		int countIgnoreFuns = funsAllConsumers.size();

		mLogger.info("All funs: {}, (ignored) consumer funs: {}", countAllFuns, countIgnoreFuns);
		if (countAllFuns - countIgnoreFuns > LINEARIZE_ALL_QUERY_LIMIT) {
			mLogger.warn("Refusing to build preditor cache, as there are too many producer-only funs. " +
					"Increase LINEARIZE_ALL_QUERY_LIMIT if its current value {} is too low.", LINEARIZE_ALL_QUERY_LIMIT);
			return;
		}

		langToTokenToCats.clear();
		mCatToSize.clear();
		langToIriToToken.clear();
		// Iterate over all the categories that have producer functions
		for (String cat : cats) {
			mCatToSize.put(cat, 0);
			// For each category look at its producers
			for (String f : getProducers(cat)) {
				// If this function is also a consumer, then throw it out
				if (funsAllConsumers.contains(f)) {
					continue;
				}
				// Increment the counter of producers that are not consumers for this category
				mCatToSize.put(cat, mCatToSize.get(cat) + 1);
				// Otherwise get all of its linearizations in all the languages.
				// This includes all the wordforms and variants, because the linearization
				// is likely to be a complex record that holds many strings.
				GfServiceResultLinearizeAll result = mGfService.linearizeAll(f, null);
				Map<String, List<String>> langToTokens = result.getTexts();
				// Extract the logical symbol that corresponds to this function.
				// The logical symbol is present in the Ape-linearization.
				String logicalSymbol = extractLogicalSymbolFromApe(langToTokens.get(mGfServiceResultGrammar.getName() + SUFFIX_APE));
				for (Entry<String, List<String>> entry2 : langToTokens.entrySet()) {
					String lang = entry2.getKey();
					Multimap<String, String> tokenToCats = langToTokenToCats.get(lang);
					// If we haven't seen this language before then create a new hash table entry for it
					if (tokenToCats == null) {
						tokenToCats = HashMultimap.create();
						langToTokenToCats.put(lang, tokenToCats);
					}
					// Store each linearization together with its category.
					// The linearization is represented by its "most important" token.
					for (String lin : entry2.getValue()) {
						String indexToken = getIndexToken(lin);
						if (indexToken != null) {
							tokenToCats.put(indexToken, cat);
						}
					}


					if (logicalSymbol != null) {
						Map<String, String> iriToToken = langToIriToToken.get(lang);
						// If we haven't seen this language before then create a new hash table entry for it
						if (iriToToken == null) {
							iriToToken =  Maps.newHashMap();
							langToIriToToken.put(lang, iriToToken);
						}
						for (String lin : entry2.getValue()) {
							iriToToken.put(logicalSymbol, lin);
							// TODO: We assume that the dictionary form is always the first.
							// Of course, this does not always hold.
							// Unfortunately, LinearizeAll cannot be used to obtain a GF record,
							// with all the category labels of the strings, but just a list of plain strings.
							break;
						}
					}
				}
			}
		}
	}


	/**
	 * It does not make sense to index linearizations which contain multiple tokens
	 * or which are empty strings, as these cannot be matched during (single token)
	 * lookahead editing. If there are multiple tokens in the given linearization, e.g.
	 * the + Atlantic_Ocean, des + Atlantischen_Ozeans, Atlandi_Ookean + &+ + il;
	 * then we return the longest token (picking the last one in case there are several).
	 * TODO: this is a hack while we're waiting for a cleaner solution.
	 */
	private static String getIndexToken(String lin) {
		int max = 0;
		String returnTok = null;
		for (String tok : GF_TOKEN_SPLITTER.omitEmptyStrings().split(lin)) {
			if (tok.length() >= max) {
				max = tok.length();
				returnTok = tok;
			}
		}
		return returnTok;
	}


	/**
	 * <p>Extracts the logical symbol (which is used by APE as the
	 * OWL entity IRI) from the Ape-linearization of a function, assuming
	 * that the function is a lexical function.
	 * Returns {@code null} in case the extraction fails.</p>
	 *
	 * <p>We assume that the Ape linearizations have the form
	 * {@code The_Hague|pn_sg|The_Hague_PN|neutr}, where the logical symbol
	 * is always in the same field and is always the same in case there are
	 * several linearizations.</p>
	 */
	private static String extractLogicalSymbolFromApe(List<String> lins) {
		if (lins == null || lins.isEmpty()) {
			return null;
		}
		int count = 0;
		for (String field : GF_APE_SPLITTER.split(lins.get(0))) {
			if (++count == GF_APE_FIELD_LOGICAL_SYMBOL) {
				return field;
			}
		}
		return null;
	}
}