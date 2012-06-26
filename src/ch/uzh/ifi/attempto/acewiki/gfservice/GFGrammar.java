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

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;

import ch.uzh.ifi.attempto.gfservice.GfService;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultComplete;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultLinearize;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultParse;
import ch.uzh.ifi.attempto.gfservice.gfwebservice.GfWebService;


/**
 * This class wraps GF features of a particular GF grammar.
 *
 * @author Kaarel Kaljurand
 */
public class GFGrammar {

	private final GfService mGfService;
	private final String mCat;


	/**
	 * Creates a new GF grammar object.
	 */
	public GFGrammar(URI serviceUri, String pgfName, String cat) {
		mGfService = new GfWebService(serviceUri, pgfName);
		mCat = cat;

	}


	/**
	 * @return set of names of the concrete languages defined in the grammar
	 */
	public Set<String> getLanguages() {
		try {
			return mGfService.grammar().getLanguages().keySet();
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return Collections.emptySet();
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
		GfServiceResultParse result = mGfService.parse(mCat, text, language);
		return result.getTrees(language);
	}


	/**
	 * Parses the given tokens in the given language.
	 *
	 * @param tokens The tokens.
	 * @param language The language.
	 * @return The parse result.
	 * @throws GfServiceException
	 */
	/*
	public Set<String> parse(String[] tokens, String language) throws GfServiceException {
		return parse(Joiner.on(" ").join(tokens), language);
	}
	 */


	/**
	 * Deserializes a serialized representation into a parse state.
	 *
	 * @param serialized The serialized representation
	 * @return The parse state.
	 */
	public static ParseState deserialize(String serialized) {
		return new ParseState(Splitter.on('|').split(serialized));
	}


	/**
	 * Note we return just the first linearization.
	 */
	public Iterable<String> linearizeAsTokens(String tree, String language) throws GfServiceException {
		GfServiceResultLinearize result = mGfService.linearize(tree, language);
		Set<String> texts = result.getTexts(language);
		if (texts.isEmpty()) {
			return Collections.emptyList();
		}
		return Splitter.on(' ').split(texts.iterator().next());
	}


	public Map<String, Set<String>> linearize(String tree) throws GfServiceException {
		GfServiceResultLinearize result = mGfService.linearize(tree, null);
		return result.getTexts();
	}


	/**
	 * Linearizes a parse state in the given language.
	 *
	 * @param parseState The parse state.
	 * @param language The language.
	 * @return The linearization as a list of tokens.
	 * @throws GfServiceException
	 *
	 * @Deprecated
	 */
	public Iterable<String> linearizeAsTokens(Set<String> trees, String language) throws GfServiceException {
		String result = linearizeAsString(trees, language);
		if (result == null) {
			return Collections.emptyList();
		}
		return Splitter.on(' ').split(result);
	}


	public Set<String> complete(List<String> tokens, String language) throws GfServiceException {
		return complete(mCat, tokens, language);
	}


	/**
	 * @param cat start category for the parser
	 * @param tokens list of tokens the last of which is to be completed
	 * @param language language of the input tokens
	 * @return list of possible completions
	 * @throws GfServiceException
	 */
	public Set<String> complete(String cat, List<String> tokens, String language) throws GfServiceException {
		GfServiceResultComplete result = mGfService.complete(cat, getCompletionInput(tokens), language, null);
		return result.getCompletions(language);
	}

	/**
	 * TODO: visualize all the trees
	 */
	public String abstrtree(ParseState mParseState) throws GfServiceException {
		return mGfService.abstrtree(mParseState.getTree()).getDataUri();
	}

	/**
	 * TODO: visualize all the trees
	 */
	public String parsetree(ParseState mParseState, String from) throws GfServiceException {
		return mGfService.parsetree(mParseState.getTree(), from).getDataUri();
	}


	/**
	 * TODO: visualize all the trees
	 */
	public String alignment(ParseState mParseState) throws GfServiceException {
		return mGfService.alignment(mParseState.getTree()).getDataUri();
	}


	/**
	 * Serializes a given parse state.
	 *
	 * @param parseState The parse state.
	 * @return The serialization.
	 */
	public static String serialize(ParseState mParseState) {
		return Joiner.on("|").join(mParseState.getTrees());
	}


	private static String getCompletionInput(List<String> tokens) {
		if (tokens.isEmpty()) {
			return "";
		}
		return Joiner.on(" ").join(tokens) + " ";
	}


	/**
	 * <p>Linearizes the first tree in the given set of trees
	 * and returns the first linearization, or <code>null</code>
	 * iff there are no linearizations.</p>
	 *
	 * @param parseState The parse state.
	 * @param language The language.
	 * @return The linearization as a string.
	 * @throws GfServiceException
	 *
	 * @Deprecated
	 */
	private String linearizeAsString(Set<String> trees, String language) throws GfServiceException {
		if (trees.isEmpty()) {
			return null;
		}
		GfServiceResultLinearize result = mGfService.linearize(trees.iterator().next(), language);
		Set<String> texts = result.getTexts(language);
		if (texts.isEmpty()) {
			return null;
		}
		return texts.iterator().next();
	}

}
