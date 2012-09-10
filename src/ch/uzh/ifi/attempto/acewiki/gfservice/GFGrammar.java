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

	private final static char GF_TOKEN_SEPARATOR = ' ';
	private final static char GF_TREE_SEPARATOR = '|';

	public final static Splitter GF_TOKEN_SPLITTER = Splitter.on(GF_TOKEN_SEPARATOR);

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
	 * Deserializes a serialized representation into a parse state.
	 *
	 * @param serialized The serialized representation
	 * @return The parse state.
	 */
	public static ParseState deserialize(String serialized) {
		return new ParseState(Splitter.on(GF_TREE_SEPARATOR).split(serialized));
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

	public String abstrtree(String tree) throws GfServiceException {
		return mGfService.abstrtree(tree).getDataUri();
	}

	public String parsetree(String tree, String from) throws GfServiceException {
		return mGfService.parsetree(tree, from).getDataUri();
	}

	public String alignment(String tree) throws GfServiceException {
		return mGfService.alignment(tree).getDataUri();
	}


	/**
	 * Serializes a given parse state.
	 *
	 * @param parseState The parse state.
	 * @return The serialization.
	 */
	public static String serialize(ParseState parseState) {
		return Joiner.on(GF_TREE_SEPARATOR).join(parseState.getTrees());
	}


	private static String getCompletionInput(List<String> tokens) {
		if (tokens.isEmpty()) {
			return "";
		}
		return Joiner.on(GF_TOKEN_SEPARATOR).join(tokens) + GF_TOKEN_SEPARATOR;
	}

}
