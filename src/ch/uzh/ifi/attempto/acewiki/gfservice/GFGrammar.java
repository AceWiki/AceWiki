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
import java.util.Set;

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


	/**
	 * Creates a new GF grammar object.
	 */
	public GFGrammar(URI serviceUri, String pgfName) {
		mGfService = new GfWebService(serviceUri, pgfName);
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
		GfServiceResultParse result = mGfService.parse(null, text, language);
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
	 * @throws GfServiceException
	 */
	public String deserialize(String serialized) throws GfServiceException {
		return serialized;
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


	public Set<String> complete(String text, String language) throws GfServiceException {
		GfServiceResultComplete result = mGfService.complete(null, text, language, -1);
		return result.getCompletions(language);
	}


	/**
	 * Serializes a given parse state.
	 *
	 * @param parseState The parse state.
	 * @return The serialization.
	 * @throws GfServiceException
	 */
	public String serialize(String tree) throws GfServiceException {
		return tree;
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