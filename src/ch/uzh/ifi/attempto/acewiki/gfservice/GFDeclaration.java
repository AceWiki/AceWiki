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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.uzh.ifi.attempto.acewiki.core.AbstractSentence;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * The "declaration" is a tree set that can be linearized into multiple
 * languages.
 * 
 * @author Kaarel Kaljurand
 */
public class GFDeclaration extends AbstractSentence implements Declaration {

	final Logger logger = LoggerFactory.getLogger(GFDeclaration.class);

	// TODO: move it somewhere else
	private static final String BIND = "&+";

	private final GFGrammar mGfGrammar;

	// Maps languages to text containers
	private final Map<String, TextContainer> textContainers = new HashMap<String, TextContainer>();

	private ParseState mParseState;

	/**
	 * Creates a new GF declaration object from a parse state.
	 * 
	 * @param parseState The parse state.
	 * @param gfGrammar The grammar object.
	 */
	public GFDeclaration(ParseState parseState, GFGrammar gfGrammar) {
		mParseState = parseState;
		mGfGrammar = gfGrammar;
	}

	/**
	 * Creates a new GF declaration object from a text in a given language.
	 *
	 * TODO: the input text should probably be in the form of a token list
	 * 
	 * @param text The declaration text.
	 * @param language The language.
	 * @param gfGrammar The grammar object.
	 */
	public GFDeclaration(String text, String language, GFGrammar gfGrammar) {
		// TODO: quick and ugly hack to be able to move on
		text = text.replaceAll("([\\.?!])", " $1");
		mGfGrammar = gfGrammar;
		try {
			Set<String> trees = getGFGrammar().parse(text, language);
			mParseState = new ParseState(trees);
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	protected TextContainer getTextContainer(String language) {
		TextContainer tc = textContainers.get(language);
		if (tc == null) {
			tc = new TextContainer();
			try {
				// TODO: separate the linearizations of different trees
				for (String tree : mParseState.getTrees()) {
					for (String s : getGFGrammar().linearizeAsTokens(tree, language)) {
						// TODO: handle BIND-symbols somewhere else
						tc.addElement(new TextElement(getTokenText(s)));
					}
				}
			} catch (GfServiceException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			textContainers.put(language, tc);
		}
		return tc;
	}

	public List<TextElement> getTextElements(String language) {
		//logger.info("getTextElements {}: {}", language, getTextContainer(language).getTextElements());
		return getTextContainer(language).getTextElements();
	}

	public boolean contains(OntologyElement e) {
		// TODO
		return true;
	}


	/**
	 * Returns the details of this tree set:
	 * 
	 *   - abstract trees;
	 *   - translations;
	 *   - abstract tree diagram;
	 *   - parse tree diagram;
	 *   - word alignment diagram;
	 *   - ...
	 * 
	 * The output highlights the given language.
	 *
	 * TODO: everything should be hyperlinked.
	 */
	public List<SentenceDetail> getDetails(String lang) {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();

		for (String tree : mParseState.getTrees()) {
			l.addAll(formatTree(mGfGrammar, tree, lang));
		}

		return l;
	}


	public boolean isReasonable() {
		return true;
	}

	public void update() {
	}

	public String serialize() {
		return GFGrammar.serialize(mParseState);
	}

	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	public GFGrammar getGFGrammar() {
		return mGfGrammar;
	}


	private String getAbstrtreeAsHtml(String tree) {
		try {
			return getImg(getGFGrammar().abstrtree(tree));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getParsetreeAsHtml(String tree, String language) {
		try {
			return getImg(getGFGrammar().parsetree(tree, language));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getAlignmentAsHtml(String tree) {
		try {
			return getImg(getGFGrammar().alignment(tree));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getImg(String dataUri) {
		return "<img src=\"" + dataUri + "\"/>";
	}


	private static String getError(String message) {
		return "<p style=\"color: red\">" + message + "</p>";
	}


	private static SentenceDetail getError(String tree, String lang, String message) {
		return new SentenceDetail("ERROR", getError(tree + ": " + lang + ": " + message));
	}


	private List<SentenceDetail> formatTree(GFGrammar grammar, String tree, String lang) {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();
		l.add(new SentenceDetail(
				"Tree (ASCII)",
				"<pre>" + tree + "</pre>"
				));
		l.add(new SentenceDetail("Tree (diagram)", getAbstrtreeAsHtml(tree)));
		l.add(new SentenceDetail("Parsetree for " + lang, getParsetreeAsHtml(tree, lang)));
		try {
			Map<String, Set<String>> m = grammar.linearize(tree);
			StringBuilder sb = new StringBuilder();
			sb.append("<ul>");
			for (String key : m.keySet()) {
				if (key.equals(lang)) {
					sb.append("<li style='background-color: yellow'><b>" + key + "</b>: " + m.get(key) + "</li>");
				} else {
					sb.append("<li><b>" + key + "</b>: " + m.get(key) + "</li>");
				}
			}
			sb.append("</ul>");
			l.add(new SentenceDetail(
					"Translations",
					sb.toString()
					));
		} catch (GfServiceException e) {
			l.add(getError(tree, lang, "linearization failed: " + e.getMessage()));
		}

		l.add(new SentenceDetail("Word alignment", getAlignmentAsHtml(tree)));

		return l;
	}


	private static String getTokenText(String s) {
		if (BIND.equals(s)) {
			return "·";
		}
		return s;
	}
}