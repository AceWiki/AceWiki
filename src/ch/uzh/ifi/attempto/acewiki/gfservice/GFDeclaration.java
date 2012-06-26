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

import com.google.common.base.Joiner;

import ch.uzh.ifi.attempto.acewiki.core.AbstractSentence;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * 
 * @author Kaarel Kaljurand
 */
public class GFDeclaration extends AbstractSentence implements Declaration {

	private final GFGrammar gfGrammar;
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
		this.gfGrammar = gfGrammar;
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
		this.gfGrammar = gfGrammar;
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
				// TODO: handle all the trees
				for (String s : getGFGrammar().linearizeAsTokens(mParseState.getTree(), language)) {
					tc.addElement(new TextElement(s));
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
		return getTextContainer(language).getTextElements();
	}

	public boolean contains(OntologyElement e) {
		// TODO
		return false;
	}


	/**
	 * <p>Returns the details of this sentence:</p>
	 * <ul>
	 * <li>language;</li>
	 * <li>abstract tree;</li>
	 * <li>translations;</li>
	 * <li>abstract tree diagram;</li>
	 * <li>parse tree diagram;</li>
	 * <li>word alignment diagram;</li>
	 * <li>...</li>
	 * </ul>
	 * <p>TODO: everything should be hyperlinked.</p>
	 */
	public List<SentenceDetail> getDetails(String language) {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();
		l.add(new SentenceDetail(
				"Language",
				"<pre>" + language + "</pre>"
				));
		l.add(new SentenceDetail(
				"Abstract trees (" + mParseState.size() + ")",
				"<pre>" + Joiner.on('\n').join(mParseState.getTrees()) + "</pre>"
				));
		try {
			// TODO: handle all the trees
			Map<String, Set<String>> m = getGFGrammar().linearize(mParseState.getTree());
			StringBuilder sb = new StringBuilder();
			sb.append("<ul>");
			for (String key : m.keySet()) {
				if (! key.equals(language)) {
					sb.append("<li><b>" + key + "</b>: " + m.get(key) + "</li>");
				}
			}
			sb.append("</ul>");
			l.add(new SentenceDetail(
					"Translations",
					sb.toString()
					));
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		l.add(new SentenceDetail("Abstract tree", getAbstrtreeAsHtml()));
		l.add(new SentenceDetail("Parsetree", getParsetreeAsHtml(language)));
		l.add(new SentenceDetail("Word alignment", getAlignmentAsHtml()));

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
		return gfGrammar;
	}


	private String getAbstrtreeAsHtml() {
		try {
			return getImg(getGFGrammar().abstrtree(mParseState));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getParsetreeAsHtml(String language) {
		try {
			return getImg(getGFGrammar().parsetree(mParseState, language));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getAlignmentAsHtml() {
		try {
			return getImg(getGFGrammar().alignment(mParseState));
		} catch (GfServiceException e) {
			return getError(e.getMessage());
		}
	}


	private String getImg(String dataUri) {
		return "<img src=\"" + dataUri + "\"/>";
	}


	private String getError(String message) {
		return "<p style=\"color: red\">" + message + "</p>";
	}

}