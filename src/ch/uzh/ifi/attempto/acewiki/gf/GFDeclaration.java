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

import static ch.uzh.ifi.attempto.ape.OutputType.DRSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLXML;
import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.MultilingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.base.APE;
import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.MultiTextContainer;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

import com.google.common.base.Joiner;
import com.google.common.collect.Sets;

/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * The "declaration" is a tree set that can be linearized into multiple
 * languages.
 *
 * TODO: store also the language in whose context this declaration (treeset) was created,
 * knowing this can be useful in certain situations
 *
 * TODO: move the HTML-formatting out of this class
 * 
 * @author Kaarel Kaljurand
 */
public class GFDeclaration extends MultilingualSentence implements Declaration {

	final Logger logger = LoggerFactory.getLogger(GFDeclaration.class);

	private final GFGrammar mGfGrammar;

	// maps a language identifier to the set of linearizations (text containers) in this language
	private final Map<String, MultiTextContainer> textContainers = new HashMap<String, MultiTextContainer>();

	private TreeList mTreeSet;
	private final String mLang;

	public GFDeclaration(String lang, GFGrammar gfGrammar) {
		mGfGrammar = gfGrammar;
		mLang = lang;
		try {
			mTreeSet = new TreeList(gfGrammar.random());
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Creates a new GF declaration object from a parse state.
	 * 
	 * @param treeSet The tree set.
	 * @param gfGrammar The grammar object.
	 */
	public GFDeclaration(TreeList treeSet, String lang, GFGrammar gfGrammar) {
		mTreeSet = treeSet;
		mGfGrammar = gfGrammar;
		mLang = lang;
	}

	/**
	 * Creates a new GF declaration object from a text in a given language.
	 *
	 * TODO: the input text should probably be in the form of a token list
	 * 
	 * @param text The declaration text.
	 * @param lh The language handler.
	 * @param gfGrammar The grammar object.
	 */
	public GFDeclaration(String text, LanguageHandler lh, GFGrammar gfGrammar) {
		String tokenText = "";
		for (String t : lh.getTextOperator().splitIntoTokens(text)) {
			tokenText += t + " ";
		}
		mGfGrammar = gfGrammar;
		mLang = lh.getLanguage();
		try {
			mTreeSet = new TreeList(getGFGrammar().parse(tokenText, mLang));
			if (mTreeSet.size() == 0) {
				// TODO this should be done properly; see GfTextOperator
				// If parsing fails: first char to lower case
				tokenText = DefaultTextOperator.firstCharToLowerCase(tokenText);
				mTreeSet = new TreeList(getGFGrammar().parse(tokenText, mLang));
			}
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public MultiTextContainer getTextContainer(String language) {
		MultiTextContainer mtc = textContainers.get(language);
		if (mtc == null) {
			List<TextContainer> tmp = new ArrayList<>();
			Set<String> seen = Sets.newHashSet();
			for (String tree : mTreeSet.getTrees()) {
				Set<String> lins = getLins(tree, language);

				if (lins == null) {
					logger.info("getTextContainerSet: null {}: {}", language, tree);
					// TODO do it properly
					TextContainer tc = new TextContainer();
					tc.addElement(new TextElement("-NULL-"));
					tmp.add(tc);
				} else if (lins.isEmpty()) {
					logger.info("getTextContainerSet: 0 els {}: {}", language, tree);
					// TODO do it properly
					TextContainer tc = new TextContainer();
					tc.addElement(new TextElement("-EMPTY-"));
					tmp.add(tc);
				} else if (seen.contains(lins.iterator().next())) {
					// Don't show the same linearization twice
				} else {
					// TODO: limitation: we only work with the first linearization
					String lin = lins.iterator().next();
					seen.add(lin);
					TextOperator to = getTextOperator(language);
					TextContainer tc = new TextContainer(to);
					for (String s : to.splitIntoTokens(lin)) {
						tc.addElement(new TextElement(s));
					}
					tmp.add(tc);
				}
			}
			mtc = new MultiTextContainer(tmp);
			textContainers.put(language, mtc);
		}
		return mtc;
	}

	/**
	 * TODO
	 */
	public boolean contains(OntologyElement e) {
		return false;
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
	public List<SentenceDetail> getDetails(String lang, int index) {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();

		if (mGfGrammar.isAceCompatible()) {
			l.addAll(getSemantics());
		}

		l.addAll(formatTree(mGfGrammar, lang, index));

		return l;
	}


	public boolean isReasonable() {
		return true;
	}


	public int getNumberOfRepresentations() {
		return mTreeSet.size();
	}


	public List<String> getParseTrees() {
		return mTreeSet.getTrees();
	}


	public void update() {
	}

	public String serialize() {
		return GFGrammar.serialize(mTreeSet);
	}

	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	private GFGrammar getGFGrammar() {
		return mGfGrammar;
	}


	// Return some of the APE analysis of this tree set, assuming it is a singleton.
	// The APE analysis is obtained by first linearizing the tree in "Ape".
	// This only works if the wiki is ACE-based.
	//
	// TODO: experimental
	private List<SentenceDetail> getSemantics() {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();

		String tree = mTreeSet.getTree();

		if (tree == null) {
			if (mTreeSet.size() == 0) {
				l.add(new SentenceDetail("ERROR", "Statement is not well-formed"));
			} else if (mTreeSet.size() > 1) {
				l.add(new SentenceDetail("ERROR", "Statement is ambiguous and therefore it cannot be assigned semantics"));
			}
			return l;
		}

		Set<String> lins = null;
		String targetLang = mGfGrammar.getGrammar().getName() + GFGrammar.SUFFIX_APE;
		try {
			lins = mGfGrammar.linearize(tree, targetLang);
		} catch (GfServiceException e) {
			l.add(new SentenceDetail("ERROR in translation to " + targetLang, "<pre>" + e.getMessage() + "</pre>"));
			return l;
		}

		if (lins == null || lins.size() != 1) {
			l.add(new SentenceDetail("ERROR", "Bad linearization"));
			return l;
		}

		ACEText acetext = new ACEText(lins.iterator().next());

		ACEParserResult parserResult = parse(acetext, getOntology().getURI());

		l.add(new SentenceDetail("ACE", "<pre>" + acetext.getText() + "</pre>"));
		l.add(new SentenceDetail("Lexicon", "<pre>" + Joiner.on('\n').join(acetext.getLexicon().getEntries()) + "</pre>"));
		l.add(new SentenceDetail("ACE (paraphrase)", "<pre>" + parserResult.get(PARAPHRASE1) + "</pre>"));
		l.add(new SentenceDetail("DRS", "<pre>" + parserResult.get(DRSPP) + "</pre>"));
		l.add(new SentenceDetail("OWL", "<pre>" + parserResult.get(OWLFSSPP) + "</pre>"));
		l.add(new SentenceDetail("Messages",
				"<pre>" + Joiner.on('\n').join(parserResult.getMessageContainer().getMessages()) + "</pre>"));

		return l;
	}


	private String getAbstrtreeAsHtml(String tree) {
		try {
			return getImg(getGFGrammar().abstrtree(tree));
		} catch (GfServiceException e) {
			return getError(e);
		}
	}


	private String getParsetreeAsHtml(String tree, String language) {
		try {
			return getImg(getGFGrammar().parsetree(tree, language));
		} catch (GfServiceException e) {
			return getError(e);
		}
	}


	private String getImg(String dataUri) {
		return "<a href=\"" + dataUri + "\"><img src=\"" + dataUri + "\" style=\"max-height:500px\"/></a>";
	}


	private static String getError(Exception e) {
		return "<p style=\"color: red\">" + e.getMessage() + "</p>";
	}


	private List<SentenceDetail> formatTree(GFGrammar grammar, String lang, int index) {
		String tree = mTreeSet.getTrees().get(index);
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();
		l.add(new SentenceDetail("acewiki_details_syntree", getParsetreeAsHtml(tree, lang)));
		l.add(new SentenceDetail(
				LocaleResources.getString("acewiki_details_internal") + " (ASCII)",
				"<p><code>" + tree + "</code></p>"
				));
		l.add(new SentenceDetail("acewiki_details_internal", getAbstrtreeAsHtml(tree)));
		return l;
	}


	// TODO: linearize into all the languages at once for better
	// performance
	private Set<String> getLins(String tree, String language) {
		try {
			return getGFGrammar().linearize(tree, language);
		} catch (GfServiceException e) {
			// TODO find out what happened, i.e.
			// why was the tree not supported by the grammar.
			return null;
		}
	}


	/**
	 * This was taken from:
	 * ch.uzh.ifi.attempto.acewiki.aceowl.ACESentence
	 */
	private static ACEParserResult parse(ACEText acetext, String uri) {
		ACEParser ape = APE.getParser();
		synchronized (ape) {
			ape.setURI(uri);
			ape.setClexEnabled(false);

			return ape.getMultiOutput(
					acetext.getText(),
					acetext.getLexicon(),
					PARAPHRASE1,
					OWLXML,
					OWLFSSPP,
					DRSPP
					);
		}
	}

}