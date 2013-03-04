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
import ch.uzh.ifi.attempto.acewiki.core.MultilingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.PrettyTextElement;
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
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * The "declaration" is a tree set that can be linearized into multiple
 * languages.
 *
 * TODO: move the HTML-formatting out of this class
 * 
 * @author Kaarel Kaljurand
 */
public class GfDeclaration extends MultilingualSentence implements Declaration {

	final Logger mLogger = LoggerFactory.getLogger(GfDeclaration.class);

	private final GfGrammar mGfGrammar;
	private final GfWikiEntry mGfWikiEntry;

	// Use the original input in the text container
	// TODO: this is a hack
	private boolean mUseOriginal = true;

	// Maps a language identifier to the set of linearizations (text containers) in this language
	private final Map<String, MultiTextContainer> textContainers = new HashMap<>();
	
	// Maps a tree to the set of linearizations for each language.
	// ie. Map<Tree, Map<Language, Set<Linearization>>>
	// Lazy initialized as tree linearizations are requested, but done for all languages at once - performance reasons.
	private final Map<String, Map<String, Set<String>>> treeLinearizations = new HashMap<>();


	/**
	 * Creates a declaration from an existing wiki entry (from the storage).
	 * Each wiki entry is tagged with the language in which the entry was created.
	 * Each wiki entry also contains the original text that was used to create the
	 * trees. In case the trees can not be linearized (because the grammar has changed)
	 * then we try to parse the original text instead (which might also fail).
	 */
	public GfDeclaration(GfGrammar grammar, GfWikiEntry entry) {
		mGfGrammar = grammar;

		boolean isParseable = (entry.getLanguage() != null && entry.getText() != null);

		if (isParseable && hasBadTrees(entry.getLanguage(), entry.getTrees())) {
			mGfWikiEntry = createGfWikiEntry(grammar, entry.getLanguage(), entry.getText());
		} else {
			mGfWikiEntry = entry;
		}
	}


	/**
	 * Creates a declaration object from the given text.
	 * The text will be parsed to get the trees.
	 */
	public GfDeclaration(GfGrammar grammar, String language, String text) {
		mGfGrammar = grammar;
		mGfWikiEntry = createGfWikiEntry(grammar, language, text);
	}


	/**
	 * TODO: this is temporary. Also not that it does not have effect on the
	 * original input as this is always taken from mGfWikiEntry.getText()
	 */
	public void removeTextContainer(String language) {
		textContainers.put(language, null);
		mUseOriginal = false;
	}


	public MultiTextContainer getTextContainer(String language) {
		MultiTextContainer mtc = textContainers.get(language);
		if (mtc == null) {
			List<TextContainer> tmp = new ArrayList<>();
			TextOperator to = getTextOperator(language);

			// If the text is requested in the original language (i.e. in which the entry was first created)
			// then we return the original text. The benefit is that we do not need to make a call
			// to the linearizer. Also, there is no danger that the original text would be replaced by a variant
			// (e.g. "does not -> doesn't") which would be confusing.
			if (mUseOriginal && language.equals(mGfWikiEntry.getLanguage()) && mGfWikiEntry.getText() != null) {
				tmp = ImmutableList.of(makeTextContainer(to, mGfWikiEntry.getText()));
			} else {
				Set<String> seen = Sets.newHashSet();
				for (String tree : mGfWikiEntry.getTrees().getTrees()) {
					Set<String> lins = getLins(tree, language);
					if (lins == null) {
						mLogger.info("getTextContainerSet: null {}: {}", language, tree);
						// TODO do it properly
						tmp.add(new TextContainer(new TextElement("-NULL-")));
					} else if (lins.isEmpty()) {
						mLogger.info("getTextContainerSet: 0 els {}: {}", language, tree);
						// TODO do it properly
						tmp.add(new TextContainer(new TextElement("-EMPTY-")));
					} else if (seen.contains(lins.iterator().next())) {
						// Don't show the same linearization twice
					} else {
						// TODO: limitation: we only work with the first linearization
						String lin = lins.iterator().next();
						seen.add(lin);
						tmp.add(makeTextContainer(to, lin));
					}
				}
			}
			if (tmp.isEmpty()) {
				tmp.add(new TextContainer(new TextElement("-SYNTAX ERROR-")));
			}
			mtc = new MultiTextContainer(tmp);
			textContainers.put(language, mtc);
		}
		return mtc;
	}


	private TextContainer makeTextContainer(TextOperator to, String str) {
		TextContainer tc = new TextContainer(to);
		for (String s : to.splitIntoTokens(str)) {
			tc.addElement(new PrettyTextElement(s));
		}
		return tc;
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
		return mGfWikiEntry.getTrees().size();
	}


	public List<String> getParseTrees() {
		return mGfWikiEntry.getTrees().getTrees();
	}


	public void update() {
	}

	public String serialize() {
		return GfGrammar.serialize(mGfWikiEntry);
	}

	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	private GfGrammar getGfGrammar() {
		return mGfGrammar;
	}


	// Return some of the APE analysis of this tree set, assuming it is a singleton.
	// The APE analysis is obtained by first linearizing the tree in "Ape".
	// This only works if the wiki is ACE-based.
	//
	// TODO: experimental
	private List<SentenceDetail> getSemantics() {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();

		String tree = mGfWikiEntry.getTrees().getTree();

		if (tree == null) {
			if (mGfWikiEntry.getTrees().size() == 0) {
				l.add(new SentenceDetail("ERROR", "Statement is not well-formed"));
			} else if (mGfWikiEntry.getTrees().size() > 1) {
				l.add(new SentenceDetail("ERROR", "Statement is ambiguous and therefore it cannot be assigned semantics"));
			}
			return l;
		}

		Set<String> lins = null;
		String targetLang = mGfGrammar.getGrammar().getName() + GfGrammar.SUFFIX_APE;
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
			return getImg(getGfGrammar().abstrtree(tree));
		} catch (GfServiceException e) {
			return getError(e);
		}
	}


	private String getParsetreeAsHtml(String tree, String language) {
		try {
			return getImg(getGfGrammar().parsetree(tree, language));
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


	private List<SentenceDetail> formatTree(GfGrammar grammar, String lang, int index) {
		String tree = mGfWikiEntry.getTrees().getTrees().get(index);
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();
		l.add(new SentenceDetail("acewiki_details_syntree", getParsetreeAsHtml(tree, lang)));
		l.add(new SentenceDetail(
				LocaleResources.getString("acewiki_details_internal") + " (ASCII)",
				"<p><code>" + tree + "</code></p>"
				));
		l.add(new SentenceDetail("acewiki_details_internal", getAbstrtreeAsHtml(tree)));
		return l;
	}


	private Set<String> getLins(String tree, String language) {
		// Linearization of a single tree to all possible languages.
		Map<String, Set<String>> tl = treeLinearizations.get(tree);
		
		if (tl == null) {
			try {
				tl = getGfGrammar().linearize(tree);
				treeLinearizations.put(tree, tl);
			} catch (GfServiceException e) {
				// TODO find out what happened, i.e.
				// why was the tree not supported by the grammar.
				mLogger.warn("tree not supported by the grammar - {}: {}", language, tree);
				return null;
			}
		}
		
		return tl.get(language);
	}

	private boolean hasBadTrees(String language, TreeList treeList) {
		for (String tree : treeList.getTrees()) {
			Set<String> lins = getLins(tree, language);
			if (lins == null || lins.isEmpty()) {
				return true;
			}
		}
		return false;
	}


	private static GfWikiEntry createGfWikiEntry(GfGrammar grammar, String language, String text) {
		try {
			Set<String> trees = grammar.parse(text, language);
			if (trees == null) {
				throw new RuntimeException("No such language: " + language);
			}
			if (trees.isEmpty()) {
				// TODO this should be done properly; see GfTextOperator
				// If parsing fails: first char to lower case
				text = DefaultTextOperator.firstCharToLowerCase(text);
				trees = grammar.parse(text, language);
			}
			return new GfWikiEntry(language, text, new TreeList(trees));
		} catch (GfServiceException e) {
			throw new RuntimeException(e.getMessage());
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