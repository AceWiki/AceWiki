// This file is part of AceWiki.
// Copyright 2013, AceWiki developers.
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
import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringEscapeUtils;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.SWRLRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.MultilingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.PrettyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.acewiki.owl.AceWikiOWLReasoner2;
import ch.uzh.ifi.attempto.acewiki.owl.OWLSentence;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.MultiTextContainer;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * This class represents a sentence for the GF AceWiki engine.
 * A "sentence" is a tree set that can be linearized into multiple
 * languages.
 *
 * @author Kaarel Kaljurand
 */
public abstract class GfSentence extends MultilingualSentence implements OWLSentence {

	private final Logger mLogger = LoggerFactory.getLogger(GfSentence.class);

	protected final GfGrammar mGfGrammar;
	protected final GfWikiEntry mGfWikiEntry;

	// Use the original input in the text container
	// TODO: this is a hack
	boolean mUseOriginal = true;

	// Maps a language identifier to the set of linearizations (text containers) in this language
	final Map<String, MultiTextContainer> textContainers = new HashMap<>();

	// Maps a tree to the set of linearizations for each language.
	// ie. Map<Tree, Map<Language, Set<Linearization>>>
	// Lazy initialized as tree linearizations are requested, but done for all languages at once - performance reasons.
	private final Map<String, Map<String, Set<String>>> treeLinearizations = new HashMap<>();


	// These fields are evaluated lazily
	private Boolean isReasonable;
	private Boolean isOWL;
	private Boolean isOWLSWRL;
	private Set<OWLAxiom> owlAxioms;

	/**
	 * Creates a declaration from an existing wiki entry (from the storage).
	 * Each wiki entry is tagged with the language in which the entry was created.
	 * Each wiki entry also contains the original text that was used to create the
	 * trees. In case the trees can not be linearized (because the grammar has changed)
	 * then we try to parse the original text instead (which might also fail).
	 */
	public GfSentence(GfGrammar grammar, GfWikiEntry entry) {
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
	public GfSentence(GfGrammar grammar, String language, String text) {
		mGfGrammar = grammar;
		mGfWikiEntry = createGfWikiEntry(grammar, language, text);
	}

	public static GfSentence createGfSentence(GfGrammar grammar, GfWikiEntry entry) {
		if (entry.getText().contains("?")) {
			return new GfQuestion(grammar, entry);
		}
		return new GfDeclaration(grammar, entry);
	}

	/**
	 * Maps this declaration to its visual representation in the given language.
	 * The declaration is a set of trees.
	 * Each tree can in principle have multiple linearizations (variants), but we currently
	 * consider only the first (canonical) variant.
	 * We have to handle the following:
	 *   - the linearization fails (e.g. tree is malformed)
	 *   - a tree has an empty set of linearizations
	 *   - a linearization is an empty string
	 *   - a lineariation repeats
	 *   - the user wants to see what he/she originally entered not a rewrite into the canonical variant
	 */
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
						mLogger.info("getTextContainer: null {}: {}", language, tree);
						// TODO do it properly
						tmp.add(new TextContainer(new TextElement("-NULL-" + tree)));
					} else if (lins.isEmpty()) {
						mLogger.info("getTextContainer: empty {}: {}", language, tree);
						// TODO do it properly
						tmp.add(new TextContainer(new TextElement("-EMPTY-" + tree)));
					} else {
						String lin = lins.iterator().next();
						if (lin.isEmpty() || seen.contains(lin)) {
							// Don't show an empty lin and the same lin twice
						} else {
							seen.add(lin);
							tmp.add(makeTextContainer(to, lin));
						}
					}
				}
			}
			if (tmp.isEmpty()) {
				tmp.add(new TextContainer(new TextElement("-NO_LINEARIZATION_FOUND-")));
			}
			mtc = new MultiTextContainer(tmp);
			textContainers.put(language, mtc);
		}
		return mtc;
	}

	/**
	 * TODO: this is temporary. Also note that it does not have effect on the
	 * original input as this is always taken from mGfWikiEntry.getText()
	 */
	public void clearLinearizations() {
		textContainers.clear();
		treeLinearizations.clear();
		mUseOriginal = false;
	}


	TextContainer makeTextContainer(TextOperator to, String str) {
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
			l.addAll(getSemantics(index));
		}

		l.addAll(formatTree(mGfGrammar, lang, index));

		return l;
	}


	public int getNumberOfRepresentations() {
		return mGfWikiEntry.getTrees().size();
	}


	public List<String> getParseTrees() {
		return mGfWikiEntry.getTrees().getTrees();
	}


	public GfWikiEntry getGfWikiEntry() {
		return mGfWikiEntry;
	}


	public String serialize() {
		return GfGrammar.serialize(mGfWikiEntry);
	}


	public void update() {
		Set<Set<OWLAxiom>> setOfSetOfAxiom = null;
		String uri = getOntology().getURI();
		try {
			setOfSetOfAxiom = GfOwlConverter.convert(mGfGrammar, uri, mGfWikiEntry);
		} catch (OWLOntologyCreationException e1) {
			// TODO
		}

		if (setOfSetOfAxiom == null || setOfSetOfAxiom.isEmpty()) {
			isOWLSWRL = isOWL = isReasonable = false;
			owlAxioms = new HashSet<OWLAxiom>();
		} else {
			isOWLSWRL = isOWL = isReasonable = true;
			owlAxioms = GfOwlConverter.disambiguate(setOfSetOfAxiom);

			// TODO: currently not reasoning with SWRL rules, this should
			// be controlled by the profile instead
			for (OWLAxiom ax : owlAxioms) {
				if (ax instanceof SWRLRule) {
					isOWL = isReasonable = false;
					mLogger.info("Axiom is SWRL rule: {}", ax);
					break;
				}
			}
		}

		// TODO: check also questions somehow, e.g. EL probably does not allow inverse properties in questions
		if (isReasonable && this instanceof Declaration) {
			AceWikiOWLReasoner2 reasoner = (AceWikiOWLReasoner2) getOntology().getReasoner().getWrappedReasoner();
			isReasonable = GfOwlConverter.isReasonable(reasoner, owlAxioms);
		}

		if (!isReasonable && isIntegrated()) {
			super.setIntegrated(false);
		}
	}


	// TODO: this method does not make sense for GF-wiki entries
	// because they can be ambiguous.
	public String getPrettyOWL() {
		return null;
	}


	public boolean isReasonable() {
		if (isReasonable == null) {
			update();
		}
		return isReasonable;
	}

	public boolean isOWL() {
		if (isOWL == null) {
			update();
		}
		return isOWL;
	}

	public boolean isOWLSWRL() {
		if (isOWLSWRL == null) {
			update();
		}
		return isOWLSWRL;
	}

	public Set<OWLAxiom> getOWLAxioms() {
		if (owlAxioms == null) {
			update();
		}
		return owlAxioms;
	}

	/**
	 * Returns the grammar object.
	 * 
	 * @return The grammar object.
	 */
	protected GfGrammar getGfGrammar() {
		return mGfGrammar;
	}


	// Return some of the APE analysis of the tree at the given index.
	// The APE analysis is obtained by first linearizing the tree in "Ape".
	// This only works if the wiki is ACE-based.
	//
	// TODO: experimental
	private List<SentenceDetail> getSemantics(int index) {
		String tree = mGfWikiEntry.getTrees().getTrees().get(index);
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();

		if (tree == null) {
			l.add(new SentenceDetail("ERROR", "Statement is not well-formed"));
			return l;
		}

		try {
			ACEText acetext = GfWikiUtils.getACEText(mGfGrammar, tree);
			ACEParserResult pr = GfWikiUtils.parse(acetext, getOntology().getURI(), PARAPHRASE1, OWLFSSPP, DRSPP);

			l.add(new SentenceDetail("ACE", "<pre>" + StringEscapeUtils.escapeHtml(acetext.getText()) + "</pre>"));
			l.add(new SentenceDetail("ACE (paraphrase)", "<pre>" + StringEscapeUtils.escapeHtml(pr.get(PARAPHRASE1)) + "</pre>"));
			l.add(new SentenceDetail("OWL", "<pre>" + StringEscapeUtils.escapeHtml(pr.get(OWLFSSPP)) + "</pre>"));
			l.add(new SentenceDetail("DRS", "<pre>" + StringEscapeUtils.escapeHtml(pr.get(DRSPP)) + "</pre>"));
			l.add(new SentenceDetail("Lexicon", "<pre>" + StringEscapeUtils.escapeHtml(Joiner.on('\n').join(acetext.getLexicon().getEntries())) + "</pre>"));
			l.add(new SentenceDetail("Messages",
					"<pre>" + StringEscapeUtils.escapeHtml(Joiner.on('\n').join(pr.getMessageContainer().getMessages())) + "</pre>"));
		} catch (Exception e) {
			l.add(new SentenceDetail("ERROR", e.getMessage()));
		}

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


	Set<String> getLins(String tree, String language) {
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
}
