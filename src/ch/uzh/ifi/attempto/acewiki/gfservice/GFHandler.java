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
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.aceowl.ProperNameChanger;
import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This is a language handler for languages written in GF.
 * 
 * @author Kaarel Kaljurand
 */
public class GFHandler extends AbstractLanguageHandler {

	private final String mLanguage;
	private final EditorController mEditorController = new EditorController();
	private final GFGrammar mGfGrammar;

	private TextOperator mTextOperator;

	/**
	 * Creates a new GF handler for the given language.
	 * 
	 * @param language The name of the language.
	 * @param gfGrammar The grammar object.
	 */
	public GFHandler(String language, GFGrammar gfGrammar) {
		mLanguage = language;
		mGfGrammar = gfGrammar;

		setLexiconChanger(GFEngine.TYPE_ARTICLE, new ArticleChanger());
		setLexiconChanger(GFEngine.TYPE_TEST, new ProperNameChanger());
	}

	public String getLanguage() {
		return mLanguage;
	}

	public void init(Ontology ontology) {
		mTextOperator = new GfTextOperator(ontology);
	}

	public TextOperator getTextOperator() {
		return mTextOperator;
	}

	public List<Sentence> extractSentences(TextContainer tc, PredictiveParser parser) {
		List<Sentence> l = new ArrayList<Sentence>();
		l.add(new GFDeclaration(tc.getText(), mLanguage, mGfGrammar));
		return l;
	}

	public PredictiveParser getPredictiveParser() {
		return new GFPredictiveParser(mGfGrammar, mLanguage);
	}

	public EditorController getEditorController() {
		return mEditorController;
	}

	public SentenceSuggestion getSuggestion(Sentence sentence) {
		if (sentence.getNumberOfParseTrees() > 1) {
			return new DisambiguationSuggestion(sentence);
		}
		return null;
	}
}
