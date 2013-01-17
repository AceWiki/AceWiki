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
import java.util.Locale;

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.acewiki.core.TopicChanger;
import ch.uzh.ifi.attempto.base.LocaleResources;
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
	private String mLanguageName;
	private final Locale mLocale;
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

		setLexiconChanger(GeneralTopic.NORMAL_TYPE, new TopicChanger());
		setLexiconChanger(TypeGfModule.INTERNAL_TYPE, new GfModuleChanger());

		// TODO generalize:
		if (mLanguage.endsWith("Ger")) {
			mLocale = new Locale("de", "DE");
			mLanguageName = LocaleResources.getString(mLocale, "general_language_this");
		} else if (mLanguage.endsWith("Eng")) {
			mLocale = new Locale("en", "US");
			mLanguageName = LocaleResources.getString(mLocale, "general_language_this");
		} else if (mLanguage.endsWith("Spa")) {
			mLocale = new Locale("es", "ES");
			mLanguageName = LocaleResources.getString(mLocale, "general_language_this");
		} else if (mLanguage.endsWith("Ace")) {
			mLocale = new Locale("en", "US");
			mLanguageName = "ACE";
		} else {
			mLocale = LocaleResources.defaultLocale;
			mLanguageName = mLanguage;
		}

		if (mLanguageName != mLanguage && mLanguage.startsWith("Disamb")) {
			mLanguageName += " (disamb.)";
		}
	}

	public String getLanguage() {
		return mLanguage;
	}

	public String getLanguageName() {
		return mLanguageName;
	}

	public Locale getLocale() {
		return mLocale;
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

	/**
	 * If there is an issue with the sentence (e.g. if it is ambiguous)
	 * then we can return a suggestion to ask the user to immediately
	 * deal with the issue.
	 *
	 * TODO: We currently don't do anything, e.g. ambiguity can be dealt with
	 * later as well.
	 */
	public SentenceSuggestion getSuggestion(Sentence sentence) {
		/*
		if (sentence.getNumberOfParseTrees() > 1) {
			return new DisambiguationSuggestion(sentence);
		}
		 */
		return null;
	}
}