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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.acewiki.core.TopicChanger;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextOperator;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;

/**
 * This is a language handler for languages written in GF.
 * 
 * @author Kaarel Kaljurand
 */
public class GfHandler extends AbstractLanguageHandler {

	// This map is here to override the Java Locale-constructor.
	// Note that we need to provide also the country code for the UI localization to work,
	// TODO maybe this can be made more flexible.
	public static final ImmutableMap<String, Locale> ISO3_TO_LOCALE =
			new ImmutableMap.Builder<String, Locale>()
			.put("Dut", new Locale("nl"))
			.put("Eng", new Locale("en", "US"))
			.put("Est", new Locale("et"))
			.put("Fin", new Locale("fi"))
			.put("Fre", new Locale("fr"))
			.put("Ger", new Locale("de", "DE"))
			.put("Ron", new Locale("ro"))
			.put("Spa", new Locale("es", "ES"))
			.put("Tha", new Locale("th"))
			.build();

	private static final Splitter LOCALE_SPLITTER = Splitter.onPattern("[-_]").trimResults().omitEmptyStrings();

	private final Logger mLogger = LoggerFactory.getLogger(GfHandler.class);

	private final String mLanguage;
	private final String mLanguageName;
	private final Locale mLocale;
	private final GfGrammar mGfGrammar;

	private TextOperator mTextOperator;

	// We use an editor controller which shows the largest categories in the
	// grammar and labels them in a language sensitive way.
	// TODO: we currently do not update that editor controller, even though
	// the sizes and labels of the categories can change since the grammar is
	// editable in the general case.
	private final EditorController mEditorController;

	/**
	 * Creates a new GF handler for the given language.
	 * 
	 * @param language The name of the language.
	 * @param gfGrammar The grammar object.
	 */
	public GfHandler(String language, GfGrammar gfGrammar) {
		mLanguage = language;
		mGfGrammar = gfGrammar;

		setLexiconChanger(GeneralTopic.NORMAL_TYPE, new TopicChanger());
		setLexiconChanger(TypeGfModule.INTERNAL_TYPE, new GfModuleChanger());

		Locale locale = guessLocale(language, gfGrammar);
		String languageName = getLocaleDisplayLanguage(locale);

		// Some ACE-specific overriding
		if (mLanguage.endsWith("Ace")) {
			languageName = "ACE";
			if (locale != null) locale = ISO3_TO_LOCALE.get("Eng");
		} else if (mLanguage.endsWith("Ape")) {
			languageName = "ACE+lex";
			if (locale != null) locale = ISO3_TO_LOCALE.get("Eng");
		}

		if (locale == null) {
			locale = LocaleResources.defaultLocale;
		}

		if (languageName == null) {
			languageName = mLanguage;
		}

		// Support for disambiguation languages (as in MOLTO Phrasebook),
		// which have a name in the form "DisambPhrasebookEng".
		if (languageName != mLanguage && mLanguage.startsWith(GfGrammar.PREFIX_DISAMB)) {
			languageName += " (disamb.)"; // TODO: make the "disamb." localizable
		}

		// Capitalize first letter:
		languageName = languageName.substring(0, 1).toUpperCase() + languageName.substring(1);

		mLocale = locale;
		mLanguageName = languageName;
		mEditorController = GfEditorControllerFactory.createFromCats(mGfGrammar, mLanguage);

		mLogger.info("Locale: {} -> {} -> {}", language, mLocale, mLanguageName);
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
		String tokenText = GfGrammar.GF_TOKEN_JOINER.join(tc.getTokens());
		List<Sentence> l = new ArrayList<Sentence>();
		l.add(new GfDeclaration(mGfGrammar, mLanguage, tokenText));
		return l;
	}

	public PredictiveParser getPredictiveParser() {
		return new GfPredictiveParser(mGfGrammar, mLanguage);
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


	/**
	 * Determines the locale on the basis of the concrete language and the grammar.
	 */
	private static Locale guessLocale(String language, GfGrammar grammar) {
		// The grammar can explicitly define the locale, if it does then we use this locale.
		Set<String> locales = grammar.getGrammar().getLanguages().get(language);
		if (! locales.isEmpty()) {
			// For some reason the locale set can contain more than one element,
			// we just take the first one.
			String code = locales.iterator().next();
			Locale locale = null;

			List<String> localeSplits = ImmutableList.copyOf(LOCALE_SPLITTER.split(code));
			switch (localeSplits.size()) {
			case 1: locale = new Locale(localeSplits.get(0)); break;
			case 2: locale = new Locale(localeSplits.get(0), localeSplits.get(1)); break;
			case 3: locale = new Locale(localeSplits.get(0), localeSplits.get(1), localeSplits.get(2)); break;
			}
			if (locale != null && locale.toString().length() > 0) {
				return locale;
			}
		}

		// Otherwise we guess the locale on the basis of the concrete language name.
		// We assume that the name contains the language information in the last 3 chars.
		// If this 3-letter sequence is not found in our map, then we assume it is
		// a 3-letter ISO language code (capitalized).
		int len = language.length();
		if (len >= 3) {
			String iso3 = language.substring(len-3, len);
			Locale locale = ISO3_TO_LOCALE.get(iso3);
			if (locale == null && Character.isUpperCase(iso3.charAt(0))) {
				locale = new Locale(iso3);
			}
			if (locale != null && locale.toString().length() > 0) {
				return locale;
			}
		}
		// Guessing failed, return null
		return null;
	}


	private static String getLocaleDisplayLanguage(Locale locale) {
		if (locale == null) {
			return null;
		}
		return locale.getDisplayLanguage(locale);
	}
}
