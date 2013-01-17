package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

/**
 * This class represents a sentence for a multilingual AceWiki engine.
 * 
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public abstract class MultilingualSentence extends AbstractSentence {

	/**
	 * Returns a list of sentence details describing translations into all other languages.
	 * 
	 * @param currentLanguage The current language (to be excluded).
	 * @return A list of sentence details on translations.
	 */
	public abstract List<SentenceDetail> getTranslations(String currentLanguage);

}
