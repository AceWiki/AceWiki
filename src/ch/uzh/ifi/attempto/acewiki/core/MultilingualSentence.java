package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

public abstract class MultilingualSentence extends AbstractSentence {

	public abstract List<SentenceDetail> getTranslations(String currentLanguage);

}
