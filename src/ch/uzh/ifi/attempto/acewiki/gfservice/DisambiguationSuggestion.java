package ch.uzh.ifi.attempto.acewiki.gfservice;

import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;

public class DisambiguationSuggestion implements SentenceSuggestion {

	private Sentence mSentence;

	DisambiguationSuggestion(Sentence sentence) {
		mSentence = sentence;
	}

	public String getMessage() {
		return "Your sentence has " + mSentence.getNumberOfParseTrees() + " parse trees.\n" +
				"Which one did you have in mind?";
	}

	public String[] getOptions() {
		// TODO: implement
		return new String[] {"1", "2", "Keep all"};
	}

	public Sentence getSentence(String option) {
		if (option.equals("1")) {
			// TODO: implement
		}
		return mSentence;
	}
}