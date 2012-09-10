package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

import ch.uzh.ifi.attempto.base.TextContainerSet;
import ch.uzh.ifi.attempto.base.TextElement;

public abstract class MultilingualSentence extends AbstractSentence {

	@Override
	public List<TextElement> getTextElements(String language) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isReasonable() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean contains(OntologyElement e) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void update() {
		// TODO Auto-generated method stub

	}

	@Override
	public List<SentenceDetail> getDetails(String language) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String serialize() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TextContainerSet getTextContainerSet(String language) {
		// TODO Auto-generated method stub
		return null;
	}

	public abstract List<SentenceDetail> getTranslations(String language);

}
