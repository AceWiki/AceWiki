package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents ACE questions.
 * 
 * @author Tobias Kuhn
 */
public class Question extends Sentence {
	
	private boolean uncertainAnswers = false;
	
	private List<OntologyElement> answerCache;
	private long answerCacheStateID = -1;
	
	/**
	 * Creates a new question.
	 * 
	 * @param text The question text.
	 * @param owner The owner ontology element.
	 */
	protected Question(String text, OntologyElement owner) {
		super(text, owner);
	}
	
	public boolean areUncertainAnswersEnabled() {
		return uncertainAnswers;
	}
	
	public void setUncertainAnswersEnabled(boolean uncertainAnswers) {
		if (this.uncertainAnswers == uncertainAnswers) return;
		answerCache = null;
		answerCacheStateID = -1;
		this.uncertainAnswers = uncertainAnswers;
	}
	
	/**
	 * Returns all ontology elements that answer this question. In the case the sentence has the
	 * form "what is (Individual)?" then the answer contains all concepts the individual belongs
	 * to. Otherwise, the question is processed as a "DL Query" that describes a concept. In this
	 * case, the answer consists of all individuals that belong to the concept. 
	 * 
	 * @return A list of ontology elements that are the answer for the question.
	 * @see Ontology#getAnswer(Question)
	 */
	public synchronized List<OntologyElement> getAnswer() {
		Ontology o = getOntology();
		if (answerCacheStateID != o.getStateID()) {
			answerCache = o.getAnswer(this);
			answerCacheStateID = o.getStateID();
		}
		if (answerCache == null) {
			return null;
		} else {
			return new ArrayList<OntologyElement>(answerCache);
		}
	}
	
	/**
	 * Returns the cached answer. Null is returned if there is no cached answer. This returned
	 * answer might not be up-to-date.
	 * 
	 * @return A list of ontology elements that are the cached answer for the question.
	 */
	public List<OntologyElement> getCachedAnswer() {
		if (answerCache == null) return null;
		return new ArrayList<OntologyElement>(answerCache);
	}
	
	/**
	 * Returns true if the answer to the question is cached and up-to-date and thus does not have
	 * to be recalculated.
	 * 
	 * @return true if the answer is cached.
	 */
	public boolean isAnswerCached() {
		return answerCacheStateID == getOntology().getStateID();
	}
	
	public boolean isReasonerParticipant() {
		return false;
	}
	
	public boolean isReadOnly() {
		return false;
	}

}
