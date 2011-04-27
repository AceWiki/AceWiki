package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.preditor.TextContainer;

// TODO synchronization?
public class ReasonerManager {
	
	private AceWikiReasoner reasoner;
	
	private Map<String, CachedAnswer> answerCache = new HashMap<String, CachedAnswer>();
	private Map<Long, CachedConcepts> conCache = new HashMap<Long, CachedConcepts>();
	private Map<Long, CachedIndividuals> indCache = new HashMap<Long, CachedIndividuals>();
	private Map<Long, CachedConcepts> supConCache = new HashMap<Long, CachedConcepts>();
	private Map<Long, CachedConcepts> subConCache = new HashMap<Long, CachedConcepts>();
	
	
	public ReasonerManager(AceWikiReasoner reasoner) {
		this.reasoner = reasoner;
	}
	
	public AceWikiReasoner getReasoner() {
		return reasoner;
	}
	
	private long getState() {
		return reasoner.getOntology().getStateID();
	}
	
	public boolean isCachedAnswerUpToDate(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null) {
			return a.state == getState();
		} else {
			return false;
		}
	}
	
	public List<TextContainer> getCachedAnswer(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null) {
			return new ArrayList<TextContainer>(a.list);
		} else {
			return null;
		}
	}
	
	public List<TextContainer> getAnswer(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null && a.state == getState()) {
			return new ArrayList<TextContainer>(a.list);
		} else {
			a = new CachedAnswer();
			a.list = reasoner.getAnswer(question);
			a.state = getState();
			answerCache.put(question.serialize(true), a);
			return a.list;
		}
	}

	/**
	 * Returns true if the concepts of this individual are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the cached concepts are up-to-date.
	 */
	public boolean areCachedConceptsUpToDate(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached concepts or null if there are no cached concepts. The returned
	 * concepts might not be up-to-date.
	 * 
	 * @return A list of the cached concepts of this individual.
	 */
	public  List<Concept> getCachedConcepts(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}

	/**
	 * Returns all concepts this individual belongs to.
	 * 
	 * @return A list of all concepts of this individual.
	 */
	public List<Concept> getConcepts(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = reasoner.getConcepts(ind);
			c.state = getState();
			conCache.put(ind.getId(), c);
			return c.list;
		}
	}

	/**
	 * Returns true if the individuals of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the cached individuals are up-to-date.
	 */
	public boolean areCachedIndividualsUpToDate(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null) {
			return i.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached individuals or null if there are no cached individuals. The returned
	 * individuals might not be up-to-date.
	 * 
	 * @return A list of the cached individuals of this concept.
	 */
	public  List<Individual> getCachedIndividuals(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null) {
			return new ArrayList<Individual>(i.list);
		} else {
			return null;
		}
	}

	/**
	 * Returns all individuals that belong to this concept.
	 * 
	 * @return A list of all individuals of this concept.
	 */
	public List<Individual> getIndividuals(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null && i.state == getState()) {
			return new ArrayList<Individual>(i.list);
		} else {
			i = new CachedIndividuals();
			i.list = reasoner.getIndividuals(concept);
			i.state = getState();
			indCache.put(concept.getId(), i);
			return i.list;
		}
	}

	/**
	 * Returns true if the suber-concepts of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the cached super-concepts are up-to-date.
	 */
	public boolean areCachedSuperConceptsUpToDate(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached super-concepts or null if there are no cached super-concepts. The returned
	 * super-concepts might not be up-to-date.
	 * 
	 * @return A list of the cached super-concepts of this concept.
	 */
	public  List<Concept> getCachedSuperConcepts(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}

	/**
	 * Returns all super-concepts of this concept.
	 * 
	 * @return A list of all super-concepts.
	 */
	public List<Concept> getSuperConcepts(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = reasoner.getSuperConcepts(concept);
			c.state = getState();
			supConCache.put(concept.getId(), c);
			return c.list;
		}
	}

	/**
	 * Returns true if the sub-concepts of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the cached sub-concepts are up-to-date.
	 */
	public boolean areCachedSubConceptsUpToDate(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached sub-concepts or null if there are no cached sub-concepts. The returned
	 * sub-concepts might not be up-to-date.
	 * 
	 * @return A list of the cached sub-concepts of this concept.
	 */
	public  List<Concept> getCachedSubConcepts(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}

	/**
	 * Returns all sub-concepts of this concept.
	 * 
	 * @return A list of all sub-concepts.
	 */
	public List<Concept> getSubConcepts(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = reasoner.getSubConcepts(concept);
			c.state = getState();
			subConCache.put(concept.getId(), c);
			return c.list;
		}
	}

	/**
	 * Returns the name of the reasoner.
	 * 
	 * @return The name of the reasoner.
	 */
	public String getReasonerName() {
		return reasoner.getReasonerName();
	}
	
	/**
	 * Return the version of the reasoner.
	 * 
	 * @return The version of the reasoner.
	 */
	public String getReasonerVersion() {
		return reasoner.getReasonerVersion();
	}

	/**
	 * Return the type of the reasoner.
	 * 
	 * @return The reasoner type.
	 */
	public String getReasonerType() {
		return reasoner.getReasonerType();
	}
	
	public String[] getInfo() {
		return reasoner.getInfo();
	}

	/**
	 * Loads the reasoner or reasoner interface.
	 */
	public void load() {
		reasoner.load();
	}
	
	public void loadElement(OntologyElement element) {
		reasoner.loadElement(element);
	}
	
	public void unloadElement(OntologyElement element) {
		reasoner.unloadElement(element);
	}
	
	public boolean isConsistent() {
		return reasoner.isConsistent();
	}
	
	public boolean isSatisfiable(Concept concept) {
		return reasoner.isSatisfiable(concept);
	}
	
	public void loadSentence(Sentence sentence) {
		reasoner.loadSentence(sentence);
	}
	
	public void unloadSentence(Sentence sentence) {
		reasoner.unloadSentence(sentence);
	}
	
	public void flushReasoner() {
		reasoner.flushReasoner();
	}
	
	
	private static class CachedAnswer {
		long state = -1;
		List<TextContainer> list;
	}
	
	private static class CachedIndividuals {
		long state = -1;
		List<Individual> list;
	}
	
	private static class CachedConcepts {
		long state = -1;
		List<Concept> list;
	}

}
