// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.preditor.TextContainer;

/**
 * This reasoner class wraps another reasoner and adds caching functionality.
 * 
 * @author Tobias Kuhn
 */
public class CachingReasoner implements AceWikiReasoner {
	
	private AceWikiReasoner wrappedReasoner;
	private Ontology ontology;
	
	private Map<String, CachedAnswer> answerCache = new HashMap<String, CachedAnswer>();
	private Map<Long, CachedConcepts> conCache = new HashMap<Long, CachedConcepts>();
	private Map<Long, CachedIndividuals> indCache = new HashMap<Long, CachedIndividuals>();
	private Map<Long, CachedConcepts> supConCache = new HashMap<Long, CachedConcepts>();
	private Map<Long, CachedConcepts> subConCache = new HashMap<Long, CachedConcepts>();
	
	/**
	 * Creates a new caching reasoner for the given reasoner to be wrapped.
	 * 
	 * @param wrappedReasoner The reasoner to be wrapped.
	 */
	CachingReasoner(AceWikiReasoner wrappedReasoner) {
		this.wrappedReasoner = wrappedReasoner;
	}

	public synchronized void init(Ontology ontology) {
		this.ontology = ontology;
	}
	
	/**
	 * Returns the wrapped reasoner.
	 * 
	 * @return The wrapped reasoner.
	 */
	public AceWikiReasoner getWrappedReasoner() {
		return wrappedReasoner;
	}
	
	private long getState() {
		return ontology.getStateID();
	}
	
	/**
	 * Returns whether the there is an up-to-date cached answer for the given question.
	 * 
	 * @param question The question.
	 * @return true if there is an up-to-date cached answer.
	 */
	public synchronized boolean isCachedAnswerUpToDate(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null) {
			return a.state == getState();
		} else {
			return false;
		}
	}
	
	/**
	 * Returns the cached answer for the given question, or null if no cached answer exists.
	 * The answer might not be up-to-date.
	 * 
	 * @param question The question.
	 * @return The cached answer.
	 */
	public synchronized List<TextContainer> getCachedAnswer(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null) {
			return new ArrayList<TextContainer>(a.list);
		} else {
			return null;
		}
	}
	
	/**
	 * Returns the answer for the given question. The cache is used if it is up-to-date.
	 */
	public synchronized List<TextContainer> getAnswer(Question question) {
		CachedAnswer a = answerCache.get(question.serialize(true));
		if (a != null && a.state == getState()) {
			return new ArrayList<TextContainer>(a.list);
		} else {
			a = new CachedAnswer();
			a.list = wrappedReasoner.getAnswer(question);
			a.state = getState();
			answerCache.put(question.serialize(true), a);
			return a.list;
		}
	}

	/**
	 * Returns true if the concepts of the given individual are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @param ind The individual.
	 * @return true if the cached concepts are up-to-date.
	 */
	public synchronized boolean areCachedConceptsUpToDate(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached concepts for the given individual or null if there are no cached
	 * concepts. The returned concepts might not be up-to-date.
	 * 
	 * @param ind The individual.
	 * @return A list of the cached concepts for the given individual.
	 */
	public synchronized List<Concept> getCachedConcepts(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}
	
	public synchronized List<Concept> getConcepts(Individual ind) {
		CachedConcepts c = conCache.get(ind.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = wrappedReasoner.getConcepts(ind);
			c.state = getState();
			conCache.put(ind.getId(), c);
			return c.list;
		}
	}

	/**
	 * Returns true if the individuals of the given concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @param concept The concept.
	 * @return true if the cached individuals are up-to-date.
	 */
	public synchronized boolean areCachedIndividualsUpToDate(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null) {
			return i.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached individuals for the given concept or null if there are no cached
	 * individuals. The returned individuals might not be up-to-date.
	 * 
	 * @param concept The concept.
	 * @return A list of the cached individuals for the given concept.
	 */
	public synchronized List<Individual> getCachedIndividuals(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null) {
			return new ArrayList<Individual>(i.list);
		} else {
			return null;
		}
	}
	
	public synchronized List<Individual> getIndividuals(Concept concept) {
		CachedIndividuals i = indCache.get(concept.getId());
		if (i != null && i.state == getState()) {
			return new ArrayList<Individual>(i.list);
		} else {
			i = new CachedIndividuals();
			i.list = wrappedReasoner.getIndividuals(concept);
			i.state = getState();
			indCache.put(concept.getId(), i);
			return i.list;
		}
	}

	/**
	 * Returns true if the suber-concepts of the given concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @param concept The concept.
	 * @return true if the cached super-concepts are up-to-date.
	 */
	public synchronized boolean areCachedSuperConceptsUpToDate(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached super-concepts for the given concept or null if there are no cached
	 * super-concepts. The returned super-concepts might not be up-to-date.
	 * 
	 * @param concept The concept.
	 * @return A list of the cached super-concepts for the given concept.
	 */
	public synchronized List<Concept> getCachedSuperConcepts(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}
	
	public synchronized List<Concept> getSuperConcepts(Concept concept) {
		CachedConcepts c = supConCache.get(concept.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = wrappedReasoner.getSuperConcepts(concept);
			c.state = getState();
			supConCache.put(concept.getId(), c);
			return c.list;
		}
	}

	/**
	 * Returns true if the sub-concepts of the given concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @param concept The concept.
	 * @return true if the cached sub-concepts are up-to-date.
	 */
	public synchronized boolean areCachedSubConceptsUpToDate(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null) {
			return c.state == getState();
		} else {
			return false;
		}
	}

	/**
	 * Returns the cached sub-concepts for the given concept or null if there are no cached
	 * sub-concepts. The returned sub-concepts might not be up-to-date.
	 * 
	 * @param concept The concept.
	 * @return A list of the cached sub-concepts for the given concept.
	 */
	public synchronized List<Concept> getCachedSubConcepts(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null) {
			return new ArrayList<Concept>(c.list);
		} else {
			return null;
		}
	}
	
	public synchronized List<Concept> getSubConcepts(Concept concept) {
		CachedConcepts c = subConCache.get(concept.getId());
		if (c != null && c.state == getState()) {
			return new ArrayList<Concept>(c.list);
		} else {
			c = new CachedConcepts();
			c.list = wrappedReasoner.getSubConcepts(concept);
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
		return wrappedReasoner.getReasonerName();
	}
	
	/**
	 * Return the version of the reasoner.
	 * 
	 * @return The version of the reasoner.
	 */
	public String getReasonerVersion() {
		return wrappedReasoner.getReasonerVersion();
	}

	/**
	 * Return the type of the reasoner.
	 * 
	 * @return The reasoner type.
	 */
	public String getReasonerType() {
		return wrappedReasoner.getReasonerType();
	}
	
	public Map<String, String> getInfo() {
		return wrappedReasoner.getInfo();
	}
	
	/**
	 * Loads the reasoner or reasoner interface.
	 */
	public synchronized void load() {
		wrappedReasoner.load();
	}
	
	public synchronized void loadElement(OntologyElement element) {
		wrappedReasoner.loadElement(element);
	}
	
	public synchronized void unloadElement(OntologyElement element) {
		wrappedReasoner.unloadElement(element);
	}
	
	public synchronized boolean isConsistent() {
		return wrappedReasoner.isConsistent();
	}
	
	public synchronized boolean isSatisfiable(Concept concept) {
		return wrappedReasoner.isSatisfiable(concept);
	}
	
	public synchronized void loadSentence(Sentence sentence) {
		wrappedReasoner.loadSentence(sentence);
	}
	
	public synchronized void unloadSentence(Sentence sentence) {
		wrappedReasoner.unloadSentence(sentence);
	}
	
	public synchronized void flushElements() {
		wrappedReasoner.flushElements();
	}
	
	
	// Small internal classes for cached objects:
	
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
