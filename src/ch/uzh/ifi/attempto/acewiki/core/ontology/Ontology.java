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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import ch.uzh.ifi.attempto.echocomp.Logger;
import ch.uzh.ifi.attempto.preditor.TextOperator;

// TODO Synchronization is not 100% secure.

/**
 * This class represents an AceWiki ontology which consists of ontology element definitions and
 * of ontological statements. Each ontology element has its own article that consists of
 * ontological statements.
 * 
 * @author Tobias Kuhn
 */
public class Ontology {
	
	private AceWikiReasoner reasoner;
	private StatementFactory statementFactory;
	private LanguageFactory languageFactory;
	private AceWikiStorage storage;
	
	private Map<String, OntologyElement> wordIndex = new TreeMap<String, OntologyElement>();
	private Map<Long, OntologyElement> idIndex = new TreeMap<Long, OntologyElement>();
	
	private final String name;
	private final String baseURI;
	private long idCount = 0;
	private long stateID = 0;
	
	private AceWikiTextOperator textOperator;
	
	private Map<String, String> parameters;
	
	/**
	 * Creates a new empty ontology with the given name and parameters.
	 * 
	 * @param name The name of the ontology.
	 * @param parameters The parameters.
	 */
	Ontology(String name, Map<String, String> parameters, AceWikiStorage storage) {
		this.name = name.toString();  // null value throws an exception
		this.parameters = parameters;
		this.storage = storage;
		
		// TODO: make this general
		reasoner = new AceWikiOWLReasoner(this);
		statementFactory = new StatementFactory(this);
		languageFactory = new ACELanguageFactory();
		
		String b = getParameter("baseuri");
		if (b == null || b.equals("")) {
			baseURI = "http://attempto.ifi.uzh.ch/acewiki/default/";
		} else {
			if (b.endsWith("/")) {
				baseURI = b;
			} else {
				baseURI = b + "/";
			}
		}
		
		textOperator = new AceWikiTextOperator(this);
	}
	
	public AceWikiReasoner getReasoner() {
		return reasoner;
	}
	
	public StatementFactory getStatementFactory() {
		return statementFactory;
	}
	
	public LanguageFactory getLanguageFactory() {
		return languageFactory;
	}
	
	public AceWikiStorage getStorage() {
		return storage;
	}
	
	synchronized void register(OntologyElement element) {
		if (contains(element)) {
			log("error: element already registered");
			throw new RuntimeException("Registration failed: Element is already registered.");
		}
		
		log("register: " + element);
		stateID++;
		
		if (element.getId() == -1) {
			element.setId(nextId());
		}
		idIndex.put(element.getId(), element);
		if (element.getId() > idCount) idCount = element.getId();
		
		for (String word : element.getWords()) {
			if (word == null) continue;
			
			if (getElement(word) == null) {
				wordIndex.put(word, element);
			} else if (getElement(word) != element) {
				log("error: word already used");
				throw new RuntimeException(
						"Registration failed: The word '" + word + "' is already used."
					);
			}
		}
		
		getReasoner().loadElement(element);
		getReasoner().flushReasoner();
	}
	
	/**
	 * Removes the given ontology element from the ontology.
	 * 
	 * @param element The ontology element to be removed.
	 */
	public synchronized void remove(OntologyElement element) {
		if (!contains(element)) {
			log("error: unknown element");
			return;
		}
		
		log("remove: " + element.getWord());
		stateID++;
		
		for (String word : element.getWords()) {
			if (word == null) continue;
			wordIndex.remove(word);
		}
		idIndex.remove(element.getId());
		for (Sentence s : element.getArticle().getSentences()) {
			retractSentence(s);
		}
		storage.save(element);
		
		getReasoner().unloadElement(element);
		getReasoner().flushReasoner();
	}
	
	synchronized void removeFromWordIndex(OntologyElement oe) {
		for (String word : oe.getWords()) {
			if (word != null) {
				wordIndex.remove(word);
			}
		}
		getReasoner().unloadElement(oe);
	}
	
	synchronized void addToWordIndex(OntologyElement oe) {
		for (String word : oe.getWords()) {
			if (word != null) {
				if (getElement(word) == null) {
					wordIndex.put(word, oe);
				} else if (getElement(word) != oe) {
					throw new RuntimeException(
							"Word update failed: The word '" + word + "' is already used."
						);
				}
			}
		}
		getReasoner().loadElement(oe);
	}
	
	/**
	 * Returns all the sentences that use the given word form (by word number) of the given
	 * ontology element.
	 * 
	 * @param element The ontology element.
	 * @param wordNumber The word number.
	 * @return A list of all sentence that contain the word.
	 */
	public synchronized List<Sentence> getReferences(OntologyElement element, int wordNumber) {
		List<Sentence> list = new ArrayList<Sentence>();
		for (OntologyElement el : idIndex.values()) {
			for (Sentence s : el.getArticle().getSentences()) {
				if (wordNumber == -1 && s.contains(element)) {
					list.add(s);
				} else if (wordNumber > -1 && s.contains(element, wordNumber)) {
					list.add(s);
				}
			}
		}
		return list;
	}

	/**
	 * Returns all the sentences that use the given ontology element (no matter which word form
	 * is used).
	 * 
	 * @param element The ontology element.
	 * @return A list of all sentence that contain the ontology element.
	 */
	public synchronized List<Sentence> getReferences(OntologyElement element) {
		return getReferences(element, -1);
	}
	
	/**
	 * Returns the ontology element with the given name, or null if there is no such element.
	 * 
	 * @param name The name of the ontology element.
	 * @return The ontology element.
	 */
	public OntologyElement getElement(String name) {
		return wordIndex.get(name);
	}
	
	/**
	 * Returns the ontology element with the given id, or null if there is no such element.
	 * 
	 * @param id The id of the ontology element.
	 * @return The ontology element.
	 */
	public OntologyElement get(long id) {
		return idIndex.get(id);
	}
	
	/**
	 * Returns all ontology elements. The list is a copy of the internal list.
	 * 
	 * @return A list of all ontology elements.
	 */
	public List<OntologyElement> getOntologyElements() {
		return new ArrayList<OntologyElement>(idIndex.values());
	}
	
	/**
	 * Returns true if the given ontology element is contained by the ontology (identity check).
	 * 
	 * @param ontologyElement The ontology element.
	 * @return true if the ontology element is contained by the ontology.
	 */
	public boolean contains(OntologyElement ontologyElement) {
		return idIndex.containsValue(ontologyElement);
	}
	
	/**
	 * Returns the name of the ontology.
	 * 
	 * @return The name of the ontology.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns the URI of the ontology (baseURI + name).
	 * 
	 * @return The URI of the ontology.
	 */
	public String getURI() {
		return baseURI + name;
	}
	
	/**
	 * Refreshes the given ontology element. All sentences that use the ontology element are
	 * updated.
	 * 
	 * @param element The ontology element to be refreshed.
	 */
	synchronized void refresh(OntologyElement element) {
		for (Sentence s : getReferences(element)) {
			if (s.isIntegrated()) {
				retractSentence(s);
				s.parse();
				commitSentence(s);
			} else {
				s.parse();
			}
		}
		storage.save(element);
	}
	
	/**
	 * Commits the sentence. This means that it is added to the reasoner. An integer value is
	 * returned that denotes the success or failure of the operation:
	 * 0 is returned if the operation succeeds.
	 * 1 is returned if it fails because the sentence introduces inconsistency into the knowledge
	 *   base.
	 * 2 is returned if the reasoner runs out of memory (this can occur sometimes with large
	 *   ontologies).
	 * 
	 * @param sentence The sentence to be commited.
	 * @return An integer value denoting the success/failure of the operation.
	 */
	protected synchronized int commitSentence(Sentence sentence) {
		if (sentence == null || sentence.isIntegrated()) return 0;
		
		if (!sentence.isReasonerParticipant()) {
			sentence.setIntegrated(true);
			return 0;
		}
		
		log("commit sentence");
		
		boolean inconsistencyEncountered = false;
		boolean errorEncountered = false;
		
		try {
			getReasoner().loadSentence(sentence);
		} catch (OutOfMemoryError err) {
			log("error: out of memory");
			System.gc();
			getReasoner().load();
			return 2;
		} catch (InconsistencyException ex) {
			inconsistencyEncountered = true;
		} catch (Exception ex) {
			errorEncountered = true;
			ex.printStackTrace();
		}
		
		log("check for consistency");
		if (errorEncountered) {
			log("error encountered!");
			getReasoner().unloadSentence(sentence);
			// TODO return a different value here:
			return 1;
		} else if (inconsistencyEncountered || !isConsistent()) {
			log("not consistent!");
			getReasoner().unloadSentence(sentence);
			return 1;
		} else {
			log("consistent!");
			sentence.setIntegrated(true);
			stateID++;
			return 0;
		}
	}
	
	/**
	 * Retracts the sentence. This means that the sentence is removed from the reasoner.
	 * 
	 * @param sentence The sentence to be retracted.
	 */
	protected synchronized void retractSentence(Sentence sentence) {
		if (
			sentence == null ||
			!sentence.isIntegrated() ||
			!sentence.isReasonerParticipant()
		) return;
		
		log("retract sentence");
		stateID++;
		getReasoner().unloadSentence(sentence);
		sentence.setIntegrated(false);
	}
	
	void log(String text) {
		Logger.log(name, "onto", 0, "onto", text);
	}
	
	/**
	 * Returns all concepts the given individual belongs to. The reasoner is used for this.
	 * 
	 * @param ind The individual.
	 * @return A list of all concepts of the individual.
	 * @see Individual#getConcepts()
	 */
	public List<Concept> getConcepts(Individual ind) {
		return getReasoner().getConcepts(ind);
	}
	
	/**
	 * Returns all individuals that belong to the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept.
	 * @return A list of all individuals of the concept.
	 * @see Concept#getIndividuals()
	 */
	public List<Individual> getIndividuals(Concept concept) {
		return getReasoner().getIndividuals(concept);
	}
	
	/**
	 * Returns all super-concepts of the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept for which all super-concepts should be returned.
	 * @return A list of all super-concepts.
	 * @see Concept#getSuperConcepts()
	 */
	public List<Concept> getSuperConcepts(Concept concept) {
		return getReasoner().getSuperConcepts(concept);
	}
	
	/**
	 * Returns all the sub-concepts of the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept for which all sub-concepts should be returned.
	 * @return A list of all sub-concepts.
	 * @see Concept#getSubConcepts()
	 */
	public List<Concept> getSubConcepts(Concept concept) {
		return getReasoner().getSubConcepts(concept);
	}
	
	/**
	 * Returns a list of ontology elements that answer the given question. The reasoner is used
	 * for this. In the case the sentence has the form "what is (Individual)?" then the answer
	 * contains all concepts the individual belongs to. Otherwise, the question is
	 * processed as a "DL Query" that describes a concept. In this case, the answer consists
	 * of all individuals that belong to the concept. The null value is returned if the
	 * sentence is not a question.
	 * 
	 * @param question The question to be answered.
	 * @return A list of ontology elements that are the answer for the question.
	 * @see Question#getAnswer()
	 */
	public List<OntologyElement> getAnswer(Question question) {
		return getReasoner().getAnswer(question);
	}
	
	/**
	 * Returns true if the ontology is consistent. If nothing goes wrong, this should always return
	 * true. The reasoner is used for this.
	 * 
	 * @return true if the ontology is consistent.
	 */
	public boolean isConsistent() {
		return getReasoner().isConsistent();
	}
	
	/**
	 * Checks if the given concept is satisfiable. The reasoner is used for this.
	 * 
	 * @param concept The concept.
	 * @return true if the concept is satisfiable.
	 */
	public synchronized boolean isSatisfiable(Concept concept) {
		return getReasoner().isSatisfiable(concept);
	}
	
	private long nextId() {
		return ++idCount;
	}
	
	/**
	 * Returns the state id of the ontology. This id increases each time the ontology changes (more
	 * precisely: each time the part of the ontology that participates in reasoning changes). This
	 * id is used to find out whether cached information is still valid or has to be recalculated.
	 * 
	 * @return The state id of the ontology.
	 */
	long getStateID() {
		return stateID;
	}
	
	/**
	 * Returns the text operator.
	 * 
	 * @return The text operator.
	 */
	public TextOperator getTextOperator() {
		return textOperator;
	}
	
	public String getParameter(String name) {
		return parameters.get(name);
	}
	
}
