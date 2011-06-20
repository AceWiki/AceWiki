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
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import ch.uzh.ifi.attempto.echocomp.Logger;
import ch.uzh.ifi.attempto.preditor.TextOperator;

/**
 * This class represents an AceWiki ontology which consists of ontology element definitions and
 * of ontological statements. Each ontology element has its own article that consists of
 * ontological statements.
 * 
 * @author Tobias Kuhn
 */
public class Ontology {
	
	private LanguageEngine languageEngine;
	private CachingReasoner reasoner;
	private StatementFactory statementFactory;
	private AceWikiStorage storage;
	private Logger logger;
	
	private Map<String, OntologyElement> wordIndex = new TreeMap<String, OntologyElement>();
	private Map<Long, OntologyElement> idIndex = new TreeMap<Long, OntologyElement>();
	
	private final String name;
	private final String baseURI;
	private long idCount = 0;
	private long stateID = 0;
	
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
		
		logger = new Logger(parameters.get("context:logdir") + "/" + name, "onto", 0);
		
		languageEngine = AbstractLanguageEngine.createLanguageEngine(this);
		reasoner = new CachingReasoner(languageEngine.getReasoner());
		reasoner.init(this);
		statementFactory = new StatementFactory(this);
		
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
	}
	
	/**
	 * Returns the languge engine.
	 * 
	 * @return The language engine.
	 */
	public LanguageEngine getLanguageEngine() {
		return languageEngine;
	}
	
	/**
	 * Returns the reasoner object in the form of a caching reasoner.
	 * 
	 * @return The reasoner.
	 */
	public CachingReasoner getReasoner() {
		return reasoner;
	}
	
	/**
	 * Returns the statement factory.
	 * 
	 * @return The statement factory.
	 */
	public StatementFactory getStatementFactory() {
		return statementFactory;
	}
	
	/**
	 * Returns the language factory.
	 * 
	 * @return The language factory.
	 */
	public LanguageFactory getLanguageFactory() {
		return languageEngine.getLanguageFactory();
	}
	
	/**
	 * Returns the text operator.
	 * 
	 * @return The text operator.
	 */
	public TextOperator getTextOperator() {
		return getLanguageFactory().getTextOperator();
	}
	
	/**
	 * Returns the storage object.
	 * 
	 * @return The storage object.
	 */
	public AceWikiStorage getStorage() {
		return storage;
	}
	
	/**
	 * Registers the given ontology element.
	 * 
	 * @param element The ontology element to register.
	 */
	public synchronized void register(OntologyElement element) {
		if (contains(element)) {
			log("error: element already registered");
			throw new RuntimeException("Registration failed: Element is already registered.");
		}
		element.initOntology(this);
		
		log("register: " + element);
		stateID++;
		
		if (element.getId() == -1) {
			element.initId(nextId());
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
		getReasoner().flushElements();
		
		getStorage().save(element);
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
		getReasoner().flushElements();
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
	public synchronized OntologyElement getElement(String name) {
		return wordIndex.get(name);
	}
	
	/**
	 * Returns the ontology element with the given id, or null if there is no such element.
	 * 
	 * @param id The id of the ontology element.
	 * @return The ontology element.
	 */
	public synchronized OntologyElement get(long id) {
		return idIndex.get(id);
	}
	
	/**
	 * Returns all ontology elements. The list is a copy of the internal list.
	 * 
	 * @return A list of all ontology elements.
	 */
	public synchronized List<OntologyElement> getOntologyElements() {
		return new ArrayList<OntologyElement>(idIndex.values());
	}
	
	/**
	 * Returns true if the given ontology element is contained by the ontology (identity check).
	 * 
	 * @param ontologyElement The ontology element.
	 * @return true if the ontology element is contained by the ontology.
	 */
	public synchronized boolean contains(OntologyElement ontologyElement) {
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
				s.update();
				commitSentence(s);
			} else {
				s.update();
			}
		}
		storage.save(element);
	}
	
	/**
	 * Commits the sentence. This means that it is added to the reasoner.
	 * 
	 * @param sentence The sentence to be commited.
	 * @throws InconsistencyException if the sentence is inconsistent with the existing sentences.
	 */
	protected synchronized void commitSentence(Sentence sentence) throws InconsistencyException {
		if (sentence == null || sentence.isIntegrated()) return;
		
		if (!sentence.isReasonable()) {
			sentence.setIntegrated(true);
			return;
		}
		
		log("commit sentence");
		
		try {
			getReasoner().loadSentence(sentence);
		} catch (InconsistencyException ex) {
			log("not consistent!");
			getReasoner().unloadSentence(sentence);
			throw ex;
		} catch (Throwable t) {
			log("error encountered!");
			t.printStackTrace();
			System.gc();
			getReasoner().unloadSentence(sentence);
			return;
		}
		
		if (!getReasoner().isConsistent()) {
			log("not consistent!");
			getReasoner().unloadSentence(sentence);
			throw new InconsistencyException();
		}
		
		log("consistent!");
		sentence.setIntegrated(true);
		stateID++;
	}
	
	/**
	 * This method tries to reassert a sentence that is not yet integrated.
	 * 
	 * @param sentence The sentence to be reasserted.
	 * @throws InconsistencyException if the sentence is inconsistent with the existing sentences.
	 */
	public synchronized void reassert(Sentence sentence) throws InconsistencyException {
		commitSentence(sentence);
		getStorage().save(sentence.getArticle().getOntologyElement());
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
			!sentence.isReasonable()
		) return;
		
		log("retract sentence");
		stateID++;
		getReasoner().unloadSentence(sentence);
		sentence.setIntegrated(false);
	}
	
	/**
	 * This method retracts an integrated sentence so that it is still part of the wiki
	 * article but does not participate in reasoning anymore.
	 * 
	 * @param sentence The sentence to be retracted.
	 */
	public synchronized void retract(Sentence sentence) {
		retractSentence(sentence);
		getStorage().save(sentence.getArticle().getOntologyElement());
	}
	
	/**
	 * Writes a log entry.
	 * 
	 * @param text Log text.
	 */
	public void log(String text) {
		logger.log("onto", text);
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
	public long getStateID() {
		return stateID;
	}
	
	/**
	 * Returns a parameter value.
	 * 
	 * @param name The parameter name.
	 * @return The parameter value.
	 */
	public String getParameter(String name) {
		return parameters.get(name);
	}
	
	/**
	 * Returns true if the string represents a valid word form.
	 * 
	 * @param s The string.
	 * @return true if the string represents a valid word form.
	 */
	public static boolean isValidWordOrEmpty(String s) {
		return s.matches("([a-zA-Z][a-zA-Z0-9_-]*)?");
	}
	
	/**
	 * Normalizes the string. White space characters are replaced by underscores.
	 * 
	 * @param s The input string.
	 * @return The normalized string.
	 */
	public static String normalize(String s) {
		return s.replaceAll("(\\s|_)+", "_").replaceAll("(^_|_$)", "");
	}
	
}
