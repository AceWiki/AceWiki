// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLogicalEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory;
import org.semanticweb.owlapi.owllink.builtin.response.OWLlinkErrorResponseException;
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.Version;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLObjectComplementOfImpl;
import ch.uzh.ifi.attempto.echocomp.Logger;

/**
 * This class represents an AceWiki ontology which consists of ontology element definitions and
 * of ontological statements. Each ontology element has its own article that consists of
 * ontological statements.
 * 
 * @author Tobias Kuhn
 */
public class Ontology {
	
	private static final HashMap<String, Ontology> ontologies = new HashMap<String, Ontology>();
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	
	private List<OntologyElement> elements = new ArrayList<OntologyElement>();
	private Map<String, OntologyElement> wordIndex = new Hashtable<String, OntologyElement>();
	private Map<Long, OntologyElement> idIndex = new Hashtable<Long, OntologyElement>();
	
	private final String name;
	private final String baseURI;
	private final String globalRestrPolicy;
	private long idCount = 0;
	private long stateID = 0;
	
	private OWLOntologyManager manager;
	private OWLOntology owlOntology;
	private HashMap<OWLAxiom, Integer> axiomsMap = new HashMap<OWLAxiom, Integer>();
	private OWLReasoner reasoner;
	private String reasonerType = "none";
	private OWLDifferentIndividualsAxiom differentIndividualsAxiom;
	
	/**
	 * Creates a new empty ontology with the given name and base URI.
	 * 
	 * @param name The name of the ontology.
	 * @param baseURI The base URI that is used to identify the ontology elements.
	 * @param grp The global restrictions policy.
	 */
	private Ontology(String name, String baseURI, String grp) {
		this.name = name.toString();  // null value throws an exception
		this.baseURI = baseURI;
		if (baseURI == null) {
			baseURI = "";
		}
		ontologies.put(name, this);
		
		manager = OWLManager.createOWLOntologyManager();
		try {
			owlOntology = manager.createOntology();
		} catch (OWLOntologyCreationException ex) {
			ex.printStackTrace();
		}
		
		if (grp == null || grp.length() == 0) {
			globalRestrPolicy = "noChains";
		} else if (grp.toLowerCase().equals("unchecked")) {
			globalRestrPolicy = "unchecked";
		} else {
			globalRestrPolicy = "noChains";
		}
	}
	
	/**
	 * Loads an ontology (or creates an empty ontology if the ontology cannot be found). The
	 * complete URI of the ontology is baseURI + name.
	 * 
	 * @param name The name of the ontology.
	 * @param baseURI The base URI that is used to identify the ontology elements.
	 * @param globalRestrPolicy A string representing the policy how to enforce the global
	 *     restrictions on axioms in OWL 2.
	 * @return The loaded ontology.
	 */
	public synchronized static Ontology loadOntology(String name, String baseURI,
			String globalRestrPolicy) {
		
		if (ontologies.get(name) != null) {
			return ontologies.get(name);
		}
		Ontology ontology = new Ontology(name, baseURI, globalRestrPolicy);
		ontology.log("loading ontology");
		System.err.println("Loading '" + name + "'");
		File dataDir = new File("data/" + name);
		if (dataDir.exists()) {
			System.err.print("Entities:   ");
			ConsoleProgressBar pb1 = new ConsoleProgressBar(dataDir.listFiles().length);
			for (File file : dataDir.listFiles()) {
				pb1.addOne();
				try {
					long id = new Long(file.getName());
					ontology.log("reading file: " + file.getName());
					FileInputStream in = new FileInputStream(file);
					byte[] bytes = new byte[in.available()];
					in.read(bytes);
					in.close();
					String s = new String(bytes, "UTF-8");
					OntologyElement.loadOntologyElement(s, id, ontology);
				} catch (NumberFormatException ex) {
					ontology.log("ignoring file: " + file.getName());
				} catch (IOException ex) {
					ontology.log("cannot read file: " + file.getName());
				}
			}
			pb1.complete();
		} else {
			ontology.log("no data found; blank ontology is created");
		}

		ontology.log("loading statements");
		System.err.print("Statements: ");
		ConsoleProgressBar pb2 = new ConsoleProgressBar(ontology.elements.size());
		for (OntologyElement oe : ontology.elements) {
			pb2.addOne();
			ontology.loadElement(oe);
			for (Sentence s : oe.getSentences()) {
				if (s.isReasonerParticipant() && s.isIntegrated()) {
					ontology.loadSentence(s);
				}
			}
		}
		pb2.complete();
		
		return ontology;
	}
	
	synchronized void save(OntologyElement oe) {
		if (!(new File("data")).exists()) (new File("data")).mkdir();
		if (!(new File("data/" + name)).exists()) (new File("data/" + name)).mkdir();
		
		if (!elements.contains(oe)) {
			(new File("data/" + name + "/" + oe.getId())).delete();
			return;
		}
		
		try {
			FileOutputStream out = new FileOutputStream("data/" + name + "/" + oe.getId());
			out.write(oe.serialize().getBytes("UTF-8"));
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	synchronized void register(OntologyElement element) {
		if (elements.contains(element)) {
			log("error: element already registered");
			throw new RuntimeException("Registration failed: Element is already registered.");
		}
		
		log("register: " + element);
		stateID++;
		
		if (element.getId() == -1) {
			element.setId(nextId());
		}
		elements.add(element);
		idIndex.put(element.getId(), element);
		if (element.getId() > idCount) idCount = element.getId();
		
		for (String word : element.getWords()) {
			if (word == null) continue;
			
			if (wordIndex.get(word) == null) {
				wordIndex.put(word, element);
			} else if (wordIndex.get(word) != element) {
				log("error: word already used");
				throw new RuntimeException(
						"Registration failed: The word '" + word + "' is already used."
					);
			}
		}
		
		loadElement(element);
		if (element instanceof Individual) {
			updateDifferentIndividualsAxiom();
		}
		flushReasoner();
	}
	
	/**
	 * Removes the given ontology element from the ontology.
	 * 
	 * @param element The ontology element to be removed.
	 */
	public synchronized void remove(OntologyElement element) {
		if (!elements.contains(element)) {
			log("error: unknown element");
			return;
		}
		
		log("remove: " + element.getWord());
		stateID++;
		
		for (String word : element.getWords()) {
			if (word == null) continue;
			wordIndex.remove(word);
		}
		elements.remove(element);
		idIndex.remove(element.getId());
		for (Sentence s : element.getSentences()) {
			retractSentence(s);
		}
		save(element);
		
		unloadElement(element);
		if (element instanceof Individual) {
			updateDifferentIndividualsAxiom();
		}
		flushReasoner();
	}
	
	synchronized void removeFromWordIndex(OntologyElement oe) {
		for (String word : oe.getWords()) {
			if (word != null) {
				wordIndex.remove(word);
			}
		}
		unloadElement(oe);
	}
	
	synchronized void addToWordIndex(OntologyElement oe) {
		for (String word : oe.getWords()) {
			if (word != null) {
				if (wordIndex.get(word) == null) {
					wordIndex.put(word, oe);
				} else if (wordIndex.get(word) != oe) {
					throw new RuntimeException(
							"Word update failed: The word '" + word + "' is already used."
						);
				}
			}
		}
		loadElement(oe);
	}

	/**
	 * Returns an OWL ontology object representing the consistent part of the ontology.
	 * 
	 * @return An OWL ontology object of the consistent ontology.
	 */
	public synchronized OWLOntology getOWLOntology() {
		return owlOntology;
	}

	/**
	 * Returns an OWL ontology object representing the full ontology, including inconsistent
	 * statements.
	 * 
	 * @return An OWL ontology object of the full ontology.
	 */
	public synchronized OWLOntology getFullOWLOntology() {
		Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		for (OntologyElement el : elements) {
			axioms.add(el.getOWLDeclaration());
			for (Sentence s : el.getSentences()) {
				if (s instanceof Question || !s.isOWL()) continue;
				if (!s.isReasonerParticipant() || !s.isIntegrated()) continue;
				axioms.addAll(s.getOWLAxioms());
			}
		}
		axioms.add(differentIndividualsAxiom);

		OWLOntology fullOWLOntology = null;
		try {
			fullOWLOntology = manager.createOntology(axioms);
			manager.removeOntology(fullOWLOntology);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		
		return fullOWLOntology;
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
		for (OntologyElement el : elements) {
			for (Sentence s : el.getSentences()) {
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
	public OntologyElement get(String name) {
		return wordIndex.get(name);
	}
	
	private OntologyElement get(OWLLogicalEntity owlEntity) {
		if (owlEntity == null) return null;
		if (owlEntity.isTopEntity() || owlEntity.isBottomEntity()) return null;
		String iri = owlEntity.getIRI().toString();
		if (!iri.startsWith(getURI())) return null;
		String name = iri.substring(iri.indexOf("#") + 1);
		return get(name);
	}
	
	/**
	 * Returns the ontology element with the given id, or null if there is no such element.
	 * 
	 * @param id The id of the ontology element.
	 * @return The ontology element.
	 */
	OntologyElement get(long id) {
		return idIndex.get(id);
	}
	
	/**
	 * Returns all ontology elements. The list is a copy of the internal list.
	 * 
	 * @return A list of all ontology elements.
	 */
	public List<OntologyElement> getOntologyElements() {
		return new ArrayList<OntologyElement>(elements);
	}
	
	/**
	 * Returns true if the given ontology element is contained by the ontology (identity check).
	 * 
	 * @param ontologyElement The ontology element.
	 * @return true if the ontology element is contained by the ontology.
	 */
	public boolean contains(OntologyElement ontologyElement) {
		return elements.contains(ontologyElement);
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
	 * Returns the OWL ontology manager.
	 * 
	 * @return The OWL ontology manager.
	 */
	public OWLOntologyManager getOWLOntologyManager() {
		return manager;
	}
	
	/**
	 * Returns a string representing the policy how to enforce the global restrictions on the
	 * axioms in OWL 2.
	 * 
	 * @return The global restrictions policy.
	 */
	public String getGlobalRestrictionsPolicy() {
		return globalRestrPolicy;
	}
	
	/**
	 * Returns the name of the reasoner interface or integrated reasoner currently used.
	 * 
	 * @return The reasoner type.
	 */
	public String getReasonerType() {
		return reasonerType;
	}
	
	/**
	 * Returns the name of the currently used reasoner.
	 * 
	 * @return The name of the reasoner.
	 */
	public String getReasonerName() {
		if (reasoner == null) return null;
		return reasoner.getReasonerName();
	}
	
	/**
	 * Returns the version of the currently used reasoner.
	 * 
	 * @return The version of the reasoner.
	 */
	public String getReasonerVersion() {
		if (reasoner == null) return null;
		Version v = reasoner.getReasonerVersion();
		if (v == null) return null;
		return v.getMajor() + "." + v.getMinor() + "." + v.getPatch() + "." + v.getBuild();
	}
	
	public boolean isReasonerLoaded() {
		return (reasoner != null);
	}
	
	/**
	 * Loads a reasoner or reasoner interface. Currently supported are the HermiT reasoner
	 * ("HermiT"), the Pellet reasoner ("Pellet"), the OWLlink interface ("OWLlink"), or none
	 * ("none").
	 * 
	 * @param type The reasoner type as shown above.
	 */
	public void loadReasoner(String type) {
		// TODO extract this code into a new class
		
		log("loading reasoner");
		if (type == null) type = "";
		type = type.toLowerCase();
		reasonerType = type;
		
		if (reasoner != null) reasoner.dispose();
		
		if (type.equals("none")) {
			log("no reasoner");
			reasonerType = "none";
			reasoner = null;
		} else if (type.equals("hermit")) {
			log("loading HermiT");
			reasonerType = "HermiT";
			reasoner = new Reasoner(owlOntology);
		} else if (type.equals("pellet")) {
			log("loading Pellet");
			reasonerType = "Pellet";
			// The Pellet libraries are not part of the AceWiki package (because of license
			// reasons). For that reason, the pellet reasoner has to be loaded dynamically.
			OWLReasonerFactory reasonerFactory = null;
			try {
				ClassLoader classLoader = Ontology.class.getClassLoader();
				String className = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";
				reasonerFactory = (OWLReasonerFactory) classLoader.loadClass(className).newInstance();
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			reasoner = reasonerFactory.createNonBufferingReasoner(owlOntology);
		} else if (type.equals("owllink")) {
			log("loading OWLlink");
			reasonerType = "OWLlink";
			reasoner = (new OWLlinkHTTPXMLReasonerFactory()).createReasoner(owlOntology);
		//} else if (type.equals("dig")) {
            //try {
			//	reasoner = new DIGReasoner(OWLManager.createOWLOntologyManager());
	        //	((DIGReasoner) reasoner).getReasoner().setReasonerURL(new URL("http://localhost:8081"));
			//} catch (Exception e) { e.printStackTrace(); }
		} else if (type.equals("")) {
			log("no reasoner type specified: loading HermiT as default");
			reasonerType = "HermiT";
			reasoner = new Reasoner(owlOntology);
		} else {
			log("ERROR: Unknown reasoner type: " + type);
			reasonerType = "none";
			reasoner = null;
		}
		updateDifferentIndividualsAxiom();
		flushReasoner();
		
		log("reasoner loaded");
	}
	
	private synchronized void refreshReasoner() {
		loadReasoner(reasonerType);
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
		save(element);
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
			loadSentence(sentence);
		} catch (OutOfMemoryError err) {
			log("error: out of memory");
			System.gc();
			refreshReasoner();
			return 2;
		} catch (OWLlinkErrorResponseException ex) {
			// FaCT++ throws an exception here when inconsistency is encountered
			// TODO Is this always the case?
			if ("FaCT++.Kernel: inconsistent ontology".equals(ex.getMessage())) {
				inconsistencyEncountered = true;
			} else {
				// We get here when the global restrictions are violated with FaCT++ and OWLlink
				errorEncountered = true;
			}
		} catch (IllegalArgumentException ex) {
			// We get here when the global restrictions are violated with HermiT
			errorEncountered = true;
		} catch (Exception ex) {
			errorEncountered = true;
			ex.printStackTrace();
		}

		log("check for consistency");
		if (errorEncountered) {
			log("error encountered!");
			unloadSentence(sentence);
			// TODO return a different value here:
			return 1;
		} else if (inconsistencyEncountered || !isConsistent()) {
			log("not consistent!");
			unloadSentence(sentence);
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
		unloadSentence(sentence);
		sentence.setIntegrated(false);
	}
	
	void log(String text) {
		Logger.log(name, "onto", 0, "onto", text);
	}
	
	/**
	 * Updates the axiom that states that all named individuals are different. Thus, unique
	 * name assumption is applied.
	 */
	private synchronized void updateDifferentIndividualsAxiom() {
		if (differentIndividualsAxiom != null) {
			unloadAxiom(differentIndividualsAxiom);
		}
		
		Set<OWLIndividual> inds = new HashSet<OWLIndividual>();
		for (OntologyElement oe : getOntologyElements()) {
			if (oe instanceof Individual) {
				inds.add(((Individual) oe).getOWLRepresentation());
			}
		}
		differentIndividualsAxiom = dataFactory.getOWLDifferentIndividualsAxiom(inds);
		
		loadAxiom(differentIndividualsAxiom);
	}
	
	/**
	 * Returns all concepts the given individual belongs to. The reasoner is used for this.
	 * 
	 * @param ind The individual.
	 * @return A list of all concepts of the individual.
	 * @see Individual#getConcepts()
	 */
	public synchronized List<Concept> getConcepts(Individual ind) {
		List<Concept> concepts = new ArrayList<Concept>();
		if (reasoner == null) return concepts;
		Set<OWLClass> owlClasses = reasoner.getTypes(ind.getOWLRepresentation(), false).getFlattened();
		for (OWLClass oc : owlClasses) {
			if (oc.isOWLThing() || oc.isOWLNothing()) continue;
			String conceptURI = oc.getIRI().toString();
			if (conceptURI.startsWith("http://attempto.ifi.uzh.ch/ace#")) continue;
			String conceptName = conceptURI.substring(conceptURI.indexOf("#") + 1);
			concepts.add((Concept) get(conceptName));
		}
		return concepts;
	}
	
	/**
	 * Returns all individuals that belong to the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept.
	 * @return A list of all individuals of the concept.
	 * @see Concept#getIndividuals()
	 */
	public synchronized List<Individual> getIndividuals(Concept concept) {
		List<Individual> inds = new ArrayList<Individual>();
		if (reasoner == null) return inds;
		Set<OWLNamedIndividual> owlInds = reasoner.getInstances(concept.getOWLRepresentation(), false).getFlattened();
		for (OWLNamedIndividual oi : owlInds) {
			OntologyElement oe = get(oi);
			if (oe instanceof Individual) {
				inds.add((Individual) oe);
			}
		}
		return inds;
	}
	
	/**
	 * Returns all super-concepts of the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept for which all super-concepts should be returned.
	 * @return A list of all super-concepts.
	 * @see Concept#getSuperConcepts()
	 */
	public synchronized List<Concept> getSuperConcepts(Concept concept) {
		List<Concept> concepts = new ArrayList<Concept>();
		if (reasoner == null) return concepts;
		Set<OWLClass> owlClasses = reasoner.getSuperClasses(concept.getOWLRepresentation(), false).getFlattened();
		for (OWLClass oc : owlClasses) {
			OntologyElement oe = get(oc);
			if (oe instanceof Concept) {
				concepts.add((Concept) oe);
			}
		}
		return concepts;
	}
	
	/**
	 * Returns all the sub-concepts of the given concept. The reasoner is used for this.
	 * 
	 * @param concept The concept for which all sub-concepts should be returned.
	 * @return A list of all sub-concepts.
	 * @see Concept#getSubConcepts()
	 */
	public synchronized List<Concept> getSubConcepts(Concept concept) {
		List<Concept> concepts = new ArrayList<Concept>();
		if (reasoner == null) return concepts;
		Set<OWLClass> owlClasses = reasoner.getSubClasses(concept.getOWLRepresentation(), false).getFlattened();
		for (OWLClass oc : owlClasses) {
			OntologyElement oe = get(oc);
			if (oe instanceof Concept) {
				concepts.add((Concept) oe);
			}
		}
		return concepts;
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
	public synchronized List<OntologyElement> getAnswer(Question question) {
		if (reasoner == null) return null;
		
		List<OntologyElement> answer = new ArrayList<OntologyElement>();
		
		OWLNamedIndividual quInd = question.getQuestionOWLIndividual();
		OWLClassExpression quClass = question.getQuestionOWLClass();
		
		if (question.isShowPossibleAnswersEnabled() && quClass != null) {
			quClass = new OWLObjectComplementOfImpl(manager.getOWLDataFactory(), quClass);
		}
		
		if (quInd != null) {
			Set<OWLClass> owlClasses = reasoner.getTypes(quInd, false).getFlattened();
			for (OWLClass oc : owlClasses) {
				OntologyElement oe = get(oc);
				if (oe instanceof Concept) {
					answer.add(oe);
				}
			}
		} else if (quClass != null) {
			Set<OWLNamedIndividual> owlInds = reasoner.getInstances(quClass, false).getFlattened();
			for (OWLNamedIndividual oi : owlInds) {
				OntologyElement oe = get(oi);
				if (oe instanceof Individual) {
					answer.add(oe);
				}
			}
		}
		
		if (question.isShowPossibleAnswersEnabled()) {
			List<OntologyElement> realAnswer = new ArrayList<OntologyElement>();
			for (OntologyElement oe : getOntologyElements()) {
				if (oe instanceof Individual && !answer.contains(oe)) {
					realAnswer.add(oe);
				}
			}
			return realAnswer;
		}
		
		return answer;
	}
	
	/**
	 * Returns true if the ontology is consistent. If nothing goes wrong, this should always return
	 * true. The reasoner is used for this.
	 * 
	 * @return true if the ontology is consistent.
	 */
	public synchronized boolean isConsistent() {
		if (reasoner == null) return true;
		boolean c = true;
		try {
			// The method isConsistent is poorly supported by the implementations.
			//c = reasoner.isConsistent();
			c = reasoner.isSatisfiable(dataFactory.getOWLThing());
		} catch (InconsistentOntologyException ex) {
			c = false;
		}
		return c;
	}
	
	/**
	 * Checks if the given concept is satisfiable. The reasoner is used for this.
	 * 
	 * @param concept The concept.
	 * @return true if the concept is satisfiable.
	 */
	public synchronized boolean isSatisfiable(Concept concept) {
		if (reasoner == null) return true;
		if (owlOntology.containsClassInSignature(concept.getIRI())) {
			return reasoner.isSatisfiable(concept.getOWLRepresentation());
		} else {
			return true;
		}
	}
	
	private void flushReasoner() {
		if (reasoner != null) reasoner.flush();
	}
	
	private void loadElement(OntologyElement element) {
		manager.addAxiom(owlOntology, element.getOWLDeclaration());
		flushReasoner();
	}
	
	private void unloadElement(OntologyElement element) {
		manager.removeAxiom(owlOntology, element.getOWLDeclaration());
		flushReasoner();
	}
	
	private void loadSentence(Sentence sentence) {
		for (OWLAxiom ax : sentence.getOWLAxioms()) {
			loadAxiom(ax);
		}
		flushReasoner();
	}
	
	private void unloadSentence(Sentence sentence) {
		for (OWLAxiom ax : sentence.getOWLAxioms()) {
			unloadAxiom(ax);
		}
		flushReasoner();
	}
	
	private void loadAxiom(OWLAxiom ax) {
		Integer count = axiomsMap.get(ax);
		if (count == null) count = 0;
		if (count == 0) {
			manager.addAxiom(owlOntology, ax);
		}
		axiomsMap.put(ax, count+1);
	}
	
	private void unloadAxiom(OWLAxiom ax) {
		Integer count = axiomsMap.get(ax);
		if (count == 1) {
			manager.removeAxiom(owlOntology, ax);
		}
		axiomsMap.put(ax, count-1);
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
	
}
