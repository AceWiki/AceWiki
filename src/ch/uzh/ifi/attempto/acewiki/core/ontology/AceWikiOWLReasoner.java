package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLogicalEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory;
import org.semanticweb.owlapi.owllink.builtin.response.OWLlinkErrorResponseException;
import org.semanticweb.owlapi.profiles.OWL2ELProfile;
import org.semanticweb.owlapi.profiles.OWL2QLProfile;
import org.semanticweb.owlapi.profiles.OWL2RLProfile;
import org.semanticweb.owlapi.profiles.OWLProfile;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.Version;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLObjectComplementOfImpl;


public class AceWikiOWLReasoner implements AceWikiReasoner {
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	private static OWLlinkHTTPXMLReasonerFactory owllinkReasonerFactory;
	
	private static Object owllinkReasonerSyncToken = new Object();
	
	private Ontology ontology;
	
	private OWLOntologyManager manager;
	private OWLOntology owlOntology;
	private Map<String, List<OntologyElement>> answerCache = new HashMap<String, List<OntologyElement>>();
	private long answerCacheState = -1;
	private Map<OWLAxiom, Integer> axiomsMap = new HashMap<OWLAxiom, Integer>();
	private OWLReasoner owlReasoner;
	private String reasonerType = "none";
	private Object reasonerSyncToken = new Object();
	private OWLDifferentIndividualsAxiom diffIndsAxiom;
	private boolean diffIndsAxiomOutdated = true;
	private final OWLProfile owlProfile;
	private final String globalRestrPolicy;
	
	public AceWikiOWLReasoner(Ontology ontology) {
		this.ontology = ontology;
		String p = (getParameter("owl_profile") + "").toLowerCase();
		if (p.equals("owl2el")) {
			owlProfile = new OWL2ELProfile();
		} else if (p.equals("owl2ql")) {
			owlProfile = new OWL2QLProfile();
		} else if (p.equals("owl2rl")) {
			owlProfile = new OWL2RLProfile();
		} else {
			owlProfile = null;
		}
		
		manager = OWLManager.createOWLOntologyManager();
		try {
			owlOntology = manager.createOntology();
		} catch (OWLOntologyCreationException ex) {
			ex.printStackTrace();
		}
		
		String grp = (getParameter("global_restrictions_policy") + "").toLowerCase();
		if (grp.equals("unchecked")) {
			globalRestrPolicy = "unchecked";
		} else {
			globalRestrPolicy = "no_chains";
		}
	}
	
	private List<OntologyElement> getOntologyElements() {
		return ontology.getOntologyElements();
	}
	
	private OntologyElement getOntologyElement(String name) {
		return ontology.getElement(name);
	}
	
	public String getParameter(String name) {
		return ontology.getParameter(name);
	}
	
	public String[] getInfo() {
		return new String[] {
			"global restrictions policy: " + getGlobalRestrictionsPolicy(),
			"OWL profile: " + getOWLProfileName()
		};
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
	
	private IRI getIRI() {
		return IRI.create(ontology.getURI());
	}
	
	/**
	 * Returns the OWL profile that defines which statements are used for reasoning, or null if the
	 * full language of OWL is used.
	 * 
	 * @return The used OWL profile.
	 */
	public OWLProfile getOWLProfile() {
		return owlProfile;
	}
	
	/**
	 * Returns the name of the current OWL profile.
	 * 
	 * @return The OWL profile name.
	 */
	public String getOWLProfileName() {
		if (owlProfile == null) return "OWL 2 Full";
		return owlProfile.getName();
	}

	/**
	 * Returns a new OWL ontology object representing the full ontology or the consistent part of
	 * it.
	 * 
	 * @param consistent true if only the consistent part should be exported.
	 * @return An OWL ontology object of the full ontology.
	 */
	public OWLOntology exportOWLOntology(boolean consistent) {
		Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		for (OntologyElement el : getOntologyElements()) {
			OWLDeclarationAxiom owlDecl = el.getOWLDeclaration();
			if (owlDecl != null) {
				axioms.add(owlDecl);
			}
			for (Sentence s : el.getSentences()) {
				if (s instanceof Question || !s.isOWL()) continue;
				if (consistent && (!s.isReasonerParticipant() || !s.isIntegrated())) continue;
				axioms.addAll(s.getOWLAxioms());
			}
		}
		axioms.add(diffIndsAxiom);
		
		OWLOntology o = null;
		try {
			o = manager.createOntology(axioms, getIRI());
			manager.removeOntology(o);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		
		return o;
	}
	
	private OntologyElement get(OWLLogicalEntity owlEntity) {
		if (owlEntity == null) return null;
		if (owlEntity.isTopEntity() || owlEntity.isBottomEntity()) return null;
		String iri = owlEntity.getIRI().toString();
		if (!iri.startsWith(ontology.getURI())) return null;
		String name = iri.substring(iri.indexOf("#") + 1);
		return getOntologyElement(name);
	}
	
	/**
	 * Returns the OWL ontology manager.
	 * 
	 * @return The OWL ontology manager.
	 */
	public OWLOntologyManager getOWLOntologyManager() {
		return manager;
	}
	
	public String getReasonerName() {
		if (owlReasoner == null) return null;
		return owlReasoner.getReasonerName();
	}
	
	public String getReasonerVersion() {
		if (owlReasoner == null) return null;
		Version v = owlReasoner.getReasonerVersion();
		if (v == null) return null;
		return v.getMajor() + "." + v.getMinor() + "." + v.getPatch() + "." + v.getBuild();
	}
	
	public String getReasonerType() {
		return reasonerType;
	}
	
	public void load() {
		log("loading reasoner");
		String type = getParameter("reasoner");
		if (type == null) type = "";
		type = type.toLowerCase();
		reasonerType = type;
		
		if (owlReasoner != null) owlReasoner.dispose();
		
		if (type.equals("none")) {
			log("no reasoner");
			reasonerType = "none";
			owlReasoner = null;
		} else if (type.equals("hermit")) {
			log("loading HermiT");
			reasonerType = "HermiT";
			owlReasoner = new Reasoner(owlOntology);
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
			owlReasoner = reasonerFactory.createNonBufferingReasoner(owlOntology);
		} else if (type.equals("owllink")) {
			log("loading OWLlink");
			reasonerType = "OWLlink";
			if (owllinkReasonerFactory == null) {
				owllinkReasonerFactory = new OWLlinkHTTPXMLReasonerFactory();
			}
			owlReasoner = owllinkReasonerFactory.createReasoner(owlOntology);
			// reasoner calls over OWLlink have to be synchronized:
			reasonerSyncToken = owllinkReasonerSyncToken;
		//} else if (type.equals("dig")) {
            //try {
			//	reasoner = new DIGReasoner(OWLManager.createOWLOntologyManager());
	        //	((DIGReasoner) reasoner).getReasoner().setReasonerURL(new URL("http://localhost:8081"));
			//} catch (Exception e) { e.printStackTrace(); }
		} else if (type.equals("")) {
			log("no reasoner type specified: loading HermiT as default");
			reasonerType = "HermiT";
			owlReasoner = new Reasoner(owlOntology);
		} else {
			log("ERROR: Unknown reasoner type: " + type);
			reasonerType = "none";
			owlReasoner = null;
		}
		updateDifferentIndividualsAxiom();
		flushReasoner();
		
		log("reasoner loaded");
	}
	
	/**
	 * Updates the axiom that states that all named individuals are different. Thus, unique
	 * name assumption is applied.
	 */
	private synchronized void updateDifferentIndividualsAxiom() {
		if (!diffIndsAxiomOutdated) return;
		
		if (diffIndsAxiom != null) {
			unloadAxiom(diffIndsAxiom);
		}
		
		Set<OWLIndividual> inds = new HashSet<OWLIndividual>();
		for (OntologyElement oe : getOntologyElements()) {
			if (oe instanceof Individual) {
				inds.add(((Individual) oe).getOWLRepresentation());
			}
		}
		diffIndsAxiom = dataFactory.getOWLDifferentIndividualsAxiom(inds);
		
		loadAxiom(diffIndsAxiom);
		
		diffIndsAxiomOutdated = false;
	}
	
	public void flushReasoner() {
		if (owlReasoner != null) {
			synchronized (reasonerSyncToken) {
				owlReasoner.flush();
			}
		}
	}
	
	public void loadElement(OntologyElement element) {
		OWLDeclarationAxiom owlDecl = element.getOWLDeclaration();
		if (owlDecl != null) {
			manager.addAxiom(owlOntology, owlDecl);
			flushReasoner();
		}
		if (element instanceof Individual) {
			diffIndsAxiomOutdated = true;
		}
	}
	
	public void unloadElement(OntologyElement element) {
		OWLDeclarationAxiom owlDecl = element.getOWLDeclaration();
		if (owlDecl != null) {
			manager.removeAxiom(owlOntology, owlDecl);
			flushReasoner();
		}
		if (element instanceof Individual) {
			diffIndsAxiomOutdated = true;
		}
	}
	
	public synchronized List<Concept> getConcepts(Individual ind) {
		List<Concept> concepts = new ArrayList<Concept>();
		for (OWLClass oc : getConcepts(ind.getOWLRepresentation())) {
			if (oc.isOWLThing() || oc.isOWLNothing()) continue;
			String conceptURI = oc.getIRI().toString();
			if (conceptURI.startsWith("http://attempto.ifi.uzh.ch/ace#")) continue;
			String conceptName = conceptURI.substring(conceptURI.indexOf("#") + 1);
			concepts.add((Concept) getOntologyElement(conceptName));
		}
		return concepts;
	}
	
	private synchronized Set<OWLClass> getConcepts(OWLNamedIndividual owlInd) {
		if (owlReasoner == null) {
			return Collections.emptySet();
		} else {
			synchronized (reasonerSyncToken) {
				return owlReasoner.getTypes(owlInd, false).getFlattened();
			}
		}
	}
	
	public synchronized List<Individual> getIndividuals(Concept concept) {
		List<Individual> inds = new ArrayList<Individual>();
		for (OWLNamedIndividual oi : getIndividuals(concept.getOWLRepresentation())) {
			OntologyElement oe = get(oi);
			if (oe instanceof Individual) {
				inds.add((Individual) oe);
			}
		}
		return inds;
	}
	
	private synchronized Set<OWLNamedIndividual> getIndividuals(OWLClassExpression owlClass) {
		if (owlReasoner == null) {
			return Collections.emptySet();
		} else {
			synchronized (reasonerSyncToken) {
				return owlReasoner.getInstances(owlClass, false).getFlattened();
			}
		}
	}
	
	public synchronized List<Concept> getSuperConcepts(Concept concept) {
		List<Concept> concepts = new ArrayList<Concept>();
		for (OWLClass oc : getSuperConcepts(concept.getOWLRepresentation())) {
			OntologyElement oe = get(oc);
			if (oe instanceof Concept) {
				concepts.add((Concept) oe);
			}
		}
		return concepts;
	}
	
	private synchronized Set<OWLClass> getSuperConcepts(OWLClass owlClass) {
		if (owlReasoner == null) {
			return Collections.emptySet();
		} else {
			synchronized (reasonerSyncToken) {
				return owlReasoner.getSuperClasses(owlClass, false).getFlattened();
			}
		}
	}
	
	public synchronized List<Concept> getSubConcepts(Concept concept) {
		List<Concept> concepts = new ArrayList<Concept>();
		for (OWLClass oc : getSubConcepts(concept.getOWLRepresentation())) {
			OntologyElement oe = get(oc);
			if (oe instanceof Concept) {
				concepts.add((Concept) oe);
			}
		}
		return concepts;
	}
	
	private synchronized Set<OWLClass> getSubConcepts(OWLClass owlClass) {
		if (owlReasoner == null) {
			return Collections.emptySet();
		} else {
			synchronized (reasonerSyncToken) {
				return owlReasoner.getSubClasses(owlClass, false).getFlattened();
			}
		}
	}
	
	public boolean isAnswerCacheUpToDate() {
		return answerCacheState == ontology.getStateID();
	}
	
	private void updateAnswerCache() {
		if (!isAnswerCacheUpToDate()) {
			answerCache = new HashMap<String, List<OntologyElement>>();
			answerCacheState = ontology.getStateID();
		}
	}
	
	public synchronized List<OntologyElement> getCachedAnswer(Question question) {
		List<OntologyElement> a = answerCache.get(question.serialize(true));
		if (a != null) {
			return new ArrayList<OntologyElement>(a);
		} else {
			return null;
		}
	}
	
	public synchronized List<OntologyElement> getAnswer(Question question) {
		if (owlReasoner == null) return null;
		
		updateAnswerCache();
		List<OntologyElement> answer = answerCache.get(question.serialize(true));
		if (answer != null) {
			return new ArrayList<OntologyElement>(answer);
		}
		
		answer = new ArrayList<OntologyElement>();
		
		OWLNamedIndividual quInd = question.getQuestionOWLIndividual();
		OWLClassExpression quClass = question.getQuestionOWLClass();
		
		if (quInd != null) {
			Set<OWLClass> owlClasses = getConcepts(quInd);
			for (OWLClass oc : owlClasses) {
				OntologyElement oe = get(oc);
				if (oe instanceof Concept) {
					answer.add(oe);
				}
			}
		} else if (quClass != null) {
			Set<OWLNamedIndividual> owlInds = getIndividuals(quClass);
			for (OWLNamedIndividual oi : owlInds) {
				OntologyElement oe = get(oi);
				if (oe instanceof Individual) {
					answer.add(oe);
				}
			}
		}
		
		answerCache.put(question.serialize(true), answer);
		return new ArrayList<OntologyElement>(answer);
	}
	
	public synchronized boolean isConsistent() {
		if (owlReasoner == null) return true;
		boolean c = true;
		try {
			synchronized (reasonerSyncToken) {
				// The method isConsistent is poorly supported by the implementations.
				//c = reasoner.isConsistent();
				c = owlReasoner.isSatisfiable(dataFactory.getOWLThing());
			}
		} catch (Exception ex) {
			c = false;
		}
		return c;
	}
	
	public synchronized boolean isSatisfiable(Concept concept) {
		if (owlReasoner == null) return true;
		if (owlOntology.containsClassInSignature(concept.getIRI())) {
			synchronized (reasonerSyncToken) {
				return owlReasoner.isSatisfiable(concept.getOWLRepresentation());
			}
		} else {
			return true;
		}
	}
	
	public void loadSentence(Sentence sentence) {
		try {
			for (OWLAxiom ax : sentence.getOWLAxioms()) {
				loadAxiom(ax);
			}
			flushReasoner();
		} catch (OWLlinkErrorResponseException ex) {
			// FaCT++ throws an exception here when inconsistency is encountered
			// TODO Is this always the case?
			if ("FaCT++.Kernel: inconsistent ontology".equals(ex.getMessage())) {
				throw new InconsistencyException();
			} else {
				// We get here when the global restrictions are violated with FaCT++ and OWLlink
				throw ex;
			}
		} catch (IllegalArgumentException ex) {
			// We get here when the global restrictions are violated with HermiT
			throw ex;
		}
	}
	
	public void unloadSentence(Sentence sentence) {
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
	
	private void log(String text) {
		ontology.log(text);
	}

}
