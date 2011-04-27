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

package ch.uzh.ifi.attempto.acewiki.aceowl;

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
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.InconsistencyException;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

public class AceWikiOWLReasoner implements AceWikiReasoner {
	
	private static OWLDataFactory dataFactory = new OWLDataFactoryImpl();
	private static OWLlinkHTTPXMLReasonerFactory owllinkReasonerFactory;
	
	private static Object owllinkReasonerSyncToken = new Object();
	
	private Ontology ontology;
	
	private OWLOntologyManager manager;
	private OWLOntology owlOntology;
	private Map<OWLAxiom, Integer> axiomsMap = new HashMap<OWLAxiom, Integer>();
	private OWLReasoner owlReasoner;
	private String reasonerType = "none";
	private Object reasonerSyncToken = new Object();
	private OWLDifferentIndividualsAxiom diffIndsAxiom;
	private boolean diffIndsAxiomOutdated = true;
	private OWLProfile owlProfile;
	private String globalRestrPolicy;
	
	public AceWikiOWLReasoner() {
		manager = OWLManager.createOWLOntologyManager();
		try {
			owlOntology = manager.createOntology();
		} catch (OWLOntologyCreationException ex) {
			ex.printStackTrace();
		}
	}
	
	public void init(Ontology ontology) {
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
		
		String grp = (getParameter("global_restrictions_policy") + "").toLowerCase();
		if (grp.equals("unchecked")) {
			globalRestrPolicy = "unchecked";
		} else {
			globalRestrPolicy = "no_chains";
		}
	}
	
	public Ontology getOntology() {
		return ontology;
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
			OWLDeclarationAxiom owlDecl = null;
			if (el instanceof ACEOWLOntoElement) {
				owlDecl = ((ACEOWLOntoElement) el).getOWLDeclaration();
			}
			if (owlDecl != null) {
				axioms.add(owlDecl);
			}
			for (Sentence s : el.getArticle().getSentences()) {
				ACESentence as = (ACESentence) s;
				if (as instanceof Question || !as.isOWL()) continue;
				if (consistent && (!as.isReasonerParticipant() || !as.isIntegrated())) continue;
				axioms.addAll(as.getOWLAxioms());
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
			if (oe instanceof ProperNameIndividual) {
				inds.add(((ProperNameIndividual) oe).getOWLRepresentation());
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
		OWLDeclarationAxiom owlDecl = null;
		if (element instanceof ACEOWLOntoElement) {
			owlDecl = ((ACEOWLOntoElement) element).getOWLDeclaration();
		}
		if (owlDecl != null) {
			manager.addAxiom(owlOntology, owlDecl);
			flushReasoner();
		}
		if (element instanceof ACEOWLIndividual) {
			diffIndsAxiomOutdated = true;
		}
	}
	
	public void unloadElement(OntologyElement element) {
		OWLDeclarationAxiom owlDecl = null;
		if (element instanceof ACEOWLOntoElement) {
			owlDecl = ((ACEOWLOntoElement) element).getOWLDeclaration();
		}
		if (owlDecl != null) {
			manager.removeAxiom(owlOntology, owlDecl);
			flushReasoner();
		}
		if (element instanceof ACEOWLIndividual) {
			diffIndsAxiomOutdated = true;
		}
	}
	
	public synchronized List<Concept> getConcepts(Individual ind) {
		List<Concept> concepts = new ArrayList<Concept>();
		ProperNameIndividual pnInd = (ProperNameIndividual) ind;
		for (OWLClass oc : getConcepts(pnInd.getOWLRepresentation())) {
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
		ACEOWLConcept ac = (ACEOWLConcept) concept;
		List<Individual> inds = new ArrayList<Individual>();
		for (OWLNamedIndividual oi : getIndividuals(ac.getOWLRepresentation())) {
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
		ACEOWLConcept ac = (ACEOWLConcept) concept;
		List<Concept> concepts = new ArrayList<Concept>();
		for (OWLClass oc : getSuperConcepts(ac.getOWLRepresentation())) {
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
		ACEOWLConcept ac = (ACEOWLConcept) concept;
		List<Concept> concepts = new ArrayList<Concept>();
		for (OWLClass oc : getSubConcepts(ac.getOWLRepresentation())) {
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
	
	public synchronized List<TextContainer> getAnswer(Question q) {
		if (owlReasoner == null) return null;
		
		ACEQuestion question = (ACEQuestion) q;
		
		List<TextContainer> answer = new ArrayList<TextContainer>();
		
		OWLNamedIndividual quInd = question.getQuestionOWLIndividual();
		OWLClassExpression quClass = question.getQuestionOWLClass();
		List<OntologyElement> list = new ArrayList<OntologyElement>();
		
		if (quInd != null) {
			for (OWLClass oc : getConcepts(quInd)) {
				OntologyElement oe = get(oc);
				if (oe instanceof Concept) {
					list.add(oe);
				}
			}
			Collections.sort(list);
			for (OntologyElement oe : list) {
				boolean an = ACEUtils.useIndefiniteArticleAn(oe.getWord());
				answer.add(new TextContainer(
						new TextElement(an ? "an" : "a"),
						new OntologyTextElement(oe, 0)
					));
			}
		} else if (quClass != null) {
			Set<OWLNamedIndividual> owlInds = getIndividuals(quClass);
			for (OWLNamedIndividual oi : owlInds) {
				OntologyElement oe = get(oi);
				if (oe instanceof Individual) {
					list.add(oe);
				}
			}
			Collections.sort(list);
			for (OntologyElement oe : list) {
				answer.add(new TextContainer(new OntologyTextElement(oe, 1)));
			}
		}
		
		return new ArrayList<TextContainer>(answer);
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
		if (!(concept instanceof ACEOWLConcept)) return false;
		if (owlOntology.containsClassInSignature(((ACEOWLConcept) concept).getIRI())) {
			synchronized (reasonerSyncToken) {
				ACEOWLConcept ac = (ACEOWLConcept) concept;
				return owlReasoner.isSatisfiable(ac.getOWLRepresentation());
			}
		} else {
			return true;
		}
	}
	
	public void loadSentence(Sentence s) {
		ACESentence sentence = (ACESentence) s;
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
	
	public void unloadSentence(Sentence s) {
		ACESentence sentence = (ACESentence) s;
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
