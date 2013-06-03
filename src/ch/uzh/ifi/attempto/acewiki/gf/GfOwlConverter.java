package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.StringDocumentSource;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.profiles.OWLProfile;
import org.semanticweb.owlapi.profiles.OWLProfileReport;
import org.semanticweb.owlapi.profiles.OWLProfileViolation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.owl.AceWikiOWLReasoner2;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.ape.OutputType;


/**
 * Some static methods for the OWL conversion of GF-based wiki entries.
 *
 * TODO: work in progress
 *
 * @author Kaarel Kaljurand
 */
public class GfOwlConverter {

	private static final OWLOntologyManager OWL_ONTOLOGY_MANAGER = OWLManager.createOWLOntologyManager();
	private static final OutputType OWL_SERIALIZATION_TYPE = OutputType.OWLFSSPP;

	private static final Logger mLogger = LoggerFactory.getLogger(GfOwlConverter.class);


	/**
	 * Converts the given GF wiki entry into OWL.
	 * This succeeds if the entry is compatible with ACE and OWL, i.e.
	 * at least one of the trees can be mapped to OWL.
	 */
	public static Set<Set<OWLAxiom>> convert(GfGrammar gfGrammar, String uri, GfWikiEntry gfWikiEntry) throws OWLOntologyCreationException {
		if (! gfGrammar.isAceCompatible()) {
			// TODO: throw exception instead
			return null;
		}

		// Convert all trees to ACE texts
		Set<ACEText> acetexts = new HashSet<ACEText>();
		for (String tree : gfWikiEntry.getTrees().getTrees()) {
			try {
				ACEText acetext = GfWikiUtils.getACEText(gfGrammar, tree);
				if (acetext != null) {
					acetexts.add(acetext);
				}
			} catch (Exception e) {
				// TODO do not ignore exception
			}
		}

		// Stop if no tree mapped to an ACE text
		if (acetexts.isEmpty()) {
			// TODO: throw exception instead
			return null;
		}

		// Set of axioms from each reading
		Set<Set<OWLAxiom>> setOfSetOfAxiom = Sets.newHashSet();

		for (ACEText acetext : acetexts) {
			ACEParserResult parserResult = GfWikiUtils.parse(acetext, uri, OWL_SERIALIZATION_TYPE);
			Set<OWLAxiom> axioms = getOwlAxiomsFromString(parserResult.get(OWL_SERIALIZATION_TYPE));

			if (axioms.isEmpty()) {
				// One of the readings did not result in any axioms
				// TODO: not sure what to do here, maybe just ignore?
			} else {
				setOfSetOfAxiom.add(axioms);
			}
		}

		return setOfSetOfAxiom;
	}


	/**
	 * TODO: move to its own class
	 *
	 * Given a set of sets of axioms, choose a single set and return it.
	 * 
	 * TODO: there can be many disambiguation techniques and the user might want to
	 * configure which one to use:
	 * 
	 * 1. choose the first reading
	 * 2. combine the readings into a disjunction
	 * 3. prune the readings by checking which are consistent and redundant with respect to the KB, then choose by 1. or 2.
	 * 4. ...
	 */
	public static Set<OWLAxiom> disambiguate(Set<Set<OWLAxiom>> setOfSetOfAxiom) {
		// We have now a set of axiom sets.

		if (setOfSetOfAxiom.isEmpty()) {
			throw new IllegalArgumentException("Set of OWL axioms must not be empty");
		}

		if (setOfSetOfAxiom.size() == 1) {
			// If this set contain a single element, then select this element.
			return setOfSetOfAxiom.iterator().next();
		}

		// If this set contains multiple elements (i.e. is ambiguous), then resolve it somehow,
		// selecting only a single element.
		// TODO
		return setOfSetOfAxiom.iterator().next();
	}


	/**
	 * <p>Return {@code true} iff the given axioms are reasonable for the given reasoner.</p>
	 */
	public static boolean isReasonable(AceWikiOWLReasoner2 reasoner, Set<OWLAxiom> axioms) {
		// If the reasoner policy does not allow property chains, then
		// check if the axioms contain such chains and if so, then return false.
		if ("no_chains".equals(reasoner.getGlobalRestrictionsPolicy())) {
			for (OWLAxiom ax : axioms) {
				if (ax instanceof OWLTransitiveObjectPropertyAxiom || ax instanceof OWLSubPropertyChainOfAxiom) {
					mLogger.info("Reasoner policy ({}) violated: {}", reasoner.getGlobalRestrictionsPolicy(), ax);
					return false;
				}
			}
		}

		// If the reasoner profile does not allow the given axioms then return false.
		// TODO: make it more efficient: we probably do not need to create a new ontology
		boolean isReasonable = true;
		OWLProfile owlProfile = reasoner.getOWLProfile();
		if (owlProfile == null) {
			// Using the default and most permissive profile
		} else {
			OWLOntology owlOntology;
			try {
				owlOntology = OWL_ONTOLOGY_MANAGER.createOntology(axioms);
			} catch (OWLOntologyCreationException e) {
				return false;
			}
			if (owlOntology != null) {
				OWLProfileReport r = owlProfile.checkOntology(owlOntology);
				for (OWLProfileViolation v : r.getViolations()) {
					// We do not care about undeclared entities
					if (!v.toString().startsWith("Use of undeclared")) {
						isReasonable = false;
						mLogger.info("Reasoner profile ({}) violation: {}", owlProfile.getName(), v);
						break;
					}
				}
			}
			OWL_ONTOLOGY_MANAGER.removeOntology(owlOntology);
		}
		return isReasonable;
	}


	/**
	 * <p>Interprets the given string as a serialization of an OWL ontology,
	 * maps it to a set of OWL axioms and returns those.</p>
	 *
	 * @param str serialization of an OWL ontology e.g. in the OWLFSSPP format
	 * @return set of OWL axioms
	 * @throws OWLOntologyCreationException
	 *
	 * TODO: not sure if this is the most efficient way to do this
	 */
	public static Set<OWLAxiom> getOwlAxiomsFromString(String str) throws OWLOntologyCreationException {
		OWLOntology owlOntology = OWL_ONTOLOGY_MANAGER.loadOntologyFromOntologyDocument(new StringDocumentSource(str));
		OWL_ONTOLOGY_MANAGER.removeOntology(owlOntology);
		return owlOntology.getAxioms();
	}
}