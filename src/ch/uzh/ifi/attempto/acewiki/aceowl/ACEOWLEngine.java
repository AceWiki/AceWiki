// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

import ch.uzh.ifi.attempto.acewiki.core.AbstractAceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.owl.AceWikiOWLReasoner;
import ch.uzh.ifi.attempto.acewiki.owl.OWLXMLExporter;

/**
 * This is the AceWiki language engine for ACE/OWL. It delivers the grammar, the lexicon, the
 * language factory, the reasoner, and more.
 * 
 * @author Tobias Kuhn
 */
public class ACEOWLEngine extends AbstractAceWikiEngine {
	
	private ACEHandler languageHandler = new ACEHandler();
	private AceWikiOWLReasoner reasoner = new AceWikiOWLReasoner();
	
	/**
	 * Creates a new language engine for ACE/OWL.
	 */
	public ACEOWLEngine() {
		addExporter(new OWLXMLExporter(true));
		addExporter(new OWLXMLExporter(false));
		addExporter(new ACETextExporter(true));
		addExporter(new ACETextExporter(false));
		addExporter(new ACELexiconExporter());
		
		setLexicalTypes("propername", "noun", "nounof", "trverb", "tradj");
	}
	
	public void init(Ontology ontology) {
		super.init(ontology);
	}

	public LanguageHandler getLanguageHandler() {
		return languageHandler;
	}

	public AceWikiReasoner getReasoner() {
		return reasoner;
	}

	public OntologyElement createOntologyElement(String type) {
		if (type.equals("propername")) {
			return new ProperNameIndividual();
		} else if (type.equals("noun")) {
			return new NounConcept();
		} else if (type.equals("nounof")) {
			return new OfRelation();
		} else if (type.equals("trverb")) {
			return new VerbRelation();
		} else if (type.equals("tradj")) {
			return new TrAdjRelation();
		}
		return null;
	}
	
	public Sentence createSentence(String serialized) {
		// remove leading and trailing blank spaces.
		String s = serialized.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (s.substring(s.length()-1).equals("?")) {
			return new ACEQuestion(s);
		} else {
			return new ACEDeclaration(s);
		}
	}
	
	public Sentence createAssignmentSentence(Individual ind, Concept concept) {
		return createSentence(ind.getWord(2) + " is a " + concept.getWord() + ".");
	}
	
	public Sentence createHierarchySentence(Concept subConcept, Concept superConcept) {
		return createSentence("Every " + subConcept.getWord() + " is a " +
				superConcept.getWord() + ".");
	}

}
