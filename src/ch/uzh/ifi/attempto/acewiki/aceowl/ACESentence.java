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

import static ch.uzh.ifi.attempto.ape.OutputType.DRSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLXML;
import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;
import static ch.uzh.ifi.attempto.ape.OutputType.SYNTAX;
import static ch.uzh.ifi.attempto.ape.OutputType.SYNTAXPP;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.StringEscapeUtils;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.StringDocumentSource;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.profiles.OWLProfile;
import org.semanticweb.owlapi.profiles.OWLProfileReport;
import org.semanticweb.owlapi.profiles.OWLProfileViolation;

import ch.uzh.ifi.attempto.acewiki.core.AbstractSentence;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.ape.MessageContainer;
import ch.uzh.ifi.attempto.ape.SyntaxBoxes;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class represents an ACE sentence, which can be either a declarative sentence or a question.
 * 
 * @author Tobias Kuhn
 */
public abstract class ACESentence extends AbstractSentence {
	
	private static OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

	private String text;
	
	// These fields are evaluated lazily:
	private TextContainer textContainer;
	private ACEParserResult parserResult;
	private Boolean reasonable;
	private Boolean isOWL;
	private Boolean isOWLSWRL;
	private Set<OWLAxiom> owlAxioms;
	
	/**
	 * Initializes a new sentence.
	 * 
	 * @param text The sentence text.
	 */
	protected ACESentence(String text) {
		this.text = text;
	}
	
	public List<TextElement> getTextElements() {
		List<TextElement> list = new ArrayList<TextElement>();
		// TODO: this should be done in a different way
		for (TextElement e : getTextContainer().getTextElements()) {
			if (e instanceof OntologyTextElement) {
				OntologyTextElement ote = (OntologyTextElement) e;
				OntologyElement oe = ote.getOntologyElement();
				if (oe instanceof ProperNameIndividual) {
					// Proper names with definite articles are handled differently: The "the" is
					// not a part of the link.
					ProperNameIndividual ind = (ProperNameIndividual) oe;
					int wn = ote.getWordNumber();
					if (ind.hasDefiniteArticle(wn)) {
						list.add(new TextElement(e.getText().substring(0, 3)));
						list.add(new OntologyTextElement(ind, wn+1));
					} else {
						list.add(e);
					}
				} else {
					list.add(e);
				}
			} else {
				list.add(e);
			}
		}
		return list;
	}
	
	protected TextContainer getTextContainer() {
		if (textContainer == null) {
			tokenize();
		}
		return textContainer;
	}
	
	private void tokenize() {
		textContainer = Tokenizer.tokenize(text, getOntology());
	}
	
	/**
	 * Returns the parser result object.
	 * 
	 * @return The parser result object.
	 */
	public ACEParserResult getParserResult() {
		if (parserResult == null) {
			update();
		}
		return parserResult;
	}
	
	/**
	 * Returns a pretty-printed OWL representation of this sentence.
	 * 
	 * @return The pretty-printed OWL representation.
	 */
	public String getPrettyOWL() {
		if (parserResult == null) {
			update();
		}
		return parserResult.get(OWLFSSPP);
	}
	
	public boolean isReasonable() {
		if (reasonable == null) {
			update();
		}
		return reasonable;
	}
	
	/**
	 * Returns true if this sentence has an OWL representation.
	 * 
	 * @return true if this sentence has an OWL representation.
	 */
	public boolean isOWL() {
		if (isOWL == null) {
			update();
		}
		return isOWL;
	}
	
	/**
	 * Returns true if this sentence has an OWL or SWRL representation.
	 * 
	 * @return true if this sentence has an OWL or SWRL representation.
	 */
	public boolean isOWLSWRL() {
		if (isOWLSWRL == null) {
			update();
		}
		return isOWLSWRL;
	}
	
	/**
	 * Returns a set of OWL axioms that represent this sentence.
	 * 
	 * @return The OWL axioms.
	 */
	public Set<OWLAxiom> getOWLAxioms() {
		if (parserResult == null) {
			update();
		}
		if (owlAxioms == null) {
			owlAxioms = new HashSet<OWLAxiom>();
		}
		return owlAxioms;
	}
	
	public void update() {
		// TODO: refactor and clean-up!
		AceWikiOWLReasoner reasoner = (AceWikiOWLReasoner) getOntology()
				.getReasoner().getWrappedReasoner();
		
		APELocal ape = APELocal.getInstance();
		synchronized (ape) {
			ape.setURI(getOntology().getURI());
			ape.setClexEnabled(false);
			Lexicon lexicon = new Lexicon();
			for (TextElement te : getTextContainer().getTextElements()) {
				if (te instanceof OntologyTextElement) {
					OntologyElement oe = ((OntologyTextElement) te).getOntologyElement();
					if (oe instanceof ACEOWLOntoElement) {
						for (LexiconEntry le : ((ACEOWLOntoElement) oe).getLexiconEntries()) {
							lexicon.addEntry(le);
						}
					}
				}
			}
			parserResult = ape.getMultiOutput(
					getText(),
					lexicon,
					PARAPHRASE1,
					SYNTAX,
					SYNTAXPP,
					OWLXML,
					OWLFSSPP,
					DRSPP
				);
		}
		MessageContainer mc = parserResult.getMessageContainer();
		String owlxml = parserResult.get(OWLXML);
		
		isOWLSWRL =
			(mc.getMessages("owl").size() == 0) &&
			(owlxml.length() > 0);
		
		isOWL = isOWLSWRL &&
			(owlxml.indexOf("<swrl:Imp>") < 0) &&
			(owlxml.indexOf("<DLSafeRule>") < 0);
		
		if (isOWL && reasoner.getGlobalRestrictionsPolicy().equals("no_chains")) {
			reasonable =
				(owlxml.indexOf("<TransitiveObjectProperty>") < 0) &&
				(owlxml.indexOf("<ObjectPropertyChain>") < 0);
		} else {
			reasonable = isOWL;
		}
		
		owlAxioms = null;
		OWLOntology owlOntology = null;
		if (isOWL) {
			try {
				owlOntology = ontologyManager.loadOntologyFromOntologyDocument(
						new StringDocumentSource(owlxml)
					);
				if (owlOntology.isEmpty()) {
					reasonable = false;
					isOWL = false;
					isOWLSWRL = false;
				} else {
					owlAxioms = owlOntology.getAxioms();
				}
			} catch (OWLOntologyCreationException ex) {
				ex.printStackTrace();
			}
		}
		OWLProfile owlProfile = reasoner.getOWLProfile();
		if (reasonable && owlOntology != null && owlProfile != null && this instanceof Declaration) {
			OWLProfileReport r = owlProfile.checkOntology(owlOntology);
			for (OWLProfileViolation v : r.getViolations()) {
				if (!v.toString().startsWith("Use of undeclared")) {
					reasonable = false;
					break;
				}
			}
		}
		if (owlOntology != null) {
			ontologyManager.removeOntology(owlOntology);
		}
		if (!reasonable && isIntegrated()) {
			super.setIntegrated(false);
		}
		//String messages = mc.toString();
		//if (messages.length() > 0) {
		//	System.err.println("Parser messages: " + messages);
		//}
	}
	
	public void setIntegrated(boolean integrated) {
		if (integrated && reasonable != null && !reasonable) {
			super.setIntegrated(false);
		} else {
			super.setIntegrated(integrated);
		}
	}
	
	public boolean contains(OntologyElement e, int wordNumber) {
		if (textContainer == null) {
			tokenize();
		}
		for (TextElement t : textContainer.getTextElements()) {
			if (t instanceof OntologyTextElement) {
				OntologyTextElement ot = (OntologyTextElement) t;
				if (e == ot.getOntologyElement() && wordNumber == -1) return true;
				if (e == ot.getOntologyElement() && wordNumber == ot.getWordNumber()) return true;
			}
		}
		return false;
	}
	
	public boolean contains(OntologyElement e) {
		return contains(e, -1);
	}
	
	public String serialize(boolean encodeWords) {
		if (textContainer == null) {
			tokenize();
		}
		String s;
		if (isIntegrated()) {
			s = "|";
		} else {
			s = "#";
		}
		for (TextElement te : textContainer.getTextElements()) {
			if (te instanceof OntologyTextElement) {
				OntologyTextElement ot = (OntologyTextElement) te;
				if (encodeWords) {
					s += " <" + ot.getOntologyElement().getId() + "," + ot.getWordNumber() + ">";
				} else {
					s += " " + ot.getUnderscoredText();
				}
			} else {
				s += " " + te.getText();
			}
		}
		return s + "\n";
	}

	public List<SentenceDetail> getDetails() {
		List<SentenceDetail> l = new ArrayList<SentenceDetail>();
		l.add(new SentenceDetail(
				"Paraphrase",
				StringEscapeUtils.escapeHtml(getParserResult().get(PARAPHRASE1))
			));
		l.add(new SentenceDetail(
				"Syntax Boxes",
				SyntaxBoxes.getBoxesHtml(getParserResult())
			));
		l.add(new SentenceDetail(
				"Syntax Tree",
				"<pre>" + getParserResult().get(SYNTAXPP) + "</pre>"
			));
		l.add(new SentenceDetail(
				"Logical representation",
				"<i><pre>" + StringEscapeUtils.escapeHtml(getParserResult().get(DRSPP)) + "</pre></i>"
			));
		if (isOWLSWRL()) {
			l.add(new SentenceDetail(
					"OWL",
					"<i><pre>" + StringEscapeUtils.escapeHtml(getPrettyOWL()) + "</pre></i>"
				));
		}
		return l;
	}

}
