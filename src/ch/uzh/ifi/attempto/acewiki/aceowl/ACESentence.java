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
import ch.uzh.ifi.attempto.acewiki.core.SentenceInfo;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.ape.MessageContainer;
import ch.uzh.ifi.attempto.ape.SyntaxBoxes;
import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

public abstract class ACESentence extends AbstractSentence {
	
	private static OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

	private String text;
	
	// These fields are evaluated lazily:
	private TextContainer textContainer;
	private ACEParserResult parserResult;
	private Boolean reasonerParticipant;
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
	
	private TextContainer getTextContainer() {
		if (textContainer == null) {
			tokenize();
		}
		return textContainer;
	}
	
	public String getText() {
		if (textContainer == null) {
			tokenize();
		}
		return getUnderscoredText(textContainer);
	}
	
	private void tokenize() {
		textContainer = Tokenizer.tokenize(text, getOntology());
	}
	
	public String getPrettyText() {
		return textContainer.getText();
	}
	
	/**
	 * Returns the parser result object.
	 * 
	 * @return The parser result object.
	 */
	public ACEParserResult getParserResult() {
		if (parserResult == null) {
			parse();
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
			parse();
		}
		return parserResult.get(OWLFSSPP);
	}
	
	public boolean isReasonerParticipant() {
		if (reasonerParticipant == null) {
			parse();
		}
		return reasonerParticipant;
	}
	
	/**
	 * Returns true if this sentence has an OWL representation.
	 * 
	 * @return true if this sentence has an OWL representation.
	 */
	public boolean isOWL() {
		if (isOWL == null) {
			parse();
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
			parse();
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
			parse();
		}
		if (owlAxioms == null) {
			owlAxioms = new HashSet<OWLAxiom>();
		}
		return owlAxioms;
	}
	
	/**
	 * Parses the sentence text. The OWL and SWRL representations are calculated if possible.
	 * This method is called automatically the first time a parsing result is needed.
	 * Furthermore, it needs to be called each time a word form of an ontology element
	 * (that occurs in the sentence) has changed.
	 */
	public void parse() {
		// TODO: refactor and clean-up!
		AceWikiOWLReasoner reasoner = (AceWikiOWLReasoner) getOntology()
				.getReasonerManager().getReasoner();
		
		synchronized (APELocal.class) {
			APELocal.getInstance().setURI(getOntology().getURI());
			APELocal.getInstance().setClexEnabled(false);
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
			parserResult = APELocal.getInstance().getMultiOutput(
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
		if (owlxml != null) {
			owlxml = OWLXMLTransformer.transform(owlxml);
		}
		
		isOWLSWRL =
			(mc.getMessages("owl").size() == 0) &&
			(owlxml.length() > 0);
		
		isOWL = isOWLSWRL &&
			(owlxml.indexOf("<swrl:Imp>") < 0) &&
			(owlxml.indexOf("<DLSafeRule>") < 0);
		
		if (isOWL && reasoner.getGlobalRestrictionsPolicy().equals("no_chains")) {
			reasonerParticipant =
				(owlxml.indexOf("<TransitiveObjectProperty>") < 0) &&
				(owlxml.indexOf("<ObjectPropertyChain>") < 0);
		} else {
			reasonerParticipant = isOWL;
		}
		
		owlAxioms = null;
		OWLOntology owlOntology = null;
		if (isOWL) {
			try {
				owlOntology = ontologyManager.loadOntologyFromOntologyDocument(
						new StringDocumentSource(owlxml)
					);
				if (owlOntology.isEmpty()) {
					reasonerParticipant = false;
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
		if (reasonerParticipant && owlOntology != null && owlProfile != null && this instanceof Declaration) {
			OWLProfileReport r = owlProfile.checkOntology(owlOntology);
			for (OWLProfileViolation v : r.getViolations()) {
				if (!v.toString().startsWith("Use of undeclared")) {
					reasonerParticipant = false;
					break;
				}
			}
		}
		if (owlOntology != null) {
			ontologyManager.removeOntology(owlOntology);
		}
		if (!reasonerParticipant && isIntegrated()) {
			super.setIntegrated(false);
		}
		//String messages = mc.toString();
		//if (messages.length() > 0) {
		//	System.err.println("Parser messages: " + messages);
		//}
	}
	
	public void setIntegrated(boolean integrated) {
		if (integrated && reasonerParticipant != null && !reasonerParticipant) {
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

	public List<SentenceInfo> getDetailInfo() {
		List<SentenceInfo> l = new ArrayList<SentenceInfo>();
		l.add(new SentenceInfo(
				"Paraphrase",
				StringEscapeUtils.escapeHtml(getParserResult().get(PARAPHRASE1))
			));
		l.add(new SentenceInfo(
				"Syntax Boxes",
				SyntaxBoxes.getBoxesHtml(getParserResult())
			));
		l.add(new SentenceInfo(
				"Syntax Tree",
				"<pre>" + getParserResult().get(SYNTAXPP) + "</pre>"
			));
		return l;
	}

	public List<SentenceInfo> getLogicInfo() {
		List<SentenceInfo> l = new ArrayList<SentenceInfo>();
		l.add(new SentenceInfo(
				"Logical representation",
				"<i><pre>" + StringEscapeUtils.escapeHtml(getParserResult().get(DRSPP)) + "</pre></i>"
			));
		if (isOWLSWRL()) {
			l.add(new SentenceInfo(
					"OWL",
					"<i><pre>" + StringEscapeUtils.escapeHtml(getPrettyOWL()) + "</pre></i>"
				));
		}
		return l;
	}

}
