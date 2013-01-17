// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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
import java.util.Arrays;
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

import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.MonolingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.acewiki.owl.AceWikiOWLReasoner;
import ch.uzh.ifi.attempto.acewiki.owl.OWLSentence;
import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.ape.MessageContainer;
import ch.uzh.ifi.attempto.ape.SyntaxBoxes;
import ch.uzh.ifi.attempto.base.APE;
import ch.uzh.ifi.attempto.base.MultiTextContainer;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class represents an ACE sentence, which can be either a declarative sentence or a question.
 *
 * @author Tobias Kuhn
 */
public abstract class ACESentence extends MonolingualSentence implements OWLSentence {

	private static OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

	// This field is either initialized when the object is created, or otherwise unused:
	private String serialized;

	// Unless initialized when the object is created, this field is evaluated lazily:
	private TextContainer textContainer;

	// These fields are evaluated lazily:
	private ACEParserResult parserResult;
	private Boolean reasonable;
	private Boolean isOWL;
	private Boolean isOWLSWRL;
	private Set<OWLAxiom> owlAxioms;

	/**
	 * Initializes a new ACE sentence.
	 *
	 * @param serialized The serialized representation of the sentence.
	 */
	protected ACESentence(String serialized) {
		this.serialized = serialized;
	}

	/**
	 * Initializes a new ACE sentence.
	 *
	 * @param textContainer The text container with the sentence text.
	 */
	protected ACESentence(TextContainer textContainer) {
		this.textContainer = textContainer;
	}

	public List<TextElement> getTextElements() {
		List<TextElement> list = new ArrayList<TextElement>();
		// TODO: this should be done in a different way
		for (TextElement e : getTextContainer().getTextElements()) {
			if (e instanceof OntologyTextElement) {
				OntologyTextElement ote = (OntologyTextElement) e;
				OntologyElement oe = ote.getOntologyElement();
				if (ote.getPreText().length() > 0) {
					list.add(new TextElement(ote.getPreText()));
					ote = new OntologyTextElement(oe, ote.getWordNumber());
				}
				if (oe instanceof ProperNameIndividual) {
					// Proper names with definite articles are handled differently: The "the" is
					// not a part of the link.
					ProperNameIndividual ind = (ProperNameIndividual) oe;
					int wn = ote.getWordNumber();
					if (ind.hasDefiniteArticle(wn)) {
						list.add(new TextElement(e.getText().substring(0, 3)));
						list.add(new OntologyTextElement(ind, wn+1));
					} else {
						list.add(ote);
					}
				} else {
					list.add(ote);
				}
			} else {
				list.add(e);
			}
		}
		return list;
	}

	public MultiTextContainer getTextContainer() {
		if (textContainer == null) {
			tokenize();
		}
		return new MultiTextContainer(textContainer);
	}

	private void tokenize() {
		textContainer = new TextContainer(getTextOperator());

		// TODO Remove legacy code at some point

		// Replace for legacy code below:
		//List<String> tokens = Arrays.asList(serialized.split(" "));

		// This is legacy code to support old acewikidata files:
		String t = "&" + serialized + "&";
		t = t.replaceAll(" ", "&");
		t = t.replaceAll("\\.", "&.&");
		t = t.replaceAll("\\?", "&?&");
		t = t.replaceAll("&of&", " of&");
		t = t.replaceAll("&by&", " by&");

		List<String> tokens = new ArrayList<String>(Arrays.asList(t.split("&")));

		while (tokens.contains("")) {
			tokens.remove("");
		}
		// End of legacy code

		for (String s : tokens) {
			if (s.startsWith("<")) {
				OntologyTextElement te;
				try {
					long oeId = new Long(s.substring(1, s.indexOf(",")));
					int wordNumber = new Integer(s.substring(s.indexOf(",")+1, s.indexOf(">")));
					OntologyElement oe = getOntology().get(oeId);
					te = new OntologyTextElement(oe, wordNumber);
				} catch (Exception ex) {
					throw new RuntimeException("Could not resolve link: " + s, ex);
				}
				textContainer.addElement(te);
			} else {
				TextElement te = getTextOperator().createTextElement(s);
				if (!(te instanceof OntologyTextElement) || serialized.indexOf("<") > -1) {
					textContainer.addElement(te);
				} else {
					// This is legacy code to support old acewikidata files:
					OntologyTextElement ote = (OntologyTextElement) te;
					OntologyElement oe = ote.getOntologyElement();
					int wordId = ote.getWordNumber();
					if (oe instanceof ProperNameIndividual) {
						ProperNameIndividual ind = (ProperNameIndividual) oe;
						if (ind.hasDefiniteArticle(wordId-1) && textContainer.getTextElementsCount() > 0) {
							String precedingText = textContainer.
							getTextElement(textContainer.getTextElementsCount()-1).
							getText();
							if (precedingText.equals("the") || precedingText.equals("The")) {
								textContainer.removeLastElement();
								wordId--;
							}
						}
					}
					textContainer.addElement(new OntologyTextElement(oe, wordId));
					// End of legacy code
				}
			}
		}
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

	public boolean isOWL() {
		if (isOWL == null) {
			update();
		}
		return isOWL;
	}

	public boolean isOWLSWRL() {
		if (isOWLSWRL == null) {
			update();
		}
		return isOWLSWRL;
	}

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

		ACEParser ape = APE.getParser();
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

	public boolean contains(OntologyElement e) {
		for (TextElement t : getTextContainer().getTextElements()) {
			if (t instanceof OntologyTextElement) {
				if (e == ((OntologyTextElement) t).getOntologyElement()) return true;
			}
		}
		return false;
	}

	public String serialize() {
		String s = "";
		for (TextElement te : getTextContainer().getTextElements()) {
			if (te instanceof OntologyTextElement) {
				OntologyTextElement ot = (OntologyTextElement) te;
				s += ot.getPreText();
				s += "<" + ot.getOntologyElement().getId() + "," + ot.getWordNumber() + "> ";
				s += ot.getPostText();
			} else {
				s += te.getText() + " ";
			}
		}
		s = s.replaceAll(" $", "");
		return s;
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
