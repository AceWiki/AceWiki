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

import static ch.uzh.ifi.attempto.ape.OutputType.DRSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLRDF;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLXML;
import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;
import static ch.uzh.ifi.attempto.ape.OutputType.SYNTAX;
import static ch.uzh.ifi.attempto.ape.OutputType.SYNTAXPP;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.ape.MessageContainer;
import ch.uzh.ifi.attempto.preditor.ContextChecker;
import ch.uzh.ifi.attempto.preditor.EnglishContextChecker;
import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class represents an ACE sentence which is either a declarative statement or a question.
 * Some declarative sentences can be translated into OWL and can participate in reasoning. Other
 * sentences have no OWL representation and do not participate in reasoning.
 *<p>
 * ACE sentences can either have an ontology element as owner (in the case of asserted sentences)
 * or it can be an independent statement that has no owner (in the case of inferred sentences).
 *<p>
 * Parsing of the sentence is done lazily, i.e. at the first time when a parsing result is
 * required. Parsing fails silently. No exceptions are thrown if a sentence is not ACE compliant.
 * 
 * @author Tobias Kuhn
 */
public class Sentence extends Statement {
	
	/**
	 * The context checker used for AceWiki.
	 */
	public static final ContextChecker contextChecker = new EnglishContextChecker(true, true);
	
	private String text;
	private boolean integrated = false;
	
	// These fields are evaluated lazily:
	private TextContainer textContainer;
	private ACEParserResult parserResult;
	private String owlxml;
	private Boolean reasonerParticipant;
	private Boolean isOWL;
	private Boolean isOWLSWRL;
	private OWLOntology owlOntology;
	
	private List<OntologyElement> answerCache;
	private long answerCacheStateID = -1;
	
	/**
	 * Creates a new asserted sentence. Asserted sentences must have an owner.
	 * 
	 * @param text The sentence text.
	 * @param owner The owner ontology element.
	 */
	public Sentence(String text, OntologyElement owner) {
		super(owner);
		setText(text);
	}
	
	/**
	 * Creates a new inferred sentence. Inferred sentence have no owner.
	 * 
	 * @param text The sentence text.
	 * @param ontology The ontology.
	 */
	public Sentence(String text, Ontology ontology) {
		super(ontology);
		setText(text);
	}
	
	/**
	 * Generates sentence objects out of a text container.
	 * 
	 * @param tc The text container.
	 * @param owner The owner ontology element of the sentences.
	 * @return A list of sentences.
	 */
	public static List<Sentence> generateSentences(TextContainer tc, OntologyElement owner) {
		List<Sentence> l = new ArrayList<Sentence>();
		TextContainer c = new TextContainer(contextChecker);
		for (TextElement e : tc.getTextElements()) {
			c.addElement(e);
			if (e.getText().matches("[.?]")) {
				l.add(new Sentence(getUnderscoredText(c), owner));
				c = new TextContainer(contextChecker);
			}
		}
		return l;
	}
	
	/**
	 * Returns a list of text elements that represent the tokens of this sentence.
	 * 
	 * @return A token list.
	 */
	public List<TextElement> getTextElements() {
		if (textContainer == null) {
			tokenize();
		}
		return textContainer.getTextElements();
	}
	
	private void setText(String text) {
		// remove trailing blank spaces.
		this.text = text.replaceFirst("\\s+$", "");
	}
	
	/**
	 * Returns the sentence text as a string. Underscores are used for compound words,
	 * e.g. "credit_card".
	 * 
	 * @return The sentence text as a string.
	 */
	public String getText() {
		if (textContainer == null) {
			tokenize();
		}
		return getUnderscoredText(textContainer);
	}
	
	/**
	 * Returns the sentence text as a string with underscores displayed as blanks. Compound
	 * words containing underscores like "credit_cards" are pretty-printed with blank characters:
	 * "credit card".
	 * 
	 * @return The sentence text as a pretty-printed string.
	 */
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
	 * Returns the OWL/XML representation of this sentence as a string.
	 * 
	 * @return The OWL/XML representation.
	 */
	public String getOWLXML() {
		if (owlxml == null) {
			parse();
		}
		return owlxml;
	}
	
	/**
	 * Returns true if this sentence participates in reasoning.
	 * 
	 * @return true if this sentence participates in reasoning.
	 */
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
	 * Returns the OWL ontology object that contains the OWL representation of this
	 * sentence. Null is returned if there is no OWL representation of this sentence
	 * or if the creation of the OWL ontology object failed.
	 * 
	 * @return The OWL ontology object.
	 */
	public OWLOntology getOWLOntology() {
		if (owlxml == null) {
			parse();
		}
		return owlOntology;
	}
	
	/**
	 * Tokenizes the sentence text. A text container object is created.
	 */
	private void tokenize() {
		textContainer = new TextContainer(contextChecker);
		
		String t = "&" + text + "&";
		t = t.replaceAll(" ", "&");
		t = t.replaceAll("\\.", "&.&");
		t = t.replaceAll("\\?", "&?&");
		t = t.replaceAll("&of&", " of&");
		t = t.replaceAll("&by&", " by&");
		
		List<String> tokens = new ArrayList<String>(Arrays.asList(t.split("&")));
		
		while (tokens.contains("")) {
			tokens.remove("");
		}
		
		toString();
		
		for (String s : tokens) {
			if (s.startsWith("<")) {
				OntologyTextElement te;
				try {
					long oeId = new Long(s.substring(1, s.indexOf(",")));
					int wordNumber = new Integer(s.substring(s.indexOf(",")+1, s.indexOf(">")));
					OntologyElement oe = getOntology().get(oeId);
					te = OntologyTextElement.createTextElement(oe, wordNumber);
				} catch (Exception ex) {
					throw new RuntimeException("Could not resolve link: " + s, ex);
				}
				if (te != null) {
					textContainer.addElement(te);
				} else {
					throw new RuntimeException("Could not resolve link: " + s);
				}
			} else {
				OntologyElement oe = getOntology().get(s);
				
				if (oe == null) {
					textContainer.addElement(new TextElement(s));
				} else {
					// TODO: not 100% clean solution (several word forms of the same word can be
					// identical):
					int wordId = Arrays.asList(oe.getWords()).indexOf(s);
					if (oe instanceof Individual) {
						// TODO: this should probably be done at a different place...
						Individual ind = (Individual) oe;
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
					textContainer.addElement(OntologyTextElement.createTextElement(oe, wordId));
				}
			}
		}
	}
	
	/**
	 * Parses the sentence text. The OWL and SWRL representations are calculated if possible.
	 * This method is called automatically the first time a parsing result is needed.
	 * Furthermore, it needs to be called each time a word form of an ontology element
	 * (that occurs in the sentence) has changed.
	 */
	synchronized void parse() {
		APELocal.getInstance().setURI(getOntology().getURI());
		APELocal.getInstance().setClexEnabled(false);
		Lexicon lexicon = new Lexicon();
		for (TextElement te : getTextElements()) {
			if (te instanceof OntologyTextElement) {
				OntologyElement oe = ((OntologyTextElement) te).getOntologyElement();
				for (LexiconEntry le : oe.getLexiconEntries()) {
					lexicon.addEntry(le);
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
				OWLRDF,
				DRSPP
			);
		MessageContainer mc = parserResult.getMessageContainer();
		owlxml = parserResult.get(OWLXML);
		if (owlxml != null) {
			// Every OWL ontology object needs its own URI:
			long hashCode = (long) getText().hashCode() - Integer.MIN_VALUE;
			String uri = getOntology().getURI();
			owlxml = owlxml.replace("URI=\"" + uri + "\">", "URI=\"" + uri + "/" + hashCode + "\">");
			
			// Transform from OWL 1.1 to OWL 2:
			owlxml = owlxml.replaceAll("http://www.w3.org/2006/12/owl11-xml#", "http://www.w3.org/2002/07/owl#");
			owlxml = owlxml.replaceAll("InverseObjectProperty>", "ObjectInverseOf>");
			owlxml = owlxml.replaceAll("SubObjectPropertyChain>", "ObjectPropertyChain>");
			owlxml = owlxml.replaceAll("ObjectExistsSelf>", "ObjectHasSelf>");
			
			//if (isQuestion()) {
			//	owlxml = owlxml.replace("<Class URI=\"http://www.w3.org/2002/07/owl#Thing\"/>\n  </SubClassOf>\n</Ontology>",
			//				"<Class URI=\"http://attempto.ifi.uzh.ch/ace#Question" + hashCode + "\"/>/>\n  </SubClassOf>\n</Ontology>");
			//}
		}
		
		reasonerParticipant =
			(mc.getMessages("owl").size() == 0) &&
			(owlxml.indexOf("<swrl:Imp>") < 0) &&
			(owlxml.indexOf("<ObjectHasSelf>") < 0) &&
			(owlxml.length() > 0);
		
		if (reasonerParticipant && getOntology().getGlobalRestrictionsPolicy().equals("noChains")) {
			reasonerParticipant =
				(owlxml.indexOf("<TransitiveObjectProperty>") < 0) &&
				(owlxml.indexOf("<ObjectPropertyChain>") < 0);
		}
		
		isOWL =
			(mc.getMessages("owl").size() == 0) &&
			(owlxml.indexOf("<swrl:Imp>") < 0) &&
			(owlxml.length() > 0);
		isOWLSWRL =
			(mc.getMessages("owl").size() == 0) &&
			(owlxml.length() > 0);
		owlOntology = null;
		if (isOWL) {
		try {
				owlOntology = getOntology().readOWLOntology(owlxml);
				if (owlOntology.isEmpty()) {
					reasonerParticipant = false;
					isOWL = false;
					isOWLSWRL = false;
				}
			} catch (OWLOntologyCreationException ex) {
				ex.printStackTrace();
			}
		}
		if (isQuestion()) {
			reasonerParticipant = false;
		}
		//String messages = mc.toString();
		//if (messages.length() > 0) {
		//	System.err.println("Parser messages: " + messages);
		//}
	}
	
	/**
	 * This method tries to reassert a sentence that is not yet integrated. This is
	 * used for sentences that have an OWL representation but the integration failed
	 * because it introduced an inconsistency. Later, when the ontology has changed,
	 * the integration might succeed.
	 * 
	 * @return An integer value denoting the success/failure of the operation.
	 * @see Ontology#commitSentence(Sentence)
	 */
	public int reassert() {
		int success = getOntology().commitSentence(this);
		getOntology().save(getOwner());
		return success;
	}
	
	/**
	 * Returns true if the sentence is integrated into the ontology.
	 * 
	 * @return true if the sentence is integrated into the ontology.
	 */
	public boolean isIntegrated() {
		return integrated;
	}
	
	void setIntegrated(boolean integrated) {
		this.integrated = integrated;
	}
	
	/**
	 * Returns true if the sentence is a question.
	 * 
	 * @return true if the sentence is a question.
	 */
	public boolean isQuestion() {
		return text.substring(text.length()-1).equals("?");
	}
	
	/**
	 * Checks if the sentence is inferred or asserted.
	 * 
	 * @return true if the sentence is inferred, false if it is asserted.
	 */
	public boolean isInferred() {
		return getOwner() == null;
	}
	
	/**
	 * Checks whether the sentence contains the given word form (by word number) of the
	 * given ontology element.
	 * 
	 * @param e The ontology element.
	 * @param wordNumber The word number.
	 * @return true if the word form occurs in this sentence.
	 */
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

	/**
	 * Checks whether the sentence contains the given ontology element (no matter which
	 * word form).
	 * 
	 * @param e The ontology element.
	 * @return true if the ontology element occurs in this sentence.
	 */
	public boolean contains(OntologyElement e) {
		return contains(e, -1);
	}
	
	/**
	 * Returns all ontology elements that answer this question. In the case the sentence has the
	 * form "what is (Individual)?" then the answer contains all concepts the individual belongs
	 * to. Otherwise, the question is processed as a "DL Query" that describes a concept. In this
	 * case, the answer consists of all individuals that belong to the concept. 
	 * The null value is returned if the sentence is not a question.
	 * 
	 * @return A list of ontology elements that are the answer for the question.
	 * @see Ontology#getAnswer(Sentence)
	 */
	public synchronized List<OntologyElement> getAnswer() {
		if (!isQuestion()) return null;
		
		Ontology o = getOntology();
		if (answerCacheStateID != o.getStateID()) {
			answerCache = o.getAnswer(this);
			answerCacheStateID = o.getStateID();
		}
		if (answerCache == null) {
			return null;
		} else {
			return new ArrayList<OntologyElement>(answerCache);
		}
	}
	
	/**
	 * Returns the cached answer if the sentence is a question. Null is returned if the the
	 * sentence is no question or there is no cached answer. This returned answer might not be
	 * up-to-date.
	 * 
	 * @return A list of ontology elements that are the cached answer for the question.
	 */
	public List<OntologyElement> getCachedAnswer() {
		if (!isQuestion() || answerCache == null) return null;
		return new ArrayList<OntologyElement>(answerCache);
	}
	
	/**
	 * Returns true if the sentence is a question and the answer to the question is cached and
	 * up-to-date and thus does not have to be recalculated.
	 * 
	 * @return true if the answer is cached.
	 */
	public boolean isAnswerCached() {
		if (!isQuestion()) return false;
		return answerCacheStateID == getOntology().getStateID();
	}
	
	private static String getUnderscoredText(TextContainer textContainer) {
		String t = "";
		for (TextElement te : textContainer.getTextElements()) {
			if (te instanceof OntologyTextElement) {
				t += " " + ((OntologyTextElement) te).getUnderscoredText();
			} else if (te.getText().matches("[.?]")) {
				t += te.getText();
			} else {
				t += " " + te.getText();
			}
		}
		if (t.length() > 0) {
			t = t.substring(1);
		}
		return t;
	}
	
	String serialize() {
		if (textContainer == null) {
			tokenize();
		}
		String s;
		if (integrated) {
			s = "|";
		} else {
			s = "#";
		}
		for (TextElement te : textContainer.getTextElements()) {
			if (te instanceof OntologyTextElement) {
				OntologyTextElement ot = (OntologyTextElement) te;
				s += " <" + ot.getOntologyElement().getId() + "," + ot.getWordNumber() + ">";
			} else {
				s += " " + te.getText();
			}
		}
		return s + "\n";
	}
	
	public String toString() {
		return getText();
	}

}
