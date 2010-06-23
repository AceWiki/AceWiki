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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.semanticweb.owlapi.model.IRI;

import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class represents an ontology element which can be an individual ("constant"), a concept
 * ("unary relation", "class", "type"), or a role ("binary relation", "property"). See the
 * respective sub-classes.
 *<p>
 * In AceWiki, each ontology element corresponds to a word which has one or more word forms.
 * Word forms are identified by a number (the word form id).
 *<p>
 * Every ontology element has an article which consists of a list of statements.
 * 
 * @author Tobias Kuhn
 */
public abstract class OntologyElement implements Comparable<OntologyElement> {
	
	private Ontology ontology;
	
	private final Vector<Statement> statements = new Vector<Statement>();
	private long id = -1;
	
	/**
	 * Initializes the ontology element.
	 */
	protected OntologyElement() {
	}
	
	/**
	 * Loads an ontology element from its serialized form.
	 * 
	 * @param serializedElement The serialized ontology element.
	 * @param id The id of the ontology element.
	 * @param ontology The ontology at which the ontology element should be registered.
	 * @return The ontology element.
	 */
	static OntologyElement loadOntologyElement(String serializedElement, long id,
			Ontology ontology) {
		String[] lines = serializedElement.split("\n");
		if (!lines[0].startsWith("type:") || !lines[1].startsWith("words:")) {
			System.err.println("Cannot read ontology element " + id);
			return null;
		}
		String type = lines[0].substring("type:".length());
		String[] words = lines[1].substring("words:".length()).split(";");
		OntologyElement oe;
		if (type.equals("propername")) {
			oe = new Individual();
		} else if (type.equals("noun")) {
			oe = new NounConcept();
		} else if (type.equals("nounof")) {
			oe = new OfRole();
		} else if (type.equals("trverb")) {
			oe = new VerbRole();
		} else if (type.equals("tradj")) {
			oe = new TrAdjRole();
		} else {
			System.err.println("Cannot read ontology element " + id);
			return null;
		}
		oe.setId(id);
		oe.setWords(words);
		for (int i=2 ; i < lines.length ; i++) {
			Statement statement = StatementFactory.loadStatement(lines[i], oe);
			oe.statements.add(statement);
		}
		oe.ontology = ontology;
		ontology.register(oe);
		return oe;
	}
	
	/**
	 * Returns the word forms. The position in the array corresponds to the word form id.
	 * 
	 * @return An array containing the word forms.
	 */
	public abstract String[] getWords();
	
	/**
	 * Returns the word form for the given word form id.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getWord(int n) {
		return getWords()[n];
	}
	
	/**
	 * Returns the word form with the id 0 (the default word form).
	 * 
	 * @return The word form.
	 */
	public String getWord() {
		return getWord(0);
	}
	
	/**
	 * Returns the pretty-printed word form for the given word form id. The pretty-printing
	 * transforms underscores into blanks.
	 * 
	 * @param n The word form id.
	 * @return The word form.
	 */
	public String getPrettyWord(int n) {
		String w = getWord(n);
		if (w == null) return null;
		return w.replace("_", " ");
	}
	
	/**
	 * Sets the word forms. The order reflects the word form ids. The indexes of the
	 * ontology are automatically updated.
	 * 
	 * @param words The word forms.
	 */
	public final void setWords(String... words) {
		if (ontology == null) {
			changeWords(words);
		} else {
			synchronized (ontology) {
				ontology.removeFromWordIndex(this);
				changeWords(words);
				ontology.addToWordIndex(this);
				ontology.refresh(this);
			}
		}
	}
	
	/**
	 * Changes the word forms without updating the ontology indexes. The order reflects
	 * the word form ids.
	 * 
	 * @param words The word forms.
	 */
	protected abstract void changeWords(String... words);
	
	/**
	 * Returns the headword that is used in the GUI to refer to this ontology element.
	 * For example, it is used for the title of the article. Unless overridden, it is
	 * the same as the pretty-printed word form with the id 0. 
	 * 
	 * @return The headword.
	 */
	public String getHeadword() {
		return getPrettyWord(0);
	}
	
	/**
	 * Returns a list of words that should be listed in the index to point to this ontology
	 * element.
	 * 
	 * @return The index words.
	 */
	public String[] getIndexEntries() {
		return new String[] {getHeadword()};
	}
	
	/**
	 * Returns the word type as it is shown to the user. Newer versions of AceWiki can
	 * savely change this value.
	 * 
	 * @return The word type.
	 */
	public abstract String getType();
	
	/**
	 * Returns the word type as it is used internally. Changing this value in newer versions
	 * of AceWiki breaks backwards compatibility for loading ontologies.
	 * 
	 * @return The internal word type.
	 */
	public abstract String getInternalType();
	
	/**
	 * Returns the ontology this ontology element is registered at.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	/**
	 * Registers this ontology element at the given ontology. An ontology element can be
	 * registered only once.
	 * 
	 * @param ontology
	 */
	public void registerAt(Ontology ontology) {
		if (this.ontology != null) {
			throw new RuntimeException("Cannot change the ontology for element " + toString());
		}
		if (ontology == null) {
			return;
		}
		this.ontology = ontology;
		synchronized (ontology) {
			ontology.register(this);
			ontology.save(this);
		}
	}
	
	/**
	 * Returns the article text as a list of statements.
	 * 
	 * @return The statements.
	 */
	public List<Statement> getStatements() {
		return new ArrayList<Statement>(statements);
	}
	
	/**
	 * Returns the ACE sentences of the article text.
	 * 
	 * @return The ACE sentences.
	 */
	public List<Sentence> getSentences() {
		List<Sentence> sentences = new ArrayList<Sentence>();
		for (Statement s : statements) {
			if (s instanceof Sentence) {
				sentences.add((Sentence) s);
			}
		}
		return sentences;
	}
	
	/**
	 * Edits a statement of the article. The old statement is replaced by the new statement.
	 * 
	 * @param oldStatement The statement that should be edited.
	 * @param newStatement The new statement.
	 * @return An integer value denoting the success/failure of the operation.
	 * @see Ontology#commitSentence(Sentence)
	 */
	public int edit(Statement oldStatement, Statement newStatement) {
		List<Statement> newStatements = new ArrayList<Statement>();
		newStatements.add(newStatement);
		int success = edit(oldStatement, newStatements);
		return success;
	}
	
	/**
	 * Edits a statement of the article. The old statement is replaced by the new statements.
	 * 
	 * @param oldStatement The statement that should be edited.
	 * @param newStatements The new statements.
	 * @return An integer value denoting the success/failure of the operation.
	 * @see Ontology#commitSentence(Sentence)
	 */
	public int edit(Statement oldStatement, List<Statement> newStatements) {
		log("edit statement of " + getWord() + ": " + oldStatement.getText() +
				" > " + getStatementsString(newStatements));
		
		synchronized (ontology) {
			if (statements.contains(oldStatement)) {
				int i = statements.indexOf(oldStatement);
				statements.remove(i);
				statements.addAll(i, newStatements);
			} else {
				log("error: statement is not around anymore");
				statements.addAll(0, newStatements);
			}
			int success = 0;
			if (ontology != null) {
				if (oldStatement instanceof Sentence) {
					ontology.retractSentence((Sentence) oldStatement);
				}
				for (Statement s : newStatements) {
					if (s instanceof Sentence) {
						int successThis = ontology.commitSentence((Sentence) s);
						if (successThis > success) success = successThis;
					}
				}
				ontology.save(this);
			}
			return success;
		}
	}
	
	/**
	 * Adds one new statement to the article. One has to specify in front of which
	 * statement the new statement should be added.
	 * 
	 * @param followingStatement The statement in front of which the new statement should be added,
	 *     or null if the statement should be added to the end of the article.
	 * @param newStatement The new statement to be added.
	 * @return An integer value denoting the success/failure of the operation.
	 * @see Ontology#commitSentence(Sentence)
	 */
	public int add(Statement followingStatement, Statement newStatement) {
		List<Statement> newStatements = new ArrayList<Statement>();
		newStatements.add(newStatement);
		int success = add(followingStatement, newStatements);
		return success;
	}
	
	/**
	 * Adds one or more new statements to the article. It has to be specified in front of which
	 * statement the new statement should be added.
	 * 
	 * @param followingStatement The statement in front of which the new statements should be
	 *     added, or null if the statements should be added to the end of the article.
	 * @param newStatements The new statements to be added.
	 * @return An integer value denoting the success/failure of the operation.
	 * @see Ontology#commitSentence(Sentence)
	 */
	public int add(Statement followingStatement, List<Statement> newStatements) {
		log("add statements of " + getWord() + ": " + getStatementsString(newStatements));

		synchronized (ontology) {
			if (statements.contains(followingStatement)) {
				statements.addAll(statements.indexOf(followingStatement), newStatements);
			} else {
				if (followingStatement != null) {
					log("error: statement is not around anymore");
				}
				statements.addAll(newStatements);
			}
			int success = 0;
			if (ontology != null) {
				for (Statement s : newStatements) {
					if (s instanceof Sentence) {
						int successThis = ontology.commitSentence((Sentence) s);
						if (successThis > success) success = successThis;
					}
				}
				ontology.save(this);
			}
			return success;
		}
	}
	
	private String getStatementsString(List<Statement> statements) {
		String result = "";
		for (Statement s : statements) {
			result += s.getText() + " ";
		}
		return result;
	}
	
	/**
	 * Removes the given statement from the article.
	 * 
	 * @param statement The statement to be removed.
	 */
	public void remove(Statement statement) {
		synchronized (ontology) {
			if (statements.contains(statement)) {
				log("remove statement: " + statement.getText());
				statements.remove(statement);
			}
			if (ontology != null) {
				if (statement instanceof Sentence) {
					ontology.retractSentence((Sentence) statement);
				}
				ontology.save(this);
			}
		}
	}
	
	/**
	 * Returns the lexicon entries (one for each word form).
	 * 
	 * @return The lexicon entries.
	 */
	List<LexiconEntry> getLexiconEntries() {
		return null;
	}
	
	private String getURIString() {
		String ontologyURI = "";
		if (ontology != null) {
			ontologyURI = ontology.getURI();
		}
		return ontologyURI + getURISuffix();
	}
	
	/**
	 * Returns the URI of the ontology element. This URI is a concatenation of the
	 * ontology URI and the URI suffix of the ontology element.
	 * 
	 * @return The URI.
	 * @see #getURISuffix()
	 */
	public final URI getURI() {
		URI uri = null;
		try {
			uri = new URI(getURIString());
		} catch (URISyntaxException ex) {
			ex.printStackTrace();
		}
		return uri;
	}
	
	/**
	 * Returns the URI suffix of this ontology element. For example "#country".
	 * 
	 * @return The URI suffix.
	 * @see #getURI()
	 */
	public String getURISuffix() {
		return "#" + getWord();
	}
	
	public IRI getIRI() {
		return IRI.create(getURIString());
	}
	
	final void setId(long id) {
		this.id = id;
	}
	
	final long getId() {
		return id;
	}
	
	/**
	 * Writes the text to the log file.
	 * 
	 * @param text The text to be written to the log file.
	 */
	protected void log(String text) {
		if (ontology != null) {
			ontology.log(text);
		}
	}
	
	/**
	 * Serializes this ontology element as a string.
	 * 
	 * @return The serialized ontology element.
	 */
	String serialize() {
		String s = "type:";
		s += getInternalType() + "\nwords:";
		for (String word : getWords()) {
			if (word == null) {
				s += ";";
			} else {
				s += word + ";";
			}
		}
		s += "\n";
		for (Statement statement : statements) {
			s += statement.serialize();
		}
		return s;
	}

	public int compareTo(OntologyElement e) {
		return getHeadword().toLowerCase().compareTo(e.getHeadword().toLowerCase());
	}
	
	public String toString() {
		String l = "";
		for (String s : getWords()) {
			if (s == null) {
				l += ",";
			} else {
				l += s + ",";
			}
		}
		if (l.length() > 0) l = l.substring(0, l.length()-1);
		return getType() + "{" + l + "}";
	}

}
