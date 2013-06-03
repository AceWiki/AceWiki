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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

/**
 * This class represents a wiki article.
 * 
 * @author Tobias Kuhn
 */
public class Article {
	
	private Vector<Statement> statements = new Vector<Statement>();
	private final OntologyElement element;
	private final Ontology ontology;
	
	/**
	 * Creates a new article for the given ontology element.
	 * 
	 * @param element The ontology element.
	 */
	protected Article(OntologyElement element) {
		this.element = element;
		this.ontology = element.getOntology();
	}
	
	void initStatements(List<Statement> statements) {
		this.statements = new Vector<Statement>(statements);
	}
	
	/**
	 * Returns the ontology element of this article.
	 * 
	 * @return The ontology element.
	 */
	public OntologyElement getOntologyElement() {
		return element;
	}
	
	/**
	 * Returns the ontology object.
	 * 
	 * @return The ontoloy.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	/**
	 * Returns the article text as a list of statements.
	 * 
	 * @return The statements.
	 */
	public List<Statement> getStatements() {
		return new ArrayList<Statement>(statements);
	}
	
	public void shuffleStatements() {
		Collections.shuffle(statements);
		ontology.getStorage().save(element);
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
	 */
	public void edit(Statement oldStatement, Statement newStatement) {
		List<Statement> newStatements = new ArrayList<Statement>();
		newStatements.add(newStatement);
		edit(oldStatement, newStatements);
	}
	
	/**
	 * Edits a statement of the article. The old statement is replaced by the new statements.
	 * 
	 * @param oldStatement The statement that should be edited.
	 * @param newStatements The new statements.
	 */
	public void edit(Statement oldStatement, List<Statement> newStatements) {
		synchronized (ontology) {
			if (statements.contains(oldStatement)) {
				int i = statements.indexOf(oldStatement);
				statements.remove(i);
				statements.addAll(i, newStatements);
			} else {
				log("error: statement is not around anymore");
				statements.addAll(0, newStatements);
			}
			if (ontology != null) {
				if (oldStatement instanceof Sentence) {
					ontology.retractSentence((Sentence) oldStatement);
				}
				for (Statement s : newStatements) {
					if (s instanceof Sentence) {
						ontology.commitSentence((Sentence) s);
					}
				}
				ontology.getStorage().save(element);
			}
		}
	}
	
	/**
	 * Adds one new statement to the article. One has to specify in front of which
	 * statement the new statement should be added.
	 * 
	 * @param followingStatement The statement in front of which the new statement should be added,
	 *     or null if the statement should be added to the end of the article.
	 * @param newStatement The new statement to be added.
	 */
	public void add(Statement followingStatement, Statement newStatement) {
		List<Statement> newStatements = new ArrayList<Statement>();
		newStatements.add(newStatement);
		add(followingStatement, newStatements);
	}
	
	/**
	 * Adds one or more new statements to the article. It has to be specified in front of which
	 * statement the new statement should be added.
	 * 
	 * @param followingStatement The statement in front of which the new statements should be
	 *     added, or null if the statements should be added to the end of the article.
	 * @param newStatements The new statements to be added.
	 */
	public void add(Statement followingStatement, List<Statement> newStatements) {
		synchronized (ontology) {
			if (statements.contains(followingStatement)) {
				statements.addAll(statements.indexOf(followingStatement), newStatements);
			} else {
				if (followingStatement != null) {
					log("error: statement is not around anymore");
				}
				statements.addAll(newStatements);
			}
			if (ontology != null) {
				for (Statement s : newStatements) {
					if (s instanceof Sentence) {
						ontology.commitSentence((Sentence) s);
					}
				}
				ontology.getStorage().save(element);
			}
		}
	}
	
	public void addCopiedStatement(Statement followingStatement, Statement newStatement) {
		synchronized (ontology) {
			if (statements.contains(followingStatement)) {
				statements.add(statements.indexOf(followingStatement), newStatement);
			} else {
				if (followingStatement != null) {
					log("error: statement is not around anymore");
				}
				statements.add(newStatement);
			}
			if (ontology != null) {
				ontology.getStorage().save(element);
			}
		}
	}
	
	/**
	 * Removes the given statement from the article.
	 * 
	 * @param statement The statement to be removed.
	 */
	public void remove(Statement statement) {
		synchronized (ontology) {
			if (statements.contains(statement)) {
				statements.remove(statement);
			}
			if (ontology != null) {
				if (statement instanceof Sentence) {
					ontology.retractSentence((Sentence) statement);
				}
				ontology.getStorage().save(element);
			}
		}
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
}
