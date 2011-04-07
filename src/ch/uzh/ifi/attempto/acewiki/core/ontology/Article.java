package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

public class Article {
	
	private Vector<Statement> statements = new Vector<Statement>();
	private final OntologyElement element;
	private final Ontology ontology;
	
	protected Article(OntologyElement element) {
		this.element = element;
		this.ontology = element.getOntology();
	}
	
	void initStatements(List<Statement> statements) {
		this.statements = new Vector<Statement>(statements);
	}
	
	public OntologyElement getOntologyElement() {
		return element;
	}
	
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
		log("edit statement of " + element.getWord() + ": " + oldStatement.getText() +
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
				ontology.getStorage().save(element);
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
		log("add statements of " + element.getWord() + ": " + getStatementsString(newStatements));

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
				ontology.getStorage().save(element);
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
				ontology.getStorage().save(element);
			}
		}
	}
	
	String serialize(boolean encodeWords) {
		String s = "";
		for (Statement statement : statements) {
			s += statement.serialize(encodeWords);
		}
		return s;
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
