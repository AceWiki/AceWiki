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

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This factory class is used to generate different kind of statements (declarations, questions and
 * comments).
 * 
 * @author Tobias Kuhn
 */
public class StatementFactory {
	
	// no instances allowed
	private StatementFactory() {}

	/**
	 * Creates a new asserted declaration (declarative sentence). Asserted declarations must have
	 * an owner.
	 * 
	 * @param text The sentence text.
	 * @param owner The owner ontology element.
	 */
	public static Declaration createDeclaration(String text, OntologyElement owner) {
		return new Declaration(text, owner);
	}

	/**
	 * Creates a new inferred declaration (declarative sentence). Inferred declarations have no
	 * owner.
	 * 
	 * @param text The sentence text.
	 * @param ontology The ontology.
	 */
	public static Declaration createDeclaration(String text, Ontology ontology) {
		return new Declaration(text, ontology);
	}
	
	/**
	 * Creates a new question. Questions must have an owner.
	 * 
	 * @param text The question text.
	 * @param owner The owner ontology element.
	 */
	protected Question createQuestion(String text, OntologyElement owner) {
		return new Question(text, owner);
	}

	/**
	 * Creates a new comment. Comments must have an owner.
	 * 
	 * @param text The comment text.
	 * @param owner The owner ontology element.
	 */
	public static Comment createComment(String text, OntologyElement owner) {
		return new Comment(text, owner);
	}

	/**
	 * Creates a new sentence object (either a declaration or a question) with the given ontology
	 * element as its owner.
	 * 
	 * @param text The sentence text.
	 * @param owner The owner ontology element.
	 * @return The new sentence object.
	 */
	public static Sentence createSentence(String text, OntologyElement owner) {
		// remove leading and trailing blank spaces.
		text = text.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (text.substring(text.length()-1).equals("?")) {
			return new Question(text, owner);
		} else {
			return new Declaration(text, owner);
		}
	}
	
	/**
	 * Loads a statement from a serialized form.
	 * 
	 * @param serializedStatement The serialized statement as a string.
	 * @param owner The owner ontology element of the statement.
	 * @return The new statement object.
	 */
	public static Statement loadStatement(String serializedStatement, OntologyElement owner) {
		if (serializedStatement.length() < 2) return null;
		String s = serializedStatement.substring(2);

		if (serializedStatement.startsWith("| ")) {
			Sentence sentence = StatementFactory.createSentence(s, owner);
			sentence.setIntegrated(true);
			return sentence;
		} else if (serializedStatement.startsWith("# ")) {
			Sentence sentence = StatementFactory.createSentence(s, owner);
			sentence.setIntegrated(false);
			return sentence;
		} else if (serializedStatement.startsWith("c ")) {
			return new Comment(s.replaceAll("~n", "\n").replaceAll("~t", "~"), owner);
		}
		
		return null;
	}

	/**
	 * Generates sentence objects out of a text container.
	 * 
	 * @param tc The text container.
	 * @param owner The owner ontology element of the sentences.
	 * @return A list of sentences.
	 */
	public static List<Sentence> createSentences(TextContainer tc, OntologyElement owner) {
		List<Sentence> l = new ArrayList<Sentence>();
		TextContainer c = new TextContainer(Sentence.contextChecker);
		for (TextElement e : tc.getTextElements()) {
			c.addElement(e);
			if (e.getText().matches("[.?]")) {
				l.add(createSentence(Sentence.getUnderscoredText(c), owner));
				c = new TextContainer(Sentence.contextChecker);
			}
		}
		return l;
	}

}
