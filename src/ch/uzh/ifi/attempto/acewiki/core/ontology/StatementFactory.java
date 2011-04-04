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
	 * Creates a new asserted declaration (declarative sentence). Asserted declarations must be
	 * part of an article.
	 * 
	 * @param text The sentence text.
	 * @param article The article
	 * @return The new declaration.
	 */
	public static Declaration createDeclaration(String text, Article article) {
		return new Declaration(text, article);
	}

	/**
	 * Creates a new inferred declaration (declarative sentence). Inferred declarations have no
	 * articles.
	 * 
	 * @param text The sentence text.
	 * @param ontology The ontology.
	 * @return The new declaration.
	 */
	public static Declaration createDeclaration(String text, Ontology ontology) {
		return new Declaration(text, ontology);
	}
	
	/**
	 * Creates a new question. Questions must be part of an article.
	 * 
	 * @param text The question text.
	 * @param article The article.
	 * @return The new declaration.
	 */
	protected Question createQuestion(String text, Article article) {
		return new Question(text, article);
	}

	/**
	 * Creates a new comment. Comments must be part of an article.
	 * 
	 * @param text The comment text.
	 * @param article The article.
	 * @return The new comment.
	 */
	public static Comment createComment(String text, Article article) {
		return new Comment(text, article);
	}

	/**
	 * Creates a new sentence object (either a declaration or a question) with the given article.
	 * 
	 * @param text The sentence text.
	 * @param article The article.
	 * @return The new sentence object.
	 */
	public static Sentence createSentence(String text, Article article) {
		// remove leading and trailing blank spaces.
		text = text.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (text.substring(text.length()-1).equals("?")) {
			return new Question(text, article);
		} else {
			return new Declaration(text, article);
		}
	}
	
	/**
	 * Loads a statement from a serialized form.
	 * 
	 * @param serializedStatement The serialized statement as a string.
	 * @param article The article of the statement.
	 * @return The new statement object.
	 */
	public static Statement loadStatement(String serializedStatement, Article article) {
		if (serializedStatement.length() < 2) return null;
		String s = serializedStatement.substring(2);

		if (serializedStatement.startsWith("| ")) {
			Sentence sentence = StatementFactory.createSentence(s, article);
			sentence.setIntegrated(true);
			return sentence;
		} else if (serializedStatement.startsWith("# ")) {
			Sentence sentence = StatementFactory.createSentence(s, article);
			sentence.setIntegrated(false);
			return sentence;
		} else if (serializedStatement.startsWith("c ")) {
			return new Comment(s.replaceAll("~n", "\n").replaceAll("~t", "~"), article);
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
	public static List<Sentence> createSentences(TextContainer tc, Article article) {
		List<Sentence> l = new ArrayList<Sentence>();
		TextContainer c = new TextContainer(tc.getTextOperator());
		for (TextElement e : tc.getTextElements()) {
			c.addElement(e);
			if (e.getText().matches("[.?]")) {
				l.add(createSentence(Sentence.getUnderscoredText(c), article));
				c = new TextContainer(tc.getTextOperator());
			}
		}
		return l;
	}

}
