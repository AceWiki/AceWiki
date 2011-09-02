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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.chartparser.ParseTree;

/**
 * This factory class is used to generate different kind of statements (declarations, questions and
 * comments).
 * 
 * @author Tobias Kuhn
 */
public class StatementFactory {
	
	private Ontology ontology;
	
	StatementFactory(Ontology ontology) {
		this.ontology = ontology;
	}

	/**
	 * Creates a new comment. Comments must be part of an article.
	 * 
	 * @param text The comment text.
	 * @param article The article.
	 * @return The new comment.
	 */
	public Comment createComment(String text, Article article) {
		Comment c = new Comment(text);
		c.init(ontology, article);
		return c;
	}

	/**
	 * Creates a new sentence object with the given article.
	 * 
	 * @param text The sentence text.
	 * @param article The article.
	 * @return The new sentence object.
	 */
	public Sentence createSentence(String text, Article article) {
		Sentence s = ontology.getLanguageFactory().createSentence(text);
		s.init(ontology, article);
		return s;
	}

	/**
	 * Generates sentence objects out of a text container and a parse tree.
	 * 
	 * @param tc The text container.
	 * @param parseTree The parse tree.
	 * @param article The article of the sentences.
	 * @return A list of sentences.
	 */
	public List<Sentence> createSentences(TextContainer tc, ParseTree parseTree, Article article) {
		String s = ontology.getLanguageEngine().getSentenceCategory();
		List<ParseTree> subTrees = parseTree.getSubTrees(s);
		List<Sentence> l = new ArrayList<Sentence>();
		for (ParseTree pt : subTrees) {
			TextContainer c = tc.getSubTextContainer(pt.getStartPos(), pt.getEndPos());
			String t = AbstractSentence.getUnderscoredText(c, ontology.getTextOperator());
			l.add(createSentence(t, article));
		}
		return l;
	}
	
	/**
	 * Creates an assignement sentence.
	 * 
	 * @param ind The individual.
	 * @param conc The concept.
	 * @return A new assignement sentence.
	 */
	public Sentence createAssignmentSentence(Individual ind, Concept conc) {
		Sentence s = ontology.getLanguageFactory().createAssignmentSentence(ind, conc);
		s.init(ontology, null);
		return s;
	}
	
	/**
	 * Creates a hierarchy sentence.
	 * 
	 * @param subConc The sub-concept.
	 * @param superConc The super-concept.
	 * @return A new hierarchy sentence.
	 */
	public Sentence createHierarchySentence(Concept subConc, Concept superConc) {
		Sentence s = ontology.getLanguageFactory().createHierarchySentence(subConc, superConc);
		s.init(ontology, null);
		return s;
	}

}
