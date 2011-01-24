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

package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.Collections;
import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Question;
import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class shows a list of ontology elements as the answer for a given question. If the answer
 * has to be recalculated, this recalculation is done asynchronously.
 * 
 * @author Tobias Kuhn
 */
class AnswerList extends Column {
	
	private static final long serialVersionUID = -2489300900348078442L;
	
	private Wiki wiki;
	private Question question;
	private RecalcIcon recalcIcon;
	
	/**
	 * Creates a new answer list for a given question.
	 * 
	 * @param wiki The wiki object.
	 * @param question The question for which the answers should be shown.
	 * @param recalcIcon The recalculation icon.
	 */
	public AnswerList(Wiki wiki, Question question, RecalcIcon recalcIcon) {
		this.wiki = wiki;
		this.question = question;
		this.recalcIcon = recalcIcon;
		
		setInsets(new Insets(20, 0, 0, 0));
		
		updateAnswers();
	}
	
	/**
	 * Recalculates the answers and updates the answer list.
	 */
	public void updateAnswers() {
		clear();
		
		Column cachedAnswerCol = new Column();
		addAnswerToColumn(question.getCachedAnswer(), cachedAnswerCol);
		
		if (question.isAnswerCached()) {
			// Cached answer is up-to-date: no recalculation
			add(cachedAnswerCol);
		} else {
			// Answer is not cached or not up-to-date: recalculation
			recalcIcon.setVisible(true);
			add(cachedAnswerCol);
			wiki.enqueueWeakAsyncTask(new Task() {
				
				private Column column;
				
				public void run() {
					column = new Column();
					addAnswerToColumn(question.getAnswer(), column);
				}
				
				public void updateGUI() {
					clear();
					add(column);
					recalcIcon.setVisible(false);
				}
				
			});
		}
	}
	
	private void clear() {
		removeAll();
		
		// Experimental "possible answers" feature:
		if (question.isShowPossibleAnswersEnabled()) {
			add(new SolidLabel("possibly:", Font.ITALIC, 10));
		}
	}
	
	/**
	 * This method adds the graphical components for the given answer to the given column.
	 * 
	 * @param answer The answer as a list of ontology elements.
	 * @param column The column to which the answer should be added.
	 */
	private void addAnswerToColumn(List<OntologyElement> answer, Column column) {
		if (answer == null) {
			// The answer is still being calculated, or an error occurred
			column.add(new SolidLabel("...", Font.ITALIC, 10));
		} else if (answer.size() > 0) {
			// Non-empty answer
			Collections.sort(answer);
			for (OntologyElement oe : answer) {
				Row answerRow = new Row();
				if (oe instanceof NounConcept) {
					// Nouns as answer are preceded by the article "a" or "an"
					boolean an = ACEUtils.useIndefiniteArticleAn(oe.getWord());
					answerRow.add(new ListItem(
							new SolidLabel(an ? "an" : "a"),
							new HSpace(),
							new WikiLink(oe, wiki)
						));
				} else {
					// Proper names as answer
					answerRow.add(new ListItem(new WikiLink(oe, wiki)));
				}
				column.add(answerRow);
			}
		} else {
			// Empty answer
			column.add(new SolidLabel("(no answer found)", Font.ITALIC, 10));
		}
		column.add(new VSpace(4));
	}

}
