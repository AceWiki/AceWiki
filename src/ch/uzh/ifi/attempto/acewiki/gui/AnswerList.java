// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AnswerElement;
import ch.uzh.ifi.attempto.acewiki.core.CachingReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.base.TextContainer;
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
		removeAll();
		
		final CachingReasoner cr = wiki.getOntology().getReasoner();
		Column cachedAnswerCol = new Column();
		List<AnswerElement> cachedAnswer = cr.getCachedAnswer(question);
		addAnswerToColumn(cachedAnswer, cachedAnswerCol);
		add(cachedAnswerCol);
		
		if (cachedAnswer == null || !cr.isCachedAnswerUpToDate(question)) {
			// Answer is not cached or not up-to-date: recalculation
			recalcIcon.setVisible(true);
			wiki.enqueueWeakAsyncTask(new Task() {
				
				private Column column;
				
				public void run() {
					column = new Column();
					addAnswerToColumn(cr.getAnswer(question), column);
				}
				
				public void updateGUI() {
					removeAll();
					add(column);
					recalcIcon.setVisible(false);
				}
				
			});
		}
	}
	
	/**
	 * This method adds the graphical components for the given answer to the given column.
	 * 
	 * @param answer The answer as a list of ontology elements.
	 * @param column The column to which the answer should be added.
	 */
	private void addAnswerToColumn(List<AnswerElement> answer, Column column) {
		if (answer == null) {
			// The answer is still being calculated, or an error occurred
			column.add(new SolidLabel("...", Font.ITALIC, 10));
		} else if (answer.size() > 0) {
			// Non-empty answer
			for (AnswerElement ae : answer) {
				TextContainer tc = ae.getAnswerText();
				column.add(new ListItem(new TextRow(tc.getTextElements(), wiki)));
			}
		} else {
			// Empty answer
			column.add(new SolidLabel("(no answer found)", Font.ITALIC, 10));
		}
		column.add(new VSpace(4));
	}

}
