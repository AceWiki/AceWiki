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

package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.Collections;
import java.util.List;

import nextapp.echo2.app.Column;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
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
	
	/**
	 * Creates a new answer list.
	 * 
	 * @param textRow The text row for the answer list.
	 */
	public AnswerList(final TextRow textRow) {
		
		// TODO: clean-up and document this ugly code.
		
		final Wiki wiki = textRow.getWiki();
		final Question question;
		
		if (textRow.getSentence() instanceof Question) {
			question = (Question) textRow.getSentence();
		} else {
			return;
		}
		
		if (question.areUncertainAnswersEnabled()) {
			Column pCol = new Column();
			pCol.setInsets(new Insets(20, 0, 0, 0));
			pCol.add(new SolidLabel("possibly:", Font.ITALIC, 10));
			add(pCol);
		}
		
		final Column answerColumn = new Column();
		answerColumn.setInsets(new Insets(20, 0, 0, 0));
		add(answerColumn);
		
		Column cachedAnswerCol = new Column();
		List<OntologyElement> answer = question.getCachedAnswer();
		if (answer == null) {
			cachedAnswerCol.add(new SolidLabel("...", Font.ITALIC, 10));
		} else if (answer.size() > 0) {
			Collections.sort(answer);
			for (OntologyElement oe : answer) {
				Row answerRow = new Row();
				if (oe instanceof NounConcept) {
					String t = (ACEUtils.useIndefiniteArticleAn(oe.getWord()) ? "an" : "a");
					answerRow.add(new ListItem(
							new SolidLabel(t),
							new HSpace(),
							new WikiLink(oe, wiki)
						));
				} else {
					answerRow.add(new ListItem(new WikiLink(oe, wiki)));
				}
				cachedAnswerCol.add(answerRow);
			}
		} else {
			cachedAnswerCol.add(new SolidLabel("(no answer found)", Font.ITALIC, 10));
		}
		cachedAnswerCol.add(new VSpace(4));
		
		if (question.isAnswerCached()) {
			
			answerColumn.add(cachedAnswerCol);
			
		} else {
			textRow.setRecalcIconVisible(true);
			answerColumn.add(cachedAnswerCol);
			wiki.enqueueWeakAsyncTask(new Task() {
				
				private Column column;
				
				public void run() {
					column = new Column();
					List<OntologyElement> answer = question.getAnswer();
					if (answer == null) {
						column.add(new SolidLabel("(error)", Font.ITALIC, 10));
					} else if (answer.size() > 0) {
						Collections.sort(answer);
						for (OntologyElement oe : answer) {
							Row answerRow = new Row();
							if (oe instanceof NounConcept) {
								String t = "a";
								if (ACEUtils.useIndefiniteArticleAn(oe.getWord())) {
									t = "an";
								}
								answerRow.add(new ListItem(
										new SolidLabel(t),
										new HSpace(),
										new WikiLink(oe, wiki)
									));
							} else {
								answerRow.add(new ListItem(new WikiLink(oe, wiki)));
							}
							column.add(answerRow);
						}
					} else {
						column.add(new SolidLabel("(no answer found)", Font.ITALIC, 10));
					}
					column.add(new VSpace(4));
				}
				
				public void updateGUI() {
					answerColumn.removeAll();
					answerColumn.add(column);
					textRow.setRecalcIconVisible(false);
				}
				
			});
		}
		
	}

}
