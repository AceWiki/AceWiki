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

import nextapp.echo.app.Column;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.editor.CommentEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.editor.SentenceEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.page.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.LogicPage;
import ch.uzh.ifi.attempto.acewiki.gui.page.SentencePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.WikiPage;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;

/**
 * This class represents a sentence component consisting of a drop down menu and the sentence text.
 * 
 * @author Tobias Kuhn
 */
public class SentenceComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;
	
	private Sentence sentence;
	private Wiki wiki;
	private WikiPage hostPage;
	
	private Row sentenceRow = new Row();
	private StatementMenu dropDown;
	private RecalcIcon recalcIcon;
	
	/**
	 * Creates a new text row. The host page is the page that contains the text row (which is
	 * not necessarily the owner page of the sentence).
	 * 
	 * @param sentence The sentence to be shown.
	 * @param hostPage The host page of the text row.
	 */
	public SentenceComponent(Sentence sentence, WikiPage hostPage) {
		this.sentence = sentence;
		this.hostPage = hostPage;
		this.wiki = hostPage.getWiki();
		this.recalcIcon = new RecalcIcon("The answer to this question is being updated.");
		update();
	}
	
	private void update() {
		if (sentence.isReadOnly()) {
			dropDown = new StatementMenu(StatementMenu.INFERRED_TYPE, wiki, this);
		} else if (sentence instanceof Question) {
			dropDown = new StatementMenu(StatementMenu.QUESTION_TYPE, wiki, this);
		} else if (sentence.isReasonerParticipant()) {
			dropDown = new StatementMenu(StatementMenu.REASONING_TYPE, wiki, this);
		} else {
			dropDown = new StatementMenu(StatementMenu.NOREASONING_TYPE, wiki, this);
		}
		
		if (!wiki.isReadOnly() && !sentence.isReadOnly()) {
			dropDown.addMenuEntry("Edit...", "Edit this sentence");
			if (sentence.isReasonerParticipant()) {
				if (sentence.isIntegrated()) {
					dropDown.addMenuEntry("Retract", "Retract this sentence from the knowledge base");
				} else {
					dropDown.addMenuEntry("Reassert", "Reassert this sentence in the knowledge base");
				}
			}
			dropDown.addMenuEntry("Delete", "Delete this sentence from the article");
			dropDown.addMenuSeparator();
		}
		
		if (!wiki.isReadOnly() && hostPage instanceof ArticlePage) {
			dropDown.addMenuEntry("Add Sentence...", "Add a new sentence here");
			dropDown.addMenuEntry("Add Comment...", "Add a new comment here");
			dropDown.addMenuSeparator();
		}
		
		dropDown.addMenuEntry("Details", "Show the details of this sentence");
		dropDown.addMenuEntry("Logic", "Show the logic of this sentence");
		
		boolean isRed = !sentence.isIntegrated() && !sentence.isReadOnly() && !(sentence instanceof Question);
		
		removeAll();
		sentenceRow.removeAll();
		sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(new TextRow(sentence.getTextElements(), wiki, isRed));
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(recalcIcon);
		recalcIcon.setVisible(false);
		sentenceRow.add(new HSpace(5));
		add(sentenceRow);
		
		// Question Answering:
		if (sentence instanceof Question && hostPage instanceof ArticlePage) {
			add(new AnswerList(wiki, (Question) sentence, recalcIcon));
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Edit...")) {
			wiki.log("page", "dropdown: edit sentence: " + sentence.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				OntologyElement el = sentence.getArticle().getOntologyElement();
				ArticlePage page = ArticlePage.create(el, wiki);
				wiki.showPage(page);
				wiki.showWindow(SentenceEditorHandler.generateEditWindow(sentence, page));
			}
		} else if (e.getActionCommand().equals("Add Sentence...")) {
			wiki.log("page", "dropdown: add sentence");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
						sentence,
						(ArticlePage) hostPage
					));
			}
		} else if (e.getActionCommand().equals("Add Comment...")) {
			wiki.log("page", "dropdown: add comment");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(CommentEditorHandler.generateCreationWindow(
						sentence,
						(ArticlePage) hostPage
					));
			}
		} else if (e.getActionCommand().equals("Delete")) {
			wiki.log("page", "dropdown: delete sentence: " + sentence.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(new MessageWindow(
						"Delete",
						"Do you really want to delete this sentence?",
						null,
						this,
						"Yes",
						"No"
					));
			}
		} else if (e.getActionCommand().equals("Reassert")) {
			wiki.log("page", "dropdown: reassert: " + sentence.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				int success = wiki.getOntology().reassert(sentence);
				if (success == 1) {
					wiki.showWindow(new MessageWindow(
							"Conflict",
							"A sentence is in conflict with the current knowledge. For that reason, " +
								"it cannot be added to the knowledge base.",
							"OK"
						));
				} else if (success == 2) {
					wiki.showWindow(new MessageWindow(
							"Error",
							"A sentence could not be added to the knowledge base because the " +
								"knowledge base got too complex.",
							"OK"
						));
				}
				if (sentence.isIntegrated()) {
					update();
					hostPage.update();
				}
			}
		} else if (e.getActionCommand().equals("Retract")) {
			wiki.log("page", "dropdown: retract: " + sentence.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.getOntology().retract(sentence);
				update();
				hostPage.update();
			}
		} else if (e.getActionCommand().equals("Details")) {
			wiki.log("page", "dropdown: details sentence: " + sentence.getText());
			wiki.showPage(new SentencePage(wiki, sentence));
		} else if (e.getActionCommand().equals("Logic")) {
			wiki.log("page", "dropdown: logic sentence: " + sentence.getText());
			wiki.showPage(new LogicPage(wiki, sentence));
		} else if (e.getSource() instanceof MessageWindow && e.getActionCommand().equals("Yes")) {
			wiki.log("page", "dropdown: delete confirmed: " + sentence.getText());
			
			wiki.enqueueStrongAsyncTask(
				"Updating",
				"The sentence is being removed from the knowledge base...",
				new Task() {
					public void run() {
						sentence.getArticle().remove(sentence);
					}
					public void updateGUI() {
						wiki.update();
						wiki.refresh();
					}
				}
			);
		}
	}

}
