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

import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.editor.CommentEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.editor.SentenceEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.page.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.LogicPage;
import ch.uzh.ifi.attempto.acewiki.gui.page.SentencePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.WikiPage;
import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class represents a text row that consists of a drop down menu and an ACE text.
 * 
 * @author Tobias Kuhn
 */
public class TextRow extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;
	
	private Sentence sentence;
	private Wiki wiki;
	private WikiPage hostPage;
	
	private Row sentenceRow = new Row();
	private DropDownMenu dropDown;
	private RecalcIcon recalcIcon;
	
	/**
	 * Creates a new text row. The host page is the page that contains the text row (which is
	 * not necessarily the owner page of the sentence).
	 * 
	 * @param sentence The sentence to be shown.
	 * @param hostPage The host page of the text row.
	 */
	public TextRow(Sentence sentence, WikiPage hostPage) {
		this.sentence = sentence;
		this.hostPage = hostPage;
		this.wiki = hostPage.getWiki();
		this.recalcIcon = new RecalcIcon("The answer to this question is being updated.");
		update();
	}
	
	private void update() {
		if (sentence.isInferred()) {
			dropDown = new DropDownMenu(DropDownMenu.INFERRED_TYPE, this);
		} else if (sentence.isQuestion()) {
			dropDown = new DropDownMenu(DropDownMenu.QUESTION_TYPE, this);
		} else if (sentence.isReasonerParticipant()) {
			dropDown = new DropDownMenu(DropDownMenu.REASONING_TYPE, this);
		} else {
			dropDown = new DropDownMenu(DropDownMenu.NOREASONING_TYPE, this);
		}
		if (sentence.isQuestion() && "on".equals(wiki.getParameter("possibleAnswers"))) {
			if (sentence.areUncertainAnswersEnabled()) {
				dropDown.addMenuEntry("Show necessary Answers");
			} else {
				dropDown.addMenuEntry("Show possible Answers");
			}
			dropDown.addMenuSeparator();
		}
		if (!sentence.isIntegrated() && !sentence.isInferred()) {
			dropDown.addMenuEntry("Reassert");
			dropDown.addMenuSeparator();
		}
		if (!sentence.isInferred()) {
			dropDown.addMenuEntry("Edit...");
		}
		if (hostPage instanceof ArticlePage) {
			dropDown.addMenuEntry("Add Sentence...");
			dropDown.addMenuEntry("Add Comment...");
		}
		if (!sentence.isInferred()) {
			dropDown.addMenuEntry("Delete");
		}
		dropDown.addMenuSeparator();
		dropDown.addMenuEntry("Details");
		dropDown.addMenuEntry("Logic");
		
		Row r = new Row();
		Color color = Color.BLACK;
		boolean isRed = !sentence.isIntegrated() && !sentence.isInferred() && !sentence.isQuestion();
		if (isRed) {
			color = new Color(193, 0, 0);
		}
		for (TextElement e : sentence.getTextElements()) {
			if (!e.getText().matches("[.?]") && r.getComponentCount() > 0) {
				r.add(new HSpace());
			}
			if (e instanceof OntologyTextElement) {
				OntologyTextElement ote = (OntologyTextElement) e;
				OntologyElement oe = ote.getOntologyElement();
				if (oe instanceof Individual) {
					// Proper names with definite articles are handled differently: The "the" is
					// not a part of the link.
					// TODO: Probably, this should be done at a different place...
					Individual ind = (Individual) oe;
					int wn = ote.getWordNumber();
					
					if (ind.hasDefiniteArticle(wn)) {
						SolidLabel l = new SolidLabel(e.getText().substring(0, 3));
						l.setForeground(color);
						r.add(l);
						r.add(new HSpace());
						r.add(new WikiLink(oe, oe.getPrettyWord(wn + 1), wiki, isRed));
					} else {
						r.add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
					}
				} else {
					r.add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
				}
			} else {
				SolidLabel l = new SolidLabel(e.getText());
				l.setForeground(color);
				r.add(l);
			}
		}
		
		removeAll();
		sentenceRow.removeAll();
		sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(r);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(recalcIcon);
		recalcIcon.setVisible(false);
		sentenceRow.add(new HSpace(5));
		add(sentenceRow);
		
		// Question Answering:
		if (sentence.isQuestion() && hostPage instanceof ArticlePage) {
			// TODO: clean-up and document this ugly code.

			if (sentence.areUncertainAnswersEnabled()) {
				Column pCol = new Column();
				pCol.setInsets(new Insets(20, 0, 0, 0));
				pCol.add(new SolidLabel("possibly:", Font.ITALIC, 10));
				add(pCol);
			}
			
			final Column answerColumn = new Column();
			answerColumn.setInsets(new Insets(20, 0, 0, 0));
			add(answerColumn);
			
			Column cachedAnswerCol = new Column();
			List<OntologyElement> answer = sentence.getCachedAnswer();
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
			
			if (sentence.isAnswerCached()) {
				
				answerColumn.add(cachedAnswerCol);
				
			} else {
				recalcIcon.setVisible(true);
				answerColumn.add(cachedAnswerCol);
				wiki.enqueueWeakAsyncTask(new Task() {
					
					private Column column;
					
					public void run() {
						column = new Column();
						List<OntologyElement> answer = sentence.getAnswer();
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
						recalcIcon.setVisible(false);
					}
					
				});
			}
			
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Edit...")) {
			wiki.log("page", "dropdown: edit sentence: " + sentence.getText());
			ArticlePage page = ArticlePage.create(sentence.getOwner(), wiki);
			wiki.showPage(page);
			wiki.showWindow(SentenceEditorHandler.generateEditWindow(sentence, page));
		} else if (e.getActionCommand().equals("Add Sentence...")) {
			wiki.log("page", "dropdown: add sentence");
			wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
				));
		} else if (e.getActionCommand().equals("Add Comment...")) {
			wiki.log("page", "dropdown: add comment");
			wiki.showWindow(CommentEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
				));
		} else if (e.getActionCommand().equals("Delete")) {
			wiki.log("page", "dropdown: delete sentence: " + sentence.getText());
			wiki.showWindow(new MessageWindow(
					"Delete",
					"Do you really want to delete this sentence?",
					null,
					this,
					"Yes",
					"No"
				));
		} else if (e.getActionCommand().equals("Show necessary Answers")) {
			sentence.setUncertainAnswersEnabled(false);
			update();
		} else if (e.getActionCommand().equals("Show possible Answers")) {
			sentence.setUncertainAnswersEnabled(true);
			update();
		} else if (e.getActionCommand().equals("Reassert")) {
			int success = sentence.reassert();
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
						sentence.getOwner().remove(sentence);
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
