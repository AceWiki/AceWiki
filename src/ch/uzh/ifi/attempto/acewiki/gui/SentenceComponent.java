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

package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Column;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.InconsistencyException;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SmallButton;

import com.google.common.collect.ImmutableSet;

/**
 * This class represents a sentence component consisting of a drop down menu and the sentence text.
 * 
 * @author Tobias Kuhn
 */
public class SentenceComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;

	private static final ImmutableSet<String> EDIT_ACTIONS = new ImmutableSet.Builder<String>()
			.add("acewiki_statementmenu_edit")
			.add("acewiki_statementmenu_addsent")
			.add("acewiki_statementmenu_addcomm")
			.add("acewiki_statementmenu_reassert")
			.add("acewiki_statementmenu_retract")
			.add("acewiki_statementmenu_delete")
			.build();
	
	private Sentence sentence;
	private Wiki wiki;
	private WikiPage hostPage;

	private Row sentenceRow = new Row();
	private StatementMenu dropDown;
	private RecalcIcon recalcIcon;
	private SmallButton alternativesButton;

	/**
	 * Creates a new sentence component. The host page is the page that contains the text row
	 * (which is not necessarily the owner page of the sentence).
	 * 
	 * @param sentence The sentence to be shown.
	 * @param hostPage The host page of the text row.
	 */
	public SentenceComponent(Sentence sentence, WikiPage hostPage) {
		this.sentence = sentence;
		this.hostPage = hostPage;
		this.wiki = hostPage.getWiki();
		this.recalcIcon = new RecalcIcon(wiki.getGUIText("acewiki_answer_recalctooltip"));
		update();
	}

	private void update() {
		if (sentence.isImmutable()) {
			dropDown = new StatementMenu(StatementMenu.INFERRED_TYPE, wiki, this);
		} else if (sentence instanceof Question) {
			dropDown = new StatementMenu(StatementMenu.QUESTION_TYPE, wiki, this);
		} else if (sentence.isReasonable()) {
			dropDown = new StatementMenu(StatementMenu.REASONING_TYPE, wiki, this);
		} else {
			dropDown = new StatementMenu(StatementMenu.NOREASONING_TYPE, wiki, this);
		}

		if (!wiki.isReadOnly() && !sentence.isImmutable()) {
			dropDown.addMenuEntry("acewiki_statementmenu_edit", "acewiki_statementmenu_editsenttooltip");
			if (wiki.getEngine().getReasoner() != null && sentence.isReasonable()) {
				if (sentence.isIntegrated()) {
					dropDown.addMenuEntry("acewiki_statementmenu_retract", "acewiki_statementmenu_retracttooltip");
				} else {
					dropDown.addMenuEntry("acewiki_statementmenu_reassert", "acewiki_statementmenu_reasserttooltip");
				}
			}
			dropDown.addMenuEntry("acewiki_statementmenu_delete", "acewiki_statementmenu_delsenttooltip");
		}

		if (sentence.getTextContainer(wiki.getLanguage()).size() > 1) {
			dropDown.addMenuEntry("acewiki_statementmenu_alternatives", "acewiki_statementmenu_alternativestooltip");
		}

		dropDown.addMenuEntry("acewiki_statementmenu_details", "acewiki_statementmenu_detailstooltip");

		if (wiki.isMultilingual()) {
			dropDown.addMenuEntry("acewiki_statementmenu_transl", "acewiki_statementmenu_transltooltip");
		}

		if (!wiki.isReadOnly() && hostPage instanceof ArticlePage) {
			dropDown.addMenuSeparator();
			dropDown.addMenuEntry("acewiki_statementmenu_addsent", "acewiki_statementmenu_addsenttooltip");
			if (wiki.isCommentingEnabled()) {
				dropDown.addMenuEntry("acewiki_statementmenu_addcomm", "acewiki_statementmenu_addcommtooltip");
			}
		}


		boolean isRed = !sentence.isIntegrated() && !sentence.isImmutable() && !(sentence instanceof Question);

		removeAll();
		sentenceRow.removeAll();
		sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(new TextRow(sentence.getTextElements(wiki.getLanguage()), wiki, isRed));
		int a = sentence.getTextContainer(wiki.getLanguage()).size();
		if (a > 1) {
			// The sentence has more than one alternative
			sentenceRow.add(new HSpace(10));
			String t = wiki.getGUIText("acewiki_statement_alternatives");
			sentenceRow.add(alternativesButton = new SmallButton("(" + a + " " + t + ")", this));
		}
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
		String c = e.getActionCommand();
		Object src = e.getSource();

		if (!wiki.isEditable() && EDIT_ACTIONS.contains(c)) {
			wiki.showLoginWindow();
			return;
		}

		if ("acewiki_statementmenu_edit".equals(c)) {
			log("dropdown: edit sentence:");
			OntologyElement el = sentence.getArticle().getOntologyElement();
			ArticlePage page = ArticlePage.create(el, wiki);
			wiki.showPage(page);
			wiki.showWindow(SentenceEditorHandler.generateEditWindow(sentence, page));
		} else if ("acewiki_statementmenu_addsent".equals(c)) {
			log("dropdown: add sentence");
			wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
					));
		} else if ("acewiki_statementmenu_addcomm".equals(c)) {
			log("dropdown: add comment");
			wiki.showWindow(CommentEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
					));
		} else if ("acewiki_statementmenu_delete".equals(c)) {
			log("dropdown: delete sentence:");
			wiki.showWindow(new MessageWindow(
					"acewiki_message_delstatementtitle",
					"acewiki_message_delsentence",
					null,
					this,
					"general_action_yes", "general_action_no"
					));
		} else if ("acewiki_statementmenu_reassert".equals(c)) {
			log("dropdown: reassert:");
			try {
				wiki.getOntology().reassert(sentence);
			} catch (InconsistencyException ex) {
				wiki.showWindow(new MessageWindow(
						"acewiki_message_conflicttitle",
						"acewiki_message_conflict",
						"general_action_ok"
						));
			}
			if (sentence.isIntegrated()) {
				update();
				hostPage.update();
			}
		} else if ("acewiki_statementmenu_retract".equals(c)) {
			log("dropdown: retract:");
			wiki.getOntology().retract(sentence);
			update();
			hostPage.update();
		} else if ("acewiki_statementmenu_details".equals(c)) {
			log("dropdown: details sentence:");
			wiki.showPage(new DetailsPage(wiki, sentence));
		} else if ("acewiki_statementmenu_transl".equals(c)) {
			log("dropdown: translations sentence:");
			wiki.showPage(new TranslationsPage(wiki, sentence));
		} else if ("acewiki_statementmenu_alternatives".equals(c) || src == alternativesButton) {
			log("dropdown: alternatives:");
			wiki.showWindow(new AlternativesWindow(sentence, wiki));
		} else if (src instanceof MessageWindow && "general_action_yes".equals(c)) {
			log("dropdown: delete confirmed:");

			wiki.enqueueStrongAsyncTask(
					wiki.getGUIText("acewiki_message_updatetitle"),
					wiki.getGUIText("acewiki_message_update"),
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

	private void log(String text) {
		if (text.endsWith(":")) {
			text += " " + sentence.getText(wiki.getEngine().getLanguages()[0]);
		}
		wiki.log("page", text);
	}

}
