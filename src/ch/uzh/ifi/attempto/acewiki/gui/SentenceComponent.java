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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.InconsistencyException;
import ch.uzh.ifi.attempto.acewiki.core.MultilingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.acewiki.gfservice.GFDeclaration;
import ch.uzh.ifi.attempto.acewiki.gfservice.GFEngine;
import ch.uzh.ifi.attempto.acewiki.gfservice.GFGrammar;
import ch.uzh.ifi.attempto.acewiki.gfservice.TreeSet;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;

/**
 * This class represents a sentence component consisting of a drop down menu and the sentence text.
 * 
 * @author Tobias Kuhn
 */
public class SentenceComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;

	private static final String ACTION_SHOW_TRANSLATIONS = "Show Translations";

	// TODO: this is GF-specific, in normal AceWiki these actions do not make sense
	private static final SentenceAction actionGenSentence = new SentenceAction(
			"Generate Sentence",
			"Generate a new sentence here",
			"",
			"A new sentence is being randomly generated...");

	private static final SentenceAction actionReanalyze = new SentenceAction(
			"Reanalyze",
			"Reanalyze this sentence",
			"Do you really want to reparse this sentence?");

	private static final ImmutableSet<String> EDIT_ACTIONS = new ImmutableSet.Builder<String>()
			.add("acewiki_statementmenu_edit")
			.add("acewiki_statementmenu_addsent")
			.add("acewiki_statementmenu_addcomm")
			.add("acewiki_statementmenu_reassert")
			.add("acewiki_statementmenu_retract")
			.add("acewiki_statementmenu_delete")
			.add(actionGenSentence.getTitle())
			.add(actionReanalyze.getTitle())
			.build();


	private Sentence sentence;
	private Wiki wiki;
	private WikiPage hostPage;

	private Row sentenceRow = new Row();
	private StatementMenu dropDown;
	private RecalcIcon recalcIcon;

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
			if (sentence.isReasonable()) {
				if (sentence.isIntegrated()) {
					dropDown.addMenuEntry("acewiki_statementmenu_retract", "acewiki_statementmenu_retracttooltip");
				} else {
					dropDown.addMenuEntry("acewiki_statementmenu_reassert", "acewiki_statementmenu_reasserttooltip");
				}
			}
			dropDown.addMenuEntry("acewiki_statementmenu_delete", "acewiki_statementmenu_delsenttooltip");
			dropDown.addMenuEntry(actionReanalyze.getTitle(), actionReanalyze.getDesc());
		}

		dropDown.addMenuEntry("acewiki_statementmenu_details", "acewiki_statementmenu_detailstooltip");

		if (sentence instanceof MultilingualSentence) {
			dropDown.addMenuEntry(ACTION_SHOW_TRANSLATIONS, "Show the translations of this sentence");
			dropDown.addMenuEntry(actionGenSentence.getTitle(), actionGenSentence.getDesc());
		}

		if (!wiki.isReadOnly() && hostPage instanceof ArticlePage) {
			dropDown.addMenuSeparator();
			dropDown.addMenuEntry("acewiki_statementmenu_addsent", "acewiki_statementmenu_addsenttooltip");
			dropDown.addMenuEntry("acewiki_statementmenu_addcomm", "acewiki_statementmenu_addcommtooltip");
		}


		boolean isRed = !sentence.isIntegrated() && !sentence.isImmutable() && !(sentence instanceof Question);

		removeAll();
		sentenceRow.removeAll();
		sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(new TextRow(sentence.getTextContainerSet(wiki.getLanguage()), wiki, isRed));
		sentenceRow.add(new HSpace(5));
		// If the sentence is ambiguous then show the number of trees
		if (sentence.getNumberOfParseTrees() > 1) {
			sentenceRow.add(new Label("(" + sentence.getNumberOfParseTrees() + ")", Font.BOLD));
		}
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(recalcIcon);
		recalcIcon.setVisible(false);
		sentenceRow.add(new HSpace(5));

		// Move to triangle to the top left of the row
		RowLayoutData rowLayoutData = new RowLayoutData();
		rowLayoutData.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		dropDown.setLayoutData(rowLayoutData);

		add(sentenceRow);

		// Question Answering:
		if (sentence instanceof Question && hostPage instanceof ArticlePage) {
			add(new AnswerList(wiki, (Question) sentence, recalcIcon));
		}
	}

	public void actionPerformed(ActionEvent e) {
		String actionCommand = e.getActionCommand();

		if (!wiki.isEditable() && EDIT_ACTIONS.contains(actionCommand)) {
			wiki.showLoginWindow();
			return;
		}

		if ("acewiki_statementmenu_edit".equals(actionCommand)) {
			log("dropdown: edit sentence:");
			OntologyElement el = sentence.getArticle().getOntologyElement();
			ArticlePage page = ArticlePage.create(el, wiki);
			wiki.showPage(page);
			wiki.showWindow(SentenceEditorHandler.generateEditWindow(sentence, page));
		} else if ("acewiki_statementmenu_addsent".equals(actionCommand)) {
			log("dropdown: add sentence");
			wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
					));
		} else if ("acewiki_statementmenu_addcomm".equals(actionCommand)) {
			log("dropdown: add comment");
			wiki.showWindow(CommentEditorHandler.generateCreationWindow(
					sentence,
					(ArticlePage) hostPage
					));
		} else if ("acewiki_statementmenu_delete".equals(actionCommand)) {
			log("dropdown: delete sentence:");
			wiki.showWindow(new MessageWindow(
					"acewiki_message_delstatementtitle",
					"acewiki_message_delsentence",
					null,
					this,
					"general_action_yes", "general_action_no"
				));
		} else if (actionGenSentence.hasTitle(actionCommand)) {
			final AceWikiEngine engine = wiki.getEngine();
			if (engine instanceof GFEngine) {
				actionGenSentence.performAction(wiki, new Executable() {

					@Override
					public void execute() {
						GFDeclaration gfDecl = new GFDeclaration(wiki.getLanguage(), ((GFEngine) engine).getGFGrammar());
						Article article = sentence.getArticle();
						// TODO: understand better why the init-call is needed
						gfDecl.init(article.getOntology(), article);
						article.add(sentence, ImmutableList.of((Statement) gfDecl));
					}

				});
			}
		}
		else if (actionReanalyze.hasTitle(actionCommand)) {
			final AceWikiEngine engine = wiki.getEngine();
			if (engine instanceof GFEngine) {
				log("dropdown: reparse sentence:");

				actionReanalyze.performAction(wiki, new Executable() {

					@Override
					public void execute() {
						TreeSet parseState = new TreeSet(sentence.getParseTrees());
						GFGrammar grammar = ((GFEngine) engine).getGFGrammar();
						GFDeclaration gfDecl = new GFDeclaration(parseState, wiki.getLanguage(), grammar);
						Article article = sentence.getArticle();
						// TODO: understand better why the init-call is needed
						gfDecl.init(article.getOntology(), article);
						article.edit(sentence, ImmutableList.of((Statement) gfDecl));
					}

				});
			}
		} else if ("acewiki_statementmenu_reassert".equals(actionCommand)) {
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
		} else if ("acewiki_statementmenu_retract".equals(actionCommand)) {
			log("dropdown: retract:");
			wiki.getOntology().retract(sentence);
			update();
			hostPage.update();
		} else if ("acewiki_statementmenu_details".equals(actionCommand)) {
			log("dropdown: details sentence:");
			wiki.showPage(new SentencePage(wiki, sentence));
		} else if (ACTION_SHOW_TRANSLATIONS.equals(actionCommand)) {
			log("dropdown: translations sentence:");
			wiki.showPage(new TranslationsPage(wiki, (MultilingualSentence) sentence));
		} else if (e.getSource() instanceof MessageWindow && actionCommand.equals("general_action_yes")) {
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
