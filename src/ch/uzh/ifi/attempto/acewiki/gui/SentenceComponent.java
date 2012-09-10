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

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
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
import ch.uzh.ifi.attempto.acewiki.gfservice.ParseState;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.MultipleChoiceWindow;

/**
 * This class represents a sentence component consisting of a drop down menu and the sentence text.
 * 
 * @author Tobias Kuhn
 */
public class SentenceComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;

	// TODO: move this to a central place (and allow it to be localized)
	private static final String YES = "Yes";

	private static final String ACTION_EDIT = "Edit...";
	private static final String ACTION_ADD_SENTENCE = "Add Sentence...";
	private static final String ACTION_ADD_COMMENT = "Add Comment...";
	private static final String ACTION_PRUNE = "Prune";
	private static final String ACTION_REASSERT = "Reassert";
	private static final String ACTION_RETRACT = "Retract";
	private static final String ACTION_SHOW_DETAILS = "Show Details";
	private static final String ACTION_SHOW_TRANSLATIONS = "Show Translations";

	private static final SentenceAction actionDelete = new SentenceAction("Delete",
			"Delete this sentence from the article",
			"Do you really want to delete this sentence?",
			"The sentence is being removed from the knowledge base...");

	// TODO: this is GF-specific, in normal AceWiki reparsing does not make sense
	private static final SentenceAction actionReparse = new SentenceAction("Reparse",
			"Reparse this sentence", "Do you really want to reparse this sentence?");

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
		this.recalcIcon = new RecalcIcon("The answer to this question is being updated.");
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
			dropDown.addMenuEntry(ACTION_EDIT, "Edit this sentence");
			if (sentence.isReasonable()) {
				if (sentence.isIntegrated()) {
					dropDown.addMenuEntry(ACTION_RETRACT, "Retract this sentence from the knowledge base");
				} else {
					dropDown.addMenuEntry(ACTION_REASSERT, "Reassert this sentence into the knowledge base");
				}
			}
			if (sentence.getNumberOfParseTrees() > 1) {
				dropDown.addMenuEntry(ACTION_PRUNE, "Remove some of the " + sentence.getNumberOfParseTrees() + " trees");
			}
			dropDown.addMenuEntry(actionDelete.getTitle(), actionDelete.getDesc());
			dropDown.addMenuEntry(actionReparse.getTitle(), actionReparse.getDesc());
		}

		dropDown.addMenuEntry(ACTION_SHOW_DETAILS, "Show the details of this sentence");

		if (sentence instanceof MultilingualSentence) {
			dropDown.addMenuEntry(ACTION_SHOW_TRANSLATIONS, "Show the translations of this sentence");
		}

		if (!wiki.isReadOnly() && hostPage instanceof ArticlePage) {
			dropDown.addMenuSeparator();
			dropDown.addMenuEntry(ACTION_ADD_SENTENCE, "Add a new sentence here");
			dropDown.addMenuEntry(ACTION_ADD_COMMENT, "Add a new comment here");
		}


		boolean isRed = !sentence.isIntegrated() && !sentence.isImmutable() && !(sentence instanceof Question);

		removeAll();
		sentenceRow.removeAll();
		sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(new TextRow(sentence.getTextContainerSet(wiki.getLanguage()), wiki, isRed));
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
		if (ACTION_EDIT.equals(actionCommand)) {
			log("dropdown: edit sentence:");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				OntologyElement el = sentence.getArticle().getOntologyElement();
				ArticlePage page = ArticlePage.create(el, wiki);
				wiki.showPage(page);
				wiki.showWindow(SentenceEditorHandler.generateEditWindow(sentence, page));
			}
		} else if (ACTION_ADD_SENTENCE.equals(actionCommand)) {
			log("dropdown: add sentence");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
						sentence,
						(ArticlePage) hostPage
						));
			}
		} else if (ACTION_ADD_COMMENT.equals(actionCommand)) {
			log("dropdown: add comment");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(CommentEditorHandler.generateCreationWindow(
						sentence,
						(ArticlePage) hostPage
						));
			}
		} else if (ACTION_PRUNE.equals(actionCommand)) {
			log("dropdown: prune sentence:");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(new MultipleChoiceWindow(
						ACTION_PRUNE,
						"Which meanings do you want to keep?\n[TODO: show lins instead of trees]",
						sentence.getParseTrees(),
						null,
						new ActionListener() {

							private static final long serialVersionUID = 7820078327036231367L;

							@Override
							public void actionPerformed(ActionEvent arg0) {
								wiki.showWindow(new MessageWindow(
										"TEST",
										arg0.getActionCommand(),
										null,
										this
										));
							}							
						}
						));
			}
		} else if (actionDelete.hasTitle(actionCommand)) {
			log("dropdown: delete sentence:");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(actionDelete.getYesNoDialog(null, this));
			}
		} else if (actionReparse.hasTitle(actionCommand)) {
			final AceWikiEngine engine = wiki.getEngine();
			if (engine instanceof GFEngine) {
				log("dropdown: reparse sentence:");
				if (!wiki.isEditable()) {
					wiki.showLoginWindow();
				} else {
					actionReparse.performAction(wiki, new Executable() {

						@Override
						public void execute() {
							ParseState parseState = new ParseState(sentence.getParseTrees());
							GFGrammar grammar = ((GFEngine) engine).getGFGrammar();
							GFDeclaration gfDecl = new GFDeclaration(parseState, grammar);
							Article article = sentence.getArticle();
							// TODO: understand better why the init-call is needed
							gfDecl.init(article.getOntology(), article);
							article.edit(sentence, ImmutableList.of((Statement) gfDecl));
						}

					});
				}
			}
		} else if (ACTION_REASSERT.equals(actionCommand)) {
			log("dropdown: reassert:");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				try {
					wiki.getOntology().reassert(sentence);
				} catch (InconsistencyException ex) {
					wiki.showWindow(new MessageWindow(
							"Conflict",
							"The sentence is in conflict with the current knowledge. For that " +
									"reason, it cannot be added to the knowledge base.",
									"OK"
							));
				}
				if (sentence.isIntegrated()) {
					update();
					hostPage.update();
				}
			}
		} else if (ACTION_RETRACT.equals(actionCommand)) {
			log("dropdown: retract:");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.getOntology().retract(sentence);
				update();
				hostPage.update();
			}
		} else if (ACTION_SHOW_DETAILS.equals(actionCommand)) {
			log("dropdown: details sentence:");
			wiki.showPage(new SentencePage(wiki, sentence));
		} else if (ACTION_SHOW_TRANSLATIONS.equals(actionCommand)) {
			log("dropdown: translations sentence:");
			wiki.showPage(new TranslationsPage(wiki, (MultilingualSentence) sentence));
		} else if (e.getSource() instanceof MessageWindow && actionCommand.equals(YES)) {

			// TODO: move this code closer to the respective actions
			MessageWindow window = (MessageWindow) e.getSource();

			if (actionDelete.hasTitle(window.getTitle())) {
				log("dropdown: delete confirmed:");

				actionDelete.performAction(wiki, new Executable() {
					@Override
					public void execute() {
						sentence.getArticle().remove(sentence);
					}
				});
			}
		}
	}

	private void log(String text) {
		if (text.endsWith(":")) {
			text += " " + sentence.getText(wiki.getEngine().getLanguages()[0]);
		}
		wiki.log("page", text);
	}

}
