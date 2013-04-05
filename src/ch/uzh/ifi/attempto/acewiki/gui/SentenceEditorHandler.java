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

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.InconsistencyException;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.preditor.PreditorWindow;

/**
 * This class manages the predictive editor. It creates the editor window and handles its
 * responses.
 * 
 * @author Tobias Kuhn
 */
public class SentenceEditorHandler implements ActionListener {
	
	private static final long serialVersionUID = -2083910385095284075L;
	
	private PreditorWindow editorWindow;
	private MessageWindow messageWindow;
	private ArticlePage page;
	private Wiki wiki;
	private boolean edit;
	private Statement statement;
	private List<Sentence> newSentences = new ArrayList<Sentence>();
	private int checked = 0;
	private SentenceSuggestion suggestion;
	
	private SentenceEditorHandler(Statement statement, ArticlePage page, boolean edit) {
		this.page = page;
		this.wiki = page.getWiki();
		this.edit = edit;
		this.statement = statement;
		AceWikiMenuCreator menuCreator = new AceWikiMenuCreator(
				wiki,
				page.getOntologyElement(),
				this
			);
		LanguageHandler lh = wiki.getLanguageHandler();
		editorWindow = new PreditorWindow(wiki.getGUIText("acewiki_preditor_title"), lh.getPredictiveParser());
		editorWindow.setMenuCreator(menuCreator);
		// TODO(uvictor): remove logger
		editorWindow.setLogger(wiki.getLogger());
		editorWindow.setLoggerContext(wiki.getLoggerContext());
		editorWindow.addActionListener(this);
		editorWindow.setTextOperator(lh.getTextOperator());
		
		if (edit) {
			String t = statement.getText(wiki.getLanguage());
			editorWindow.addText(t + " ");
		}
	}
	
	/**
	 * Generates a new predictive editor window for the creation of a new sentence.
	 * 
	 * @param followingStatement The statement in front of which the new sentences should be added,
	 *   or null if the sentences should be added to the end of the article.
	 * @param page The host page into which the sentence should be added.
	 * @return A new preditor window.
	 */
	public static PreditorWindow generateCreationWindow(Statement followingStatement,
			ArticlePage page) {
		SentenceEditorHandler h = new SentenceEditorHandler(followingStatement, page, false);
		return h.getWindow();
	}
	
	/**
	 * Generates a new predictive editor window for editing an existing sentence.
	 * 
	 * @param sentence The sentence that should be edited.
	 * @param page The host page which contains the sentence to be edited.
	 * @return A new preditor window.
	 */
	public static PreditorWindow generateEditWindow(Sentence sentence, ArticlePage page) {
		SentenceEditorHandler h = new SentenceEditorHandler(sentence, page, true);
		return h.getWindow();
	}
	
	private PreditorWindow getWindow() {
		return editorWindow;
	}

	public void actionPerformed(ActionEvent e) {
		Object src = e.getSource();
		String c = e.getActionCommand();
		if (src == editorWindow && c.matches("OK|Enter")) {
			Ontology o = wiki.getOntology();
			LanguageHandler lh = wiki.getLanguageHandler();
			
			for (String t : lh.getEditorController().getAutocompleteTokens()) {
				if (editorWindow.isPossibleNextToken(t)) {
					editorWindow.addTextElement(lh.getTextOperator().createTextElement(t));
					break;
				}
			}
			
			PredictiveParser parser = editorWindow.getPredictiveParser();
			if (parser.getTokenCount() == 0) {
				wiki.removeWindow(editorWindow);
			} else if (parser.isComplete()) {
				newSentences = o.getStatementFactory().extractSentences(
						wiki.getLanguageHandler(),
						editorWindow.getTextContainer(),
						parser,
						page.getArticle()
					);
				checkSentence();
			} else if (c.equals("OK")) {
				wiki.log("edit", "error: unfinished sentences");
				MessageWindow mw = new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_unfinishedsent",
						editorWindow,
						"general_action_ok"
					);
				page.getWiki().showWindow(mw);
			}
		} else if (src == editorWindow && c.matches("Cancel|Close|Escape")) {
			wiki.removeWindow(editorWindow);
		} else if (src == messageWindow && c.equals("general_action_close")) {
			checked = 0;
		} else if (src == messageWindow && suggestion != null) {
			Sentence s = suggestion.getSentence(c);
			if (s != newSentences.get(checked)) {
				newSentences.remove(checked);
				newSentences.add(checked, s);
			}
			checked++;
			checkSentence();
		} else if (src instanceof TextElement) {
			editorWindow.addTextElement((TextElement) src);
		}
	}
	
	private void checkSentence() {
		if (checked >= newSentences.size()) {
			assertSentences();
		} else {
			suggestion = wiki.getLanguageHandler().getSuggestion(newSentences.get(checked));
			if (suggestion != null) {
				messageWindow = new MessageWindow(
						"acewiki_message_suggestiontitle",
						suggestion.getMessage(),
						editorWindow,
						this,
						suggestion.getOptions()
					);
				wiki.showWindow(messageWindow);
			} else {
				checked++;
				checkSentence();
			}
		}
	}
	
	private void assertSentences() {
		final TextContainer textContainer = editorWindow.getTextContainer();
		final Article a = page.getArticle();
		
		Task task = new Task() {
			
			boolean inconsistent = false;
			
			public void run() {
				try {
					if (edit) {
						wiki.log("edit", "sentence updated: " + textContainer.getText());
						List<Statement> l = new ArrayList<Statement>(newSentences);
						a.edit(statement, l);
						wiki.updateStatement(statement, l);
					} else {
						wiki.log("edit", "sentence created: " + textContainer.getText());
						a.add(statement, new ArrayList<Statement>(newSentences));
					}
				} catch (InconsistencyException ex) {
					inconsistent = true;
				}
			}
			
			public void updateGUI() {
				page.update();
				if (inconsistent) {
					wiki.showWindow(
						new MessageWindow(
							"acewiki_message_conflicttitle",
							"acewiki_message_conflict",
							"general_action_ok"
						)
					);
				}
				if (page != null) {
					page.update();
					page.getWiki().update();
				}
			}
			
		};
		
		wiki.enqueueStrongAsyncTask(
				"acewiki_message_updatetitle",
				"acewiki_message_update",
				task
			);
		
		wiki.removeWindow(editorWindow);
	}

}
