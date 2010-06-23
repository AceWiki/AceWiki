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

package ch.uzh.ifi.attempto.acewiki.gui.editor;

import java.util.ArrayList;
import java.util.List;

import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Statement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.StatementFactory;
import ch.uzh.ifi.attempto.acewiki.gui.page.ArticlePage;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.preditor.PreditorWindow;
import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

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
		editorWindow = new PreditorWindow(
				"Sentence Editor",
				wiki.getGrammar(),
				"text",
				menuCreator
			);
		editorWindow.setLogger(wiki.getLogger());
		editorWindow.addActionListener(this);
		editorWindow.setContextChecker(Sentence.contextChecker);
		
		if (edit) {
			String t = ((Sentence) statement).getPrettyText();
			// remove the last element (i.e. the period '.' or question mark '?'):
			editorWindow.addText(t.substring(0, t.length()-1) + " ");
		}
	}
	
	/**
	 * Generates a new preditive editor window for the creation of a new sentence.
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
	 * Generates a new preditive editor window for editing an existing sentence.
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
		if (e.getSource() == editorWindow && e.getActionCommand().equals("OK")) {
			TextContainer textContainer = editorWindow.getTextContainer();
			
			List<TextElement> finalElements = editorWindow.getPossibleNextTokens(".", "?");
			if (!finalElements.isEmpty()) textContainer.addElement(finalElements.get(0));
			
			List<TextElement> l = textContainer.getTextElements();
			if (l.isEmpty() || l.get(l.size() - 1).getText().matches("[.?]")) {
				newSentences = StatementFactory.createSentences(
						textContainer,
						page.getOntologyElement()
					);
				checkSentence();
			} else {
				wiki.log("edit", "error: unfinished sentences");
				MessageWindow mw = new MessageWindow(
						"Error",
						"There are unfinished sentences.",
						editorWindow,
						"OK"
					);
				page.getWiki().showWindow(mw);
			}
		} else if (e.getSource() == editorWindow && e.getActionCommand().equals("Cancel")) {
			editorWindow.setVisible(false);
			editorWindow.dispose();
		} else if (e.getSource() == messageWindow && e.getActionCommand().equals("a ...")) {
			checked++;
			checkSentence();
		} else if (e.getSource() == messageWindow && e.getActionCommand().equals("every ...")) {
			String text = newSentences.get(checked).getText();
			text = text.replaceFirst("^(A|a)n? ", "Every ");
			newSentences.remove(checked);
			newSentences.add(checked, StatementFactory.createSentence(text, page.getOntologyElement()));
			checked++;
			checkSentence();
		} else if (e.getSource() == messageWindow && e.getActionCommand().equals("Close")) {
			checked = 0;
		} else if (e.getSource() instanceof TextElement) {
			editorWindow.addTextElement((TextElement) e.getSource());
		}
	}
	
	private void checkSentence() {
		if (checked >= newSentences.size()) {
			assertSentences();
		} else {
			List<TextElement> t = newSentences.get(checked).getTextElements();
			String t0 = t.get(0).getText();
			String t1 = t.get(1).getText();
			String l = t.get(t.size()-1).getText();
			if (t0.matches("(A|a)n?") && !t1.matches(".* of") && l.equals(".")) {
				String s = t.get(1).getText();
				messageWindow = new MessageWindow(
						"Warning",
						"Your sentence \"a " + s + " ...\" is interpreted as \"there is a " + s +
							" that ...\". " + "Do you want to say \"every " + s + " ...\"?",
						editorWindow,
						this,
						"a ...", "every ..."
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
		final OntologyElement el = page.getOntologyElement();
		
		Task task = new Task() {
			
			int success;
			
			public void run() {
				if (edit) {
					wiki.log("edit", "sentence updated: " + textContainer.getText());
					success = el.edit(statement, new ArrayList<Statement>(newSentences));
				} else {
					wiki.log("edit", "sentence created: " + textContainer.getText());
					success = el.add(statement, new ArrayList<Statement>(newSentences));
				}
			}
			
			public void updateGUI() {
				page.update();
				if (success == 1) {
					wiki.showWindow(
						new MessageWindow(
							"Conflict",
							"A sentence is in conflict with the current knowledge. For that " +
								"reason, it cannot be added to the knowledge base.",
							"OK"
						)
					);
				} else if (success == 2) {
					wiki.showWindow(
						new MessageWindow(
							"Error",
							"A sentence could not be added to the knowledge base because the " +
								"knowledge base got too complex.",
							"OK"
						)
					);
				}
				if (page != null) {
					page.update();
					page.getWiki().update();
				}
			}
			
		};
		
		if (edit) {
			wiki.enqueueStrongAsyncTask(
					"Updating",
					"The knowledge base is being updated...",
					task
				);
		} else {
			wiki.enqueueStrongAsyncTask(
					"Updating",
					"The sentence is being added to the knowledge base...",
					task
				);
		}
		
		editorWindow.setVisible(false);
	}

}
