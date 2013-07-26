// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import nextapp.echo.app.Color;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.ImageReference;
import nextapp.echo.app.Insets;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.Relation;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.TopicElement;
import ch.uzh.ifi.attempto.echocomp.CheckBox;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.preditor.WordEditorForm;

/**
 * This class describes the basic structure for forms to create and modify words (represented by
 * ontology elements).
 * 
 * @author Tobias Kuhn
 */
public class FormPane extends WordEditorForm {

	private static final long serialVersionUID = 1433983400709841847L;
	
	private OntologyElement element;
	private int wordNumber;
	private Wiki wiki;
	private LexiconChanger lexiconChanger;
	private boolean locked;
	private MessageWindow delConfirmWindow;
	
	/**
	 * Creates a form pane for creating a new word.
	 * 
	 * @param type The type of the new word.
	 * @param wordNumber The word number that should be used once the word is created.
	 * @param window The parent window.
	 * @param wiki The wiki object.
	 * @param actionListener The action-listener.
	 */
	public FormPane(String type, int wordNumber, WindowPane window, Wiki wiki,
			ActionListener actionListener) {
		this(type, null, wordNumber, window, wiki, actionListener);
	}
	
	/**
	 * Creates a form pane for creating a new word.
	 * 
	 * @param type The type of the new word.
	 * @param window The parent window.
	 * @param wiki The wiki object.
	 */
	public FormPane(String type, WindowPane window, Wiki wiki) {
		this(type, null, 0, window, wiki, wiki);
	}
	
	/**
	 * Creates a form pane for modifying an existing word.
	 * 
	 * @param element The ontology element that represents the word to be modified.
	 * @param window The parent window.
	 * @param wiki The wiki object.
	 */
	public FormPane(OntologyElement element, WindowPane window, Wiki wiki) {
		this(element.getInternalType(), element, 0, window, wiki, wiki);
	}
	
	private FormPane(String type, OntologyElement element, int wordNumber, WindowPane window,
			Wiki wiki, ActionListener actionListener) {
		super(window, actionListener);
		this.wordNumber = wordNumber;
		this.wiki = wiki;
		if (element != null) {
			locked = true;
			if (wiki.getConfig().isReadOnly()) {
				setButtons("general_action_close");
			} else {
				setButtons("general_action_unlock", "general_action_close");
			}
		} else {
			element = wiki.getOntology().getEngine().createOntologyElement(type);
			locked = false;
			setButtons("general_action_ok", "general_action_cancel");
		}
		this.element = element;

		lexiconChanger = wiki.getLanguageHandler().getLexiconChanger(type);
		if (lexiconChanger == null) {
			setHidden(true);
			return;
		}
		setTitle(element.getType());
		setExplanationComponent(lexiconChanger.getDescription());
		
		for (LexiconDetail d : lexiconChanger.getDetails(element)) {
			Component formElement = null;
			Object value = d.getValue();
			boolean required = d.isRequired();
			if (value instanceof String) {
				TextField textField = new TextField();
				textField.setText(LanguageUtils.getPrettyPrinted((String) value));
				formElement = textField;
			} else if (value instanceof Boolean) {
				CheckBox checkBox = new CheckBox();
				checkBox.setSelected((Boolean) value);
				formElement = checkBox;
				required = false;
			} else {
				throw new RuntimeException("invalid class: " + value.getClass());
			}
			formElement.setEnabled(!locked);
			addRow(d.getName(), formElement, d.getDescription(), required);
		}

	}
	
	private void setExplanationComponent(String text) {
		ImageReference imageRef = null;
		if (getClass().getResource("img/" + element.getInternalType() + ".png") != null) {
			// Image file exists that corresponds to internal type of element
			imageRef = Wiki.getImage(element.getInternalType() + ".png");
		} else if (element instanceof Individual) {
			imageRef = Wiki.getImage("individual.png");
		} else if (element instanceof Concept) {
			imageRef = Wiki.getImage("concept.png");
		} else if (element instanceof Relation) {
			imageRef = Wiki.getImage("relation.png");
		} else if (element instanceof TopicElement) {
			imageRef = Wiki.getImage("topic.png");
		}
		Grid explanationComp = new Grid(2);
		explanationComp.setRowHeight(0, new Extent(110));
		explanationComp.setColumnWidth(0, new Extent(100));
		explanationComp.setInsets(new Insets(0, 0, 10, 20));
		if (imageRef != null) {
			explanationComp.add(new Label(imageRef));
		} else {
			explanationComp.add(new Label(""));
		}
		Label explLabel = new Label(text, Font.ITALIC);
		explLabel.setForeground(new Color(120, 120, 120));
		explanationComp.add(explLabel);
		setExplanationComponent(explanationComp);
	}
	
	/**
	 * Returns whether this form is locked or not.
	 * 
	 * @return true if this form is locked.
	 */
	public boolean isLocked() {
		return locked;
	}

	private void unlock() {
		setButtons(
				"acewiki_wordeditor_deletebutton",
				"acewiki_wordeditor_changebutton",
				"general_action_cancel"
			);
		for (Component c : getFormElements()) {
			c.setEnabled(true);
		}
		locked = false;
		doFocus();
	}
	
	private void prepareDelete() {
		List<Sentence> references = wiki.getOntology().getReferences(element);
		for (Sentence s : element.getArticle().getSentences()) {
			references.remove(s);
		}
		if (!references.isEmpty()) {
			wiki.log("page", "error: cannot delete article with references");
			wiki.showWindow(new MessageWindow(
					"acewiki_error_title",
					"acewiki_error_delref",
					getParentWindow(),
					(ActionListener) null,
					"general_action_ok"
				));
		} else {
			wiki.log("page", "delete confirmation");
			delConfirmWindow = new MessageWindow(
					"acewiki_message_delwordtitle",
					"acewiki_message_delword",
					getParentWindow(),
					this,
					"general_action_yes", "general_action_no"
				);
			wiki.showWindow(delConfirmWindow);
		}
	}
	
	private void delete() {
		wiki.log("page", "delete confirmed");
		wiki.getOntology().remove(element);
		wiki.showStartPage();
	}
	
	private void saveOrShowError() {
		try {
			List<Object> newValues = new ArrayList<Object>();
			for (Component c : getFormElements()) {
				if (c instanceof TextField) {
					newValues.add(((TextField) c).getText());
				} else if (c instanceof CheckBox) {
					newValues.add(((CheckBox) c).isSelected());
				}
			}
			lexiconChanger.save(element, wordNumber, newValues, wiki.getOntology());
			wiki.log("edit", element.toString());
			if (element.getOntology() == null) {
				wiki.getOntology().register(element);
			}
			wiki.removeWindow(getParentWindow());
			
			// a text element is used to store the ontology element and the word number in one object:
			OntologyTextElement te = new OntologyTextElement(element, wordNumber);
			getActionListener().actionPerformed(new ActionEvent(te, ""));
		} catch (InvalidWordException ex) {
			wiki.log("edit", "invalid word: " + ex.getMessage());
			wiki.showWindow(new MessageWindow(
					"acewiki_error_title",
					ex.getMessage(),
					getParentWindow(),
					"general_action_ok"
				));
		}
	}

	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		if (e.getSource() == delConfirmWindow) {
			if ("general_action_yes".equals(c)) {
				delete();
				wiki.removeWindow(getParentWindow());
			}
		} else if ("general_action_cancel".equals(c) || "general_action_close".equals(c)) {
			wiki.removeWindow(getParentWindow());
		} else if ("general_action_ok".equals(c) || "OK".equals(c)) {
			saveOrShowError();
		} else if ("general_action_unlock".equals(c)) {
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				unlock();
			}
		} else if ("acewiki_wordeditor_changebutton".equals(c)) {
			saveOrShowError();
		} else if ("acewiki_wordeditor_deletebutton".equals(c)) {
			prepareDelete();
		}
	}

}
