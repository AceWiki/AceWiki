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

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.Color;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.ImageReference;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.echocomp.CheckBox;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.preditor.WordEditorForm;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This abstract class contains the basic structure for forms to create and modify words
 * (represented by ontology elements).
 * 
 * @author Tobias Kuhn
 */
public class FormPane extends WordEditorForm {

	private static final long serialVersionUID = 1433983400709841847L;
	
	private String type;
	private OntologyElement element;
	private int wordNumber;
	private Wiki wiki;
	private LexiconChanger lexiconChanger;
	private boolean locked;
	private MessageWindow delConfirmWindow;
	
	public FormPane(String type, int wordNumber, WindowPane window, Wiki wiki,
			ActionListener actionListener) {
		this(type, null, wordNumber, window, wiki, actionListener);
	}
	
	public FormPane(String type, WindowPane window, Wiki wiki) {
		this(type, null, 0, window, wiki, wiki);
	}
	
	public FormPane(OntologyElement element, WindowPane window, Wiki wiki) {
		this(element.getInternalType(), element, 0, window, wiki, wiki);
	}
	
	private FormPane(String type, OntologyElement element, int wordNumber, WindowPane window,
			Wiki wiki, ActionListener actionListener) {
		super(window, actionListener);
		this.type = type;
		this.wordNumber = wordNumber;
		this.wiki = wiki;
		if (element != null) {
			this.locked = true;
			if (wiki.isReadOnly()) {
				setButtons("Close");
			} else {
				setButtons("Unlock", "Close");
			}
		} else {
			element = wiki.getOntology().getLanguageFactory().createOntologyElement(type);
			this.locked = false;
			setButtons("OK", "Cancel");
		}
		this.element = element;
		
		lexiconChanger = wiki.getLanguageEngine().getLexiconChanger(type);
		setTitle(lexiconChanger.getTitle());
		setExplanationComponent(lexiconChanger.getImage(), lexiconChanger.getDescription());
		
		for (LexiconDetail d : lexiconChanger.getDetails(element)) {
			Component formElement = null;
			Object value = d.getValue();
			boolean required = d.isRequired();
			if (value instanceof String) {
				TextField textField = new TextField(this);
				textField.setText((String) value);
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
	
	/**
	 * Creates a new editor window.
	 * 
	 * @param element The ontology element to be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	// TODO: move!
	public static WordEditorWindow createEditorWindow(OntologyElement element, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new FormPane(element, editorWindow, wiki));
		return editorWindow;
	}
	
	/**
	 * Creates a new creator window.
	 * 
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 * @return The new creator window.
	 */
	// TODO: move!
	public static WordEditorWindow createCreatorWindow(String type, int wordNumber, Wiki wiki,
			ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow("Word Creator");
		creatorWindow.addTab(new FormPane(type, wordNumber, creatorWindow, wiki, actionListener));
		return creatorWindow;
	}
	
	private void setExplanationComponent(int image, String text) {
		ImageReference imageRef = null;
		String imagePath = "ch/uzh/ifi/attempto/acewiki/gui/img/";
		if (image == LexiconChanger.INDIVIDUAL_IMAGE) {
			imageRef = new ResourceImageReference(imagePath + "individual.png");
		} else if (image == LexiconChanger.CONCEPT_IMAGE) {
			imageRef = new ResourceImageReference(imagePath + "concept.png");
		} else if (image == LexiconChanger.RELATION_IMAGE) {
			imageRef = new ResourceImageReference(imagePath + "relation.png");
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
		setButtons("Delete", "Change", "Cancel");
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
					"Error",
					"This word cannot be deleted, because other articles refer to it.",
					null,
					(ActionListener) null,
					"OK"
				));
		} else {
			wiki.log("page", "delete confirmation");
			delConfirmWindow = new MessageWindow(
					"Delete",
					"Do you really want to delete this word and all the content of its article?",
					null,
					this,
					"Yes",
					"No"
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
				element.registerAt(wiki.getOntology());
			}
			wiki.removeWindow(getParentWindow());
			
			// a text element is used to store the ontology element and the word number in one object:
			OntologyTextElement te = new OntologyTextElement(element, wordNumber);
			getActionListener().actionPerformed(new ActionEvent(te, ""));
		} catch (InvalidWordException ex) {
			wiki.log("edit", "invalid word: " + ex.getMessage());
			wiki.showWindow(new MessageWindow("Error", ex.getMessage(), getParentWindow(), "OK"));
		}
	}

	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		if (e.getSource() == delConfirmWindow) {
			if ("Yes".equals(c)) {
				delete();
				wiki.removeWindow(getParentWindow());
			}
		} else if ("Cancel".equals(c) || "Close".equals(c)) {
			wiki.removeWindow(getParentWindow());
		} else if ("OK".equals(c)) {
			saveOrShowError();
		} else if ("Unlock".equals(c)) {
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				unlock();
			}
		} else if ("Change".equals(c)) {
			saveOrShowError();
		} else if ("Delete".equals(c)) {
			prepareDelete();
		}
	}

}
