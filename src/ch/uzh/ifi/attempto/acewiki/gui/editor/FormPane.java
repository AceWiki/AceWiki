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

import java.util.List;

import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.ImageReference;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Concept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OfRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.ontology.TrAdjRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.VerbRole;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import ch.uzh.ifi.attempto.preditor.WordEditorForm;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This abstract class contains the basic structure for forms to create and modify words
 * (represented by ontology elements).
 * 
 * @author Tobias Kuhn
 */
public abstract class FormPane extends WordEditorForm {

	private static final long serialVersionUID = 1433983400709841847L;
	
	private Wiki wiki;
	private boolean locked;
	private MessageWindow delConfirmWindow;
	
	/**
	 * Initializes the form pane.
	 * 
	 * @param title The title.
	 * @param edit Defines whether an existing element is edited (true) or a new element is
	 *             created (false).
	 * @param window The host window.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	protected FormPane(String title, boolean edit, WindowPane window, Wiki wiki,
			ActionListener actionListener) {
		super(title, window, actionListener);
		this.wiki = wiki;
		if (edit) {
			this.locked = true;
			if (wiki.isReadOnly()) {
				setButtons("Close");
			} else {
				setButtons("Unlock", "Close");
			}
		} else {
			this.locked = false;
			setButtons("OK", "Cancel");
		}
	}
	
	/**
	 * Creates a new editor window.
	 * 
	 * @param element The ontology element to be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(OntologyElement element, Wiki wiki) {
		if (element instanceof Concept) {
			return NounForm.createEditorWindow((NounConcept) element, wiki);
		} else if (element instanceof Individual) {
			return ProperNameForm.createEditorWindow((Individual) element, wiki);
		} else if (element instanceof OfRole) {
			return NounOfForm.createEditorWindow((OfRole) element, wiki);
		} else if (element instanceof VerbRole) {
			return VerbForm.createEditorWindow((VerbRole) element, wiki);
		} else if (element instanceof TrAdjRole) {
			return TrAdjForm.createEditorWindow((TrAdjRole) element, wiki);
		}
		return null;
	}
	
	/**
	 * This method should return the ontology element that is shown in this form pane.
	 * 
	 * @return The ontology element.
	 */
	public abstract OntologyElement getOntologyElement();
	
	/**
	 * Returns the wiki instance.
	 * 
	 * @return The wiki instance.
	 */
	protected Wiki getWiki() {
		return wiki;
	}
	
	/**
	 * This method should try to save the word with the current properties and should show
	 * error messages if this is not successful. In the case of success, one of the
	 * finished-methods has to be called.
	 * 
	 * @see #finished(OntologyElement)
	 * @see #finished(OntologyElement, int)
	 */
	protected abstract void save();
	
	/**
	 * This method should be called when the saving process is finished successfully.
	 * 
	 * @param el The created or modified ontology element.
	 */
	protected void finished(OntologyElement el) {
		finished(el, 0);
	}
	
	/**
	 * This method should be called when the saving process is finished successfully.
	 * 
	 * @param el The created or modified ontology element.
	 * @param wordNumber The word form id.
	 */
	protected void finished(OntologyElement el, int wordNumber) {
		wiki.removeWindow(getParentWindow());
		
		// a text element is used to store the ontology element and the word number in one object:
		OntologyTextElement te = new OntologyTextElement(el, wordNumber);
		getActionListener().actionPerformed(new ActionEvent(te, ""));
	}
	
	/**
	 * Shows an error message.
	 * 
	 * @param text The error text.
	 */
	protected void showErrorMessage(String text) {
		wiki.showWindow(new MessageWindow("Error", text, getParentWindow(), "OK"));
	}
	
	/**
	 * Returns true if the string represents a valid word form.
	 * 
	 * @param s The string.
	 * @return true if the string represents a valid word form.
	 */
	protected static boolean isValidWordOrEmpty(String s) {
		return s.matches("([a-zA-Z][a-zA-Z0-9_-]*)?");
	}
	
	/**
	 * Normalizes the string. White space characters are replaced by underscores.
	 * 
	 * @param s The input string.
	 * @return The normalized string.
	 */
	protected static String normalize(String s) {
		return s.replaceAll("(\\s|_)+", "_").replaceAll("(^_|_$)", "");
	}
	
	/**
	 * Sets the explanation component to be shown above the form elements. Such an explanation
	 * component consists of an image and a text.
	 * 
	 * @param image The explanation image.
	 * @param text The explanation text.
	 */
	protected void setExplanationComponent(ImageReference image, String text) {
		Grid explanationComp = new Grid(2);
		explanationComp.setRowHeight(0, new Extent(110));
		explanationComp.setColumnWidth(0, new Extent(100));
		explanationComp.setInsets(new Insets(0, 0, 10, 20));
		explanationComp.add(new Label(image));
		explanationComp.add(new Label(text, Font.ITALIC));
		setExplanationComponent(explanationComp);
	}
	
	private void unlock() {
		setButtons("Delete", "Change", "Cancel");
		for (Component c : getFormElements()) {
			c.setEnabled(true);
		}
		locked = false;
	}
	
	private void prepareDelete() {
		OntologyElement oe = getOntologyElement();
		List<Sentence> references = wiki.getOntology().getReferences(oe);
		for (Sentence s : oe.getSentences()) {
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
		wiki.getOntology().remove(getOntologyElement());
		wiki.showStartPage();
	}
	
	public void addRow(String labelText, Component formElement, String explanation,
			boolean required) {
		formElement.setEnabled(!locked);
		super.addRow(labelText, formElement, explanation, required);
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
			save();
		} else if ("Unlock".equals(c)) {
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				unlock();
			}
		} else if ("Change".equals(c)) {
			save();
		} else if ("Delete".equals(c)) {
			prepareDelete();
		}
	}

}
