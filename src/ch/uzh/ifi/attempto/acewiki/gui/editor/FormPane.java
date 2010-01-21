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

import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.ImageReference;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import ch.uzh.ifi.attempto.preditor.WordEditorForm;

/**
 * This abstract class contains the basic structure for forms to create and modify words
 * (represented by ontology elements).
 * 
 * @author Tobias Kuhn
 */
public abstract class FormPane extends WordEditorForm {

	private Wiki wiki;
	
	/**
	 * Initializes the form pane.
	 * 
	 * @param title The title.
	 * @param window The host window.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	protected FormPane(String title, WindowPane window, Wiki wiki, ActionListener actionListener) {
		super(title, window, actionListener);
		this.wiki = wiki;
	}
	
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
		parent.setVisible(false);
		dispose();
		
		// a text element is used to store the ontology element and the word number in one object:
		OntologyTextElement te = OntologyTextElement.createTextElement(el, wordNumber);
		actionListener.actionPerformed(new ActionEvent(te, ""));
	}
	
	/**
	 * Shows an error message.
	 * 
	 * @param text The error text.
	 */
	protected void showErrorMessage(String text) {
		wiki.showWindow(new MessageWindow("Error", text, parent, "OK"));
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

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == cancelButton) {
			parent.setVisible(false);
			dispose();
		} else {
			save();
		}
	}

}
