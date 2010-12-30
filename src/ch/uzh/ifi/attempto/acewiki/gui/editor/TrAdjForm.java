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

import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.TrAdjRole;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This class represents a form to create or modify transitive adjectives.
 * 
 * @author Tobias Kuhn
 */
public class TrAdjForm extends FormPane {
	
	private static final long serialVersionUID = -4367996031949560664L;

	private TextField trAdjField = new TextField(this);
	
	private TrAdjRole role;
	
	/**
	 * Creates a new form for transitive adjectives.
	 * 
	 * @param role The role that is represented by the transitive adjective.
	 * @param window The host window of the form.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	public TrAdjForm(TrAdjRole role, WindowPane window, Wiki wiki, ActionListener actionListener) {
		super("Transitive Adjective", role != null, window, wiki, actionListener);
		if (role == null) {
			role = new TrAdjRole();
		}
		this.role = role;

		setExplanationComponent(
				new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/role.png"),
				"Every transitive adjective represents a certain relation between things. " +
					"For example, the transitive adjective \"located in\" relates things to " +
					"their location. Transitive adjectives consist of an adjective that " +
					"is followed by a preposition."
			);
		addRow("tr. adjective", trAdjField, "examples: located in, matched with, fond of", true);
		
		trAdjField.setText(role.getPrettyWord(0));
	}
	
	/**
	 * Creates a new creator window for transitive adjectives.
	 * 
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 * @return The new creator window.
	 */
	public static WordEditorWindow createCreatorWindow(Wiki wiki, ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow("Word Creator");
		creatorWindow.addTab(new TrAdjForm(null, creatorWindow, wiki, actionListener));
		return creatorWindow;
	}
	
	/**
	 * Creates a new editor window for transitive adjectives.
	 * 
	 * @param role The role that is represented by the transitive adjective that should be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(TrAdjRole role, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new TrAdjForm(role, editorWindow, wiki, wiki));
		return editorWindow;
	}

	public OntologyElement getOntologyElement() {
		return role;
	}

	protected void save() {
		Wiki wiki = getWiki();
		String name = normalize(trAdjField.getText());
		String nameP = name.replace("_", " ");
		
		if (name.equals("")) {
			wiki.log("edit", "error: no word defined");
			showErrorMessage("No word defined: Please specify the transitive adjective.");
			return;
		}
		if (!isValidWordOrEmpty(name)) {
			wiki.log("edit", "error: word contains invalid character");
			showErrorMessage("Invalid character: Only a-z, A-Z, 0-9, -, and spaces are allowed, " +
				"and the first character must be one of a-z A-Z.");
			return;
		}
		if (FunctionWords.isFunctionWord(name)) {
			wiki.log("edit", "error: word is predefined");
			showErrorMessage("'" + nameP + "' is a predefined word and cannot be used here.");
			return;
		}
		OntologyElement oe = wiki.getOntology().getElement(name);
		if (oe != null && oe != role) {
			wiki.log("edit", "error: word is already used");
			showErrorMessage("The word '" + nameP + "' is already used. Please use a different one.");
			return;
		}
		role.setWords(name);
		wiki.log("edit", "transitive adjective: " + name);
		if (role.getOntology() == null) {
			role.registerAt(getWiki().getOntology());
		}
		finished(role);
	}

}
