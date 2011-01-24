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
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OfRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This class represents a form to create or modify of-constructs.
 * 
 * @author Tobias Kuhn
 */
public class NounOfForm extends FormPane {
	
	private static final long serialVersionUID = -6544810970331235077L;

	private TextField nounField = new TextField(this);
	
	private OfRole role;
	
	/**
	 * Creates a new form for of-constructs.
	 * 
	 * @param role The role that is represented by the of-construct.
	 * @param window The host window of the form.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	public NounOfForm(OfRole role, WindowPane window, Wiki wiki, ActionListener actionListener) {
		super("Of-Construct", role != null, window, wiki, actionListener);
		this.role = role;
		if (role == null) {
			role = new OfRole();
		}
		this.role = role;

		setExplanationComponent(
				new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/role.png"),
				"Every of-construct represents a certain relation between things. " +
					"For example, the of-construct \"child of\" relates persons to their " +
					"parents. Every of-construct consists of a noun plus the preposition " +
					"\"of\"."
			);
		addRow("noun", nounField, "examples: part, child, owner", true);
		
		nounField.setText(role.getPrettyNoun());
	}
	
	/**
	 * Creates a new creator window for of-constructs.
	 * 
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 * @return The new creator window.
	 */
	public static WordEditorWindow createCreatorWindow(Wiki wiki, ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow("Word Creator");
		creatorWindow.addTab(new NounOfForm(null, creatorWindow, wiki, actionListener));
		return creatorWindow;
	}
	
	/**
	 * Creates a new editor window for of-constructs.
	 * 
	 * @param role The role that is represented by the of-construct that should be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(OfRole role, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new NounOfForm(role, editorWindow, wiki, wiki));
		return editorWindow;
	}

	public OntologyElement getOntologyElement() {
		return role;
	}

	protected void save() {
		Wiki wiki = getWiki();
		String name = normalize(nounField.getText());
		if (name.toLowerCase().endsWith("_of")) {
			name = name.substring(0, name.length()-3);
		}
		String nameP = name.replace("_", " ");
		
		if (name.equals("")) {
			wiki.log("edit", "error: no word defined");
			showErrorMessage("No noun defined: Please specify the singular form of a noun.");
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
		OntologyElement oe = wiki.getOntology().getElement(name + " of");
		if (oe != null && oe != role) {
			wiki.log("edit", "error: word is already used");
			showErrorMessage("The word '" + nameP + "' is already used. Please use a different one.");
			return;
		}
		role.setWords(name);
		wiki.log("edit", "of-construct: " + name);
		if (role.getOntology() == null) {
			role.registerAt(getWiki().getOntology());
		}
		finished(role);
	}

}
