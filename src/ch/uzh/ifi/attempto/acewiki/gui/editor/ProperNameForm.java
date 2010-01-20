// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui.editor;

import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.ResourceImageReference;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.echocomp.CheckBox;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This class represents a form to create or modify proper names.
 * 
 * @author Tobias Kuhn
 */
public class ProperNameForm extends FormPane {
	
	private static final long serialVersionUID = 7860047859937196093L;
	
	private TextField nameField = new TextField(this);
	private CheckBox nameDefArtCheckBox = new CheckBox();
	private TextField abbrevField = new TextField(this);
	private CheckBox abbrevDefArtCheckBox = new CheckBox();
	
	private Individual ind;
	
	/**
	 * Creates a new proper name form.
	 * 
	 * @param ind The individual that is represented by the proper name.
	 * @param window The host window of the form.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	public ProperNameForm(Individual ind, WindowPane window, Wiki wiki, ActionListener actionListener) {
		super("Proper Name", window, wiki, actionListener);
		this.ind = ind;
		
		setExplanationComponent(
				new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/individual.png"),
				"Every proper name represents a certain individual. " +
					"The country \"Switzerland\", the person \"Bob Dylan\", the river \"Nile\", " +
					"and the organization \"United Nations\" are typical examples. " +
					"Some proper names are used with \"the\" (\"the Nile\", \"the United " +
					"Nations\") and others are not (\"Switzerland\", \"Bob Dylan\"). " +
					"Proper names can have an abbreviation that has the same meaning as the " +
					"longer proper name."
			);
		addRow("proper name", nameField, "examples: Switzerland, Bob Dylan, Nile, United Nations", true);
		addRow("... used with \"the\"", nameDefArtCheckBox, "examples: the Nile, the United Nations", false);
		addRow("abbreviation", abbrevField, "example: HTML, UN", false);
		addRow("... used with \"the\"", abbrevDefArtCheckBox, "example: the UN", false);
		
		nameField.setText(ind.getPrettyWord(1));
		nameDefArtCheckBox.setSelected(ind.hasDefiniteArticle(0));
		abbrevField.setText(ind.getAbbreviation());
		abbrevDefArtCheckBox.setSelected(ind.hasDefiniteArticle(2));
		
		ApplicationInstance.getActive().setFocusedComponent(nameField);
	}
	
	/**
	 * Creates a new creator window for proper names.
	 * 
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 * @return The new creator window.
	 */
	public static WordEditorWindow createCreatorWindow(Wiki wiki, ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow("Word Creator");
		creatorWindow.addTab(new ProperNameForm(
				new Individual(),
				creatorWindow,
				wiki,
				actionListener
			));
		return creatorWindow;
	}
	
	/**
	 * Creates a new editor window for proper names.
	 * 
	 * @param ind The individual that is represented by the proper name that should be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(Individual ind, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new ProperNameForm(ind, editorWindow, wiki, wiki));
		return editorWindow;
	}

	protected void save() {
		Wiki wiki = getWiki();
		String name = normalize(nameField.getText());
		String abbrev = normalize(abbrevField.getText());
		boolean nameDefArt = nameDefArtCheckBox.isSelected();
		boolean abbrevDefArt = abbrevDefArtCheckBox.isSelected();
		
		if (name.toLowerCase().startsWith("the_")) {
			name = name.substring(4);
			nameDefArt = true;
		}
		if (abbrev.toLowerCase().startsWith("the_")) {
			abbrev = abbrev.substring(4);
			abbrevDefArt = true;
		}
		String nameP = name.replace("_", " ");
		String abbrevP = abbrev.replace("_", " ");
		
		if (name.equals("")) {
			wiki.log("edit", "error: no word defined");
			showErrorMessage("No proper name defined: Please specify a name.");
			return;
		}
		if (!isValidWordOrEmpty(name) || !isValidWordOrEmpty(abbrev)) {
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
		if (FunctionWords.isFunctionWord(abbrev)) {
			wiki.log("edit", "error: word is predefined");
			showErrorMessage("'" + abbrevP + "' is a predefined word and cannot be used here.");
			return;
		}
		if (abbrev.length() >= name.length()) {
			wiki.log("edit", "error: abbreviation is not shorter than the full proper name");
			showErrorMessage("The abbreviation has to be shorter than the full proper name.");
			return;
		}
		OntologyElement oe = wiki.getOntology().get(name);
		if (oe != null && oe != ind) {
			wiki.log("edit", "error: word is already used");
			showErrorMessage("The word '" + nameP + "' is already used. Please use a different one.");
			return;
		}
		oe = wiki.getOntology().get(abbrev);
		if (oe != null && oe != ind) {
			wiki.log("edit", "error: word is already used");
			showErrorMessage("The word '" + abbrevP + "' is already used. Please use a different one.");
			return;
		}
		String word = name;
		if (nameDefArt) {
			word = "the " + name;
		}
		String abbrevWord = abbrev;
		if (abbrev.equals("")) {
			abbrev = null;
			abbrevWord = null;
		} else {
			if (abbrevDefArt) {
				abbrevWord = "the " + abbrev;
			}
		}
		ind.setWords(word, name, abbrevWord, abbrev);
		wiki.log("edit", "proper name: " + word + " / " + abbrevWord);
		if (ind.getOntology() == null) {
			ind.registerAt(getWiki().getOntology());
		}
		finished(ind);
	}

}
