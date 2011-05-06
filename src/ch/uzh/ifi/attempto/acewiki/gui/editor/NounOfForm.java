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

package ch.uzh.ifi.attempto.acewiki.gui.editor;

import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.aceowl.OfRelation;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
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
	
	private OfRelation relation;
	
	/**
	 * Creates a new form for of-constructs.
	 * 
	 * @param relation The relation that is represented by the of-construct.
	 * @param window The host window of the form.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	public NounOfForm(OfRelation relation, WindowPane window, Wiki wiki, ActionListener actionListener) {
		super("Of-Construct", relation != null, window, wiki, actionListener);
		this.relation = relation;
		if (relation == null) {
			relation = new OfRelation();
		}
		this.relation = relation;

		setExplanationComponent(
				new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/relation.png"),
				"Every of-construct represents a certain relation between things. " +
					"For example, the of-construct \"child of\" relates persons to their " +
					"parents. Every of-construct consists of a noun plus the preposition " +
					"\"of\"."
			);
		addRow("noun", nounField, "examples: part, child, owner", true);
		
		nounField.setText(relation.getPrettyNoun());
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
	 * @param relation The relation that is represented by the of-construct that should be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(OfRelation relation, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new NounOfForm(relation, editorWindow, wiki, wiki));
		return editorWindow;
	}

	public OntologyElement getOntologyElement() {
		return relation;
	}

	protected void save() throws InvalidWordException {
		Wiki wiki = getWiki();
		String name = normalize(nounField.getText());
		if (name.toLowerCase().endsWith("_of")) {
			name = name.substring(0, name.length()-3);
		}
		String nameP = name.replace("_", " ");
		
		if (name.equals("")) {
			throw new InvalidWordException("No noun defined: Please specify the singular form " +
				"of a noun.");
		}
		if (!isValidWordOrEmpty(name)) {
			throw new InvalidWordException("Invalid character: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		if (FunctionWords.isFunctionWord(name)) {
			throw new InvalidWordException("'" + nameP + "' is a predefined word and cannot be " +
				"used here.");
		}
		OntologyElement oe = wiki.getOntology().getElement(name + " of");
		if (oe != null && oe != relation) {
			throw new InvalidWordException("The word '" + nameP + "' is already used. Please " +
				"use a different one.");
		}
		relation.setWords(name);
		wiki.log("edit", "of-construct: " + name);
		if (relation.getOntology() == null) {
			relation.registerAt(getWiki().getOntology());
		}
		finished(relation);
	}

}
