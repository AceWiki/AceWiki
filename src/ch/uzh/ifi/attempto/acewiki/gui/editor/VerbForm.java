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
import ch.uzh.ifi.attempto.acewiki.aceowl.VerbRelation;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This class represents a form to create or modify verbs.
 * 
 * @author Tobias Kuhn
 */
public class VerbForm extends FormPane {
	
	private static final long serialVersionUID = 5050205540719470842L;
	
	private TextField thirdSgField = new TextField(this);
	private TextField infField = new TextField(this);
	private TextField pastPartField = new TextField(this);
	
	private VerbRelation relation;
	private int wordNumber;
	
	/**
	 * Creates a new verb form.
	 * 
	 * @param relation The relation that is represented by the verb.
	 * @param wordNumber The word form id (only used if called from the sentence editor).
	 * @param window The host window of the form.
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 */
	public VerbForm(VerbRelation relation, int wordNumber, WindowPane window, Wiki wiki,
			ActionListener actionListener) {
		super("Verb", relation != null, window, wiki, actionListener);
		if (relation == null) {
			relation = new VerbRelation();
		}
		this.relation = relation;
		
		this.wordNumber = wordNumber;

		setExplanationComponent(
				new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/relation.png"),
				"Every verb represents a certain relation between things. " +
					"For example, the verb \"owns\" relates persons to their possessions."
			);
		addRow("third singular", thirdSgField, "examples: owns, applies to, touches", true);
		addRow("bare infinitive", infField, "examples: own, apply to, touch", true);
		addRow("past participle", pastPartField, "examples: owned, applied to, touched", false);
		
		thirdSgField.setText(relation.getPrettyWord(0));
		infField.setText(relation.getPrettyWord(1));
		pastPartField.setText(relation.getPrettyPastPart());
	}
	
	/**
	 * Creates a new creator window for verbs.
	 * 
	 * @param wordNumber The word form id (only used if called from the sentence editor).
	 * @param wiki The wiki instance.
	 * @param actionListener The actionlistener.
	 * @return The new creator window.
	 */
	public static WordEditorWindow createCreatorWindow(int wordNumber, Wiki wiki,
			ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow("Word Creator");
		creatorWindow.addTab(new VerbForm(
				null,
				wordNumber,
				creatorWindow,
				wiki,
				actionListener
			));
		return creatorWindow;
	}
	
	/**
	 * Creates a new editor window for verbs.
	 * 
	 * @param relation The relation that is represented by the verb that should be edited.
	 * @param wiki The wiki instance.
	 * @return The new editor window.
	 */
	public static WordEditorWindow createEditorWindow(VerbRelation relation, Wiki wiki) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new VerbForm(relation, 0, editorWindow, wiki, wiki));
		return editorWindow;
	}

	public OntologyElement getOntologyElement() {
		return relation;
	}

	protected void save() throws InvalidWordException {
		Wiki wiki = getWiki();
		String thirdSg = normalize(thirdSgField.getText());
		String inf = normalize(infField.getText());
		String pastPart = normalize(pastPartField.getText());
		if (pastPart.toLowerCase().endsWith("_by")) {
			pastPart = pastPart.substring(0, pastPart.length()-3);
		}
		String thirdSgP = thirdSg.replace("_", " ");
		String infP = inf.replace("_", " ");
		String pastPartP = pastPart.replace("_", " ");
		
		Ontology ontology = wiki.getOntology();
		
		// check whether all necessary fields are filled-in
		if (thirdSg.equals("")) {
			throw new InvalidWordException("No third singular defined: Please define the third " +
				"singular form.");
		}
		if (inf.equals("")) {
			throw new InvalidWordException("No bare infinitive defined: Please define the bare " +
				"infinitive form.");
		}
		if (pastPart.equals("") && wordNumber == 2) {
			throw new InvalidWordException("No past participle defined: Please define the past " +
				"participle form.");
		}
		if (pastPart.equals("") && !ontology.getReferences(relation, 2).isEmpty()) {
			throw new InvalidWordException("The past participle form cannot be removed because " +
				"there are sentences that are using it.");
		}
		
		// check whether the words contain only valid characters
		if (!isValidWordOrEmpty(thirdSg) || !isValidWordOrEmpty(inf) || !isValidWordOrEmpty(pastPart)) {
			throw new InvalidWordException("Invalid character used: Only a-z, A-Z, 0-9, -, and " +
				"spaces are allowed, and the first character must be one of a-z A-Z.");
		}
		
		// check whether a word is a predefined function word
		if (FunctionWords.isFunctionWord(thirdSg)) {
			throw new InvalidWordException("'" + thirdSgP + "' is a predefined word and cannot " +
				"be used here.");
		}
		if (FunctionWords.isFunctionWord(inf)) {
			throw new InvalidWordException("'" + infP + "' is a predefined word and cannot be " +
				"used here.");
		}
		if (FunctionWords.isFunctionWord(pastPart)) {
			throw new InvalidWordException("'" + pastPartP + "' is a predefined word and cannot " +
				"be used here.");
		}
		
		// check whether all word forms are distinct
		if (thirdSg.equals(inf)) {
			throw new InvalidWordException("The singular and plural forms have to be distinct.");
		}
		
		// check whether a word is already defined
		OntologyElement oe1 = ontology.getElement(thirdSg);
		if (oe1 != null && oe1 != relation) {
			throw new InvalidWordException("The word '" + thirdSgP + "' is already used. Please " +
				"use a different one.");
		}
		OntologyElement oe2 = ontology.getElement(inf);
		if (oe2 != null && oe2 != relation) {
			throw new InvalidWordException("The word '" + infP + "' is already used. Please use " +
				"a different one.");
		}
		OntologyElement oe3 = ontology.getElement(pastPart);
		if (oe3 != null && oe3 != relation) {
			throw new InvalidWordException("The word '" + pastPartP + "' is already used. " +
				"Please use a different one.");
		}
		
		if (pastPart.equals("")) pastPart = null;
		relation.setWords(thirdSg, inf, pastPart);
		
		wiki.log("edit", "verb: " + thirdSg + " / " + inf + " / " + pastPart);
		if (relation.getOntology() == null) {
			relation.registerAt(getWiki().getOntology());
		}
		finished(relation, wordNumber);
	}

}
