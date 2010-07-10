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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import java.util.List;

import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.ResourceImageReference;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OfRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.ontology.TrAdjRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.VerbRole;
import ch.uzh.ifi.attempto.acewiki.gui.NameValueTable;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.acewiki.gui.editor.NounForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.NounOfForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.ProperNameForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.TrAdjForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.VerbForm;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows the details about a certain word.
 * 
 * @author Tobias Kuhn
 */
public class WordPage extends WikiPage implements ActionListener {
	
	private static final long serialVersionUID = 5800290686564822519L;

	private ArticlePage page;

	private SmallButton editButton = new SmallButton("edit...", this);
	private SmallButton delButton = new SmallButton("delete...", this);
	
	private Column textColumn = new Column();
	
	/**
	 * Creates a new word page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public WordPage(ArticlePage page) {
		super(page.getWiki(), new Title(
				page.getOntologyElement().getHeadword(),
				"- " + page.getOntologyElement().getType()
			));
		this.page = page;
		
		addTab("Article", this);
		addSelectedTab(page.getOntologyElement().getType());
		addTab("References", this);
		if (page instanceof ConceptPage) {
			addTab("Individuals", this);
			addTab("Hierarchy", this);
		}
		if (page instanceof IndividualPage) {
			addTab("Assignments", this);
		}
		
		add(new VSpace(10));

		textColumn.setInsets(new Insets(10, 10, 5, 20));
		textColumn.setCellSpacing(new Extent(5));
		add(textColumn);
	}
	
	protected void doUpdate() {
		getTitle().setText(page.getOntologyElement().getHeadword());
		
		textColumn.removeAll();
		
		Grid iconRow = new Grid(2);
		iconRow.setRowHeight(0, new Extent(110));
		iconRow.setColumnWidth(0, new Extent(100));
		iconRow.setInsets(new Insets(0, 0, 0, 10));
		textColumn.add(iconRow);
		
		NameValueTable lexiconTable = new NameValueTable();
		textColumn.add(lexiconTable);

		OntologyElement oe = page.getOntologyElement();
		if (oe instanceof Individual) {
			Individual ind = (Individual) oe;
			iconRow.add(new Label(
					new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/individual.png")
				));
			iconRow.add(new Label(
					"\"" + ind.getPrettyWord(1) + "\" is a proper name and represents an " +
					"individual. There is exactly one thing that has the name \"" + 
					ind.getPrettyWord(1) + "\". " + "This proper name is used " +
						(ind.hasDefiniteArticle() ?
								"with \"the\": \"" + ind.getPrettyWord(0) + "\"." :
								"without \"the\"."
						) + " " +
					(ind.getAbbreviation() != null ?
							"This proper name has an abbreviation: \"" + ind.getPrettyWord(2) + "\"." :
							""
					),
					Font.ITALIC
				));
			lexiconTable.addEntry("word class", "proper name");
			lexiconTable.addACEEntry("word", ind.getPrettyWord(1));
			lexiconTable.addEntry("... used with \"the\"", (ind.hasDefiniteArticle(0) ? "yes" : "no"));
			lexiconTable.addACEEntry("abbreviation", ind.getAbbreviation());
			if (ind.getAbbreviation() == null) {
				lexiconTable.addEntry("... used with \"the\"", "");
			} else {
				lexiconTable.addEntry("... used with \"the\"", (ind.hasDefiniteArticle(2) ? "yes" : "no"));
			}
		} else if (oe instanceof NounConcept) {
			NounConcept noun = (NounConcept) oe;
			String sg = noun.getPrettyWord(0);
			String pl = noun.getPrettyWord(1);
			iconRow.add(new Label(
					new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/concept.png")
				));
			iconRow.add(new Label(
					"\"" + sg + "\" is a noun and represents a type of things. " +
					"It stands for all things that are " + pl + ". " +
					"The singular form is \"" + sg + "\" and the plural form is \"" + pl + "\".",
					Font.ITALIC
				));
			lexiconTable.addEntry("word class", "noun");
			lexiconTable.addACEEntry("singular", sg);
			lexiconTable.addACEEntry("plural", pl);
		} else if (oe instanceof VerbRole) {
			VerbRole verb = (VerbRole) oe;
			String th = verb.getPrettyWord(0);
			String inf = verb.getPrettyWord(1);
			String pp = verb.getPrettyPastPart();
			iconRow.add(new Label(
					new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/role.png")
				));
			iconRow.add(new Label(
					"\"" + inf + "\" is a verb and represents a relation between things. " +
					"It represents the fact that certain things " + inf + " other things. " +
					"The third singular form is \"" + th + "\", the bare infinitive form is \"" +
					 inf + "\", " +"and the past participle form is " +
					 (pp == null ? "undefined" : "\"" + pp + "\"") + ".",
					Font.ITALIC
				));
			lexiconTable.addEntry("word class", "verb");
			lexiconTable.addACEEntry("third singular", th);
			lexiconTable.addACEEntry("bare infinitive", inf);
			lexiconTable.addACEEntry("past participle", pp);
		} else if (oe instanceof OfRole) {
			OfRole of = (OfRole) oe;
			iconRow.add(new Label(
					new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/role.png")
				));
			iconRow.add(new Label(
					"\"" + of.getPrettyNoun() + " of\" is an of-construct and represents a " +
					"relation between things. It represents the fact that certain things are a " +
					of.getPrettyNoun() + " of other things. It consists of the noun \"" +
					of.getPrettyNoun() + "\" plus the preposition \"of\".",
					Font.ITALIC
				));
			lexiconTable.addEntry("word class", "of-construct");
			lexiconTable.addACEEntry("noun", of.getPrettyNoun());
		} else if (oe instanceof TrAdjRole) {
			TrAdjRole tradj = (TrAdjRole) oe;
			iconRow.add(new Label(
					new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/role.png")
				));
			iconRow.add(new Label(
					"\"" + tradj.getPrettyWord(0) + "\" is a transitive adjective and " +
					"represents a relation between things. It represents the fact that certain " +
					"things are " + tradj.getPrettyWord(0) + " other things. ",
					Font.ITALIC
				));
			lexiconTable.addEntry("word class", "transitive adjective");
			lexiconTable.addACEEntry("word", tradj.getPrettyWord(0));
		}
		
		textColumn.add(new VSpace());
		
		if (!getWiki().isReadOnly()) {
			Row headButtonRow = new Row();
			headButtonRow.add(editButton);
			headButtonRow.add(new HSpace());
			headButtonRow.add(delButton);
			textColumn.add(headButtonRow);
		}
	}

	public void actionPerformed(ActionEvent e) {
		Wiki wiki = getWiki();
		if ("Article".equals(e.getActionCommand())) {
			log("page", "pressed: article");
			wiki.showPage(page);
		} else if ("References".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			wiki.showPage(new ReferencesPage(page));
		} else if ("Individuals".equals(e.getActionCommand())) {
			log("page", "pressed: individuals");
			wiki.showPage(new IndividualsPage((ConceptPage) page));
		} else if ("Hierarchy".equals(e.getActionCommand())) {
			log("page", "pressed: hierarchy");
			wiki.showPage(new HierarchyPage((ConceptPage) page));
		} else if ("Assignments".equals(e.getActionCommand())) {
			log("page", "pressed: assignments");
			wiki.showPage(new AssignmentsPage((IndividualPage) page));
		} else if (e.getSource() == delButton) {
			log("page", "pressed: delete");
			OntologyElement oe = page.getOntologyElement();
			List<Sentence> references = wiki.getOntology().getReferences(oe);
			for (Sentence s : oe.getSentences()) {
				references.remove(s);
			}
			if (!references.isEmpty()) {
				log("page", "error: cannot delete article with references");
				wiki.showWindow(new MessageWindow(
						"Error",
						"This article cannot be deleted, because other articles refer to it.",
						null,
						this,
						"OK"
					));
			} else {
				log("page", "delete confirmation");
				wiki.showWindow(new MessageWindow(
						"Delete",
						"Do you really want to delete this word and all the content of its article?",
						null,
						this,
						"Yes",
						"No"
					));
			}
		} else if (e.getSource() instanceof MessageWindow && e.getActionCommand().equals("Yes")) {
			log("page", "delete confirmed");
			wiki.getOntology().remove(page.getOntologyElement());
			wiki.showStartPage();
		} else if (e.getSource() == editButton) {
			log("page", "pressed: edit word");
			if (page instanceof ConceptPage) {
				wiki.showWindow(NounForm.createEditorWindow(
						(NounConcept) page.getOntologyElement(),
						wiki
					));
			} else if (page instanceof IndividualPage) {
				wiki.showWindow(ProperNameForm.createEditorWindow(
						(Individual) page.getOntologyElement(),
						wiki
					));
			} else if (page.getOntologyElement() instanceof OfRole) {
				wiki.showWindow(NounOfForm.createEditorWindow(
						(OfRole) page.getOntologyElement(),
						wiki
					));
			} else if (page.getOntologyElement() instanceof VerbRole) {
				wiki.showWindow(VerbForm.createEditorWindow(
						(VerbRole) page.getOntologyElement(),
						wiki
					));
			} else if (page.getOntologyElement() instanceof TrAdjRole) {
				wiki.showWindow(TrAdjForm.createEditorWindow(
						(TrAdjRole) page.getOntologyElement(),
						wiki
					));
			}
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof WordPage) {
			return page.equals(((WordPage) obj).page);
		}
		return false;
	}
	
	public boolean isExpired() {
		return page.isExpired();
	}
	
	public String toString() {
		return "-WORD- " + page.getOntologyElement().getWord();
	}

}
