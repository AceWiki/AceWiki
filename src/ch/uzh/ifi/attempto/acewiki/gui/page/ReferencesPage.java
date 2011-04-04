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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Article;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.IndexBar;
import ch.uzh.ifi.attempto.acewiki.gui.TextRow;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.acewiki.gui.WikiLink;
import ch.uzh.ifi.attempto.acewiki.gui.editor.FormPane;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows all references for a certain ontology element.
 * 
 * @author Tobias Kuhn
 */
public class ReferencesPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 1025665226113017153L;

	private static final int pageSize = 50;
	
	private ArticlePage page;
	private Column referenceColumn = new Column();
	private IndexBar indexBar;
	private List<Sentence> sentences;
	private int chosenPage = 0;
	private Title title;
	
	/**
	 * Creates a new references page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public ReferencesPage(ArticlePage page) {
		super(page.getWiki());
		this.page = page;
		
		addTab("Article", this);
		addSelectedTab("References");
		if (page instanceof ConceptPage) {
			addTab("Individuals", this);
			addTab("Hierarchy", this);
		}
		if (page instanceof IndividualPage) {
			addTab("Assignments", this);
		}
		
		OntologyElement oe = page.getOntologyElement();
		title = new Title(oe.getHeadword(), "- References", oe.getType(), this);
		add(title);
		addHorizontalLine();
		add(new VSpace(18));
		
		indexBar = new IndexBar("Page:", 0, this);
		add(indexBar);

		referenceColumn.setInsets(new Insets(10, 2, 5, 20));
		referenceColumn.setCellSpacing(new Extent(2));
		add(referenceColumn);
	}
	
	protected void doUpdate() {
		title.setText(page.getOntologyElement().getHeadword());
		referenceColumn.removeAll();
		List<OntologyElement> ontologyElements = getWiki().getOntologyElements();
		sentences = new ArrayList<Sentence>();
		Collections.sort(ontologyElements);
		for (OntologyElement oe : ontologyElements) {
			if (oe == page.getOntologyElement()) continue;
			for (Sentence s : oe.getArticle().getSentences()) {
				if (s.contains(page.getOntologyElement())) {
					sentences.add(s);
				}
			}
		}
		if (sentences.size() == 0) {
			indexBar.setVisible(false);
			String hw = page.getOntologyElement().getHeadword();
			referenceColumn.add(new SolidLabel(
					"(no other article refers to '" + hw + "')",
					Font.ITALIC,
					10
				));
		} else {
			int i = ((sentences.size()-1) / pageSize) + 1;
			if (chosenPage > i) chosenPage = 0;
			indexBar.setNumbers(i);
			indexBar.setActiveButton(chosenPage);
			updatePage();
		}
	}
	
	private void updatePage() {
		referenceColumn.removeAll();
		
		indexBar.setVisible(sentences.size() > pageSize);
		
		int max = sentences.size();
		if (max > (chosenPage + 1) * pageSize) max = (chosenPage + 1) * pageSize;
		
		Article a = null;
		for (int i = chosenPage * pageSize; i < max; i++) {
			Sentence s = sentences.get(i);
			if (a != s.getArticle()) {
				a = s.getArticle();
				Row r = new Row();
				Column c = new Column();
				c.add(new WikiLink(a.getOntologyElement(), getWiki()));
				Row line = new Row();
				line.setBackground(Color.DARKGRAY);
				line.setInsets(new Insets(0, 1, 0, 0));
				c.add(line);
				r.add(c);
				if (i > 0 && sentences.get(i-1).getArticle() == a) {
					r.add(new HSpace());
					r.add(new SolidLabel("(continued)", Font.ITALIC, 10));
				}
				referenceColumn.add(new VSpace());
				referenceColumn.add(r);
			}
			Row r = new Row();
			r.add(new TextRow(s, this));
			referenceColumn.add(r);
		}
	}

	public void actionPerformed(ActionEvent e) {
		Wiki wiki = getWiki();
		if ("Article".equals(e.getActionCommand())) {
			log("page", "pressed: article");
			wiki.showPage(page);
		} else if ("Individuals".equals(e.getActionCommand())) {
			log("page", "pressed: individuals");
			wiki.showPage(new IndividualsPage((ConceptPage) page));
		} else if ("Hierarchy".equals(e.getActionCommand())) {
			log("page", "pressed: hierarchy");
			wiki.showPage(new HierarchyPage((ConceptPage) page));
		} else if ("Assignments".equals(e.getActionCommand())) {
			log("page", "pressed: assignments");
			wiki.showPage(new AssignmentsPage((IndividualPage) page));
		} else if (e.getSource() == indexBar) {
			chosenPage = Integer.parseInt(e.getActionCommand()) - 1;
			log("page", "pressed: page " + (chosenPage+1));
			updatePage();
		} else if (e.getSource() == title) {
			wiki.showWindow(FormPane.createEditorWindow(page.getOntologyElement(), wiki));
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof ReferencesPage) {
			return page.equals(((ReferencesPage) obj).page);
		}
		return false;
	}
	
	public boolean isExpired() {
		return page.isExpired();
	}
	
	public String toString() {
		return "-REF- " + page.getOntologyElement().getWord();
	}

}
