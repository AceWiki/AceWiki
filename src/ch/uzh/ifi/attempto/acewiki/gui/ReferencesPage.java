// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
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
	
	private OntologyElement ontologyElement;
	private Column referenceColumn = new Column();
	private IndexBar indexBar;
	private List<Sentence> sentences;
	private int chosenPage = 0;
	private Title title;
	
	/**
	 * Creates a new references page.
	 */
	public ReferencesPage(OntologyElement ontologyElement, Wiki wiki) {
		super(wiki);
		this.ontologyElement = ontologyElement;

		title = new Title("", "", "", this);
		add(title);
		addHorizontalLine();
		add(new VSpace(18));
		
		indexBar = new IndexBar(0, this);
		add(indexBar);

		referenceColumn.setInsets(new Insets(10, 2, 5, 20));
		referenceColumn.setCellSpacing(new Extent(2));
		add(referenceColumn);
	}
	
	protected void doUpdate() {
		setTabRow(TabRow.getArticleTabRow(ontologyElement, TabRow.TAB_REFERENCES, getWiki()));

		title.setText(getHeading(ontologyElement));
		title.setPostTitle("- " + getWiki().getGUIText("acewiki_page_references"));
		title.setTooltip(ontologyElement.getType());
		referenceColumn.removeAll();
		List<OntologyElement> ontologyElements = getWiki().getOntologyElements();
		sentences = new ArrayList<Sentence>();
		LanguageUtils.sortOntologyElements(ontologyElements);
		for (OntologyElement oe : ontologyElements) {
			if (oe == ontologyElement) continue;
			for (Sentence s : oe.getArticle().getSentences()) {
				if (s.contains(ontologyElement)) {
					sentences.add(s);
				}
			}
		}
		if (sentences.size() == 0) {
			indexBar.setVisible(false);
			referenceColumn.add(new SolidLabel(
					getWiki().getGUIText("acewiki_references_empty"),
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
					r.add(new SolidLabel(getWiki().getGUIText("acewiki_list_continued"), Font.ITALIC, 10));
				}
				referenceColumn.add(new VSpace());
				referenceColumn.add(r);
			}
			Row r = new Row();
			r.add(new SentenceComponent(s, this));
			referenceColumn.add(r);
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == indexBar) {
			chosenPage = Integer.parseInt(e.getActionCommand()) - 1;
			log("page", "pressed: page " + (chosenPage+1));
			updatePage();
		} else if (e.getSource() == title) {
			getWiki().showEditorWindow(ontologyElement);
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof ReferencesPage) {
			return ontologyElement.equals(((ReferencesPage) obj).ontologyElement);
		}
		return false;
	}
	
	public boolean isExpired() {
		return !getWiki().getOntology().contains(ontologyElement);
	}
	
	public String toString() {
		return "-REF- " + ontologyElement.getWord();
	}

}
