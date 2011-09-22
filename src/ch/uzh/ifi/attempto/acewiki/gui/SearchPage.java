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

package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.WordIndex;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page on which the user can search for articles.
 * 
 * @author Tobias Kuhn
 */
public class SearchPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 7192145568847087174L;
	
	private static final int pageSize = 50;
	
	private int chosenPage = 0;
	private List<OntologyElement> searchResult;
	
	private Column resultColumn = new Column();
	private IndexBar indexBar;
	private TextField textField;
	
	/**
	 * Creates a new search page.
	 * 
	 * @param wiki The wiki instance.
	 * @param text The search text.
	 */
	public SearchPage(Wiki wiki, String text) {
		super(wiki);
		
		addTab("Main Page", this);
		addTab("Index", this);
		addSelectedTab("Search");
		addTab("About", this);
		
		add(new Title("Search", true));
		addHorizontalLine();
		add(new VSpace(15));
		
		addHeadline("Search word");
		add(new VSpace(10));
		
		Row textFieldRow = new Row();
		textFieldRow.setInsets(new Insets(10, 0));
		textFieldRow.setCellSpacing(new Extent(5));
		textFieldRow.add(textField = new TextField(this));
		textField.setWidth(new Extent(300));
		textField.addActionListener(this);
		textField.setText(text);
		textFieldRow.add(new GeneralButton("Search", this));
		add(textFieldRow);
		
		add(new VSpace(15));
		
		addHeadline("Results");
		add(new VSpace(10));
		
		indexBar = new IndexBar("Page:", 0, this);
		add(indexBar);
		
		resultColumn.setInsets(new Insets(10, 2, 5, 20));
		resultColumn.setCellSpacing(new Extent(2));
		add(resultColumn);
		
		update();
	}
	
	protected void doUpdate() {
		getWiki().getApplication().setFocusedComponent(textField);
		
		resultColumn.removeAll();
		if (textField.getText().length() == 0) {
			indexBar.setVisible(false);
			resultColumn.add(new SolidLabel("(enter a search text)", Font.ITALIC, 10));
			return;
		}
		
		WordIndex index = getWiki().getLanguageEngine().getWordIndex();
		searchResult = index.searchForElements(textField.getText());
		
		if (searchResult.size() == 0) {
			indexBar.setVisible(false);
			resultColumn.add(new SolidLabel("(nothing found)", Font.ITALIC, 10));
		} else {
			int i = ((searchResult.size()-1) / pageSize) + 1;
			if (chosenPage > i) chosenPage = 0;
			indexBar.setNumbers(i);
			indexBar.setActiveButton(chosenPage);
			updatePage();
		}
	}
	
	private void updatePage() {
		resultColumn.removeAll();
		
		indexBar.setVisible(searchResult.size() > pageSize);
		
		int max = searchResult.size();
		if (max > (chosenPage + 1) * pageSize) max = (chosenPage + 1) * pageSize;
		
		for (int i = chosenPage * pageSize; i < max; i++) {
			resultColumn.add(new ListItem(new WikiLink(searchResult.get(i), getWiki())));
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == indexBar) {
			chosenPage = Integer.parseInt(e.getActionCommand()) - 1;
			log("page", "pressed: page " + (chosenPage+1));
			updatePage();
		} else if ("Main Page".equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if ("Index".equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if ("About".equals(e.getActionCommand())) {
			getWiki().showAboutPage();
		} else {
			log("page", "search for " + textField.getText());
			update();
		}
	}
	
	public boolean equals(Object obj) {
		return obj instanceof SearchPage;
	}
	
	public String toString() {
		return "-SEARCH-";
	}

}
