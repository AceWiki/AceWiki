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

import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;

/**
 * This class represents the start page of a wiki.
 * 
 * @author Tobias Kuhn
 */
public class StartPage extends ArticlePage {
	
	private static final long serialVersionUID = -1528040616289818728L;
	
	private OntologyElement oe;
	
	/**
	 * Creates a new start page.
	 * 
	 * @param wiki The wiki instance.
	 */
	public StartPage(Wiki wiki) {
		super(wiki, wiki.getOntology().get(0));
		
		this.oe = wiki.getOntology().get(0);
		getTitle().setText("Main Page");
		
		addSelectedTab("Main Page");
		addTab("Index", this);
		addTab("Search", this);
		addTab("About", this);
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if ("Index".equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if ("Search".equals(e.getActionCommand())) {
			getWiki().showSearchPage();
		} else if ("About".equals(e.getActionCommand())) {
			getWiki().showAboutPage();
		}
	}
	
	public OntologyElement getOntologyElement() {
		return oe;
	}

	public boolean equals(Object obj) {
		return obj instanceof StartPage;
	}
	
	public String toString() {
		return "-MAIN-";
	}

}
