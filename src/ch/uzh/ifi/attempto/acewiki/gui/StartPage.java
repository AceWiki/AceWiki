// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;

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
	}

	protected void doUpdate() {
		updateTextColumn();
		setTabRow(TabRow.getMainTabRow(TabRow.TAB_MAIN, getWiki()));
		getTitle().setText(getWiki().getGUIText("acewiki_page_main"));
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
