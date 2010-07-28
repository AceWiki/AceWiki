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

import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

/**
 * This page shows an error message. It is used if a page should be shown that does not
 * exist anymore, e.g. because its ontology element has been deleted.
 * 
 * @author Tobias Kuhn
 */
public class ErrorPage extends WikiPage {
	
	private static final long serialVersionUID = -3853876045940143810L;
	
	private String text;
	
	/**
	 * Creates a new error page.
	 * 
	 * @param wiki The wiki instance.
	 * @param text The error text.
	 */
	public ErrorPage(Wiki wiki, String text) {
		super(wiki, new Title("Error", true));
		this.text = text;
		
		Row textRow = new Row();
		textRow.setInsets(new Insets(10, 10, 10, 15));
		textRow.add(new SolidLabel(text, Font.ITALIC));
		add(textRow);
	}

	public boolean equals(Object obj) {
		if (obj instanceof ErrorPage) {
			return text.equals(((ErrorPage) obj).text);
		}
		return false;
	}
	
	public String toString() {
		return "-ERROR-";
	}

}
