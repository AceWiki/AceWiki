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

import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.TopicElement;

/**
 * This class represents a page for a topic element.
 * 
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public class GeneralPage extends ArticlePage {

	private static final long serialVersionUID = -7034483028750537141L;

	private final TopicElement mElement;

	/**
	 * Creates a new page.
	 * 
	 * @param element The topic element.
	 * @param wiki The wiki object.
	 */
	public GeneralPage(TopicElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;
	}

	public TopicElement getOntologyElement() {
		return mElement;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
	}

	protected void doUpdate() {
		super.doUpdate();
		getTitle().setText(getHeading(mElement));
	}

}
