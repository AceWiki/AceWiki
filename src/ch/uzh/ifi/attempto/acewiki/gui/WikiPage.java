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

import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This is the superclass of all page classes. It represents a wiki page of AceWiki.
 * 
 * @author Tobias Kuhn
 */
public abstract class WikiPage extends Column {
	
	private static final long serialVersionUID = -1972548696966691981L;
	
	private Wiki wiki;
	private TabRow tabRow;
	
	/**
	 * Initializes a new wiki page.
	 * 
	 * @param wiki The wiki instance.
	 */
	public WikiPage(Wiki wiki) {
		this.wiki = wiki;
		
		setInsets(new Insets(0, 0, 0, 40));
	}
	
	/**
	 * Checks whether the page still exists and updates the page content.
	 */
	public final void update() {
		if (isExpired()) {
			removeAll();
			add(new ErrorPage(wiki, wiki.getGUIText("acewiki_error_deletedpage")));
		} else {
			doUpdate();
		}
	}
	
	/**
	 * Updates the page content.
	 */
	protected void doUpdate() {}
	
	/**
	 * Returns the wiki instance this page belongs to.
	 * 
	 * @return The wiki instance.
	 */
	public Wiki getWiki() {
		return wiki;
	}
	
	/**
	 * Writes a log entry.
	 * 
	 * @param type The type of the log entry.
	 * @param text The log text.
	 */
	protected void log(String type, String text) {
		wiki.log(type, text);
	}
	
	/**
	 * Checks if the page has expired. A page has expired it represents an ontology
	 * element that has been deleted.
	 * 
	 * @return true if the page has expired.
	 */
	public boolean isExpired() {
		return false;
	}
	
	public void removeAll() {
		super.removeAll();
	}

	protected void setTabRow(TabRow tabRow) {
		if (tabRow == null || indexOf(this.tabRow) < 0) {
			add(tabRow, 0);
			add(new VSpace(20), 1);
		} else {
			remove(this.tabRow);
			add(tabRow, 0);
		}
		this.tabRow = tabRow;
	}
	
	/**
	 * Adds a horizontal line to the page content.
	 */
	protected void addHorizontalLine() {
		Column horizontalLine = new Column();
		horizontalLine.setInsets(new Insets(10, 0, 10, 0));
		Column c = new Column();
		c.setBackground(Color.DARKGRAY);
		c.setInsets(new Insets(0, 1, 0, 0));
		horizontalLine.add(c);
		add(horizontalLine);
	}
	
	/**
	 * Adds a headline to the page content.
	 * 
	 * @param text The headline text (or the text key for localization).
	 */
	protected void addHeadline(String text) {
		addHeadline(text, null);
	}
	
	/**
	 * Adds a headline to the page content. The component is shown after the headline text.
	 * 
	 * @param text The headline text (or the text key for localization).
	 * @param comp The component.
	 */
	protected void addHeadline(String text, Component comp) {
		Row headline = new Row();
		headline.setInsets(new Insets(10, 10, 10, 0));
		headline.setCellSpacing(new Extent(5));
		Label title = new Label(wiki.getGUIText(text));
		title.setFont(new Font(Style.fontTypeface, Font.ITALIC | Font.UNDERLINE, new Extent(13)));
		title.setLineWrap(false);
		headline.add(title);
		if (comp != null) {
			headline.add(comp);
		}
		add(headline);
	}
	
	/**
	 * Returns the heading text for the given ontology element.
	 * 
	 * @param oe The ontology element.
	 * @return The heading.
	 */
	protected String getHeading(OntologyElement oe) {
		return LanguageUtils.getHeading(oe);
	}

	public abstract boolean equals(Object obj);

}
