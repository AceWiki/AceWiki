// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui.page;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionListener;

/**
 * This is the superclass of all page classes. It represents a wiki page of AceWiki.
 * 
 * @author Tobias Kuhn
 */
public abstract class WikiPage extends Column {
	
	private Wiki wiki;
	private Title title;
	private Row tabRow;
	
	/**
	 * Initializes a new wiki page.
	 * 
	 * @param wiki The wiki instance.
	 * @param title The title component.
	 */
	public WikiPage(Wiki wiki, Title title) {
		this.wiki = wiki;
		this.title = title;
		
		tabRow = new Row();
		tabRow.setInsets(new Insets(10, 0, 0, 0));
		add(tabRow);
		add(new VSpace(20));
		add(title);
		addHorizontalLine();
	}
	
	/**
	 * Checks whether the page still exists and updates the page content.
	 */
	public final void update() {
		if (isExpired()) {
			removeAll();
			add(new ErrorPage(wiki, "This article does no longer exist."));
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
	 * Returns the title component.
	 * 
	 * @return The title component.
	 */
	protected Title getTitle() {
		return title;
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
	
	/**
	 * Adds a new tab to the tab row.
	 * 
	 * @param tabName The name of the tab.
	 * @param actionCommand The action command that is given to the action event object.
	 * @param actionListener The actionlistener.
	 */
	protected void addTab(String tabName, String actionCommand, ActionListener actionListener) {
		SmallButton b = new SmallButton(tabName, actionListener, true);
		if (actionCommand != null) {
			b.setActionCommand(actionCommand);
		}
		tabRow.add(b);
		tabRow.add(new HSpace(8));
		tabRow.add(createTabSeparator());
		tabRow.add(new HSpace(8));
	}
	
	/**
	 * Adds a new tab to the tab row.
	 * 
	 * @param tabName The name of the tab.
	 * @param actionListener The actionlistener.
	 */
	protected void addTab(String tabName, ActionListener actionListener) {
		addTab(tabName, null, actionListener);
	}
	
	/**
	 * Adds a new tab to the tab row that is currently selected.
	 * 
	 * @param tabName The name of the tab.
	 */
	protected void addSelectedTab(String tabName) {
		tabRow.add(new SmallButton(tabName, null, false));
		tabRow.add(new HSpace(8));
		tabRow.add(createTabSeparator());
		tabRow.add(new HSpace(8));
	}
	
	private Button createTabSeparator() {
		Button tabSeparator = new Button();
		tabSeparator.setBorder(new Border(1, Color.DARKGRAY, Border.STYLE_SOLID));
		tabSeparator.setHeight(new Extent(12));
		return tabSeparator;
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
	 * @param text The headline text.
	 */
	protected void addHeadline(String text) {
		addHeadline(text, null);
	}
	
	/**
	 * Adds a headline to the page content. The component is shown after the headline text.
	 * 
	 * @param text The headline text.
	 * @param comp 
	 */
	protected void addHeadline(String text, Component comp) {
		Row headline = new Row();
		headline.setInsets(new Insets(10, 10, 0, 0));
		headline.setCellSpacing(new Extent(5));
		Label title = new Label(text);
		title.setFont(new Font(Style.fontTypeface, Font.ITALIC | Font.UNDERLINE, new Extent(13)));
		title.setLineWrap(false);
		headline.add(title);
		if (comp != null) {
			headline.add(comp);
		}
		add(headline);
	}

	public abstract boolean equals(Object obj);

}
