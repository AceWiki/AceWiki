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

package ch.uzh.ifi.attempto.preditor;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.ContentPane;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.WindowPane;
import nextapp.echo.extras.app.TabPane;
import nextapp.echo.extras.app.event.TabSelectionEvent;
import nextapp.echo.extras.app.event.TabSelectionListener;
import nextapp.echo.extras.app.layout.TabPaneLayoutData;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents a word editor that can be used to create or modify words. It can contain
 * several tabs for several types of words.
 * 
 * @author Tobias Kuhn
 */
public class WordEditorWindow extends WindowPane implements TabSelectionListener {
	
	private static final long serialVersionUID = 6805275173727379038L;
	
	private String type;
	private TabPane tabPane = new TabPane();
	
	/**
	 * Creates a new word editor window.
	 *<p>
	 * The type argument is for custom use and can be used to describe the type of word to be
	 * created. It can be retrieved later by the <code>getType</code>-method. Otherwise, the type
	 * argument has no effect and the value of the type argument is not shown in the GUI.
	 * 
	 * @param title The title of the window.
	 * @param type The type.
	 * @param width The width of the window (minimum is 400).
	 * @param height The height of the window (minimum is 250).
	 */
	public WordEditorWindow(String title, String type, int width, int height) {
		this.type = type;
		if (width < 400) width = 400;
		if (height < 250) height = 250;
		setModal(true);
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setWidth(new Extent(width));
		setHeight(new Extent(height));
		setResizable(false);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		tabPane.setInsets(new Insets(10, 12, 10, 0));
		tabPane.setTabPosition(TabPane.TAB_POSITION_TOP);
		tabPane.setBorder(new Border(0, Color.BLACK, Border.STYLE_SOLID));
		tabPane.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		tabPane.setTabActiveForeground(Color.BLACK);
		tabPane.setTabInactiveBackground(Style.shadedBackground);
		tabPane.setTabInactiveForeground(Style.darkDisabled);
		tabPane.setTabRolloverEnabled(true);
		tabPane.setTabRolloverForeground(Color.BLACK);
		tabPane.setTabRolloverBackground(Style.lightBackground);
		//tabPane.setTabHeight(new Extent(height-103));
		tabPane.addTabSelectionListener(this);
		
		ContentPane cPane = new ContentPane();
		cPane.setInsets(new Insets(10, 0));
		cPane.add(tabPane);
		
		add(cPane);
	}
	
	/**
	 * Creates a new word editor window.
	 * 
	 * @param title The title of the window.
	 * @param width The width of the window (minimum is 400).
	 * @param height The height of the window (minimum is 250).
	 */
	public WordEditorWindow(String title, int width, int height) {
		this(title, null, width, height);
	}
	
	/**
	 * Creates a new word editor window.
	 * 
	 * @param title The title of the window.
	 * @param type The type.
	 */
	public WordEditorWindow(String title, String type) {
		this(title, type, 753, 503);
	}
	
	/**
	 * Creates a new word editor window.
	 * 
	 * @param title The title of the window.
	 */
	public WordEditorWindow(String title) {
		this(title, null, 753, 503);
	}
	
	/**
	 * Returns the type of the word editor window or null if no type has been assigned.
	 * 
	 * @return The type.
	 */
	public String getType() {
		return type;
	}
	
	/**
	 * Adds a new tab containing the given word editor form.
	 * 
	 * @param form The form to be shown in a new tab.
	 */
	public void addTab(WordEditorForm form) {
		if (form.isHidden()) return;
		TabPaneLayoutData layout = new TabPaneLayoutData();
		layout.setTitle(form.getTitle());
		form.setLayoutData(layout);
		tabPane.add(form);
		if (tabPane.getComponentCount() == 1) {
			tabPane.setActiveTabIndex(0);
		}
		doFocus();
	}
	
	/**
	 * Returns the form of the currently selected tab.
	 * 
	 * @return The current tab.
	 */
	public WordEditorForm getCurrentTab() {
		return (WordEditorForm) tabPane.getComponent(tabPane.getActiveTabIndex());
	}
	
	private void doFocus() {
		getCurrentTab().doFocus();
	}
	
	public void tabSelected(TabSelectionEvent e) {
		doFocus();
	}
	
}
