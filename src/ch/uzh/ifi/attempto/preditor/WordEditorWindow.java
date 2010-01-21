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

package ch.uzh.ifi.attempto.preditor;

import java.util.ArrayList;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ChangeEvent;
import nextapp.echo2.app.event.ChangeListener;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import echopointng.ButtonEx;
import echopointng.TabbedPane;
import echopointng.tabbedpane.DefaultTabModel;

/**
 * This class represents a word editor that can be used to create or modify words. It can contain
 * several tabs for several types of words.
 * 
 * @author Tobias Kuhn
 */
public class WordEditorWindow extends WindowPane implements ChangeListener {
	
	private static final long serialVersionUID = 6805275173727379038L;
	
	private String type;
	private TabbedPane tabbedPane = new TabbedPane();
	private ArrayList<WordEditorForm> tabs = new ArrayList<WordEditorForm>();
	
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
		
		tabbedPane.setOutsets(new Insets(10));
		tabbedPane.setTabPlacement(Alignment.TOP);
		tabbedPane.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		tabbedPane.setHeight(new Extent(height-103));
		tabbedPane.getSelectionModel().addChangeListener(this);
		
		add(tabbedPane);
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
		DefaultTabModel tabModel = (DefaultTabModel) tabbedPane.getModel();
		ButtonEx tab = new ButtonEx(form.getTitle(), null);
		tab.setStyle(DefaultTabModel.DEFAULT_TOP_ALIGNED_STYLE);
		tab.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		tab.setInsets(new Insets(5, 2));
		tabModel.addTab(tab, form);
		tabs.add(form);
		doFocus();
	}
	
	/**
	 * Returns the form of the currently selected tab.
	 * 
	 * @return The current tab.
	 */
	public WordEditorForm getCurrentTab() {
		return tabs.get(tabbedPane.getSelectedIndex());
	}
	
	private void doFocus() {
		int i = tabbedPane.getSelectedIndex();
		if (i < tabs.size()) {
			doFocus(tabs.get(i));
		}
	}
	
	private boolean doFocus(Component c) {
		if (c instanceof TextField) {
			ApplicationInstance.getActive().setFocusedComponent((TextField) c);
			return true;
		} else {
			for (Component child : c.getComponents()) {
				boolean b = doFocus(child);
				if (b) return true;
			}
		}
		return false;
	}
	
	public void stateChanged(ChangeEvent e) {
		doFocus();
	}
	
}
