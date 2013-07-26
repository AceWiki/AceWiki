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

import java.awt.Font;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

/**
 * This class represents an index bar that shows either letters from A to Z or numbers. This
 * index bar is used to organize large amounts of entries.
 * 
 * @author Tobias Kuhn
 */
public class IndexBar extends Column implements ActionListener {

	private static final long serialVersionUID = -1496012516474073170L;

	private ActionListener actionListener;
	
	private SmallButton activeButton;
	private Row row;
	
	private void init(String text) {
		Row mainRow = new Row();
		mainRow.setInsets(new Insets(10, 2, 5, 2));
		mainRow.setCellSpacing(new Extent(5));
		mainRow.add(new SolidLabel(text, Font.ITALIC, 10));
		row = new Row();
		mainRow.add(row);
		add(mainRow);
	}
	
	/**
	 * Creates a new index bar showing letters from A to Z.
	 * 
	 * @param actionListener The actionlistener.
	 */
	public IndexBar(ActionListener actionListener) {
		init(LocaleResources.getString("acewiki_list_firstletter"));
		this.actionListener = actionListener;
		setLetters();
	}
	
	/**
	 * Creates a new index bar showing numbers from 1 to the specified number.
	 * 
	 * @param n The last number to be shown.
	 * @param text The text of text key for the label.
	 * @param actionListener The actionlistener.
	 */
	public IndexBar(int n, String text, ActionListener actionListener) {
		String l = LocaleResources.getString(text);
		if (l == null) l = text;
		init(l);
		this.actionListener = actionListener;
		setNumbers(n);
	}
	
	/**
	 * Creates a new index bar showing numbers from 1 to the specified number.
	 * 
	 * @param n The last number to be shown.
	 * @param actionListener The actionlistener.
	 */
	public IndexBar(int n, ActionListener actionListener) {
		this(n, "acewiki_list_page", actionListener);
	}
	
	/**
	 * Shows letters from A to Z.
	 */
	public void setLetters() {
		row.removeAll();
		char c = 'A';
		while (c <= 'Z') {
			SmallButton b = new SmallButton(new String(new char[] {c}), this, 12);
			b.setBorder(new Border(1, Color.WHITE, Border.STYLE_SOLID));
			row.add(b);
			c++;
		}
		activeButton = null;
		setActiveButton((SmallButton) row.getComponent(0));
	}
	
	/**
	 * Shows numbers from 1 to the specified number.
	 * 
	 * @param n The last number to be shown.
	 */
	public void setNumbers(int n) {
		row.removeAll();
		for (int i = 0; i < n; i++) {
			SmallButton b = new SmallButton((i+1) + "", this, 12);
			b.setBorder(new Border(1, Color.WHITE, Border.STYLE_SOLID));
			row.add(b);
		}
		activeButton = null;
		if (n > 0) setActiveButton((SmallButton) row.getComponent(0));
	}
	
	/**
	 * Sets the button (letter or number) at the given position as the currently active button.
	 * 
	 * @param i The position of the button.
	 */
	public void setActiveButton(int i) {
		if (i >= 0 && i < row.getComponentCount()) {
			setActiveButton((SmallButton) row.getComponent(i));
		}
	}
	
	private void setActiveButton(SmallButton button) {
		if (activeButton == button) return;
		if (activeButton != null) {
			activeButton.setBorder(new Border(1, Color.WHITE, Border.STYLE_SOLID));
		}
		activeButton = button;
		activeButton.setBorder(new Border(1, Color.DARKGRAY, Border.STYLE_SOLID));
	}

	public void actionPerformed(ActionEvent e) {
		setActiveButton((SmallButton) e.getSource());
		actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
	}

}
