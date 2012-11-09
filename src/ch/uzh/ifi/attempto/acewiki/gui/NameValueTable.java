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

import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;

/**
 * This class represents a graphical table that displays simple name/value pairs.
 * 
 * @author Tobias Kuhn
 */
public class NameValueTable extends Column {

	private static final long serialVersionUID = -5329037871133296963L;

	private Grid grid;

	/**
	 * Creates a new table.
	 */
	public NameValueTable() {
		grid = new Grid(2);
		grid.setInsets(new Insets(5, 1, 20, 2));
		grid.setBorder(new Border(1, Color.DARKGRAY, Border.STYLE_SOLID));
		grid.setBackground(new Color(240, 240, 240));
		add(grid);
	}

	/**
	 * Adds the name/value pair to the table. The value is displayed in italics.
	 * 
	 * @param name The name.
	 * @param value The value.
	 */
	public void addEntry(String name, String value) {
		grid.add(new SolidLabel(name, Font.ITALIC + Font.BOLD, 11));
		grid.add(new SolidLabel(value, Font.ITALIC));
	}


	/**
	 * Adds the name/value pair to the table.
	 * The name can be any component. The value is displayed in italics.
	 *
	 * @param name The name.
	 * @param value The value.
	 */
	public void addEntry(Component name, String value) {
		grid.add(name);
		grid.add(new SolidLabel(value, Font.ITALIC));
	}


	/**
	 * Adds the name/value pair to the table. The value is supposed to be an ACE phrase and is for
	 * that reason displayed in normal font (non-italics).
	 * 
	 * @param name The name.
	 * @param aceValue The ACE phrase as value.
	 */
	public void addACEEntry(String name, String aceValue) {
		grid.add(new SolidLabel(name, Font.ITALIC + Font.BOLD, 11));
		grid.add(new SolidLabel(aceValue));
	}

	/**
	 * Removes all entries.
	 */
	public void clear() {
		grid.removeAll();
	}

}
