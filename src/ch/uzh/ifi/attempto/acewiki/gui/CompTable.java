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

import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;

public class CompTable extends Column {

	private static final long serialVersionUID = -8481242896629521650L;
	private final Grid grid;

	public CompTable() {
		grid = new Grid(2);
		grid.setInsets(new Insets(4, 2, 8, 2));
		grid.setBorder(new Border(1, Color.DARKGRAY, Border.STYLE_SOLID));
		grid.setBackground(new Color(240, 240, 240));
		add(grid);
	}


	public void addEntry(String name, String value) {
		grid.add(new SolidLabel(name, Font.BOLD, 12));
		grid.add(new Label(value, Font.PLAIN, 12));
	}


	public void addEntry(Component name, String value) {
		grid.add(name);
		grid.add(new Label(value, Font.PLAIN, 12));
	}


	public void addEntry(String name, Component value) {
		grid.add(new SolidLabel(name, Font.BOLD, 12));
		grid.add(value);
	}


	public void clear() {
		grid.removeAll();
	}

}