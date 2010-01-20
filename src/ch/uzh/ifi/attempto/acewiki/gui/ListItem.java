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

package ch.uzh.ifi.attempto.acewiki.gui;

import ch.uzh.ifi.attempto.echocomp.Label;
import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.layout.RowLayoutData;

/**
 * This class represents a GUI component that is shown as a list item with a preceding "-".
 * 
 * @author Tobias Kuhn
 */
public class ListItem extends Row {
	
	private static final long serialVersionUID = 1214629285195105570L;

	/**
	 * Creates a new list item that contains the given components.
	 * 
	 * @param components The components of the list item.
	 */
	public ListItem(Component... components) {
		setCellSpacing(new Extent(2));
		Label l = new Label("-", Font.PLAIN, 11);
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		l.setLayoutData(layout);
		add(l);
		if (components.length == 1) {
			add(components[0]);
		} else {
			Column col = new Column();
			col.setCellSpacing(new Extent(2));
			Row row = new Row();
			for (Component c : components) {
				if (c == null) {
					col.add(row);
					row = new Row();
				} else {
					row.add(c);
				}
				col.add(row);
				add(col);
			}
		}
	}

}
