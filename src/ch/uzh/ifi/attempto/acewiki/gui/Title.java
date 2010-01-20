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
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.layout.RowLayoutData;

/**
 * This class represents a title label which is used for article titles.
 * 
 * @author Tobias Kuhn
 */
public class Title extends Row {
	
	private static final long serialVersionUID = 7797492687936611323L;
	
	private Label titleLabel;
	
	/**
	 * Creates a new title which has two parts. The second part is shown in gray font.
	 * 
	 * @param text The text of the main part of the title.
	 * @param postTitle The text of the second part of the title in gray font.
	 */
	public Title(String text, String postTitle) {
		setInsets(new Insets(10, 5, 10, 5));
		Row row = new Row();
		row.setCellSpacing(new Extent(5));
		titleLabel = new Label(text);
		titleLabel.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(20)));
		row.add(titleLabel);
		SolidLabel postTitleLabel = new SolidLabel(postTitle);
		postTitleLabel.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(20)));
		postTitleLabel.setForeground(Color.DARKGRAY);
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		postTitleLabel.setLayoutData(layout);
		row.add(postTitleLabel);
		add(row);
	}
	
	/**
	 * Creates a new title.
	 * 
	 * @param text The text of the title.
	 * @param italic Defines whether the title text should be displayed in italics.
	 */
	public Title(String text, boolean italic) {
		setInsets(new Insets(10, 5, 5, 5));
		titleLabel = new Label(text);
		if (italic) {
			titleLabel.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(20)));
		} else {
			titleLabel.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(20)));
		}
		add(titleLabel);
	}
	
	/**
	 * Creates a new title.
	 * 
	 * @param text The title text.
	 */
	public Title(String text) {
		this(text, false);
	}
	
	/**
	 * Resets the title text.
	 * 
	 * @param text The title text.
	 */
	public void setText(String text) {
		titleLabel.setText(text);
	}
	
	/**
	 * Sets the foreground color of the title text.
	 * 
	 * @param color The foreground color.
	 */
	public void setColor(Color color) {
		titleLabel.setForeground(color);
	}
	
}
