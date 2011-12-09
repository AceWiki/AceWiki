// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;

/**
 * This is a convenience class for easy creation of text area.
 * 
 * @author Tobias Kuhn
 */
public class TextArea extends nextapp.echo.app.TextArea {
	
	private static final long serialVersionUID = 8965038167453278878L;
	
	/**
	 * Creates a new text area.
	 * 
	 * @param width The width of the text area.
	 * @param height The height of the text area.
	 */
	public TextArea(int width, int height) {
		setWidth(new Extent(width));
		setHeight(new Extent(height));
		setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
		setBackground(Style.lightBackground);
		setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
	}
	
	/**
	 * Creates a new text area.
	 */
	public TextArea() {
		this(500, 100);
	}

}
