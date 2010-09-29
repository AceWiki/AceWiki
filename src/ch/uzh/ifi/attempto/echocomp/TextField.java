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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.event.ActionListener;

/**
 * This is a convenience class for easy creation of text fields.
 * 
 * @author Tobias Kuhn
 */
public class TextField extends nextapp.echo2.app.TextField {
	
	private static final long serialVersionUID = 8965038167453278878L;

	/**
	 * Creates a new text field.
	 * 
	 * @param width The width of the text field.
	 * @param actionListener The action-listener of the text field.
	 * @param style The style of the text.
	 */
	public TextField(int width, ActionListener actionListener, int style) {
		setWidth(new Extent(width));
		setHeight(new Extent(17));
		setFont(new Font(Style.fontTypeface, style, new Extent(13)));
		setBackground(Style.lightBackground);
		setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		setDisabledBackground(Style.lightDisabled);
		if (actionListener != null) {
			addActionListener(actionListener);
		}
	}
	
	/**
	 * Creates a new text field.
	 * 
	 * @param width The width of the text field.
	 * @param actionListener The action-listener of the text field.
	 */
	public TextField(int width, ActionListener actionListener) {
		this(width, actionListener, Font.PLAIN);
	}
	
	/**
	 * Creates a new text field.
	 * 
	 * @param width The width of the text field.
	 */
	public TextField(int width) {
		this(width, null, Font.PLAIN);
	}
	
	/**
	 * Creates a new text field.
	 * 
	 * @param actionListener The action-listener of the text field.
	 */
	public TextField(ActionListener actionListener) {
		this(500, actionListener, Font.PLAIN);
	}
	
	/**
	 * Creates a new text field.
	 */
	public TextField() {
		this(500, null, Font.PLAIN);
	}

}
