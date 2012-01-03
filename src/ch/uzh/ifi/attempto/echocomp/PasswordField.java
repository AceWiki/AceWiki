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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.event.ActionListener;

/**
 * This is a convenience class for easy creation of password fields.
 * 
 * @author Tobias Kuhn
 */
public class PasswordField extends nextapp.echo.app.PasswordField {
	
	private static final long serialVersionUID = 7918679972682431845L;
	
	/**
	 * Creates a new password field.
	 * 
	 * @param width The width of the password field.
	 * @param actionListener The action-listener.
	 */
	public PasswordField(int width, ActionListener actionListener) {
		setWidth(new Extent(width));
		setHeight(new Extent(17));
		setBackground(Style.lightBackground);
		setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		setDisabledBackground(Style.lightDisabled);
		if (actionListener != null) {
			addActionListener(actionListener);
		}
	}
	
	/**
	 * Creates a new password field.
	 * 
	 * @param actionListener The action-listener.
	 */
	public PasswordField(ActionListener actionListener) {
		this(500, actionListener);
	}
	
	/**
	 * Creates a new password field.
	 * 
	 * @param width The width of the password field.
	 */
	public PasswordField(int width) {
		this(width, null);
	}

	/**
	 * Creates a new password field.
	 */
	public PasswordField() {
		this(500, null);
	}

}
