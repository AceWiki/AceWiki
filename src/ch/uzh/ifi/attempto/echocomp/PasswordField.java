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
import nextapp.echo2.app.event.ActionListener;

/**
 * This is a convenience class for easy creation of password fields.
 * 
 * @author Tobias Kuhn
 */
public class PasswordField extends nextapp.echo2.app.PasswordField {
	
	private static final long serialVersionUID = 7918679972682431845L;

	/**
	 * Creates a new password field.
	 */
	public PasswordField() {
		setWidth(new Extent(500));
		setHeight(new Extent(17));
		setBackground(Style.lightBackground);
		setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
	}
	
	/**
	 * Creates a new password field with the given action-listener.
	 * 
	 * @param actionListener The action-listener.
	 */
	public PasswordField(ActionListener actionListener) {
		this();
		addActionListener(actionListener);
	}

}
