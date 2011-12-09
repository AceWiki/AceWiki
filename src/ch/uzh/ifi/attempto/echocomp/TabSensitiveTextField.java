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

import nextapp.echo.app.event.ActionListener;

/**
 * This class represents a text field that fires an action not only when the enter key is hit, but
 * also when the tab key is pressed.
 * 
 * @author Tobias Kuhn
 */
public class TabSensitiveTextField extends TextField {

	private static final long serialVersionUID = -5635716805022247837L;
	
	/**
	 * Creates a new tab-sensitive text field.
	 * 
	 * @param actionListener The action-listener.
	 */
	public TabSensitiveTextField(ActionListener actionListener) {
		if (actionListener != null) {
			addActionListener(actionListener);
		}
	}

}
