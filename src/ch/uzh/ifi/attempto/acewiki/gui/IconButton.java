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

import nextapp.echo.app.Button;
import nextapp.echo.app.Extent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;

/**
 * This class represents a button consisting of an icon, for example forward and backward arrows to
 * navigate through the wiki history.
 * 
 * @author Tobias Kuhn
 */
public class IconButton extends Button {
	
	private static final long serialVersionUID = 9007778082893227249L;

	/**
	 * Creates a new icon button. The name has to correspond to the name of the icon image files
	 * and is also shown as a tooltip.
	 * 
	 * @param name The name of the icon button.
	 * @param actionListener The action listener for this button.
	 */
	public IconButton(String name, ActionListener actionListener) {
		setRolloverEnabled(true);
		setWidth(new Extent(25));
		setHeight(new Extent(25));
		String filename = name.toLowerCase();
		setIcon(Wiki.getImage(filename + ".png"));
		setRolloverIcon(Wiki.getImage(filename + "h.png"));
		setDisabledIcon(Wiki.getImage(filename + "i.png"));
		setActionCommand(name);
		setToolTipText(name);
		addActionListener(actionListener);
	}

}
