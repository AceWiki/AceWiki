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

package ch.uzh.ifi.attempto.preditor;

import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This abstract class represents a menu item that performs an action when the user clicks on it.
 * 
 * @author Tobias Kuhn
 */
public class SpecialMenuItem extends MenuItem {

	private static final long serialVersionUID = -2762672905600512854L;

	/**
	 * Creates a new special menu item.
	 * 
	 * @param text The text of the menu item.
	 * @param menuGroup The menu group to which this item should be assigned.
	 * @param actionCommand The action command.
	 * @param actionListener The action listener.
	 */
	public SpecialMenuItem(String text, String menuGroup, String actionCommand, ActionListener actionListener) {
		super(menuGroup);
		if (text == null) text = "";
		setText(text);
		if (actionCommand == null) actionCommand = "";
		setActionCommand(actionCommand);
        setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(12)));
		setForeground(Style.mediumForeground);
		if (actionListener != null) {
			addActionListener(actionListener);
		}
    }
	
	/**
	 * Creates a new special menu item.
	 * 
	 * @param text The text of the menu item.
	 * @param menuGroup The menu group to which this item should be assigned.
	 * @param actionCommand The action command.
	 */
	public SpecialMenuItem(String text, String menuGroup, String actionCommand) {
		this(text, menuGroup, actionCommand, null);
    }

	/**
	 * Creates a new special menu item.
	 * 
	 * @param text The text of the menu item.
	 * @param menuGroup The menu group to which this item should be assigned.
	 */
	public SpecialMenuItem(String text, String menuGroup) {
		this(text, menuGroup, text, null);
    }

	/**
	 * Creates a new special menu item.
	 * 
	 * @param text The text of the menu item.
	 */
	public SpecialMenuItem(String text) {
		this(text, "", text, null);
	}
	
	protected String[] getContent() {
		return new String[] {"special", getText(), getMenuGroup(), getActionCommand()};
	}
    
}
