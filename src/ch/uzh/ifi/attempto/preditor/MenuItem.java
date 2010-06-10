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

package ch.uzh.ifi.attempto.preditor;

import ch.uzh.ifi.attempto.echocomp.Style;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;

/**
 * This abstract class represents an item of a menu of the predictive editor. Each menu item
 * contains the name of the menu group it belongs to.
 * 
 * @author Tobias Kuhn
 */
public abstract class MenuItem extends Button implements Comparable<MenuItem> {
	
	private static final long serialVersionUID = 6061341215846815821L;
	
	private String menuGroup;
	private String id;
	
	/**
	 * Initializes a new menu item.
	 * 
	 * @param menuGroup The menu group to which this item should be assigned.
	 */
	public MenuItem(String menuGroup) {
		if (menuGroup == null) menuGroup = "";
		this.menuGroup = menuGroup;
        setWidth(new Extent(146));
        setHeight(new Extent(15));
        setInsets(new Insets(2,0));
		setBackground(Style.mediumBackground);
		setForeground(Style.darkForeground);
		setRolloverEnabled(true);
		setRolloverForeground(Style.lightForeground);
		setRolloverBackground(Style.darkBackground);
        setLineWrap(false);
        setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(12)));
	}
	
	/**
	 * Returns the name of the menu group of this menu item.
	 * 
	 * @return The name of the menu group.
	 */
	public String getMenuGroup() {
		return menuGroup;
	}
	
	/**
	 * This method returns an identifier that is unique (within one instance of the predictive
	 * editor).
	 * 
	 * @return the identifier.
	 */
	public String getMenuItemID() {
		if (id == null) recalculateID();
		return id;
	}
	
	/**
	 * This method should be called internally whenever something changed that has an influence on
	 * the identifier.
	 */
	protected void recalculateID() {
		id = "";
		for (String s : getContent()) {
			id += s.replaceAll(":", "~:").replaceAll("~", "~~") + ":";
		}
	}

	public int compareTo(MenuItem m) {
		String s1 = getText();
		String s2 = m.getText();
		
		if (s1.startsWith("the ") || s1.startsWith("The ")) {
			s1 = s1.substring(4);
		}
		if (s2.startsWith("the ") || s2.startsWith("The ")) {
			s2 = s2.substring(4);
		}
		
		if (this instanceof SpecialMenuItem && m instanceof SpecialMenuItem) {
			return s1.compareToIgnoreCase(s2);
		} else if (this instanceof SpecialMenuItem) {
			return -1;
		} else if (m instanceof SpecialMenuItem) {
			return 1;
		}
		
		Integer i1 = null;
		Integer i2 = null;
		
		try {
			i1 = Integer.parseInt(s1);
		} catch (NumberFormatException ex) {}
		try {
			i2 = Integer.parseInt(s2);
		} catch (NumberFormatException ex) {}
		
		if (i1 == null && i2 == null) {
			return s1.compareToIgnoreCase(s2);
		} else if (i1 == null) {
			return 1;
		} else if (i2 == null) {
			return -1;
		} else {
			return i1 - i2;
		}
	}
	
	/**
	 * This method is used to calculate the unique identifier.
	 * 
	 * @return An array of strings that uniquely defines the menu item object.
	 */
	protected abstract String[] getContent();

	public boolean equals(Object obj) {
		if (obj instanceof MenuItem) {
			return getMenuItemID().equals(((MenuItem) obj).getMenuItemID());
		} else {
			return false;
		}
	}
        
}
