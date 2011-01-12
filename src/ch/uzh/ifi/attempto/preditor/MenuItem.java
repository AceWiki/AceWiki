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

import nextapp.echo.app.Button;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This abstract class represents an item of a menu of the predictive editor. Each menu item
 * contains the name of the menu group it belongs to.
 * 
 * @author Tobias Kuhn
 */
public abstract class MenuItem extends Button {
	
	private static final long serialVersionUID = 6061341215846815821L;
	
	private String menuGroup;
	private String id;
	private boolean highlighted = false;
	private int colorShift;
	
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
        updateStyle();
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
     * This method determines whether the menu entry is highlighted or not. Hightlighted menu
     * entries are displayed in bold font and are shown in front of non-highlighted menu entries
     * in sorted lists.
     * 
     * @param highlighted true if this entry should be highlighted.
     */
    public void setHighlighted(boolean highlighted) {
    	this.highlighted = highlighted;
    	updateStyle();
    }
    
    /**
     * Returns whether this menu item is highlighted or not.
     * 
     * @return true if this menu item is highlighted.
     */
    public boolean isHighlighted() {
    	return highlighted;
    }
    
    /**
     * This methods sets the color shift that defines the color in which this menu item is to be
     * displayed.
     * 
     * @see MenuCreator#getColorShift
     * @param colorShift The color shift value.
     */
    public void setColorShift(int colorShift) {
    	int colorShiftDiff = colorShift - this.colorShift;
    	this.colorShift = colorShift;
    	setBackground(Style.shiftColor(super.getBackground(), colorShiftDiff));
    	setForeground(Style.shiftColor(super.getForeground(), colorShiftDiff));
    	setRolloverBackground(Style.shiftColor(super.getRolloverBackground(), colorShiftDiff));
    	setRolloverForeground(Style.shiftColor(super.getRolloverForeground(), colorShiftDiff));
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
	
	/**
	 * This method sets the style according to whether or not this menu item is highlighted.
	 */
	protected void updateStyle() {
		if (highlighted) {
			setFont(new Font(Style.fontTypeface, Font.BOLD, new Extent(12)));
		} else {
			setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(12)));
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
