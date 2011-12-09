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

package ch.uzh.ifi.attempto.preditor;

import java.util.Comparator;
import java.util.List;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;

/**
 * This interface represents an object that can create the menus for the predictive editor.
 * {@link DefaultMenuCreator} is the menu creator used by default.
 * 
 * @author Tobias Kuhn
 */
public interface MenuCreator {
	
	/**
	 * This method must create and return the menu entry object for the given concrete option.
	 *  described by the given next token options.
	 * 
	 * @param option The concrete option that represents a possible next token.
	 * @return A new menu entry object for the given option.
	 */
	public MenuEntry createMenuEntry(ConcreteOption option);
	
	/**
	 * This method should return the special menu items for the given situation (described by the
	 * next token options).
	 * 
	 * @param options The options for the next token.
	 * @return A list of special menu items to be shown.
	 */
	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options);
	
	/**
	 * This method can be used to define the ordering of the menu groups. Menu groups with names
	 * that are not contained in the list returned by this method appear in an undefined order. All
	 * other menu groups appear in the same order as in the list.
	 * 
	 * @return A list of menu group names.
	 */
	public List<String> getMenuGroupOrdering();
	
	/**
	 * This method should return the shift of the color to be used for the given menu block. A
	 * shift value of 120, for example, means a shift by 120 "degrees" towards violet. A shift of
	 * 360 is a full rotation and result in the original color.
	 * 
	 * @param menuBlockName The name of the menu block for which the color shift should be
	 *        returned.
	 * @return The color shift value.
	 */
	public int getColorShift(String menuBlockName);
	
	/**
	 * This method can return a comparator to define the order of the menu items within each menu
	 * group.
	 * 
	 * @return A comparator to compare menu items.
	 */
	public Comparator<MenuItem> getMenuItemComparator();
	
}
