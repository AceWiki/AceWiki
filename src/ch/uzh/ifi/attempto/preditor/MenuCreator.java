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

import java.util.Comparator;
import java.util.List;

import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;

/**
 * This interface represents an object that can create the menus for the predictive editor.
 * {@link DefaultMenuCreator} is the menu creator used by default. {@link ExampleMenuCreator} is an
 * example of a more sophisticated (but still very simple) implementation.
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
	 * This method can return a comparator to define the order of the menu items within each menu
	 * group.
	 * 
	 * @return A comparator to compare menu items.
	 */
	public Comparator<MenuItem> getMenuItemComparator();
	
}
