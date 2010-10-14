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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.chartparser.Preterminal;

/**
 * This abstract class has to create the menu items for the predictive editor.
 * {@link MinimalMenuCreator} is a minimal implementation and {@link ExampleMenuCreator} is an
 * example of a more sophisticated (but still simple) implementation.
 * 
 * @author Tobias Kuhn
 */
public abstract class MenuCreator {
	
	private List<String> menuGroups = new ArrayList<String>();
	private List<String> unsortedMenuGroups = new ArrayList<String>();
	private boolean menuEntryCheckEnabled = true;
	
	/**
	 * This abstract method must be implemented by subclasses and should return a list of menu
	 * items for the situation described by the given next token options. As long as the menu
	 * entry check is enabled (which is the default), this method can also return menu entries that
	 * are actually not compliant with the given options and these menu entries are filtered out
	 * automatically.
	 * 
	 * @param options The options according to which the menu items should be generated.
	 * @return A list of menu items to be shown in the predictive editor.
	 * @see #setMenuEntryCheckEnabled
	 */
	public abstract List<MenuItem> getMenuItems(NextTokenOptions options);
	
	/**
	 * This method initializes a menu group. A menu group has a unique name and the items of a menu
	 * group can be sorted or unsorted. The order of menu group initialization defines the order in
	 * which they are shown in the predictive editor. Menu groups can also be used without
	 * initialization with the result that they appear in an undefined order.
	 * 
	 * @param name The unique name of the menu group.
	 * @param sorted true if the items of the menu group should be sorted.
	 */
	public void initializeMenuGroup(String name, boolean sorted) {
		if (menuGroups.contains(name)) return;
		menuGroups.add(name);
		if (!sorted) {
			unsortedMenuGroups.add(name);
		}
	}
	
	/**
	 * Returns true if the returned menu entries of <code>getMenuItems</code> undergo an additional
	 * check for compliance with the next token options. If this menu entry check is disabled then
	 * the implementation of <code>getMenuItems</code> is responsible for returning only valid menu
	 * entries.
	 * 
	 * @return true if the menu entries are checked
	 */
	public boolean isMenuEntryCheckEnabled() {
		return menuEntryCheckEnabled;
	}
	
	/**
	 * Defines whether menu entries returned by <code>getMenuItems</code> should undergo an
	 * additional check for compliance with the next token options. If this menu entry check is
	 * disabled then the implementation of <code>getMenuItems</code> is responsible for returning only
	 * valid menu entries.
	 * 
	 * @param enabled Defines whether the menu entry check should be performed.
	 */
	public void setMenuEntryCheckEnabled(boolean enabled) {
		this.menuEntryCheckEnabled = enabled;
	}
	
	/**
	 * Creates the menu block contents on the basis of the given options for the next token.
	 * 
	 * @param options The options for the next token.
	 * @return A list of menu block contents.
	 */
	List<MenuBlockContent> createMenu(NextTokenOptions options) {
		HashMap<String, MenuBlockContent> contentsMap = new HashMap<String, MenuBlockContent>();
		
		List<MenuItem> menuItems = getMenuItems(options);
		for (MenuItem m : menuItems) {
			if (!menuEntryCheckEnabled || isPossibleMenuItem(m, options)) {
				addMenuItem(m, contentsMap);
			}
		}
		
		List<MenuBlockContent> contentsList = new ArrayList<MenuBlockContent>();
		
		for (String mg : menuGroups) {
			if (contentsMap.containsKey(mg)) {
				contentsList.add(contentsMap.get(mg));
			}
		}
		
		for (String mg : contentsMap.keySet()) {
			if (!menuGroups.contains(mg)) {
				contentsList.add(contentsMap.get(mg));
			}
		}
		
		return contentsList;
	}
	
	/**
	 * Returns true if the given menu item complies with the given next token options.
	 * 
	 * @param menuItem The menu item.
	 * @param options The options for the next token.
	 * @return true if the menu item complies with the options.
	 */
	private boolean isPossibleMenuItem(MenuItem menuItem, NextTokenOptions options) {
		if (menuItem instanceof MenuEntry) {
			TextElement te = ((MenuEntry) menuItem).getTextElement();
			for (Preterminal p : te.getCategories()) {
				if (p == null) {
					if (options.containsTerminal(te.getOriginalText())) {
						return true;
					}
				} else {
					if (options.allowsForCategory(p)) {
						return true;
					}
				}
			}
		} else {
			return true;
		}
		return false;
	}
	
	private void addMenuItem(MenuItem menuItem, Map<String, MenuBlockContent> contentsMap) {
		String menuGroup = menuItem.getMenuGroup();
		MenuBlockContent mbc;
		if (contentsMap.containsKey(menuGroup)) {
			mbc = contentsMap.get(menuGroup);
		} else {
			mbc = new MenuBlockContent(menuGroup, !unsortedMenuGroups.contains(menuGroup));
			contentsMap.put(menuGroup, mbc);
		}
		mbc.addItem(menuItem);
	}
	
}
