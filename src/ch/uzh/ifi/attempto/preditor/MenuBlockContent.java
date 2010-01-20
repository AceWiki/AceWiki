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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * This class represents the content of a menu block. The content consists of menu items.
 * 
 * @author Tobias Kuhn
 */
class MenuBlockContent {
	
	private List<MenuItem> items = new ArrayList<MenuItem>();
	private String name;
	private String filter;
	private boolean doSort;
	private boolean isSorted = true;
	
	/**
	 * Creates a new menu block content object.
	 * 
	 * @param name The name of the menu block.
	 * @param doSort true if the content should be sorted.
	 */
	public MenuBlockContent(String name, boolean doSort) {
		this.name = name;
		this.doSort = doSort;
	}
	
	/**
	 * Add the menu items.
	 * 
	 * @param items The menu items to be added.
	 */
	public void addItems(List<MenuItem> items) {
		for (MenuItem item : items) {
			addItem(item);
		}
	}
	
	/**
	 * Adds the menu item.
	 * 
	 * @param item The menu item to be added.
	 */
	public void addItem(MenuItem item) {
		if (item instanceof MenuEntry && items.contains(item)) {
			MenuEntry m1 = (MenuEntry) item;
			MenuEntry m2 = (MenuEntry) items.get(items.indexOf(m1));
			m2.getTextElement().include(m1.getTextElement());
		} else if (!items.contains(item)) {
			items.add(item);
		}
		isSorted = false;
	}
	
	/**
	 * Returns the name of the menu block.
	 * 
	 * @return The name of the menu block.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns true if the content is empty.
	 * 
	 * @return true if the content is empty.
	 */
	public boolean isEmpty() {
		return items.isEmpty();
	}
	
	/**
	 * Returns all menu items that are menu entries.
	 * 
	 * @return A list of all menu entries.
	 */
	public List<MenuEntry> getEntries() {
		if (!isSorted && doSort) {
			sort();
		}
		List<MenuEntry> filteredEntries = new ArrayList<MenuEntry>();
		for (MenuItem item : items) {
			if (item instanceof MenuEntry) {
				MenuEntry entry = (MenuEntry) item;
				if (entry.getTextElement().getText().toLowerCase().startsWith(filter)) {
					filteredEntries.add(entry);
				}
			}
		}
		return filteredEntries;
	}
	
	/**
	 * Returns all menu items.
	 * 
	 * @return A list of all menu items.
	 */
	public List<MenuItem> getItems() {
		if (!isSorted && doSort) {
			sort();
		}
		if (filter == null || filter.length() == 0) {
			return items;
		}
		
		List<MenuItem> filteredItems = new ArrayList<MenuItem>();
		for (MenuItem item : items) {
			if (item instanceof MenuEntry) {
				if (((MenuEntry) item).getTextElement().getText().toLowerCase().startsWith(filter)) {
					filteredItems.add(item);
				}
			} else {
				filteredItems.add(item);
			}
		}
		return filteredItems;
	}
	
	/**
	 * Returns the menu entry with the given text or null if no such entry exists. If there are
	 * more than one matching entry then the first of them is returned. The look-up is not case
	 * sensitive.
	 * 
	 * @param entryText The text of the menu entry to look for.
	 * @return The menu entry.
	 */
	public MenuEntry getEntry(String entryText) {
		entryText = entryText.toLowerCase();
		for (MenuItem mi : getItems()) {
			if (mi instanceof MenuEntry) {
				MenuEntry me = (MenuEntry) mi;
				String t = me.getTextElement().getText().toLowerCase();
				if (t.equals(entryText)) return me;
			}
		}
		return null;
	}
	
	/**
	 * Returns a common string with maximal size such that each menu entry starts with the string.
	 * E.g. "ar" is returned if the menu entries are "architect", "archive" and "artist".
	 * 
	 * @return The common start string.
	 */
	public String getStartString() {
		String startString = null;
		for (MenuEntry entry : getEntries()) {
			String c = entry.getTextElement().getText().toLowerCase();
			if (startString == null) {
				startString = c;
			} else {
				for (int i = startString.length(); i >= 0; i--) {
					if (c.startsWith(startString.substring(0, i))) break;
					startString = startString.substring(0, i-1);
				}
			}
		}
		return startString;
	}
	
	/**
	 * Sets the filter. All entries that do not start with the given filter string become
	 * invisible.
	 * 
	 * @param filter The filter string.
	 */
	public void setFilter(String filter) {
		if (filter == null) {
			this.filter = null;
		} else {
			this.filter = filter.toLowerCase();
		}
	}
	
	private void sort() {
		Collections.sort(items);
	}

}
