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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * This class represents the content of a menu block. The content consists of menu items.
 * 
 * @author Tobias Kuhn
 */
class MenuBlockContent {
	
	private List<MenuItem> items = new ArrayList<MenuItem>();
	private TreeMap<String, MenuEntry> entryMap = new TreeMap<String, MenuEntry>();
	private Collection<MenuEntry> filteredEntries = entryMap.values();
	private Map<String, MenuItem> idMap = new HashMap<String, MenuItem>();
	private String name;
	private String filter = "";
	private boolean isSorted = true;
	private Comparator<MenuItem> comparator;
	
	/**
	 * Creates a new menu block content object.
	 * 
	 * @param name The name of the menu block.
	 * @param comparator The comparator to be used to sort the menu items.
	 */
	public MenuBlockContent(String name, Comparator<MenuItem> comparator) {
		this.name = name;
		this.comparator = comparator;
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
		String id = item.getMenuItemID();
		if (!(item instanceof MenuEntry && idMap.containsKey(id)) && !idMap.containsKey(id)) {
			items.add(item);
			idMap.put(id, item);
			if (item instanceof MenuEntry) {
				MenuEntry entry = (MenuEntry) item;
				entryMap.put(entry.getTextElement().getText().toLowerCase(), entry);
			}
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
		return new ArrayList<MenuEntry>(filteredEntries);
	}
	
	/**
	 * Returns the number of menu entries (after filtering).
	 * 
	 * @return The number of menu entries.
	 */
	public int getEntryCount() {
		return filteredEntries.size();
	}
	
	/**
	 * Returns all menu items.
	 * 
	 * @return A list of all menu items.
	 */
	public List<MenuItem> getItems() {
		// TODO improve this method
		if (!isSorted && comparator != null) {
			Collections.sort(items, comparator);
		}
		if (filter.length() == 0) {
			return items;
		}
		
		List<MenuItem> filteredItems = new ArrayList<MenuItem>();
		for (MenuItem item : items) {
			if (item instanceof MenuEntry) {
				// TODO this is not so nice:
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
		return entryMap.get(entryText.toLowerCase());
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
		if (filter == null || filter.length() == 0) {
			this.filter = "";
			filteredEntries = entryMap.values();
		} else {
			this.filter = filter.toLowerCase();
			// TODO find a better way to do this without using "°":
			filteredEntries = entryMap.subMap(this.filter, this.filter + "°").values();
		}
	}

}
