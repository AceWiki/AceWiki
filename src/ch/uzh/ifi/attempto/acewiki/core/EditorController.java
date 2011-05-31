// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EditorController {
	
	private List<String> menuGroups = new ArrayList<String>();
	private List<String> extensibleCategories = new ArrayList<String>();
	private Map<String, String> wordTypesForCat = new HashMap<String, String>();
	private Map<String, Integer> wordNumbersForCat = new HashMap<String, Integer>();
	private Map<String, String> menuGroupsForCat = new HashMap<String, String>();
	private Map<String, Integer> colorShifts = new HashMap<String, Integer>();
	private String[] autocompleteTokens = new String[] {};
	private String defaultMenuGroup = "word";
	
	public List<String> getMenuGroups() {
		return menuGroups;
	}
	
	public void addMenuGroup(String menuGroup, int colorShift) {
		menuGroups.add(menuGroup);
		colorShifts.put(menuGroup, colorShift);
	}
	
	public List<String> getExtensibleCategories() {
		return extensibleCategories;
	}

	public String getMenuGroup(String c) {
		String mg = menuGroupsForCat.get(c);
		if (mg != null) {
			return mg;
		} else {
			return defaultMenuGroup;
		}
	}

	public String getWordType(String c) {
		return wordTypesForCat.get(c);
	}
	
	public int getWordNumber(String c) {
		return wordNumbersForCat.get(c);
	}
	
	public void addExtensibleCategory(String category, String menuGroup, String type,
			int wordNumber) {
		extensibleCategories.add(category);
		menuGroupsForCat.put(category, menuGroup);
		wordTypesForCat.put(category, type);
		wordNumbersForCat.put(category, wordNumber);
	}
	
	public void addPlainCategory(String category, String menuGroup) {
		menuGroupsForCat.put(category, menuGroup);
	}
	
	public int getColorShift(String menuBlockName) {
		if (colorShifts.containsKey(menuBlockName)) {
			return colorShifts.get(menuBlockName);
		} else {
			return 0;
		}
	}

	public String[] getAutocompleteTokens() {
		return autocompleteTokens;
	}
	
	public void setAutocompleteTokens(String... autocompleteTokens) {
		this.autocompleteTokens = autocompleteTokens;
	}
	
	public void setDefaultMenuGroup(String defaultMenuGroup) {
		this.defaultMenuGroup = defaultMenuGroup;
	}

}
