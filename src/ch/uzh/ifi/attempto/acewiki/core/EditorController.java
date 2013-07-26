// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

/**
 * This class controls the behavior and content of the predictive editor.
 * 
 * @author Tobias Kuhn
 */
public class EditorController {
	
	private List<String> menuGroups = new ArrayList<String>();
	private List<String> extensibleCategories = new ArrayList<String>();
	private Map<String, String> wordTypesForCat = new HashMap<String, String>();
	private Map<String, Integer> wordNumbersForCat = new HashMap<String, Integer>();
	private Map<String, String> menuGroupsForCat = new HashMap<String, String>();
	private Map<String, Integer> colorShifts = new HashMap<String, Integer>();
	private String[] autocompleteTokens = new String[] {};
	private String defaultMenuGroup = "";
	
	/**
	 * Adds a menu group. The order of the method-calls define the order in which the menu groups
	 * will appear in the editor. Color shifts can be set to give the menu groups different colors.
	 * 
	 * @param menuGroup The name of the menu group to be added.
	 * @param colorShift The color shift.
	 */
	public void addMenuGroup(String menuGroup, int colorShift) {
		menuGroups.add(menuGroup);
		colorShifts.put(menuGroup, colorShift);
	}
	
	/**
	 * Sets the menu group for all words that have no menu group assigned otherwise.
	 * 
	 * @param defaultMenuGroup The name of the default menu group.
	 */
	public void setDefaultMenuGroup(String defaultMenuGroup) {
		this.defaultMenuGroup = defaultMenuGroup;
	}
	
	/**
	 * Returns all menu groups in the order they appear in the editor.
	 * 
	 * @return All menu groups.
	 */
	public List<String> getMenuGroups() {
		return menuGroups;
	}
	
	/**
	 * Returns a list of all extensible categories.
	 * 
	 * @return All extensible categories.
	 */
	public List<String> getExtensibleCategories() {
		return extensibleCategories;
	}
	
	/**
	 * Returns the color shift for the given menu group.
	 * 
	 * @param menuBlockName The name of the menu group.
	 * @return The color shift.
	 */
	public int getColorShift(String menuBlockName) {
		if (colorShifts.containsKey(menuBlockName)) {
			return colorShifts.get(menuBlockName);
		} else {
			return 0;
		}
	}
	
	/**
	 * Adds a grammatical category that is extensible. Extensible means that users can define
	 * new words in this category.
	 * 
	 * @param category The extensible category name.
	 * @param menuGroup The menu group for the category.
	 * @param type The type of the respective ontology elements.
	 * @param wordNumber The word number.
	 */
	public void addExtensibleCategory(String category, String menuGroup, String type,
			int wordNumber) {
		extensibleCategories.add(category);
		menuGroupsForCat.put(category, menuGroup);
		wordTypesForCat.put(category, type);
		wordNumbersForCat.put(category, wordNumber);
	}
	
	/**
	 * Adds a grammatical category that is not extensible.
	 * 
	 * @param category The category name.
	 * @param menuGroup The menu group for the category.
	 */
	public void addPlainCategory(String category, String menuGroup) {
		menuGroupsForCat.put(category, menuGroup);
	}
	
	/**
	 * Returns the menu group for the given category.
	 * 
	 * @param category The category name.
	 * @return The name of the menu group.
	 */
	public String getMenuGroup(String category) {
		String mg = menuGroupsForCat.get(category);
		if (mg != null) {
			return mg;
		} else {
			return defaultMenuGroup;
		}
	}
	
	/**
	 * Returns the word type for the given extensible category, or null if the category is not
	 * extensible.
	 * 
	 * @param category The category name.
	 * @return The word type.
	 */
	public String getWordType(String category) {
		return wordTypesForCat.get(category);
	}
	
	/**
	 * Returns the word number for the given extensible category, or null if the category is not
	 * extensible.
	 * 
	 * @param category The category name.
	 * @return The word number.
	 */
	public int getWordNumber(String category) {
		return wordNumbersForCat.get(category);
	}
	
	/**
	 * Sets the tokens that normally form the end of a sentence and that should be used to
	 * automatically complete sentences. These tokens are usually punctuation symbols like a dot or
	 * a question mark.
	 * 
	 * @param autocompleteTokens
	 */
	public void setAutocompleteTokens(String... autocompleteTokens) {
		this.autocompleteTokens = autocompleteTokens;
	}
	
	/**
	 * Returns the tokens used for autocompletion of sentences.
	 * 
	 * @return The autocompletion tokens.
	 */
	public String[] getAutocompleteTokens() {
		return autocompleteTokens;
	}

}
