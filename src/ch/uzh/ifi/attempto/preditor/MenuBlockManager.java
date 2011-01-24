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

package ch.uzh.ifi.attempto.preditor;

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Row;

/**
 * This is a helper class that takes care of the management and layout of the menu blocks for the
 * predictive editor.
 * 
 * @author Tobias Kuhn
 */
class MenuBlockManager {

	private List<MenuBlockContent> mbContents = new ArrayList<MenuBlockContent>();
	private List<MenuBlockColumn> mbColumns = new ArrayList<MenuBlockColumn>();
	private PreditorWindow preditor;
	private String filter;
	
	/**
	 * Creates a new menu block manager.
	 * 
	 * @param preditor The predictive editor object.
	 */
	public MenuBlockManager(PreditorWindow preditor) {
		this.preditor = preditor;
	}
	
	/**
	 * Returns the number of menu blocks.
	 * 
	 * @return The number of menu blocks.
	 */
	public int getMenuBlockCount() {
		return mbContents.size();
	}
	
	/**
	 * Adds a menu block with the given content.
	 * 
	 * @param m The content of the menu block to be added.
	 */
	public void addMenuBlockContent(MenuBlockContent m) {
		if (!m.isEmpty()) {
			mbContents.add(m);
		}
	}
	
	/**
	 * Removes all menu blocks.
	 */
	public void clear() {
		mbContents.clear();
	}
	
	/**
	 * Returns the number of menu entries.
	 * 
	 * @return The number of menu entries.
	 */
	public int getMenuEntryCount() {
		int c = 0;
		for (MenuBlockContent mc : mbContents) {
			c += mc.getEntryCount();
		}
		return c;
	}
	
	/**
	 * Sets the filter for the menu entries.
	 * 
	 * @param filter The filter to be set.
	 */
	public void setFilter(String filter) {
		if (filter == null) filter = "";
		filter = filter.replaceFirst("^\\s*", "").replaceFirst("\\s*$", "");
		
		for (MenuBlockContent c : mbContents) {
			c.setFilter(filter);
		}
		this.filter = filter;
	}
	
	/**
	 * Returns the current filter for the menu entries.
	 * 
	 * @return The filter.
	 */
	public String getFilter() {
		return filter;
	}
	
	/**
	 * Returns the longest possible string with which all menu entries start (after filtering). An
	 * empty string is returned if at least two entries start with a different character.
	 * 
	 * @return The start string.
	 */
	public String getStartString() {
		String startString = "";
		List<String> blockStartStrings = new ArrayList<String>();
		
		for (MenuBlockContent mc : mbContents) {
			String s = mc.getStartString();
			if (s != null) {
				blockStartStrings.add(s);
			}
		}
		
		if (blockStartStrings.isEmpty()) return null;
		
		String first = blockStartStrings.get(0);
		blockStartStrings.remove(0);
		
		if (blockStartStrings.isEmpty()) return first;
		
		for (int i = 0; i < first.length(); i++) {
			char c = first.charAt(i);
			boolean stop = false;
			for (String s : blockStartStrings) {
				if (s.length() <= i || s.charAt(i) != c) stop = true;
			}
			if (stop) break;
			startString += c;
		}
		
		return startString;
	}
	
	/**
	 * Creates an returns the GUI component showing all menu blocks.
	 * 
	 * @return The GUI component.
	 */
	public synchronized Component createGUI() {
		mbColumns.clear();
		for (MenuBlockContent c : mbContents) {
			mbColumns.add(new MenuBlockColumn(c, preditor));
		}
		condenseColumns();
		Row menuBlockRow = new Row();
		menuBlockRow.setCellSpacing(new Extent(9));
		int colCount = mbColumns.size();
		int width = ( 720 / ( colCount > 3 ? colCount : 3 ) ) - 12;
		for (MenuBlockColumn c : mbColumns) {
			menuBlockRow.add(c.createGUI(width));
		}
		return menuBlockRow;
	}
	
	/**
	 * This method condenses the columns by putting several menu blocks into the same column.
	 */
	private void condenseColumns() {
		int i = mbColumns.size()-1;
		while (i > 0 && mbColumns.size() > 3) {
			MergeStep step = new MergeStep(i-1);
			if (step.getHeightUnits() <= 18) {
				step.apply();
			}
			i--;
		}
		while (hasTooManyColumns()) {
			CondenseStep step = new MergeStep(0);
			for (int j = 1; j < mbColumns.size()-1; j++) {
				CondenseStep s = new MergeStep(j);
				if (s.getWeightDiff() < step.getWeightDiff()) {
					step = s;
				}
				s = new SplitStep(j);
				if (s.getWeightDiff() < step.getWeightDiff()) {
					step = s;
				}
			}
			step.apply();
		}
	}
	
	/**
	 * This method returns whether the number of columns has to be condensed further or not.
	 * 
	 * @return Whether there are still too many columns.
	 */
	private boolean hasTooManyColumns() {
		return mbContents.size() < mbColumns.size()*(mbColumns.size()-2);
	}
	
	
	/**
	 * This internal interface represents a step of condensing the columns.
	 */
	private interface CondenseStep {
		
		public int getWeightDiff();
		
		public void apply();
		
	}
	
	/**
	 * This internal class represents a condensing step of merging two columns.
	 */
	private class MergeStep implements CondenseStep {
		
		private MenuBlockColumn c1, c2;
		private int diff;
		private int heightUnits;
		
		MergeStep(int i) {
			c1 = mbColumns.get(i);
			c2 = mbColumns.get(i+1);
			diff = MenuBlockColumn.getCombinedWeight(c1, c2) - c1.getWeight() - c2.getWeight();
			heightUnits = c1.getHeightUnits() + c2.getHeightUnits();
		}
		
		public int getWeightDiff() {
			return diff;
		}
		
		public int getHeightUnits() {
			return heightUnits;
		}
		
		public void apply() {
			c1.includeAtBottom(c2);
			mbColumns.remove(c2);
		}
		
	}
	
	/**
	 * This internal class represents a condensing step of splitting one column by moving its menu
	 * blocks to the adjacent columns.
	 */
	private class SplitStep implements CondenseStep {

		private MenuBlockColumn c1, s, c2;
		private MenuBlockColumn[] splitted;
		private int diff;
		
		SplitStep(int i) {
			c1 = mbColumns.get(i-1);
			s = mbColumns.get(i);
			c2 = mbColumns.get(i+1);
			splitted = s.getSplitted(c2.getWeight() - c1.getWeight());
			int w1 = MenuBlockColumn.getCombinedWeight(c1, splitted[0]);
			int w2 = MenuBlockColumn.getCombinedWeight(c2, splitted[1]);
			diff = w1 + w2 - c1.getWeight() - s.getWeight() - c2.getWeight();
		}
		
		public int getWeightDiff() {
			return diff;
		}
		
		public void apply() {
			c1.includeAtBottom(splitted[0]);
			c2.includeAtTop(splitted[1]);
			mbColumns.remove(s);
		}
		
	}

}
