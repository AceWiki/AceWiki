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

import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;

/**
 * This class represents a column of menu blocks.
 * 
 * @author Tobias Kuhn
 */
class MenuBlockColumn {
	
	private PreditorWindow preditor;
	private List<MenuBlockContent> contents = new ArrayList<MenuBlockContent>();
	private int heightUnits, weight;
	
	/**
	 * Creates a new menu block column.
	 * 
	 * @param preditor The predictive editor object.
	 */
	private MenuBlockColumn(PreditorWindow preditor) {
		this.preditor = preditor;
	}
	
	/**
	 * Creates a new menu block column with one menu block.
	 * 
	 * @param content The content of the menu block.
	 * @param preditor The predictive editor object.
	 */
	public MenuBlockColumn(MenuBlockContent content, PreditorWindow preditor) {
		this.preditor = preditor;
		contents.add(content);
		updateValues();
	}
	
	/**
	 * Includes the menu blocks of the other column at the bottom of this one.
	 * 
	 * @param other The other menu block column.
	 */
	public void includeAtBottom(MenuBlockColumn other) {
		this.contents.addAll(other.contents);
		updateValues();
	}
	
	/**
	 * Includes the menu blocks of the other column at the top of this one.
	 * 
	 * @param other The other menu block column.
	 */
	public void includeAtTop(MenuBlockColumn other) {
		this.contents.addAll(0, other.contents);
		updateValues();
	}
	
	/**
	 * Returns two menu block columns which contain the splitted content of this one. This object
	 * remains unchanged. The trend value indicates how to split odd numbers of menu blocks.
	 * 
	 * @param trend If positive, odd numbers are split in a way that the first column has more menu
	 *     block, and vice versa for negative values.
	 * @return Two menu blocks in an array.
	 */
	public MenuBlockColumn[] getSplitted(int trend) {
		 MenuBlockColumn c1 = new MenuBlockColumn(preditor);
		 MenuBlockColumn c2 = new MenuBlockColumn(preditor);
		 int threshold;
		 if (trend > 0) {
			 threshold = (int) ((contents.size() + 0.5) / 2);
		 } else {
			 threshold = contents.size() / 2;
		 }
		 for (int i=0 ; i < contents.size() ; i++) {
			 if (i < threshold) {
				 c1.contents.add(contents.get(i));
			 } else {
				 c2.contents.add(contents.get(i));
			 }
		 }
		 c1.updateValues();
		 c2.updateValues();
		 return new MenuBlockColumn[] {c1, c2};
	}
	
	private void updateValues() {
		int hu = 2 * contents.size();
		for (MenuBlockContent c : contents) {
			int u = c.getUnfilteredItemCount();
			if (u < 32) {
				hu += u;
			} else {
				hu += 32;
			}
		}
		heightUnits = hu;
		weight = heightUnits * contents.size();
	}
	
	/**
	 * Returns the number of height units necessary to optimally display this menu block column.
	 * Menu blocks with a height of more than 32 units only count as 32 units.
	 * 
	 * @return The number of height units.
	 */
	public int getHeightUnits() {
		return heightUnits;
	}
	
	/**
	 * This method returns the "weight" of this column. This value should quantify the complexity
	 * of this column.
	 * 
	 * @return The weight of this column.
	 */
	public int getWeight() {
		return weight;
	}
	
	/**
	 * Creates and returns the GUI component of this menu block column.
	 * 
	 * @param width The width of the column.
	 * @return The GUI component.
	 */
	public Column createGUI(int width) {
		Column column = new Column();
		column.setCellSpacing(new Extent(10));
		
		int totalUnits = 18;
		int unitsRest = totalUnits - (2 * contents.size());
		int freeUnits = unitsRest;
		int freeBlocksCount = contents.size();
		double avHeightUnits = (double) unitsRest / contents.size();
		boolean overfull = totalUnits - heightUnits < 0;
		
		boolean even = true;
		for (MenuBlockContent c : contents) {
			if (overfull && c.getUnfilteredItemCount() <= avHeightUnits) {
				even = false;
				break;
			} else if (!overfull && c.getUnfilteredItemCount() >= avHeightUnits) {
				even = false;
				break;
			}
		}
		
		if (!even && overfull) {
			List<MenuBlockContent> largeContents = new ArrayList<MenuBlockContent>(contents);
			boolean changed;
			do {
				changed = false;
				int i = 0;
				while (i < largeContents.size()) {
					MenuBlockContent c = largeContents.get(i);
					if (avHeightUnits > c.getUnfilteredItemCount()) {
						changed = true;
						freeUnits -= c.getUnfilteredItemCount();
						freeBlocksCount--;
						largeContents.remove(c);
					} else {
						i++;
					}
				}
				avHeightUnits = (double) freeUnits / freeBlocksCount;
			} while (changed);
		} else if (!even) {
			freeUnits = totalUnits - heightUnits;
		}
		
		for (int i = 0 ; i < contents.size() ; i++) {
			MenuBlockContent c = contents.get(i);
			int cs = preditor.getMenuCreator().getColorShift(c.getName());
			int heightUnits;
			boolean isLast = (i == contents.size()-1);
			if (isLast) {
				heightUnits = unitsRest;
			} else if (even) {
				heightUnits = (int) ((double) freeUnits / freeBlocksCount + 0.999);
				freeUnits -= heightUnits;
				freeBlocksCount--;
			} else if (overfull) {
				if (avHeightUnits > c.getUnfilteredItemCount()) {
					heightUnits = c.getUnfilteredItemCount();
				} else {
					heightUnits = (int) ((double) freeUnits / freeBlocksCount + 0.999);
					freeUnits -= heightUnits;
					freeBlocksCount--;
				}
			} else {
				int extraCols = (int) ((double) freeUnits / (contents.size() - i) + 0.999);
				heightUnits = c.getUnfilteredItemCount() + extraCols;
				freeUnits -= extraCols;
			}
			unitsRest -= heightUnits;
			MenuBlock mb = new MenuBlock(width, heightUnits * 15, cs, preditor);
			mb.setContent(c);
			column.add(mb);
		}
		
		return column;
	}
	
	/**
	 * This method calculates the combined weight of two menu block columns.
	 * 
	 * @param c1 Menu block column number 1.
	 * @param c2 Menu block column number 2.
	 * @return The combined weight.
	 */
	public static int getCombinedWeight(MenuBlockColumn c1, MenuBlockColumn c2) {
		int h = c1.heightUnits + c2.heightUnits;
		int s = c1.contents.size() + c2.contents.size();
		return h * s;
	}

}
