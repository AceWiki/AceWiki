// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;

/**
 * This class is the default implementation of a menu creator.
 * 
 * @author Tobias Kuhn
 */
public class DefaultMenuCreator implements MenuCreator {
	
	private DefaultMenuItemComparator comparator = new DefaultMenuItemComparator();
	private Map<String, Integer> colors = new HashMap<String, Integer>();

	public MenuEntry createMenuEntry(ConcreteOption option) {
		return new MenuEntry(option, "word");
	}
	
	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options) {
		return Collections.emptyList();
	}

	public List<String> getMenuGroupOrdering() {
		return Collections.emptyList();
	}
	
	public int getColorShift(String menuBlockName) {
		if (colors.containsKey(menuBlockName)) {
			return colors.get(menuBlockName);
		} else {
			return 0;
		}
	}

    /**
     * This methods sets the color shift for the given menu block. It defines the color in which
     * the menu block is to be displayed.
     * 
     * @see MenuCreator#getColorShift
     * @param menuBlockName The name of the menu block for which the color shift should be set.
     * @param colorShift The color shift value.
     */
	public void setColorShift(String menuBlockName, int colorShift) {
		colors.put(menuBlockName, colorShift);
	}

	public Comparator<MenuItem> getMenuItemComparator() {
		return comparator;
	}

}
