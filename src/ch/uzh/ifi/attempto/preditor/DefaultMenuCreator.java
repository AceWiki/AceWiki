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

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;

/**
 * This class is the default implementation of a menu creator.
 * 
 * @author Tobias Kuhn
 */
public class DefaultMenuCreator implements MenuCreator {
	
	private DefaultMenuItemComparator comparator = new DefaultMenuItemComparator();

	public MenuEntry createMenuEntry(ConcreteOption option) {
		return new MenuEntry(option, "word");
	}
	
	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options) {
		return Collections.emptyList();
	}

	public List<String> getMenuGroupOrdering() {
		return Collections.emptyList();
	}

	public Comparator<MenuItem> getMenuItemComparator() {
		return comparator;
	}

}
