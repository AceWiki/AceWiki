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
import java.util.List;

import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;

/**
 * This class is a minimal implementation of a menu creator. No dynamic lexicon entries are
 * generated. Menu items are only generated for the lexicon entries in the grammar. All menu items
 * are put into the same menu block. See the
 * <a href="{@docRoot}/src-html/ch/uzh/ifi/attempto/preditor/MinimalMenuCreator.html">source code</a>.
 * 
 * @author Tobias Kuhn
 */
public class MinimalMenuCreator extends MenuCreator {

	public List<MenuItem> getMenuItems(NextTokenOptions options) {
		List<MenuItem> menuItems = new ArrayList<MenuItem>();
		
		for (ConcreteOption o : options.getConcreteOptions()) {
			menuItems.add(new MenuEntry(o, "function word"));
		}
		
		return menuItems;
	}

}
