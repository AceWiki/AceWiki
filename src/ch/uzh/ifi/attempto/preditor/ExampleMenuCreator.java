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
import java.util.List;

import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;

/**
 * This class is an examplary implementation of a menu creator. See the
 * <a href="{@docRoot}/src-html/ch/uzh/ifi/attempto/preditor/ExampleMenuCreator.html">source code</a>.
 * 
 * @author Tobias Kuhn
 */
public class ExampleMenuCreator extends MenuCreator {
	
	/**
	 * Creates a new menu creator instance.
	 */
	public ExampleMenuCreator() {
		initializeMenuGroup("function word", true);
		initializeMenuGroup("noun", true);
		initializeMenuGroup("proper name", true);
		initializeMenuGroup("intransitive verb", true);
		initializeMenuGroup("transitive verb", true);
	}

	public List<MenuItem> getMenuItems(NextTokenOptions options) {
		List<MenuItem> menuItems = new ArrayList<MenuItem>();
		
		for (ConcreteOption o : options.getConcreteOptions()) {
			menuItems.add(new MenuEntry(o, "function word"));
		}
		
		if (options.containsPreterminal("n")) {
			menuItems.add(new MenuEntry("man", "n", "noun"));
			menuItems.add(new MenuEntry("woman", "n", "noun"));
			menuItems.add(new MenuEntry("human", "n", "noun"));
			menuItems.add(new MenuEntry("dog", "n", "noun"));
			menuItems.add(new MenuEntry("house", "n", "noun"));
			menuItems.add(new MenuEntry("car", "n", "noun"));
		}
		if (options.containsPreterminal("pn")) {
			menuItems.add(new MenuEntry("John", "pn", "proper name"));
			menuItems.add(new MenuEntry("Bill", "pn", "proper name"));
			menuItems.add(new MenuEntry("Mary", "pn", "proper name"));
			menuItems.add(new MenuEntry("Sue", "pn", "proper name"));
			menuItems.add(new MenuEntry("Tom", "pn", "proper name"));
			menuItems.add(new MenuEntry("Rick", "pn", "proper name"));
			menuItems.add(new MenuEntry("Paul", "pn", "proper name"));
		}
		if (options.containsPreterminal("iv")) {
			menuItems.add(new MenuEntry("waits", "iv", "intransitive verb"));
			menuItems.add(new MenuEntry("sleeps", "iv", "intransitive verb"));
			menuItems.add(new MenuEntry("works", "iv", "intransitive verb"));
			menuItems.add(new MenuEntry("eats", "iv", "intransitive verb"));
			menuItems.add(new MenuEntry("drinks", "iv", "intransitive verb"));
		}
		if (options.containsPreterminal("tv")) {
			menuItems.add(new MenuEntry("sees", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("knows", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("owns", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("uses", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("buys", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("sells", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("drives", "tv", "transitive verb"));
			menuItems.add(new MenuEntry("likes", "tv", "transitive verb"));
		}
		
		return menuItems;
	}

}
