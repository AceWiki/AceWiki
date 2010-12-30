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

/**
 * This class is an examplary implementation of a menu creator. See the
 * <a href="{@docRoot}/src-html/ch/uzh/ifi/attempto/preditor/ExampleMenuCreator.html">source code</a>.
 * 
 * @author Tobias Kuhn
 */
public class ExampleMenuCreator extends DefaultMenuCreator {
	
	private static List<String> menuGroupOrdering;
	
	static {
		menuGroupOrdering = new ArrayList<String>();
		menuGroupOrdering.add("function word");
		menuGroupOrdering.add("noun");
		menuGroupOrdering.add("proper name");
		menuGroupOrdering.add("intransitive verb");
		menuGroupOrdering.add("transitive verb");
	}

	public MenuEntry createMenuEntry(ConcreteOption option) {
		String n = option.getCategoryName();
		if (n == null) {
			return new MenuEntry(option, "function word");
		} else if (n.equals("n")) {
			return new MenuEntry(option, "noun");
		} else if (n.equals("pn")) {
			return new MenuEntry(option, "proper name");
		} else if (n.equals("iv")) {
			return new MenuEntry(option, "intransitive verb");
		} else if (n.equals("tv")) {
			return new MenuEntry(option, "transitive verb");
		}
		return new MenuEntry(option, "function word");
	}
	
	public List<String> getMenuGroupOrdering() {
		return menuGroupOrdering;
	}

}
