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

import java.util.Comparator;

/**
 * This class represents the default comparator to sort menu items in the menus of the predictive
 * editor.
 * 
 * @author Tobias Kuhn
 */
public class DefaultMenuItemComparator implements Comparator<MenuItem> {

	public int compare(MenuItem m1, MenuItem m2) {
		String s1 = m1.getText();
		String s2 = m2.getText();
		
		// Special menu items come before other items:
		if (m1 instanceof SpecialMenuItem && !(m2 instanceof SpecialMenuItem)) {
			return -1;
		} else if (!(m1 instanceof SpecialMenuItem) && m2 instanceof SpecialMenuItem) {
			return 1;
		}
		
		// Highlighted items come before others:
		if (m1.isHighlighted() && !m2.isHighlighted()) {
			return -1;
		} else if (!m1.isHighlighted() && m2.isHighlighted()) {
			return 1;
		}
		
		// Special menu items are not examined further:
		if (m1 instanceof SpecialMenuItem && m2 instanceof SpecialMenuItem) {
			return s1.compareToIgnoreCase(s2);
		}
		
		// Certain prefixes are ignored for comparison:
		String[] prefixes = new String[] {"the ", "The ", "a ", "A ", "an ", "An "};
		for (String p : prefixes) {
			if (s1.startsWith(p)) {
				s1 = s1.substring(p.length());
			}
			if (s2.startsWith(p)) {
				s2 = s2.substring(p.length());
			}
		}
		
		// For items that are equal apart from trailing digits, the integer value of these trailing
		// digits is used for comparison:
		if (s1.replaceFirst("[0-9]*$", "").equals(s2.replaceFirst("[0-9]*$", ""))) {
			int i1 = 0;
			int i2 = 0;
			try {
				i1 = Integer.parseInt(s1.replaceFirst("^.*?([0-9]*)$", "$1"));
			} catch (NumberFormatException ex) {}
			try {
				i2 = Integer.parseInt(s2.replaceFirst("^.*?([0-9]*)$", "$1"));
			} catch (NumberFormatException ex) {}
			return i1 - i2;
		}
		
		// In all other cases, the texts are compared in the usual way:
		return s1.compareToIgnoreCase(s2);
	}

}
