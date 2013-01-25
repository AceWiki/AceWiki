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

import java.text.Collator;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

import ch.uzh.ifi.attempto.echocomp.LocaleResources;

/**
 * This class represents the default comparator to sort menu items in the menus of the predictive
 * editor. Prefixes can be set that are ignored for comparison. By default, these prefixes are
 * "the ", "a " and "and " plus their capizalized versions.
 * 
 * @author Tobias Kuhn
 */
public class DefaultMenuItemComparator implements Comparator<MenuItem> {
	
	private Set<String> prefixes = new HashSet<String>();
	private Collator collator;
	
	/**
	 * Creates a new default comparator for menu items.
	 */
	public DefaultMenuItemComparator() {
		collator = LocaleResources.getCollator();
		prefixes.add("the ");
		prefixes.add("The ");
		prefixes.add("a ");
		prefixes.add("A ");
		prefixes.add("an ");
		prefixes.add("An ");
	}

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
			return collator.compare(s1, s2);
		}
		
		// Certain prefixes are ignored for comparison:
		String s1n = null;
		String s2n = null;
		String p1 = "";
		String p2 = "";
		for (String p : prefixes) {
			if (s1.startsWith(p)) {
				if (s1n == null || s1n.length() > s1.length() - p.length()) {
					p1 = s1.substring(0, p.length());
					s1n = s1.substring(p.length());
				}
			}
			if (s2.startsWith(p)) {
				if (s2n == null || s2n.length() > s2.length() - p.length()) {
					p2 = s2.substring(0, p.length());
					s2n = s2.substring(p.length());
				}
			}
		}
		if (s1n != null) s1 = s1n;
		if (s2n != null) s2 = s2n;
		
		int comp;
		
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
			comp = i1 - i2;
		} else {
			comp = collator.compare(s1, s2);
		}
		
		if (comp == 0) {
			return collator.compare(p1, p2);
		} else {
			return comp;
		}
	}
	
	/**
	 * Adds a prefix.
	 * 
	 * @param prefix The prefix to be added.
	 */
	public void addPrefix(String prefix) {
		prefixes.add(prefix);
	}
	
	/**
	 * Sets the prefixes.
	 * 
	 * @param prefixes The set of prefixes.
	 */
	public void setPrefixes(Set<String> prefixes) {
		this.prefixes.clear();
		this.prefixes.addAll(prefixes);
	}

}
