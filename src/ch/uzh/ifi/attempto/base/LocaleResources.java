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

package ch.uzh.ifi.attempto.base;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * With this class, the text resources of different locales can be accessed.
 * 
 * @author Tobias Kuhn
 */
public class LocaleResources {
	
	/**
	 * The default locale.
	 */
	public static final Locale defaultLocale = new Locale("en", "US");

	private static Map<String, ResourceBundle> bundles = new HashMap<>();
	
	private static List<String> baseNames = new ArrayList<>();
	
	private LocaleResources() {}  // no instances allowed

	/**
	 * Loads a bundle, unless it is loaded already.
	 * 
	 * @param baseName The base name referring to the properties file to be loaded.
	 */
	public static void loadBundle(String baseName) {
		if (!baseNames.contains(baseName)) {
			baseNames.add(baseName);
		}
	}

	/**
	 * Returns a localized text.
	 * 
	 * @param l The locale.
	 * @param key The key of the text item.
	 * @return The localized string.
	 */
	public static String getString(Locale l, String key) {
		String s = null;
		if (l != null) {
			s = getResourceString(l, key);
		}
		if (isEmpty(s)) {
			s = getResourceString(defaultLocale, key);
		}
		if (isEmpty(s)) s = null;
		return s;
	}

	/**
	 * Returns a localized text for the default locale.
	 * 
	 * @param key The key of the text item.
	 * @return The localized string.
	 */
	public static String getString(String key) {
		return getString(defaultLocale, key);
	}
	
	private static String getResourceString(Locale l, String key) {
		String s = null;
		for (String n : baseNames) {
			s = getResourceString(n, l, key);
			if (!isEmpty(s)) return s;
		}
		return s;
	}
	
	private static String getResourceString(String baseName, Locale l, String key) {
		String s = null;
		try {
			s = getResourceBundle(baseName, l).getString(key);
		} catch (MissingResourceException ex) {}
		return s;
	}

	private static ResourceBundle getResourceBundle(String baseName, Locale l) {
		ResourceBundle r = bundles.get(baseName + " " + l);
		if (r == null) {
			try {
				r = ResourceBundle.getBundle(baseName, l);
			} catch (MissingResourceException ex) {
				r = ResourceBundle.getBundle(baseName, defaultLocale);
			}
			bundles.put(baseName + " " + l, r);
		}
		return r;
	}
	
	private static boolean isEmpty(String s) {
		return s == null || s.matches("\\s*");
	}

}
