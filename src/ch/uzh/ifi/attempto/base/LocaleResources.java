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

import java.util.HashMap;
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
	
	private LocaleResources() {}  // no instances allowed
	
	/**
	 * The default locale.
	 */
	public static final Locale defaultLocale = new Locale("en", "US");

	private static Map<String, ResourceBundle> bundles = new HashMap<String, ResourceBundle>();

	/**
	 * Returns the resource bundle from the respective properties-file.
	 * 
	 * @param baseName The base name referring to the properties file to be loaded.
	 * @param l The locale.
	 * @return The resource bundle.
	 */
	public static ResourceBundle getResourceBundle(String baseName, Locale l) {
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

	/**
	 * Returns the resource bundle from the respective properties-file for the default locale.
	 * 
	 * @param baseName The base name referring to the properties file to be loaded.
	 * @return The resource bundle.
	 */
	public static ResourceBundle getResourceBundle(String baseName) {
		return getResourceBundle(baseName, defaultLocale);
	}

	/**
	 * Returns a localized text.
	 * 
	 * @param baseName The base name referring to the properties file to be loaded.
	 * @param l The locale.
	 * @param key The key of the text item.
	 * @return The localized string.
	 */
	public static String getString(String baseName, Locale l, String key) {
		String s = getResourceBundle(baseName, l).getString(key);
		if (s == null) {
			s = getResourceBundle(baseName, defaultLocale).getString(key);
		}
		return s;
	}

	/**
	 * Returns a localized text for the default locale.
	 * 
	 * @param baseName The base name referring to the properties file to be loaded.
	 * @param key The key of the text item.
	 * @return The localized string.
	 */
	public static String getString(String baseName, String key) {
		return getString(baseName, defaultLocale, key);
	}

}
