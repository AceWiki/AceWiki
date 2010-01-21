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

import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.chartparser.StringRef;

/**
 * This is a simple implementation of a context checker for English. The words "a" and
 * "an" are adapted accoring to the following word, e.g. "a","apple" becomes "an","apple", and "an",
 * "customer" becomes "a","customer". Furthermore, words can be capitalized if they are at sentence-
 * initial position. Capitalization can be turned on or off. In both cases exceptions can be defined
 * using the feature "capitalize" in the pre-terminal category (taken from the text element). If
 * exceptions are enabled and the value of "capitalize" is "true" or "false" then this overrides the
 * default capitalization behavior.
 * 
 * @author Tobias Kuhn
 */
public class EnglishContextChecker implements ContextChecker {

	private boolean defaultCapitalize, exceptionsEnabled;
	
	/**
	 * Creates a new English context checker.
	 * 
	 * @param defaultCapitalize true if words should be capitalized by default.
	 * @param exceptionsEnabled true if exceptions can be defined using the feature "capitalize".
	 */
	public EnglishContextChecker(boolean defaultCapitalize, boolean exceptionsEnabled) {
		setDefaultCapitalize(defaultCapitalize);
		setExceptionsEnabled(exceptionsEnabled);
	}
	
	/**
	 * Creates a new English context checker with no exceptions for capitalization.
	 * 
	 * @param defaultCapitalize true if words should be capitalized by default.
	 */
	public EnglishContextChecker(boolean defaultCapitalize) {
		setDefaultCapitalize(defaultCapitalize);
	}
	
	/**
	 * Returns whether words are capitalized by default.
	 * 
	 * @return true if words are capitalized by default.
	 */
	public boolean isDefaultCapitalize() {
		return defaultCapitalize;
	}
	
	/**
	 * Enables or disables the default capitalization.
	 * 
	 * @param defaultCapitalize true if words should be capitalized by default.
	 */
	public void setDefaultCapitalize(boolean defaultCapitalize) {
		this.defaultCapitalize = defaultCapitalize;
	}
	
	/**
	 * Returns whether capitalization exceptions are enabled.
	 * 
	 * @return true if capitalization exceptions are enabled.
	 */
	public boolean areExceptionsEnabled() {
		return exceptionsEnabled;
	}
	
	/**
	 * Enables or disables capitalization exceptions.
	 * 
	 * @param exceptionsEnabled true if exceptions the feature "capitalize" should be used to define
	 *   capitalization exceptions.
	 */
	public void setExceptionsEnabled(boolean exceptionsEnabled) {
		this.exceptionsEnabled = exceptionsEnabled;
	}

	public String getTextInContext(TextElement textElement, String precedingText, String followingText) {
		String text = textElement.getOriginalText();
		String t;
		boolean capitalize = false;
		if (precedingText == null || precedingText.matches("(\\.|\\?|\\!)")) {
			capitalize = defaultCapitalize;
			if (exceptionsEnabled) {
				boolean isException = false;
				if (textElement.getCategories() != null) {
					isException = true;
					for (Preterminal cat : textElement.getCategories()) {
						StringRef sr = cat.getFeature("capitalize");
						String s = null;
						if (sr != null) s = sr.getString();
						
						if (defaultCapitalize && !"false".equals(s)) {
							isException = false;
							break;
						} else if (!defaultCapitalize && !"true".equals(s)) {
							isException = false;
							break;
						}
					}
				}
				if (isException) {
					capitalize = !capitalize;
				}
			}
		}
		if (capitalize && text.length() > 0) {
			String f = text.substring(0, 1);
			t = f.toUpperCase() + text.substring(1);
		} else {
			t = text;
		}
		
		if (followingText != null && t.matches("(A|a)n?")) {
			if (ACEUtils.useIndefiniteArticleAn(followingText)) {
				t = t.substring(0, 1) + "n";
			} else {
				t = t.substring(0, 1);
			}
		}
		return t;
	}

}
