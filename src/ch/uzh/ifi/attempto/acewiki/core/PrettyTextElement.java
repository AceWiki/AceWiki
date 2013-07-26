// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.core;

import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class represents a text element with pretty-printed text. This means that underscores are
 * shown as spaces.
 * 
 * @author Tobias Kuhn
 */
public class PrettyTextElement extends TextElement {

	/**
	 * Creates a new text element.
	 * 
	 * @param text The text.
	 */
	public PrettyTextElement(String text) {
		super(text);
	}

	/**
	 * Creates a new text element without initializing the fields of the object.
	 */
	protected PrettyTextElement() {
	}

	public String getText() {
		String t = super.getText();
		if (t == null) return null;
		return LanguageUtils.getPrettyPrinted(t);
	}

	/**
	 * Returns the text of this text element in its plain form where underscores are not
	 * replaces by blanks.
	 * 
	 * @return The plain text.
	 */
	public String getUnderscoredText() {
		return super.getText();
	}

}
