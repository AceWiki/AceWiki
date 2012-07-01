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

package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;

import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This is the text operator used to handle the GF service input/output.
 *
 * TODO: can this class be used to present the GF binding character (&+) on the
 * surface as an almost invisible dot that is glued to its neighbors?
 *
 * @author Kaarel Kaljurand
 */
public class GfTextOperator extends DefaultTextOperator {

	private static final String SPACE = " ";
	private static final String EMPTY = "";
	private static final String GF_BIND = "&+";
	private static final String GF_BIND_PRETTY = "·";

	public String getTextInContext(TextElement textElement, String preceding, String following) {
		String text = textElement.getOriginalText();
		/*
		if (GF_BIND.equals(text)) {
			return GF_BIND_PRETTY;
		}
		 */
		return text;
	}


	public List<String> splitIntoTokens(String text) {
		List<String> tokens = new ArrayList<String>();
		Iterables.addAll(tokens, Splitter.on(SPACE).omitEmptyStrings().split(text));
		return tokens;
	}


	public String getGlue(TextElement left, TextElement right) {
		/*
		// TODO: getOriginalText()
		if (GF_BIND.equals(right.getText()) || GF_BIND.equals(left.getText()) {
			return EMPTY;
		}
		// TODO: might not always be correct to glue these
		if (right.getText().matches("[.?!,;:]")) {
			return EMPTY;
		}
		 */
		return SPACE;
	}

}