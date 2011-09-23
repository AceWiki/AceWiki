// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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
import java.util.Arrays;
import java.util.List;

/**
 * This class is the default implementation of a text operator.
 * 
 * @author Tobias Kuhn
 */
public class DefaultTextOperator implements TextOperator {
	
	public TextElement createTextElement(String text) {
		return new TextElement(text);
	}

	public String getTextInContext(TextElement textElement, String preceding, String following) {
		return textElement.getOriginalText();
	}

	public List<String> splitIntoTokens(String text) {
		for (char c : ".,:;?!".toCharArray()) {
			text = text.replaceAll("\\" + c + "\\s", " " + c + " ");
			text = text.replaceAll("\\" + c + "$", " " + c);
		}
		ArrayList<String> tokens = new ArrayList<String>(Arrays.asList(text.split(" ")));
		while (tokens.contains("")) tokens.remove("");
		return tokens;
	}
	
	public String getGlue(TextElement left, TextElement right) {
		if (right.getText().matches("[.?!,;:]")) {
			return "";
		}
		return " ";
	}

}
