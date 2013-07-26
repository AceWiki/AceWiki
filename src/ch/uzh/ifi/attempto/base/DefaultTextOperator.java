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

	public static final String punctuationChars = ".,:;?!";
	public static final String leftPunctuationChars = "¿¡";
	
	public TextElement createTextElement(String text) {
		return new TextElement(text);
	}

	public String getTextInContext(TextElement textElement, String preceding, String following) {
		return textElement.getOriginalText();
	}

	public List<String> splitIntoTokens(String text) {
		for (char c : punctuationChars.toCharArray()) {
			text = text.replaceAll("\\" + c + "\\s", " " + c + " ");
			text = text.replaceAll("\\" + c + "$", " " + c);
		}
		ArrayList<String> tokens = new ArrayList<String>(Arrays.asList(text.split(" ")));
		while (tokens.contains("")) tokens.remove("");
		return tokens;
	}
	
	public String getGlue(TextElement left, TextElement right) {
		if (isPunctuationChar(right.getText())) {
			return "";
		} else if (isLeftPunctuationChar(left.getText())) {
			return "";
		}
		return " ";
	}

	public static boolean isPunctuationChar(String c) {
		return c.matches("[" + punctuationChars + "]");
	}

	public static boolean isLeftPunctuationChar(String c) {
		return c.matches("[" + leftPunctuationChars + "]");
	}

	public static String firstCharToUpperCase(String s) {
		return s.substring(0, 1).toUpperCase() + s.substring(1);
	}

	public static String firstCharToLowerCase(String s) {
		return s.substring(0, 1).toLowerCase() + s.substring(1);
	}

}
