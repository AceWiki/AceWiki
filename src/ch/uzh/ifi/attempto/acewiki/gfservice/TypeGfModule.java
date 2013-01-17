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

import ch.uzh.ifi.attempto.acewiki.core.AbstractOntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.TechnicalElement;

/**
 * Page that represents a GF grammar module
 *
 * @author Kaarel Kaljurand
 */
public class TypeGfModule extends AbstractOntologyElement implements TechnicalElement {

	public static final String TYPE = "GF Module";
	public static final String INTERNAL_TYPE = "gfmodule";

	private String mWord = "";

	public TypeGfModule() {
	}

	public String[] getWords() {
		return new String[] {mWord};
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		mWord = words[0];
	}

	public String serializeWords() {
		return mWord + ";";
	}

	public String getIRISuffix() {
		return getWord(0);
	}

	public String getType() {
		return TYPE;
	}

	public String getInternalType() {
		return INTERNAL_TYPE;
	}

	public static boolean hasType(String type) {
		return INTERNAL_TYPE.equals(type);
	}

}