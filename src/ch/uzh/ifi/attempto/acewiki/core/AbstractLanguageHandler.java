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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.HashMap;
import java.util.Map;

/**
 * This is a partial implementation of a language handler.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractLanguageHandler implements LanguageHandler {

	private Map<String, LexiconChanger> lexiconChangers = new HashMap<String, LexiconChanger>();

	/**
	 * Sets a lexicon changer for the given lexical type.
	 * 
	 * @param type The lexical type.
	 * @param lexiconChanger The lexicon changer.
	 */
	public void setLexiconChanger(String type, LexiconChanger lexiconChanger) {
		lexiconChangers.put(type, lexiconChanger);
	}
	
	public LexiconChanger getLexiconChanger(String type) {
		return lexiconChangers.get(type);
	}
	
	public SentenceSuggestion getSuggestion(Sentence sentence) {
		return null;
	}

}
