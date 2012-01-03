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

/**
 * This interface represents a change suggestion for a newly created sentence. The user can decide
 * whether to follow this suggestion or not. If the suggestion is followed, the sentence is
 * automatically changed.
 * 
 * @author Tobias Kuhn
 */
public interface SentenceSuggestion {
	
	/**
	 * Returns the message to be presented to the user. This message should explain the suggested
	 * change.
	 * 
	 * @return The message text.
	 */
	public String getMessage();
	
	/**
	 * Returns the options given to the user to change or not change the sentence.
	 * 
	 * @return The options.
	 */
	public String[] getOptions();
	
	/**
	 * Returns the possibly changed sentence accoring to the option chosen by the user.
	 * 
	 * @param option The chosen option.
	 * @return The sentence, possibly changed.
	 */
	public Sentence getSentence(String option);

}
