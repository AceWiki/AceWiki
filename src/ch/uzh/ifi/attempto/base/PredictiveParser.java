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

import java.util.List;

/**
 * This interface describes an incremental parser capable of predicting the possible next tokens.
 * 
 * @author Tobias Kuhn
 */
public interface PredictiveParser {

	/**
	 * Adds the token to the end of the token sequence.
	 * 
	 * @param token The new token to be added.
	 */
	public void addToken(String token);

	/**
	 * Adds the tokens to the token list.
	 * 
	 * @param tokens The tokens to be added.
	 */
	public void addTokens(List<String> tokens);

	/**
	 * Removes the last token.
	 */
	public void removeToken();

	/**
	 * Removes all tokens in the current token sequence.
	 */
	public void removeAllTokens();

	/**
	 * Sets the given tokens. Existing tokens are removed.
	 * 
	 * @param tokens The tokens.
	 */
	public void setTokens(List<String> tokens);

	/**
	 * Returns the current token sequence.
	 * 
	 * @return The current token sequence.
	 */
	public List<String> getTokens();

	/**
	 * Returns the number of tokens of the current (partial) text.
	 * 
	 * @return The number of tokens.
	 */
	public int getTokenCount();

	/**
	 * This method returns the options for the next token to be added at the end.
	 * 
	 * @return The options describing the possible next tokens.
	 */
	public NextTokenOptions getNextTokenOptions();

	/**
	 * Returns whether the given token is a possible next token.
	 * 
	 * @param token The token.
	 * @return true if the token is a possible next token.
	 */
	public boolean isPossibleNextToken(String token);

	/**
	 * Returns true if the current token sequence is a complete statement.
	 * 
	 * @return true if the current token sequence is complete.
	 */
	public boolean isComplete();
	
	/**
	 * This method should return the token number to which the last token refers, if it is a
	 * reference like "it". -1 should be returned if the last token is not a reference, or if
	 * reference resolution is not implemented.
	 * 
	 * @return The token number to which the last token refers.
	 */
	public int getReference();

}
