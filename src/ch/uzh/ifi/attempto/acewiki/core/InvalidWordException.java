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

package ch.uzh.ifi.attempto.acewiki.core;

import java.lang.Exception;

/**
 * This exception is thrown when the user tries to create or modify a word in an illegal way.
 * 
 * @author Tobias Kuhn
 */
public class InvalidWordException extends Exception {
	
	private static final long serialVersionUID = 74414500416995528L;

	/**
	 * Creates a new exception object.
	 * 
	 * @param text The exception text.
	 */
	public InvalidWordException(String text) {
		super(text);
	}

}
