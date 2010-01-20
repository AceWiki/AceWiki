// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.preditor;

/**
 * This class represents a context checker that is able to do small surface adaptations of a token
 * according to the surrounding tokens. E.g. in English "a" should become "an" in front of "apple".
 * 
 * @author Tobias Kuhn
 */
public interface ContextChecker {
	
	/**
	 * This method should return the adapted text of the text element if it occurs between the given
	 * tokens.
	 * 
	 * @param textElement The text element whose text should be adapted to the context.
	 * @param precedingText The preceding token.
	 * @param followingText The following token.
	 * @return The adapted text.
	 */
	public String getTextInContext(TextElement textElement, String precedingText, String followingText);
	
}
