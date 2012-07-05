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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.base.TextContainer;

/**
 * This class represents declarative ACE sentences (asserted or inferred).
 * 
 * @author Tobias Kuhn
 */
public class ACEDeclaration extends ACESentence implements Declaration {

	/**
	 * Creates a new declarative ACE sentence.
	 * 
	 * @param serialized The serialized representation of the sentence.
	 */
	public ACEDeclaration(String serialized) {
		super(serialized);
	}

	/**
	 * Creates a new declarative ACE sentence.
	 * 
	 * @param textContainer The text container with the sentence text.
	 */
	public ACEDeclaration(TextContainer textContainer) {
		super(textContainer);
	}

}
