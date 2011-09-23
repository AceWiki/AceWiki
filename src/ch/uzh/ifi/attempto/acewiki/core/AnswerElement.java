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

import ch.uzh.ifi.attempto.base.TextContainer;

/**
 * This interface describes a text element with a specific text representation when it appears as
 * the answer to a question.
 * 
 * @author Tobias Kuhn
 */
public interface AnswerElement extends OntologyElement {
	
	/**
	 * Returns the text to be shown when this ontology element is the answer to a question.
	 * 
	 * @return The answer text.
	 */
	public TextContainer getAnswerText();

}
