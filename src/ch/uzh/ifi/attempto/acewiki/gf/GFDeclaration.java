// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.AbstractSentence;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * 
 * @author Tobias Kuhn
 */
public class GFDeclaration extends AbstractSentence implements Declaration {
	
	private TextContainer textContainer = new TextContainer();
	
	/**
	 * Creates a new declaration statement with the given text.
	 * 
	 * @param text The declaration text.
	 */
	public GFDeclaration(String text) {
		for (String s : text.split("\\s+")) {
			textContainer.addElement(new TextElement(s));
		}
	}
	
	protected TextContainer getTextContainer(String language) {
		return textContainer;
	}
	
	public List<TextElement> getTextElements(String language) {
		return textContainer.getTextElements();
	}
	
	public boolean contains(OntologyElement e) {
		// TODO
		return false;
	}
	
	public List<SentenceDetail> getDetails(String language) {
		return null;
	}
	
	public boolean isReasonable() {
		return true;
	}
	
	public void update() {
	}
	
	public String serialize() {
		return textContainer.getText();
	}

}
