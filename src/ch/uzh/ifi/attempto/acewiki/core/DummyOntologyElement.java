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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This class represents a "dummy" ontology element, which cannot be part of ontological statements
 * but represents an article. This class is at the moment only used for the main page, which does
 * not represent a specific word (i.e. ontological entity) but is otherwise a normal article.
 * 
 * @author Tobias Kuhn
 */
public class DummyOntologyElement extends AbstractOntologyElement {
	
	String type;
	String text;
	
	/**
	 * Creates a new dummy ontology element.
	 * 
	 * @param type The type of the dummy ontology element.
	 * @param text The text of the dummy ontology element.
	 */
	public DummyOntologyElement(String type, String text) {
		this.type = type;
		this.text = text;
	}
	
	public String[] getWords() {
		return new String[] {};
	}
	
	public String getWord() {
		return null;
	}
	
	public String[] getHeadwords() {
		return new String[] {text};
	}
	
	public void setWords(String serializedWords) {
	}
	
	public String serializeWords() {
		return "";
	}
	
	public String getType() {
		return type;
	}
	
	public String getInternalType() {
		return type;
	}

}
