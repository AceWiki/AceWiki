// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.core.ontology;


/**
 * This class represents a comment that is a part of an article. A comment must have
 * an ontology element as owner.
 */
public class Comment extends Statement {
	
	private final String text;
	
	/**
	 * Creates a new comment.
	 * 
	 * @param text The comment text.
	 * @param owner The owner ontology element.
	 */
	protected Comment(String text, OntologyElement owner) {
		super(owner);
		this.text = text;
	}
	
	public String getText() {
		return text;
	}
	
	String serialize() {
		return "c " + text.replaceAll("~", "~t").replaceAll("\\n", "~n") + "\n";
	}

}
