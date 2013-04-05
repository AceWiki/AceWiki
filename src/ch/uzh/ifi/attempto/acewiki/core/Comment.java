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

import java.util.Set;

import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

/**
 * This class represents a comment that is a part of an article. A comment must have
 * an ontology element as owner.
 */
public class Comment extends AbstractStatement {

	private final String text;

	/**
	 * Creates a new comment.
	 * 
	 * @param text The comment text.
	 */
	public Comment(String text) {
		this.text = text;
	}


	public String getText(String language) {
		// Comments are not multilingual at this point
		return text;
	}

	/**
	 * Returns the (language-independent) text of this comment.
	 * 
	 * @return The comment text.
	 */
	public String getText() {
		return text;
	}

	public String serialize() {
		return text.replaceAll("~", "~t").replaceAll("\\n", "~n");
	}


	public Set<OntologyElement> getReferencedElements() {
		return ImmutableSet.copyOf(makeReferencedElements(text));
	}


	private Set<OntologyElement> makeReferencedElements(String text) {
		Set<OntologyElement> set = Sets.newHashSet();
		Ontology ont = getOntology();
		if (ont == null) {
			return set;
		}
		for (String s : tokenizeText(text)) {
			OntologyElement oe = ont.getElement(s);
			if (oe != null) {
				set.add(oe);
			}
		}
		return set;
	}


	public static Iterable<String> tokenizeText(String text) {
		return Splitter.on("~b").split(text.replaceAll("([a-zA-Z0-9_-]+)", "~b$1~b"));
	}

	public Comment copyFor(Article article) {
		Comment c = new Comment(text);
		c.init(getOntology(), article);
		return c;
	}

}