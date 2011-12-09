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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.Collection;
import java.util.List;


import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.owl.OWLOntoElement;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

/**
 * This interface represents an OWL ontology element with lexical entries that define its ACE
 * representation.
 * 
 * @author Tobias Kuhn
 */
public interface ACEOWLOntoElement extends OntologyElement, OWLOntoElement {
	
	/**
	 * Returns the lexicon entries (one for each word form).
	 * 
	 * @return The lexicon entries.
	 */
	public List<LexiconEntry> getLexiconEntries();
	
	/**
	 * This method should collect the lexical rules of this ontology element for the given category
	 * name.
	 * 
	 * @param catName The category name.
	 * @param lexRules The lexical rules should be added to this collection.
	 */
	// TODO: Improve this.
	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules);

}
