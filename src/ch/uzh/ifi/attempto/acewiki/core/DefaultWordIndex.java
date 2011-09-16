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

import java.util.Map;
import java.util.TreeMap;

/**
 * This class is the default implementation of a word index.
 * 
 * @author Tobias Kuhn
 */
//TODO Different ontology elements should be allowed to have overlapping word forms.
public class DefaultWordIndex implements WordIndex {
	
	private Map<String, OntologyElement> wordIndex = new TreeMap<String, OntologyElement>();
	
	public void elementAdded(OntologyElement element) {
		for (String word : element.getWords()) {
			if (word == null) continue;
			
			if (getElement(word) == null) {
				wordIndex.put(word, element);
			} else if (getElement(word) != element) {
				throw new RuntimeException(
						"Registration failed: The word '" + word + "' is already used."
					);
			}
		}
	}
	
	public void elementRemoved(OntologyElement element) {
		for (String word : element.getWords()) {
			if (word == null) continue;
			wordIndex.remove(word);
		}
	}
	
	
	public void elementBeforeChange(OntologyElement element) {
		for (String word : element.getWords()) {
			if (word != null) {
				wordIndex.remove(word);
			}
		}
	}
	
	
	public void elementAfterChange(OntologyElement element) {
		for (String word : element.getWords()) {
			if (word != null) {
				if (getElement(word) == null) {
					wordIndex.put(word, element);
				} else if (getElement(word) != element) {
					throw new RuntimeException(
							"Word update failed: The word '" + word + "' is already used."
						);
				}
			}
		}
	}
	
	
	public OntologyElement getElement(String word) {
		return wordIndex.get(word);
	}

}
