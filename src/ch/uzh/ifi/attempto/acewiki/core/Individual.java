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

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents an individual (in logic called "constant").
 * 
 * @author Tobias Kuhn
 */
public abstract class Individual extends OntologyElement {
	
	private List<Concept> conceptsCache;
	private long conceptsCacheStateID = -1;
	
	/**
	 * Calculates all concepts this individual belongs to.
	 * 
	 * @return A list of all concepts of this individual.
	 * @see Ontology#getConcepts(Individual)
	 */
	public synchronized List<Concept> getConcepts() {
		Ontology o = getOntology();
		if (conceptsCacheStateID != o.getStateID()) {
			conceptsCache = o.getConcepts(this);
			conceptsCacheStateID = o.getStateID();
		}
		return new ArrayList<Concept>(conceptsCache);
	}
	
	/**
	 * Returns the cached concepts or null if there are no cached concepts. The returned
	 * concepts might not be up-to-date.
	 * 
	 * @return A list of the cached concepts of this individual.
	 */
	public List<Concept> getCachedConcepts() {
		if (conceptsCache == null) return null;
		return new ArrayList<Concept>(conceptsCache);
	}

	/**
	 * Returns true if the concepts of this individual are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the concepts are cached.
	 */
	public boolean areConceptsCached() {
		return conceptsCacheStateID == getOntology().getStateID();
	}

}
