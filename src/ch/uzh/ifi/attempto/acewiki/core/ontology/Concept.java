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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.List;

/**
 * This abstract class represents a concept (other terminologies call it "unary relation", "class",
 * or "type").
 * 
 * @author Tobias Kuhn
 */
public abstract class Concept extends OntologyElement {
	
	private List<Individual> individualsCache;
	private long individualsCacheStateID = -1;
	
	private List<Concept> superConceptsCache;
	private long superConceptsCacheStateID = -1;
	
	private List<Concept> subConceptsCache;
	private long subConceptsCacheStateID = -1;
	
	/**
	 * Initializes the concept.
	 */
	protected Concept() {
	}
	
	/**
	 * Calculates all individuals that belong to this concept.
	 * 
	 * @return A list of all individuals of this concept.
	 * @see Ontology#getIndividuals(Concept)
	 */
	public synchronized List<Individual> getIndividuals() {
		Ontology o = getOntology();
		if (individualsCacheStateID != o.getStateID()) {
			individualsCache = o.getIndividuals(this);
			individualsCacheStateID = o.getStateID();
		}
		return new ArrayList<Individual>(individualsCache);
	}
	
	/**
	 * Returns the cached individuals or null if there are no cached individuals. The returned
	 * individuals might not be up-to-date.
	 * 
	 * @return A list of the cached individuals of this concept.
	 */
	public List<Individual> getCachedIndividuals() {
		if (individualsCache == null) return null;
		return new ArrayList<Individual>(individualsCache);
	}

	/**
	 * Returns true if the individuals of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the individuals are cached.
	 */
	public boolean areIndividualsCached() {
		return individualsCacheStateID == getOntology().getStateID();
	}
	
	/**
	 * Calculates all super-concepts of this concept.
	 * 
	 * @return A list of all super-concepts.
	 * @see Ontology#getSuperConcepts(Concept)
	 */
	public synchronized List<Concept> getSuperConcepts() {
		Ontology o = getOntology();
		if (superConceptsCacheStateID != o.getStateID()) {
			superConceptsCache = o.getSuperConcepts(this);
			superConceptsCacheStateID = o.getStateID();
		}
		return new ArrayList<Concept>(superConceptsCache);
	}
	
	/**
	 * Returns the cached super-concepts or null if there are no cached super-concepts. The returned
	 * super-concepts might not be up-to-date.
	 * 
	 * @return A list of the cached super-concepts of this concept.
	 */
	public List<Concept> getCachedSuperConcepts() {
		if (superConceptsCache == null) return null;
		return new ArrayList<Concept>(superConceptsCache);
	}
	
	/**
	 * Returns true if the suber-concepts of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the super-concepts are cached.
	 */
	public boolean areSuperConceptsCached() {
		return superConceptsCacheStateID == getOntology().getStateID();
	}
	
	/**
	 * Calculates all sub-concepts of this concept.
	 * 
	 * @return A list of all sub-concepts.
	 * @see Ontology#getSubConcepts(Concept)
	 */
	public synchronized List<Concept> getSubConcepts() {
		Ontology o = getOntology();
		if (subConceptsCacheStateID != o.getStateID()) {
			subConceptsCache = o.getSubConcepts(this);
			subConceptsCacheStateID = o.getStateID();
		}
		return new ArrayList<Concept>(subConceptsCache);
	}
	
	/**
	 * Returns the cached sub-concepts or null if there are no cached sub-concepts. The returned
	 * sub-concepts might not be up-to-date.
	 * 
	 * @return A list of the cached sub-concepts of this concept.
	 */
	public List<Concept> getCachedSubConcepts() {
		if (subConceptsCache == null) return null;
		return new ArrayList<Concept>(subConceptsCache);
	}
	
	/**
	 * Returns true if the sub-concepts of this concept are cached and up-to-date and thus
	 * do not have to be recalculated.
	 * 
	 * @return true if the sub-concepts are cached.
	 */
	public boolean areSubConceptsCached() {
		return subConceptsCacheStateID == getOntology().getStateID();
	}
	
}
