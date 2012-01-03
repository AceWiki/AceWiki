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

import java.util.Map;

/**
 * This interface describes a storage object that can store ontologies and user bases in a
 * persistent manner.
 * 
 * @author Tobias Kuhn
 */
public interface AceWikiStorage {
	
	/**
	 * Returns the ontology with the respective name.
	 * 
	 * @param name The name of the ontology.
	 * @param parameters Parameters in case the ontology object has to be created.
	 * @return The ontology object.
	 */
	public Ontology getOntology(String name, Map<String, String> parameters);
	
	/**
	 * Saves the given ontology element.
	 * 
	 * @param oe The ontology element to be saved.
	 */
	public void save(OntologyElement oe);

	/**
	 * Returns the user base for the given ontology.
	 * 
	 * @param ontology The ontology.
	 * @return The user base.
	 */
	public UserBase getUserBase(Ontology ontology);
	
	/**
	 * Saves the given user.
	 * 
	 * @param user The user to be saved.
	 */
	public void save(User user);

}
