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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.Map;


public interface AceWikiStorage {
	
	public Ontology getOntology(String name, Map<String, String> parameters);
	
	public void save(OntologyElement oe);

	/**
	 * This method should return the user base for the given ontology.
	 * 
	 * @param ontology The ontology.
	 * @return The user base.
	 */
	public UserBase getUserBase(Ontology ontology);
	
	public void save(User user);

}
