// This file is part of AceWiki.
// Copyright 2011, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki;

import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiStorage;
import ch.uzh.ifi.attempto.acewiki.core.FileBasedStorage;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;

/**
 * This class is used as the backend of an AceWiki.
 * It contains the ontology for the AceWiki and the parameters.
 *
 * @author Yu Changyuan
 */
public class Backend {

	private final Ontology ontology;
	private static AceWikiStorage storage;
	private final Map<String, String> parameters;

	static {
		LocaleResources.loadBundle("ch/uzh/ifi/attempto/echocomp/text");
		LocaleResources.loadBundle("ch/uzh/ifi/attempto/acewiki/text");
	}

	/**
	 * Creates a new Backend instance for the given parameters.
	 *
	 * @param parameters The parameters for the AceWiki Backend.
	 */
	public Backend(Map<String, String> parameters) {
		this.parameters = parameters;
		if (storage == null) {
			storage = new FileBasedStorage(parameters.get("context:datadir"));
		}
		ontology = getStorage().getOntology(parameters.get("ontology"), parameters);
	}

	/**
	 * Get the storage associated with this backend.
	 *
	 * @return The AceWikiStorage instance.
	 */
	public AceWikiStorage getStorage() {
		return storage;
	}

	/**
	 * Get the Ontology instance for this Backend.
	 *
	 * @return The Ontology object.
	 */
	public Ontology getOntology() {
		return ontology;
	}

	/**
	 * Get the parameters of this Backend.
	 *
	 * @return The parameters.
	 */
	public Map<String, String> getParameters() {
		return parameters;
	}

	/**
	 * Get a specific parameter for this Backend.
	 *
	 * @param param The parameter name.
	 * @return The value of the parameter, or null if the parameter does not exist.
	 */
	public String getParameter(String param) {
		return parameters.get(param);
	}

}
