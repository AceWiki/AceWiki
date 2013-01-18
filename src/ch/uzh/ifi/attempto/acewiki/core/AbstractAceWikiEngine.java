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

import java.util.ArrayList;
import java.util.List;

/**
 * This is a partial implementation of an AceWiki engine.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractAceWikiEngine implements AceWikiEngine {
	
	private static final String defaultEngineClassName
			= "ch.uzh.ifi.attempto.acewiki.aceowl.ACEOWLEngine";
	
	/**
	 * Creates the AceWiki engine for the given ontology.
	 * 
	 * @param ontology The ontology.
	 * @return The AceWiki engine.
	 */
	static AceWikiEngine createLanguageEngine(Ontology ontology) {
		String n = ontology.getParameter("engine_class");
		Object loadedObj = null;
		ClassLoader classLoader = AbstractAceWikiEngine.class.getClassLoader();
		
		if (n != null && !n.equals("")) {
		    try {
		    	loadedObj = classLoader.loadClass(n).newInstance();
		    } catch (ClassNotFoundException ex) {
		        ontology.log("Engine class not found: " + n);
		        System.err.println("Engine class not found: " + n);
		    } catch (Exception ex) {
		        ontology.log("Failed to load engine object: " + n);
		        System.err.println("Failed to load engine object: " + n);
		        ex.printStackTrace();
		    }
		}
		
		if (loadedObj == null) {
			n = defaultEngineClassName;
	        ontology.log("Loading default engine: " + n);
		    try {
		    	loadedObj = classLoader.loadClass(n).newInstance();
		    } catch (Exception ex) {
		        throw new RuntimeException("Failed to load default engine.", ex);
		    }
		}
	    
		if (!(loadedObj instanceof AceWikiEngine)) {
			throw new RuntimeException("Engine object must be an instance of LanguageEngine");
		}
	    
		AceWikiEngine engine = (AceWikiEngine) loadedObj;
		engine.init(ontology);
		return engine;
	}
	
	private List<OntologyExporter> exporters = new ArrayList<OntologyExporter>();
	private String[] lexicalTypes = new String[] {};
	private WordIndex wordIndex;
	
	public void init(Ontology ontology) {
		for (String language : getLanguages()) {
			getLanguageHandler(language).init(ontology);
		}
		if (getReasoner() != null) {
			getReasoner().init(ontology);
		}
	}

	/**
	 * Sets the lexical types, as defined by the respective ontology element types.
	 * 
	 * @param lexicalTypes The lexical types.
	 */
	public void setLexicalTypes(String... lexicalTypes) {
		this.lexicalTypes = lexicalTypes;
	}

	/**
	 * Adds an exporter to export the wiki content in a certain format.
	 * 
	 * @param exporter An ontology exporter.
	 */
	public void addExporter(OntologyExporter exporter) {
		exporters.add(exporter);
	}
	
	public List<OntologyExporter> getExporters() {
		return exporters;
	}
	
	public String[] getLexicalTypes() {
		return lexicalTypes;
	}
	
	public WordIndex getWordIndex() {
		if (wordIndex == null) {
			wordIndex = new DefaultWordIndex();
		}
		return wordIndex;
	}

}
