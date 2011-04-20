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

import java.util.Collections;
import java.util.List;

public abstract class AbstractLanguageEngine implements LanguageEngine {
	
	private static final String defaultEngineClassName
			= "ch.uzh.ifi.attempto.acewiki.aceowl.ACEOWLEngine";
	
	static LanguageEngine createLanguageEngine(Ontology ontology) {
		String n = ontology.getParameter("engine_class");
		Object loadedObj = null;
		ClassLoader classLoader = AbstractLanguageEngine.class.getClassLoader();
		
		if (n != null && !n.equals("")) {
		    try {
		    	loadedObj = classLoader.loadClass(n).newInstance();
		    } catch (ClassNotFoundException ex) {
		        ontology.log("Engine class not found: " + n);
		    } catch (Exception ex) {
		        ontology.log("Failed to load engine object: " + n);
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
	    
		if (!(loadedObj instanceof LanguageEngine)) {
			throw new RuntimeException("Engine object must be an instance of LanguageEngine");
		}
	    
		LanguageEngine languageEngine = (LanguageEngine) loadedObj;
		languageEngine.init(ontology);
		return languageEngine;
	}

	public void init(Ontology ontology) {
		getLexicon().init(ontology);
		getLanguageFactory().init(ontology);
		getReasoner().init(ontology);
	}
	
	public List<OntologyExporter> getExporters() {
		return Collections.emptyList();
	}

}
