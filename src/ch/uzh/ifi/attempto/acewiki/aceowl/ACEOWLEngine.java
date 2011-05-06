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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiLexicon;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.LanguageFactory;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.chartparser.Grammar;

public class ACEOWLEngine extends AbstractLanguageEngine {
	
	private ACELanguageFactory languageFactory = new ACELanguageFactory();
	private LexiconManager lexicon = new LexiconManager();
	private AceWikiOWLReasoner reasoner = new AceWikiOWLReasoner();
	private List<OntologyExporter> exporters = new ArrayList<OntologyExporter>();
	private Map<String, LexiconChanger> lexiconChangers = new HashMap<String, LexiconChanger>();
	
	public ACEOWLEngine() {
		exporters.add(new OWLXMLExporter(true));
		exporters.add(new OWLXMLExporter(false));
		exporters.add(new ACETextExporter(true));
		exporters.add(new ACETextExporter(false));
		exporters.add(new ACELexiconExporter());
		
		lexiconChangers.put("propername", new ProperNameChanger());
		lexiconChangers.put("noun", new NounChanger());
		lexiconChangers.put("nounof", new NounOfChanger());
		lexiconChangers.put("trverb", new VerbChanger());
		lexiconChangers.put("tradj", new TrAdjChanger());
	}

	public Grammar getGrammar() {
		return ACEGrammar.grammar;
	}

	public AceWikiLexicon getLexicon() {
		return lexicon;
	}

	public LanguageFactory getLanguageFactory() {
		return languageFactory;
	}

	public String[] getLexicalTypes() {
		return new String[] {"propername", "noun", "nounof", "trverb", "tradj"};
	}

	public LexiconChanger getLexiconChanger(String type) {
		return lexiconChangers.get(type);
	}

	public AceWikiReasoner getReasoner() {
		return reasoner;
	}
	
	public List<OntologyExporter> getExporters() {
		return exporters;
	}

}
