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

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiLexicon;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.LanguageFactory;
import ch.uzh.ifi.attempto.acewiki.core.MenuEngine;
import ch.uzh.ifi.attempto.chartparser.Grammar;

public class ACEOWLEngine extends AbstractLanguageEngine {
	
	private ACELanguageFactory languageFactory = new ACELanguageFactory();
	private LexiconManager lexicon = new LexiconManager();
	private AceWikiOWLReasoner reasoner = new AceWikiOWLReasoner();
	private ACEOWLMenuEngine menuEngine = new ACEOWLMenuEngine();
	
	public ACEOWLEngine() {
		setTextCategory("text");
		setSentenceCategory("complete_sentence");
		
		addExporter(new OWLXMLExporter(true));
		addExporter(new OWLXMLExporter(false));
		addExporter(new ACETextExporter(true));
		addExporter(new ACETextExporter(false));
		addExporter(new ACELexiconExporter());
		
		setLexicalTypes("propername", "noun", "nounof", "trverb", "tradj");
		
		setLexiconChanger("propername", new ProperNameChanger());
		setLexiconChanger("noun", new NounChanger());
		setLexiconChanger("nounof", new NounOfChanger());
		setLexiconChanger("trverb", new VerbChanger());
		setLexiconChanger("tradj", new TrAdjChanger());
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
	
	public MenuEngine getMenuEngine() {
		return menuEngine;
	}

	public AceWikiReasoner getReasoner() {
		return reasoner;
	}

}
