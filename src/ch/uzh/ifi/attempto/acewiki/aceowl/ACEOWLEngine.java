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

import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiLexicon;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.LanguageFactory;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.chartparser.Grammar;

/**
 * This is the AceWiki language engine for ACE/OWL. It delivers the grammar, the lexicon, the
 * language factory, the reasoner, and more.
 * 
 * @author Tobias Kuhn
 */
public class ACEOWLEngine extends AbstractLanguageEngine {
	
	private ACELanguageFactory languageFactory = new ACELanguageFactory();
	private ACEOWLLexicon lexicon = new ACEOWLLexicon();
	private AceWikiOWLReasoner reasoner = new AceWikiOWLReasoner();
	private EditorController editContr = new EditorController();
	
	/**
	 * Creates a new language engine for ACE/OWL.
	 */
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

		editContr.setDefaultMenuGroup("function word");
		
		//                     menu group      color shift
		editContr.addMenuGroup("function word",          0);
		editContr.addMenuGroup("proper name",           60);
		editContr.addMenuGroup("noun",                 100);
		editContr.addMenuGroup("plural noun",          120);
		editContr.addMenuGroup("of-construct",         140);
		editContr.addMenuGroup("transitive adjective", 180);
		editContr.addMenuGroup("verb",                 210);
		editContr.addMenuGroup("passive verb",         210);
		editContr.addMenuGroup("new variable",         320);
		editContr.addMenuGroup("reference",            320);
		
		//                              category      menu group              word type / number
		editContr.addExtensibleCategory("propername", "proper name",          "propername", 0);
		editContr.addExtensibleCategory("noun",       "noun",                 "noun",       0);
		editContr.addExtensibleCategory("nounpl",     "plural noun",          "noun",       1);
		editContr.addExtensibleCategory("nounof",     "of-construct",         "nounof",     0);
		editContr.addExtensibleCategory("verbsg",     "verb",                 "trverb",     0);
		editContr.addExtensibleCategory("verbinf",    "verb",                 "trverb",     1);
		editContr.addExtensibleCategory("pverb",      "passive verb",         "trverb",     2);
		editContr.addExtensibleCategory("tradj",      "transitive adjective", "tradj",      0);
		
		//                         category     menu group
		editContr.addPlainCategory("defnoun",   "reference");
		editContr.addPlainCategory("variable",  "new variable");
		editContr.addPlainCategory("reference", "reference");
		
		editContr.setAutocompleteTokens(".", "?");
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
	
	public EditorController getEditorController() {
		return editContr;
	}

	public AceWikiReasoner getReasoner() {
		return reasoner;
	}
	
	public SentenceSuggestion getSuggestion(Sentence sentence) {
		List<TextElement> t = sentence.getTextElements();
		String t0 = t.get(0).getText();
		String t1 = t.get(1).getText();
		String l = t.get(t.size()-1).getText();
		if (t0.matches("(A|a)n?") && !t1.matches(".* of") && l.equals(".")) {
			return new AToEverySuggestion(sentence);
		}
		return null;
	}

}
