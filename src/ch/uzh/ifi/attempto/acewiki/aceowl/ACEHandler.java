// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.MonolingualHandler;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceSuggestion;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;
import ch.uzh.ifi.attempto.chartparser.ChartParser;
import ch.uzh.ifi.attempto.chartparser.ParseTree;

/**
 * This is a language handler implementation for ACE.
 * 
 * @author Tobias Kuhn
 */
public class ACEHandler extends MonolingualHandler {
	
	private TextOperator textOperator;
	private EditorController editContr = new EditorController();
	private ACEOWLLexicon lexicon = new ACEOWLLexicon();
	
	/**
	 * Creates a new language handler for ACE.
	 */
	public ACEHandler() {
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
	
	public void init(Ontology ontology) {
		lexicon.init(ontology);
		textOperator = new ACETextOperator(ontology);
	}

	public TextOperator getTextOperator() {
		return textOperator;
	}

	public List<Sentence> extractSentences(TextContainer tc, PredictiveParser pp) {
		List<Sentence> l = new ArrayList<Sentence>();
		ChartParser parser = (ChartParser) pp;
		List<ParseTree> subTrees = parser.getParseTree().getSubTrees("complete_sentence");
		for (ParseTree pt : subTrees) {
			TextContainer c = tc.getSubTextContainer(pt.getStartPos(), pt.getEndPos());
			int s = c.getTextElementsCount();
			if (s > 0) {
				if (c.getTextElement(s-1).getOriginalText().equals("?")) {
					l.add(new ACEQuestion(c));
				} else {
					l.add(new ACEDeclaration(c));
				}
			}
		}
		return l;
	}
	
	public EditorController getEditorController() {
		return editContr;
	}

	public PredictiveParser getPredictiveParser() {
		ChartParser cp = new ChartParser(ACEGrammar.grammar, "text");
		cp.setDynamicLexicon(lexicon);
		return cp;
	}
	
	public SentenceSuggestion getSuggestion(Sentence sentence) {
		ACESentence s = (ACESentence) sentence;
		List<TextElement> t = s.getTextElements();
		String t0 = t.get(0).getText();
		String t1 = t.get(1).getText();
		String l = t.get(t.size()-1).getText();
		if (t0.matches("(A|a)n?") && !t1.matches(".* of") && l.equals(".")) {
			return new AToEverySuggestion(s);
		}
		return null;
	}

}
