// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.preditor;

import ch.uzh.ifi.attempto.chartparser.Grammar;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;
import ch.uzh.ifi.attempto.chartparser.Nonterminal;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.chartparser.GrammarRule;
import ch.uzh.ifi.attempto.chartparser.Terminal;

/**
 * This class is an examplary implementation of a grammar. See the
 * <a href="{@docRoot}/src-html/ch/uzh/ifi/attempto/preditor/ExampleGrammar.html">source code</a>.
 * 
 * @author Tobias Kuhn
 */
public class ExampleGrammar extends Grammar {

	/**
	 * Creates a new grammar instance.
	 */
	public ExampleGrammar() {
		// The comment lines show how the rules would be expressed in Codeco.
		
		// text => [].
		addGrammarRule(new GrammarRule(new Nonterminal("text"), false));
		
		// text => s, ['.'], text.
		addGrammarRule(new GrammarRule(new Nonterminal("text"), false, new Nonterminal("s"), new Terminal("."), new Nonterminal("text")));
		
		// s => exist_np, vp.
		addGrammarRule(new GrammarRule(new Nonterminal("s"), false, new Nonterminal("exist_np"), new Nonterminal("vp")));
		
		// s ~> univ_np, vp.
		addGrammarRule(new GrammarRule(new Nonterminal("s"), true, new Nonterminal("univ_np"), new Nonterminal("vp")));
		
		// univ_np => [every], $n.
		addGrammarRule(new GrammarRule(new Nonterminal("univ_np"), false, new Terminal("every"), new Preterminal("n")));
		
		// univ_np => [no], $n.
		addGrammarRule(new GrammarRule(new Nonterminal("univ_np"), false, new Terminal("no"), new Preterminal("n")));
		
		// exist_np => [a], $n.
		addGrammarRule(new GrammarRule(new Nonterminal("exist_np"), false, new Terminal("a"), new Preterminal("n")));
		
		// exist_np => $pn.
		addGrammarRule(new GrammarRule(new Nonterminal("exist_np"), false, new Preterminal("pn")));
		
		// vp => $iv.
		addGrammarRule(new GrammarRule(new Nonterminal("vp"), false, new Preterminal("iv")));
		
		// vp => $tv, exist_np.
		addGrammarRule(new GrammarRule(new Nonterminal("vp"), false, new Preterminal("tv"), new Nonterminal("exist_np")));
		
		// vp ~> $tv, univ_np.
		addGrammarRule(new GrammarRule(new Nonterminal("vp"), true, new Preterminal("tv"), new Nonterminal("univ_np")));
		
		// lexicon entries:
		addLexicalRule(new LexicalRule("n", "man"));
		addLexicalRule(new LexicalRule("n", "woman"));
		addLexicalRule(new LexicalRule("n", "human"));
		addLexicalRule(new LexicalRule("n", "dog"));
		addLexicalRule(new LexicalRule("n", "house"));
		addLexicalRule(new LexicalRule("n", "car"));
		addLexicalRule(new LexicalRule("pn", "John"));
		addLexicalRule(new LexicalRule("pn", "Bill"));
		addLexicalRule(new LexicalRule("pn", "Mary"));
		addLexicalRule(new LexicalRule("pn", "Sue"));
		addLexicalRule(new LexicalRule("pn", "Tom"));
		addLexicalRule(new LexicalRule("pn", "Rick"));
		addLexicalRule(new LexicalRule("pn", "Paul"));
		addLexicalRule(new LexicalRule("iv", "waits"));
		addLexicalRule(new LexicalRule("iv", "sleeps"));
		addLexicalRule(new LexicalRule("iv", "works"));
		addLexicalRule(new LexicalRule("iv", "eats"));
		addLexicalRule(new LexicalRule("iv", "drinks"));
		addLexicalRule(new LexicalRule("tv", "sees"));
		addLexicalRule(new LexicalRule("tv", "knows"));
		addLexicalRule(new LexicalRule("tv", "owns"));
		addLexicalRule(new LexicalRule("tv", "uses"));
		addLexicalRule(new LexicalRule("tv", "buys"));
		addLexicalRule(new LexicalRule("tv", "sells"));
		addLexicalRule(new LexicalRule("tv", "drives"));
		addLexicalRule(new LexicalRule("tv", "likes"));
	}

}
