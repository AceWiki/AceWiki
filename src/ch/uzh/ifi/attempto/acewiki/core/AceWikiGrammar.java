package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import ch.uzh.ifi.attempto.chartparser.Annotation;
import ch.uzh.ifi.attempto.chartparser.Nonterminal;
import ch.uzh.ifi.attempto.chartparser.GrammarRule;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;
import ch.uzh.ifi.attempto.chartparser.StringRef;
import ch.uzh.ifi.attempto.chartparser.Terminal;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.chartparser.Category;
import ch.uzh.ifi.attempto.chartparser.BackrefCategory;
import ch.uzh.ifi.attempto.chartparser.FeatureMap;

/**
 * This grammar class is automatically generated on the basis of a file in Codeco notation.
 *<p>
 * For more information, see the Codeco package {@link ch.uzh.ifi.attempto.codeco} of the
 * <a href="http://attempto.ifi.uzh.ch/acewiki/" target="_top">AceWiki</a> system and the thesis
 * "<a href="http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf">Controlled
 * English for Knowledge Representation</a>".
 */
@SuppressWarnings("all")
public class AceWikiGrammar extends ch.uzh.ifi.attempto.chartparser.Grammar {

	public static final AceWikiGrammar grammar = new AceWikiGrammar();
	
	/**
	 * Creates a new grammar object.
	 */
	public AceWikiGrammar() {
		List<Category> l = new ArrayList<Category>();
		Terminal term;
		Nonterminal nonterm;
		Preterminal preterm;
		BackrefCategory brefcat;
		FeatureMap fm;
		HashMap<Integer, StringRef> featureHash = new HashMap<Integer, StringRef>();
		Annotation ann;
		
		
		/* === AceWiki Grammar === */
		
		/* - Tobias Kuhn, 2 August 2010 - */
		
		/* Below, the grammar rules of the AceWiki grammar are shown: */
		
		
		/* --- Texts and Sentences --- */
		
		/* 'text' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero): */
		
		// text=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("text");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// text=>complete_sentence, text
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("text");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("complete_sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("text");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* A complete sentence is represented by the category 'complete_sentence' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark: */
		
		// complete_sentence~> //, sentence, ['.']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("complete_sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal(".");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// complete_sentence~> //, simple_sentence_2(whin:minus, whout:plus), [?]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("complete_sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("?");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		/* General sentences are represented by 'sentence': */
		
		// sentence=>sentence_coord_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// sentence~> //, ['for every'], nc(subj:minus), sentence_coord_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("for every");
		l.add(term);
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		fm.setFeature("subj", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// sentence~> //, [if], sentence_coord_1, [then], sentence_coord_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("if");
		l.add(term);
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("then");
		l.add(term);
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		/* Sentences can be coordinated using "or" ('sentence_coord_1') and "and"
		('sentence_coord_2'): */
		
		// sentence_coord_1=>sentence_coord_2
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence_coord_2");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// sentence_coord_1~> //, sentence_coord_2, [or], sentence_coord_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence_coord_2");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or");
		l.add(term);
		nonterm = new Nonterminal("sentence_coord_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// sentence_coord_2=>simple_sentence_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence_coord_2");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// sentence_coord_2=>simple_sentence_1, [and], sentence_coord_2
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("sentence_coord_2");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("sentence_coord_2");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* Uncoordinated sentences are represented in two levels by 'simple_sentence_1' and
		'simple_sentence_2': */
		
		// simple_sentence_1~> //, ['it is false that'], simple_sentence_2(whin:minus, whout:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("it is false that");
		l.add(term);
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// simple_sentence_1=>['there is'], np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("there is");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		fm.setFeature("subj", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("def", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>['there is'], np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus), ['such that'], simple_sentence_1
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("there is");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		fm.setFeature("subj", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("def", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("such that");
		l.add(term);
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>['there are'], np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, whin:minus, whout:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("there are");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		fm.setFeature("subj", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("def", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>simple_sentence_2(whin:minus, whout:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_2(whin:A, whout:B)~>np(id:C, subj:minus, pl:D, case:nom, whin:A, whout:E), vp_coord_1(subj:C, pl:D, whin:E, whout:B)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 2, featureHash);
		fm.setFeature("subj", new StringRef("minus"));
		setFeature(fm, "pl", 3, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 2, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		
		/* --- Verb Phrases --- */
		
		/* Like sentences, verb phrases can be coordinated using "or" ('vp_coord_1') and "and"
		('vp_coord_2'): */
		
		// vp_coord_1(subj:A, pl:B, whin:C, whout:D)=>vp_coord_2(subj:A, pl:B, whin:C, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vp_coord_1(subj:A, pl:B, whin:C, whout:D)~> //, vp_coord_2(subj:A, pl:B, whin:C, whout:E), [or], vp_coord_1(subj:A, pl:B, whin:E, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or");
		l.add(term);
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// vp_coord_2(subj:A, pl:B, whin:C, whout:D)=>vp(subj:A, pl:B, whin:C, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vp_coord_2(subj:A, pl:B, whin:C, whout:D)=>vp(subj:A, pl:B, whin:C, whout:E), [and], vp_coord_2(subj:A, pl:B, whin:E, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* Uncoordinated verb phrases represented by 'vp' can use an auxiliary verb: */
		
		// vp(subj:A, exist:B, rel:C, pl:D, whin:E, whout:F)~>aux(be:G, exist:B, pl:D), v(subj:A, be:G, exist:B, pl:D, rel:C, vform:inf, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		setFeature(fm, "be", 6, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "be", 6, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		fm.setFeature("vform", new StringRef("inf"));
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// vp(subj:A, exist:plus, rel:B, pl:C, whin:D, whout:E)~>v(subj:A, be:minus, exist:plus, pl:C, rel:B, vform:fin, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "pl", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "pl", 2, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("vform", new StringRef("fin"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		/* The category 'v' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement: */
		
		// v(be:minus, exist:A, pl:B, vform:C, copula:minus, whin:D, whout:D)=>verb(vcat:itr, be:minus, pl:B, exist:A, vform:C)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "exist", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "vform", 2, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("itr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "exist", 0, featureHash);
		setFeature(fm, "vform", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:minus, exist:B, rel:C, pl:D, vform:E, embv:F, copula:minus, whin:G, whout:H)=>verb(vcat:tr, be:minus, pl:D, exist:B, vform:E), np(subj:A, rel:C, vcat:tr, embv:F, case:acc, whin:G, whout:H)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "vform", 4, featureHash);
		setFeature(fm, "embv", 5, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "vform", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		fm.setFeature("vcat", new StringRef("tr"));
		setFeature(fm, "embv", 5, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:minus, whin:D, whout:E)=>verb(vcat:tr, be:plus), np(subj:A, rel:B, copula:minus, embv:C, case:acc, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:plus, whin:D, whout:E)=>np(subj:A, of:plus, rel:B, pl:minus, copula:plus, embv:C, case:acc, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("of", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, pl:minus, embv:C, copula:plus, whin:D, whout:E)=>np(subj:A, of:minus, rel:B, pl:minus, copula:plus, embv:C, case:acc, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("of", new StringRef("minus"));
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:plus, whin:D, whout:E)=> $tradj, np(subj:A, rel:B, copula:minus, embv:C, case:acc, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("tradj");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Noun Phrases --- */
		
		/* Noun phrases are represented by 'np' and can consist of proper names, variables,
		pronouns, and different noun constructs: */
		
		// np(id:A, exist:plus, rel:B, of:minus, def:plus, pl:minus, embv:C, whin:D, whout:E)=> $propername(human:F, gender:G, text:A), >>(id:A, human:F, gender:G, type:prop, hasvar:minus), relcl(subj:A, rel:B, embv:C, human:F, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("propername");
		fm = new FeatureMap();
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "gender", 6, featureHash);
		setFeature(fm, "text", 0, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal(">>");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "gender", 6, featureHash);
		fm.setFeature("type", new StringRef("prop"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> #A, newvar(var:C), >(id:A, type:var, hasvar:plus, var:C)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("newvar");
		fm = new FeatureMap();
		setFeature(fm, "var", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("type", new StringRef("var"));
		fm.setFeature("hasvar", new StringRef("plus"));
		setFeature(fm, "var", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $defnoun(noun:C), $reference(text:D), <(id:A, type:noun, hasvar:plus, noun:C, var:D, human:E, gender:F), >(id:A, human:E, gender:F, type:ref, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("defnoun");
		fm = new FeatureMap();
		setFeature(fm, "noun", 2, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		preterm = new Preterminal("reference");
		fm = new FeatureMap();
		setFeature(fm, "text", 3, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		brefcat = new BackrefCategory();
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		fm.setFeature("hasvar", new StringRef("plus"));
		setFeature(fm, "noun", 2, featureHash);
		setFeature(fm, "var", 3, featureHash);
		setFeature(fm, "human", 4, featureHash);
		setFeature(fm, "gender", 5, featureHash);
		brefcat.addPosFeatureMap(fm);
		l.add(brefcat);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 4, featureHash);
		setFeature(fm, "gender", 5, featureHash);
		fm.setFeature("type", new StringRef("ref"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $defnoun(noun:C), <(id:A, type:noun, noun:C, human:D, gender:E), >(id:A, human:D, gender:E, type:ref, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("defnoun");
		fm = new FeatureMap();
		setFeature(fm, "noun", 2, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		brefcat = new BackrefCategory();
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		setFeature(fm, "noun", 2, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "gender", 4, featureHash);
		brefcat.addPosFeatureMap(fm);
		l.add(brefcat);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "gender", 4, featureHash);
		fm.setFeature("type", new StringRef("ref"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $reference(text:C), <(id:A, hasvar:plus, var:C, human:D, gender:E), >(id:A, human:D, gender:E, type:ref, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("reference");
		fm = new FeatureMap();
		setFeature(fm, "text", 2, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		brefcat = new BackrefCategory();
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("hasvar", new StringRef("plus"));
		setFeature(fm, "var", 2, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "gender", 4, featureHash);
		brefcat.addPosFeatureMap(fm);
		l.add(brefcat);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "gender", 4, featureHash);
		fm.setFeature("type", new StringRef("ref"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, subj:B, exist:C, rel:D, of:E, pl:minus, embv:F, whin:G, whout:H)=>quant(exist:C), nc(id:A, subj:B, rel:D, of:E, embv:F, whin:G, whout:H)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		setFeature(fm, "exist", 2, featureHash);
		setFeature(fm, "rel", 3, featureHash);
		setFeature(fm, "of", 4, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "embv", 5, featureHash);
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("quant");
		fm = new FeatureMap();
		setFeature(fm, "exist", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		setFeature(fm, "rel", 3, featureHash);
		setFeature(fm, "of", 4, featureHash);
		setFeature(fm, "embv", 5, featureHash);
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:B, rel:C, of:minus, pl:minus, embv:D, whin:E, whout:F)=> #A, ipron(exist:B, human:G), opt_newvar(hasvar:H, var:I), >(id:A, human:G, type:ipron, hasvar:H, var:I), relcl(subj:A, rel:C, embv:D, human:G, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "embv", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "human", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("opt_newvar");
		fm = new FeatureMap();
		setFeature(fm, "hasvar", 7, featureHash);
		setFeature(fm, "var", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 6, featureHash);
		fm.setFeature("type", new StringRef("ipron"));
		setFeature(fm, "hasvar", 7, featureHash);
		setFeature(fm, "var", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "embv", 3, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:plus, copula:minus, whin:B, whout:B)=>num_quant, $number, #A, $nounpl
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("number");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("nounpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:minus, copula:minus, whin:B, whout:B)=>num_quant, ['1'], #A, $noun(human:C, gender:D, text:E), >(id:A, human:C, gender:D, type:noun, hasvar:minus, noun:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("1");
		l.add(term);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun");
		fm = new FeatureMap();
		setFeature(fm, "human", 2, featureHash);
		setFeature(fm, "gender", 3, featureHash);
		setFeature(fm, "text", 4, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 2, featureHash);
		setFeature(fm, "gender", 3, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		fm.setFeature("hasvar", new StringRef("minus"));
		setFeature(fm, "noun", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:minus, whout:plus)=> #A, [what], >(id:A, human:minus, type:wh, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("what");
		l.add(term);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("human", new StringRef("minus"));
		fm.setFeature("type", new StringRef("wh"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:minus, whout:plus)=> #A, [who], >(id:A, human:plus, type:wh, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("who");
		l.add(term);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("type", new StringRef("wh"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, subj:B, exist:plus, rel:C, of:D, embv:E, pl:minus, whout:plus)=>[which], nc(id:A, subj:B, rel:C, of:D, embv:E, whin:plus, whout:plus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "of", 3, featureHash);
		setFeature(fm, "embv", 4, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("which");
		l.add(term);
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "of", 3, featureHash);
		setFeature(fm, "embv", 4, featureHash);
		fm.setFeature("whin", new StringRef("plus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:plus, whout:plus)=>[which], #A, $nounpl
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("which");
		l.add(term);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("nounpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* The category 'nc' represents nouns optionally followed by variables, relative clauses,
		and of-constructs: */
		
		// nc(id:A, rel:B, of:minus, embv:C, whin:D, whout:E)=>n(id:A, human:F, gender:G, text:H), opt_newvar(hasvar:I, var:J), >(id:A, human:F, gender:G, type:noun, hasvar:I, noun:H, var:J), relcl(subj:A, rel:B, embv:C, human:F, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("of", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("n");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "gender", 6, featureHash);
		setFeature(fm, "text", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("opt_newvar");
		fm = new FeatureMap();
		setFeature(fm, "hasvar", 8, featureHash);
		setFeature(fm, "var", 9, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "gender", 6, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		setFeature(fm, "hasvar", 8, featureHash);
		setFeature(fm, "noun", 7, featureHash);
		setFeature(fm, "var", 9, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "human", 5, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// nc(subj:A, rel:B, of:plus, embv:C, whin:D, whout:E)~> $nounof, np(subj:A, rel:B, embv:C, case:acc, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("of", new StringRef("plus"));
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("nounof");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		/* The category 'n' stands for nouns: */
		
		// n(id:A, human:B, gender:C, text:D)=> #A, $noun(human:B, gender:C, text:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("n");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "gender", 2, featureHash);
		setFeature(fm, "text", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "gender", 2, featureHash);
		setFeature(fm, "text", 3, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* New variables, optional and mandatory, are represented by 'opt_newvar' and 'newvar',
		respectively: */
		
		// opt_newvar(hasvar:minus)=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("opt_newvar");
		fm = new FeatureMap();
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// opt_newvar(hasvar:plus, var:A)=>newvar(var:A)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("opt_newvar");
		fm = new FeatureMap();
		fm.setFeature("hasvar", new StringRef("plus"));
		setFeature(fm, "var", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("newvar");
		fm = new FeatureMap();
		setFeature(fm, "var", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// newvar(var:A)=> $variable(text:A), /<(hasvar:plus, var:A)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("newvar");
		fm = new FeatureMap();
		setFeature(fm, "var", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("variable");
		fm = new FeatureMap();
		setFeature(fm, "text", 0, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("/<");
		fm = new FeatureMap();
		fm.setFeature("hasvar", new StringRef("plus"));
		setFeature(fm, "var", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Relative Clauses --- */
		
		/* Relative clauses are represented by 'relcl'. They start with a relative pronoun and
		are always optional: */
		
		// relcl(whin:A, whout:A)=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl(subj:A, rel:plus, embv:plus, human:B, whin:C, whout:D)=>relpron(human:B, relpron:E), relcl1(subj:A, human:B, relpron:E, whin:C, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("plus"));
		fm.setFeature("embv", new StringRef("plus"));
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 4, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* Like sentences and verb phrases, relative clauses can be coordinated by "or"
		('relcl1') and "and" ('relcl2'): */
		
		// relcl1(subj:A, human:B, relpron:C, whin:D, whout:E)~> //, relcl2(subj:A, human:B, rel:minus, relpron:C, whin:D, whout:F), or_relpron(human:B, relpron:C), relcl1(subj:A, human:B, relpron:C, whin:F, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("or_relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// relcl1(subj:A, human:B, relpron:C, whin:D, whout:E)=>relcl2(subj:A, human:B, relpron:C, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, rel:B, relpron:C, human:D, whin:E, whout:F)=>vp(subj:A, rel:minus, pl:minus, whin:E, whout:G), and_relpron(human:D, relpron:C), relcl2(subj:A, rel:B, relpron:C, human:D, whin:G, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("and_relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, rel:B, whin:C, whout:D)=>vp(subj:A, rel:B, pl:minus, whin:C, whout:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, whin:B, whout:C)~>np(subj:A, rel:minus, copula:minus, pl:D, case:nom, refl:minus, whin:B, whout:C), aux(be:minus, exist:E, pl:D), verb(vcat:tr, be:minus, exist:E, pl:D, vform:inf)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "pl", 3, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "exist", 4, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "exist", 4, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		fm.setFeature("vform", new StringRef("inf"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// relcl2(subj:A, whin:B, whout:C)~>np(subj:A, rel:minus, copula:minus, pl:D, case:nom, refl:minus, whin:B, whout:C), verb(vcat:tr, be:minus, exist:plus, pl:D, vform:fin)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "pl", 3, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "pl", 3, featureHash);
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
		
		// relpron(relpron:that)=>[that]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		fm.setFeature("relpron", new StringRef("that"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("that");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relpron(human:plus, relpron:who)=>[who]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("relpron", new StringRef("who"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("who");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relpron(human:minus, relpron:which)=>[which]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("minus"));
		fm.setFeature("relpron", new StringRef("which"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("which");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* The categories 'or_relpron' and 'and_relpron' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor: */
		
		// or_relpron(human:A, relpron:B)=>[or], relpron(human:A, relpron:B)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("or_relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 0, featureHash);
		setFeature(fm, "relpron", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or");
		l.add(term);
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 0, featureHash);
		setFeature(fm, "relpron", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// or_relpron(relpron:that)=>['or that']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("or_relpron");
		fm = new FeatureMap();
		fm.setFeature("relpron", new StringRef("that"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or that");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// or_relpron(human:plus, relpron:who)=>['or who']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("or_relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("relpron", new StringRef("who"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or who");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// or_relpron(human:minus, relpron:which)=>['or which']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("or_relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("minus"));
		fm.setFeature("relpron", new StringRef("which"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("or which");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// and_relpron(human:A, relpron:B)=>[and], relpron(human:A, relpron:B)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("and_relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 0, featureHash);
		setFeature(fm, "relpron", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 0, featureHash);
		setFeature(fm, "relpron", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// and_relpron(relpron:that)=>['and that']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("and_relpron");
		fm = new FeatureMap();
		fm.setFeature("relpron", new StringRef("that"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and that");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// and_relpron(human:plus, relpron:who)=>['and who']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("and_relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("relpron", new StringRef("who"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and who");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// and_relpron(human:minus, relpron:which)=>['and which']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("and_relpron");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("minus"));
		fm.setFeature("relpron", new StringRef("which"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and which");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Verbs --- */
		
		/* The category 'verb' represents main verbs: */
		
		// verb(be:minus, vcat:tr, pl:minus, vform:fin)=> $verbsg
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("verbsg");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:tr, pl:plus, vform:fin)=> $verbinf
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("verbinf");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:tr, vform:inf)=> $verbinf
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("vform", new StringRef("inf"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("verbinf");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:plus, vcat:tr)=> $pverb
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("vcat", new StringRef("tr"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("pverb");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* Auxiliary verbs are represented by 'aux', which includes negation markers: */
		
		// aux(be:plus, exist:plus, pl:minus)=>[is]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("is");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:plus, exist:minus, pl:minus)=> //, ['is not']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("is not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:plus, exist:minus, pl:minus)=> //, [is, not]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("is");
		l.add(term);
		term = new Terminal("not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:plus, exist:plus, pl:plus)=>[are]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("are");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:plus, exist:minus, pl:plus)=> //, ['are not']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("are not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:plus, exist:minus, pl:plus)=> //, [are, not]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("are");
		l.add(term);
		term = new Terminal("not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:minus, exist:minus, pl:minus)=> //, ['does not']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("does not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// aux(be:minus, exist:minus, pl:plus)=> //, ['do not']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("do not");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Quantifiers --- */
		
		/* Existential and universal quantifiers are represented by 'quant': */
		
		// quant(exist:plus)=>[a]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("quant");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("a");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// quant(exist:plus)=>[an]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("quant");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("an");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// quant(exist:minus)=> //, [every]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("quant");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("every");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// quant(exist:minus)=> //, [no]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("quant");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("no");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		/* The category 'num_quant' stands for numerical quantifiers: */
		
		// num_quant=>['at least']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("at least");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// num_quant=>['at most']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("at most");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// num_quant=>['less than']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("less than");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// num_quant=>['more than']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("more than");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// num_quant=>[exactly]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("num_quant");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("exactly");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Indefinite Pronouns --- */
		
		/* Indefinite pronouns are represented by 'ipron': */
		
		// ipron(exist:plus, human:minus)=>[something]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("human", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("something");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// ipron(exist:plus, human:plus)=>[somebody]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("human", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("somebody");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// ipron(exist:minus, human:minus)=> //, [everything]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("human", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("everything");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// ipron(exist:minus, human:plus)=> //, [everybody]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("human", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("everybody");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// ipron(exist:minus, human:minus)=> //, [nothing]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("human", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("nothing");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// ipron(exist:minus, human:plus)=> //, [nobody]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("ipron");
		fm = new FeatureMap();
		fm.setFeature("exist", new StringRef("minus"));
		fm.setFeature("human", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("//");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("nobody");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));

	}
}
