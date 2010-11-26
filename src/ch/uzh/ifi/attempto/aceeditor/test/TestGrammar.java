package ch.uzh.ifi.attempto.aceeditor.test;

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
public class TestGrammar extends ch.uzh.ifi.attempto.chartparser.Grammar {

	public static final TestGrammar grammar = new TestGrammar();
	
	/**
	 * Creates a new grammar object.
	 */
	public TestGrammar() {
		List<Category> l = new ArrayList<Category>();
		Terminal term;
		Nonterminal nonterm;
		Preterminal preterm;
		BackrefCategory brefcat;
		FeatureMap fm;
		HashMap<Integer, StringRef> featureHash = new HashMap<Integer, StringRef>();
		Annotation ann;
		
		
		/* === ACE Editor Grammar === */
		
		/* - Tobias Kuhn, 26 November 2010 - */
		
		
		/* --- Texts and Sentences --- */
		
		// complete_sentence=>sentence, ['.']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("complete_sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal(".");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// complete_sentence~> //, simple_sentence_2(qu:plus, whin:minus, whout:plus), [?]
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
		fm.setFeature("qu", new StringRef("plus"));
		fm.setFeature("whin", new StringRef("minus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("?");
		l.add(term);
		addGrammarRule(new GrammarRule(ann, l, true));
		
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
		
		// sentence~> //, ['for every'], nc(subj:minus, qu:minus), sentence_coord_1
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
		fm.setFeature("qu", new StringRef("minus"));
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
		
		// simple_sentence_1~> //, ['it is false that'], simple_sentence_2(qu:minus)
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
		fm.setFeature("qu", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// simple_sentence_1=>['there is'], np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus)
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
		fm.setFeature("qu", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>['there is'], np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus), ['such that'], simple_sentence_1
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
		fm.setFeature("qu", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("such that");
		l.add(term);
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>['there are'], np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, qu:minus)
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
		fm.setFeature("qu", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_1=>simple_sentence_2(qu:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_1");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		fm.setFeature("qu", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// simple_sentence_2(qu:A, whin:B, whout:C)~>np(id:D, subj:minus, pl:E, case:nom, qu:A, whin:B, whout:F), vp_coord_1(subj:D, pl:E, qu:A, whin:F, whout:C)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("simple_sentence_2");
		fm = new FeatureMap();
		setFeature(fm, "qu", 0, featureHash);
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 3, featureHash);
		fm.setFeature("subj", new StringRef("minus"));
		setFeature(fm, "pl", 4, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		setFeature(fm, "qu", 0, featureHash);
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 3, featureHash);
		setFeature(fm, "pl", 4, featureHash);
		setFeature(fm, "qu", 0, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		
		/* --- Verb Phrases --- */
		
		// vp_coord_1(subj:A, pl:B, qu:C, whin:D, whout:E)=>vp_coord_2(subj:A, pl:B, qu:C, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vp_coord_2(subj:A, pl:B, qu:C, whin:D, whout:E)=>vp(subj:A, pl:B, qu:C, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vp_coord_2(subj:A, pl:B, qu:C, whin:D, whout:E)=>vp(subj:A, pl:B, qu:C, whin:D, whout:F), [and], vp_coord_2(subj:A, pl:B, qu:C, whin:F, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("vp_coord_2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "pl", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vp(subj:A, exist:B, rel:C, pl:D, qu:E, whin:F, whout:G)~>aux(be:H, exist:B, pl:D), v(subj:A, be:H, exist:B, pl:D, rel:C, vform:inf, embv:I, copula:J, qu:E, whin:F, whout:K), vmod(subj:A, embv:I, copula:J, qu:E, whin:K, whout:G)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		setFeature(fm, "be", 7, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "be", 7, featureHash);
		setFeature(fm, "exist", 1, featureHash);
		setFeature(fm, "pl", 3, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		fm.setFeature("vform", new StringRef("inf"));
		setFeature(fm, "embv", 8, featureHash);
		setFeature(fm, "copula", 9, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 10, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "embv", 8, featureHash);
		setFeature(fm, "copula", 9, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 10, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// vp(subj:A, exist:plus, rel:B, pl:C, qu:D, whin:E, whout:F)~>v(subj:A, be:minus, exist:plus, pl:C, rel:B, vform:fin, embv:G, copula:H, qu:D, whin:E, whout:I), vmod(subj:A, embv:G, copula:H, qu:D, whin:I, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "pl", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
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
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "copula", 7, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "copula", 7, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 8, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// v(be:minus, pl:A, vform:B, copula:minus, whin:C, whout:C)=>verb(vcat:itr, be:minus, pl:A, vform:B)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 0, featureHash);
		setFeature(fm, "vform", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "whin", 2, featureHash);
		setFeature(fm, "whout", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("itr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 0, featureHash);
		setFeature(fm, "vform", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:minus, rel:B, pl:C, vform:D, embv:E, copula:minus, qu:F, whin:G, whout:H)=>verb(vcat:tr, be:minus, pl:C, vform:D), np(subj:A, rel:B, vcat:tr, embv:E, case:acc, qu:F, whin:G, whout:H)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "pl", 2, featureHash);
		setFeature(fm, "vform", 3, featureHash);
		setFeature(fm, "embv", 4, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "qu", 5, featureHash);
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 2, featureHash);
		setFeature(fm, "vform", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("vcat", new StringRef("tr"));
		setFeature(fm, "embv", 4, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 5, featureHash);
		setFeature(fm, "whin", 6, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:minus, qu:D, whin:E, whout:F)=>verb(vcat:tr, be:plus), [by], np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:F)
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("by");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:plus, qu:D, whin:E, whout:F)=>np(subj:A, of:plus, rel:B, pl:minus, copula:plus, embv:C, case:acc, qu:D, whin:E, whout:F)
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, pl:minus, embv:C, copula:plus, qu:D, whin:E, whout:F)=>np(subj:A, of:minus, rel:B, pl:minus, copula:plus, embv:C, case:acc, qu:D, whin:E, whout:F)
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(be:plus, copula:plus, whin:A, whout:A)=>adj_coord
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("v");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("copula", new StringRef("plus"));
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// v(subj:A, be:plus, rel:B, embv:C, copula:plus, qu:D, whin:E, whout:F)=>adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Noun Phrases --- */
		
		// np(id:A, exist:plus, rel:B, of:minus, def:plus, pl:minus, embv:C, qu:D, whin:E, whout:F)=>prop(id:A, human:G, gender:H), >>(id:A, human:G, gender:H, type:prop, hasvar:minus), relcl(subj:A, rel:B, embv:C, human:G, qu:D, whin:E, whout:F)
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
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("prop");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "gender", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">>");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "gender", 7, featureHash);
		fm.setFeature("type", new StringRef("prop"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
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
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $def_noun_sg(noun:C), $ref(text:D), <(id:A, type:noun, hasvar:plus, noun:C, var:D, human:E, gender:F), >(id:A, human:E, gender:F, type:ref, hasvar:minus)
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
		preterm = new Preterminal("def_noun_sg");
		fm = new FeatureMap();
		setFeature(fm, "noun", 2, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		preterm = new Preterminal("ref");
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
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $def_noun_sg(noun:C), <(id:A, type:noun, noun:C, human:D, gender:E), >(id:A, human:D, gender:E, type:ref, hasvar:minus)
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
		preterm = new Preterminal("def_noun_sg");
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
		
		// np(id:A, exist:plus, of:minus, def:plus, pl:minus, whin:B, whout:B)=> $ref(text:C), <(id:A, hasvar:plus, var:C, human:D, gender:E), >(id:A, human:D, gender:E, type:ref, hasvar:minus)
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
		preterm = new Preterminal("ref");
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
		
		// np(id:A, subj:A, exist:plus, of:minus, def:plus, pl:minus, refl:plus, whin:B, whout:B)=> $pron(refl:plus, human:C, gender:D), <(id:A, human:C, gender:D)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("refl", new StringRef("plus"));
		setFeature(fm, "whin", 1, featureHash);
		setFeature(fm, "whout", 1, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("pron");
		fm = new FeatureMap();
		fm.setFeature("refl", new StringRef("plus"));
		setFeature(fm, "human", 2, featureHash);
		setFeature(fm, "gender", 3, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		brefcat = new BackrefCategory();
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 2, featureHash);
		setFeature(fm, "gender", 3, featureHash);
		brefcat.addPosFeatureMap(fm);
		l.add(brefcat);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, subj:B, exist:plus, of:minus, def:plus, pl:minus, refl:minus, case:C, whin:D, whout:D)=> $pron(refl:minus, case:C, human:E, gender:F), <(+(id:A, human:E, gender:F), - (id:B)), >(id:A, human:E, gender:F, type:pron, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("def", new StringRef("plus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "case", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 3, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("pron");
		fm = new FeatureMap();
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "case", 2, featureHash);
		setFeature(fm, "human", 4, featureHash);
		setFeature(fm, "gender", 5, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		brefcat = new BackrefCategory();
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 4, featureHash);
		setFeature(fm, "gender", 5, featureHash);
		brefcat.addPosFeatureMap(fm);
		fm = new FeatureMap();
		setFeature(fm, "id", 1, featureHash);
		brefcat.addNegFeatureMap(fm);
		l.add(brefcat);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 4, featureHash);
		setFeature(fm, "gender", 5, featureHash);
		fm.setFeature("type", new StringRef("pron"));
		fm.setFeature("hasvar", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, subj:B, exist:C, rel:D, of:E, pl:minus, embv:F, qu:G, whin:H, whout:I)=>quant(exist:C), nc(id:A, subj:B, rel:D, of:E, embv:F, qu:G, whin:H, whout:I)
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
		setFeature(fm, "qu", 6, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 8, featureHash);
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
		setFeature(fm, "qu", 6, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:B, rel:C, of:minus, pl:minus, embv:D, qu:E, whin:F, whout:G)=> #A, ipron(exist:B, human:H), opt_newvar(hasvar:I, var:J), >(id:A, human:H, type:ipron, hasvar:I, var:J), relcl(subj:A, rel:C, embv:D, human:H, qu:E, whin:F, whout:G)
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
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
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
		setFeature(fm, "human", 7, featureHash);
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
		setFeature(fm, "human", 7, featureHash);
		fm.setFeature("type", new StringRef("ipron"));
		setFeature(fm, "hasvar", 8, featureHash);
		setFeature(fm, "var", 9, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "embv", 3, featureHash);
		setFeature(fm, "human", 7, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:plus, copula:minus, whin:B, whout:B)=>num_quant, $num, opt_adj_coord, #A, $noun_pl
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
		preterm = new Preterminal("num");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun_pl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:minus, copula:minus, whin:B, whout:B)=>num_quant, ['1'], #A, opt_adj_coord, $noun_sg(human:C, gender:D, text:E), >(id:A, human:C, gender:D, type:noun, hasvar:minus, noun:E)
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
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun_sg");
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
		
		// np(id:A, exist:plus, of:minus, pl:minus, qu:plus, whout:plus)=> #A, [who], >(id:A, human:plus, type:wh, hasvar:minus)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("qu", new StringRef("plus"));
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
		
		// np(id:A, subj:B, exist:plus, rel:C, of:D, embv:E, pl:minus, qu:plus, whout:plus)=>[which], nc(id:A, subj:B, rel:C, of:D, embv:E, qu:plus, whin:plus, whout:plus)
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
		fm.setFeature("qu", new StringRef("plus"));
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
		fm.setFeature("qu", new StringRef("plus"));
		fm.setFeature("whin", new StringRef("plus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// np(id:A, exist:plus, of:minus, pl:plus, qu:plus, whout:plus)=>[which], opt_adj_coord, #A, $noun_pl
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		fm.setFeature("exist", new StringRef("plus"));
		fm.setFeature("of", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("qu", new StringRef("plus"));
		fm.setFeature("whout", new StringRef("plus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("which");
		l.add(term);
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun_pl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// nc(id:A, rel:B, of:minus, embv:C, qu:D, whin:E, whout:F)=>n(id:A, human:G, gender:H, text:I), opt_newvar(hasvar:J, var:K), >(id:A, human:G, gender:H, type:noun, hasvar:J, noun:I, var:K), relcl(subj:A, rel:B, embv:C, human:G, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("of", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("n");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "gender", 7, featureHash);
		setFeature(fm, "text", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("opt_newvar");
		fm = new FeatureMap();
		setFeature(fm, "hasvar", 9, featureHash);
		setFeature(fm, "var", 10, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "gender", 7, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		setFeature(fm, "hasvar", 9, featureHash);
		setFeature(fm, "noun", 8, featureHash);
		setFeature(fm, "var", 10, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "human", 6, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// nc(id:A, subj:B, rel:C, of:plus, embv:D, qu:E, whin:F, whout:G)~>n(id:A, human:H, gender:I, text:J), >(id:A, human:H, gender:I, type:noun, hasvar:minus, noun:J), [of], np(subj:B, rel:C, embv:D, case:acc, qu:E, whin:F, whout:G)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("nc");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "subj", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		fm.setFeature("of", new StringRef("plus"));
		setFeature(fm, "embv", 3, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("n");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 7, featureHash);
		setFeature(fm, "gender", 8, featureHash);
		setFeature(fm, "text", 9, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal(">");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 7, featureHash);
		setFeature(fm, "gender", 8, featureHash);
		fm.setFeature("type", new StringRef("noun"));
		fm.setFeature("hasvar", new StringRef("minus"));
		setFeature(fm, "noun", 9, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("of");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 1, featureHash);
		setFeature(fm, "rel", 2, featureHash);
		setFeature(fm, "embv", 3, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// n(id:A, human:B, gender:C, text:D)=>opt_adj_coord, #A, $noun_sg(human:B, gender:C, text:D)
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
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("#");
		fm = new FeatureMap();
		setFeature(fm, "pos", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("noun_sg");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "gender", 2, featureHash);
		setFeature(fm, "text", 3, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
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
		
		// newvar(var:A)=> $var(text:A), /<(hasvar:plus, var:A)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("newvar");
		fm = new FeatureMap();
		setFeature(fm, "var", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("var");
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
		
		// prop(id:A, human:B, gender:C)=> $prop_sg(human:B, gender:C, text:A)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("prop");
		fm = new FeatureMap();
		setFeature(fm, "id", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "gender", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("prop_sg");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "gender", 2, featureHash);
		setFeature(fm, "text", 0, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Adjectives --- */
		
		// opt_adj_coord=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// opt_adj_coord=>adj_coord
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("opt_adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adj_coord=>adj
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adj");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adj_coord=>adj, [and], adj_coord
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adj");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("adj_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adj=> $adj_itr
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adj");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adj_itr");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=>[as], $adj_itr, [as], np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("as");
		l.add(term);
		preterm = new Preterminal("adj_itr");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("as");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=> $adj_itr_comp, [than], np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adj_itr_comp");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("than");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=> $adj_tr(prep:G), np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adj_tr");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=>[as], $adj_tr(prep:G), np(subj:A, rel:minus, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:H), [as], np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("as");
		l.add(term);
		preterm = new Preterminal("adj_tr");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("as");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=>[as], $adj_tr(prep:G), np(subj:A, rel:minus, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:H), [as], $adj_prep(prep:G), np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("as");
		l.add(term);
		preterm = new Preterminal("adj_tr");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("as");
		l.add(term);
		preterm = new Preterminal("adj_prep");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=> $adj_tr_comp(prep:G), np(subj:A, rel:minus, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:H), [than], np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adj_tr_comp");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("than");
		l.add(term);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adjc(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=> $adj_tr_comp(prep:G), np(subj:A, rel:minus, copula:minus, embv:C, case:acc, qu:D, whin:E, whout:H), [than], $adj_prep(prep:G), np(subj:A, rel:B, copula:minus, embv:C, case:acc, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adjc");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adj_tr_comp");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("than");
		l.add(term);
		preterm = new Preterminal("adj_prep");
		fm = new FeatureMap();
		setFeature(fm, "prep", 6, featureHash);
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Relative Clauses --- */
		
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
		
		// relcl(subj:A, rel:plus, embv:plus, human:B, qu:C, whin:D, whout:E)=>relpron(human:B, relpron:F), relcl1(subj:A, human:B, relpron:F, qu:C, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("plus"));
		fm.setFeature("embv", new StringRef("plus"));
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relpron");
		fm = new FeatureMap();
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 5, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl1(subj:A, human:B, relpron:C, qu:D, whin:E, whout:F)=>relcl2(subj:A, human:B, relpron:C, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl1");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "human", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, rel:B, relpron:C, human:D, qu:E, whin:F, whout:G)=>vp(subj:A, rel:minus, pl:minus, qu:E, whin:F, whout:H), and_relpron(human:D, relpron:C), relcl2(subj:A, rel:B, relpron:C, human:D, qu:E, whin:H, whout:G)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "relpron", 2, featureHash);
		setFeature(fm, "human", 3, featureHash);
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 5, featureHash);
		setFeature(fm, "whout", 7, featureHash);
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
		setFeature(fm, "qu", 4, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, rel:B, qu:C, whin:D, whout:E)=>vp(subj:A, rel:B, pl:minus, qu:C, whin:D, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("pl", new StringRef("minus"));
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// relcl2(subj:A, rel:B, qu:C, whin:D, whout:E)~>np(id:F, subj:A, rel:minus, copula:minus, pl:G, embv:H, case:nom, refl:minus, qu:C, whin:D, whout:I), aux(be:minus, pl:G), verb(vcat:tr, be:minus, pl:G, vform:inf), vmod(subj:F, rel:B, embv:H, copula:minus, qu:C, whin:I, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 5, featureHash);
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "pl", 6, featureHash);
		setFeature(fm, "embv", 7, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("aux");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 6, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 6, featureHash);
		fm.setFeature("vform", new StringRef("inf"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 5, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 7, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 8, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
		// relcl2(subj:A, rel:B, qu:C, whin:D, whout:E)~>np(id:F, subj:A, rel:minus, copula:minus, pl:G, embv:H, case:nom, refl:minus, qu:C, whin:D, whout:I), verb(vcat:tr, be:minus, pl:G, vform:fin), vmod(subj:F, rel:B, embv:H, copula:minus, qu:C, whin:I, whout:E)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("relcl2");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "id", 5, featureHash);
		setFeature(fm, "subj", 0, featureHash);
		fm.setFeature("rel", new StringRef("minus"));
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "pl", 6, featureHash);
		setFeature(fm, "embv", 7, featureHash);
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("refl", new StringRef("minus"));
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 3, featureHash);
		setFeature(fm, "whout", 8, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("vcat", new StringRef("tr"));
		fm.setFeature("be", new StringRef("minus"));
		setFeature(fm, "pl", 6, featureHash);
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 5, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 7, featureHash);
		fm.setFeature("copula", new StringRef("minus"));
		setFeature(fm, "qu", 2, featureHash);
		setFeature(fm, "whin", 8, featureHash);
		setFeature(fm, "whout", 4, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, true));
		
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
		
		
		/* --- Verb Phrase Modifiers --- */
		
		/* Verb phrase modifiers are represented by 'vmod' and the auxiliary category 'vmod_x',
		and are always optional: */
		
		// vmod(whin:A, whout:A)=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vmod(subj:A, rel:B, embv:minus, copula:C, qu:D, whin:E, whout:F)=>adv_coord(copula:C), vmod_x(subj:A, rel:B, copula:C, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("embv", new StringRef("minus"));
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adv_coord");
		fm = new FeatureMap();
		setFeature(fm, "copula", 2, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod_x");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vmod(subj:A, rel:B, embv:minus, copula:C, qu:D, whin:E, whout:F)=>pp(subj:A, rel:B, embv:G, qu:D, whin:E, whout:H), vmod(subj:A, rel:B, embv:G, copula:C, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		fm.setFeature("embv", new StringRef("minus"));
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("pp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vmod_x(whin:A, whout:A)=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vmod_x");
		fm = new FeatureMap();
		setFeature(fm, "whin", 0, featureHash);
		setFeature(fm, "whout", 0, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// vmod_x(subj:A, rel:B, copula:C, qu:D, whin:E, whout:F)=>pp(subj:A, rel:B, embv:G, qu:D, whin:E, whout:H), vmod(subj:A, rel:B, embv:G, copula:C, qu:D, whin:H, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("vmod_x");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("pp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 7, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("vmod");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 6, featureHash);
		setFeature(fm, "copula", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 7, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// pp(subj:A, rel:B, embv:C, qu:D, whin:E, whout:F)=> $prep, np(subj:A, rel:B, embv:C, case:acc, qu:D, whin:E, whout:F)
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("pp");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("prep");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		nonterm = new Nonterminal("np");
		fm = new FeatureMap();
		setFeature(fm, "subj", 0, featureHash);
		setFeature(fm, "rel", 1, featureHash);
		setFeature(fm, "embv", 2, featureHash);
		fm.setFeature("case", new StringRef("acc"));
		setFeature(fm, "qu", 3, featureHash);
		setFeature(fm, "whin", 4, featureHash);
		setFeature(fm, "whout", 5, featureHash);
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adv_coord(copula:minus)=>adv_phrase
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adv_coord");
		fm = new FeatureMap();
		fm.setFeature("copula", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adv_phrase");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adv_coord(copula:minus)=>adv_phrase, [and], adv_coord
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adv_coord");
		fm = new FeatureMap();
		fm.setFeature("copula", new StringRef("minus"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("adv_phrase");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("and");
		l.add(term);
		nonterm = new Nonterminal("adv_coord");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// adv_phrase=> $adv
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("adv_phrase");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("adv");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		
		/* --- Verbs --- */
		
		// verb(be:minus, vcat:itr, pl:minus, vform:fin)=> $iv_finsg
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("itr"));
		fm.setFeature("pl", new StringRef("minus"));
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("iv_finsg");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:itr, pl:plus, vform:fin)=> $iv_infpl
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("itr"));
		fm.setFeature("pl", new StringRef("plus"));
		fm.setFeature("vform", new StringRef("fin"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("iv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:itr, vform:inf)=> $iv_infpl
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("minus"));
		fm.setFeature("vcat", new StringRef("itr"));
		fm.setFeature("vform", new StringRef("inf"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("iv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:tr, pl:minus, vform:fin)=> $tv_finsg
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
		preterm = new Preterminal("tv_finsg");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:tr, pl:plus, vform:fin)=> $tv_infpl
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
		preterm = new Preterminal("tv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:minus, vcat:tr, vform:inf)=> $tv_infpl
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
		preterm = new Preterminal("tv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// verb(be:plus, vcat:tr)=> $tv_pp
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("verb");
		fm = new FeatureMap();
		fm.setFeature("be", new StringRef("plus"));
		fm.setFeature("vcat", new StringRef("tr"));
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		preterm = new Preterminal("tv_pp");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
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
		
		
		/* --- Anaphoric Pronouns --- */
		
		// $pron(refl:plus, human:plus, gender:fem)=>[herself]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("pron");
		fm = new FeatureMap();
		fm.setFeature("refl", new StringRef("plus"));
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("gender", new StringRef("fem"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("herself");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $pron(refl:minus, case:nom, human:plus, gender:fem)=>[she]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("pron");
		fm = new FeatureMap();
		fm.setFeature("refl", new StringRef("minus"));
		fm.setFeature("case", new StringRef("nom"));
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("gender", new StringRef("fem"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("she");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $pron(refl:minus, case:acc, human:plus, gender:fem)=>[her]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("pron");
		fm = new FeatureMap();
		fm.setFeature("refl", new StringRef("minus"));
		fm.setFeature("case", new StringRef("acc"));
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("gender", new StringRef("fem"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("her");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		
		/* --- Lexicon --- */
		
		// $prop_sg(human:plus, gender:fem, text:'Mary')=>['Mary']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("prop_sg");
		fm = new FeatureMap();
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("gender", new StringRef("fem"));
		fm.setFeature("text", new StringRef("Mary"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("Mary");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $def_noun_sg(noun:woman)=>['the woman']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("def_noun_sg");
		fm = new FeatureMap();
		fm.setFeature("noun", new StringRef("woman"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("the woman");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $ref(text:'X')=>['X']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("ref");
		fm = new FeatureMap();
		fm.setFeature("text", new StringRef("X"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("X");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $num=>['2']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("num");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("2");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $noun_pl=>[women]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("noun_pl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("women");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $noun_sg(text:woman, human:plus, gender:fem)=>[woman]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("noun_sg");
		fm = new FeatureMap();
		fm.setFeature("text", new StringRef("woman"));
		fm.setFeature("human", new StringRef("plus"));
		fm.setFeature("gender", new StringRef("fem"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("woman");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $var(text:'X')=>['X']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("var");
		fm = new FeatureMap();
		fm.setFeature("text", new StringRef("X"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("X");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $iv_finsg=>[waits]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("iv_finsg");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("waits");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $iv_infpl=>[wait]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("iv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("wait");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $tv_finsg=>[asks]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("tv_finsg");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("asks");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $tv_infpl=>[ask]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("tv_infpl");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("ask");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $tv_pp=>[asked]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("tv_pp");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("asked");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adj_itr=>[young]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adj_itr");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("young");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adj_itr_comp=>[younger]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adj_itr_comp");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("younger");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adj_tr(prep:about)=>['mad-about']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adj_tr");
		fm = new FeatureMap();
		fm.setFeature("prep", new StringRef("about"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("mad-about");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adj_tr_comp(prep:about)=>['madder-about']
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adj_tr_comp");
		fm = new FeatureMap();
		fm.setFeature("prep", new StringRef("about"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("madder-about");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adj_prep(prep:about)=>[about]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adj_prep");
		fm = new FeatureMap();
		fm.setFeature("prep", new StringRef("about"));
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("about");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $prep=>[for]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("prep");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("for");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		// $adv=>[early]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		preterm = new Preterminal("adv");
		fm = new FeatureMap();
		preterm.setFeatureMap(fm);
		l.add(preterm);
		term = new Terminal("early");
		l.add(term);
		addLexicalRule(new LexicalRule(ann, l));
		
		
		/* --- Auxiliary Rules for Testing --- */
		
		// test=>complete_sentence, fill
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("test");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("complete_sentence");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		nonterm = new Nonterminal("fill");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// fill=>[]
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("fill");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));
		
		// fill=>[''], fill
		l.clear();
		featureHash.clear();
		ann = new Annotation();
		nonterm = new Nonterminal("fill");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		term = new Terminal("");
		l.add(term);
		nonterm = new Nonterminal("fill");
		fm = new FeatureMap();
		nonterm.setFeatureMap(fm);
		l.add(nonterm);
		addGrammarRule(new GrammarRule(ann, l, false));

	}
}
