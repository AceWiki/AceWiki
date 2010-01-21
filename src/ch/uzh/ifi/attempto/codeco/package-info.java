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

/**
 * This package contains transformation programs for the Codeco notation. These programs are not written in Java
 * but in SWI Prolog. Among other output formats, Java code can automatically be generated. This package
 * consists of the following programs:
 * <ul>
 * <li><a href="validate_codeco.pl" target="_top">validate_codeco.pl</a></li>
 * <li><a href="generate_dcg.pl" target="_top">generate_dcg.pl</a></li>
 * <li><a href="generate_java.pl" target="_top">generate_java.pl</a></li>
 * <li><a href="generate_html.pl" target="_top">generate_html.pl</a></li>
 * <li><a href="generate_latex.pl" target="_top">generate_latex.pl</a></li>
 * </ul>
 * Below, the Codeco notation is briefly described and it is shown how the different transformation programs can
 * be run.
 *<p>
 * The doctoral thesis "<a href="http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf">Controlled
 * English for Knowledge Representation</a>" describes the Codeco notation in detail.
 * 
 * 
 * <h4>Codeco Notation</h4>
 * 
 * Codeco stands for "COncrete and DEclarative grammar notation for COntrolled natural languages" and is a
 * Prolog-based notation that allows for defining grammars for controlled natural languages in a convenient way.
 * The most important features of Codeco are introduced below.
 *<p>
 * Simple grammar rules in Codeco look almost the same as common Prolog DCG rules. The only difference is that
 * the operator "<code>=></code>" is used instead of "<code>--></code>":
 *<blockquote><pre>
 * vp => v, np.
 * v => ['does not'], verb.
 *</pre></blockquote>
 * Terminal categories are represented in square brackets.
 *<p>
 * Complex grammar rules in Codeco are different from common Prolog DCG rules in the sense that they are using
 * features rather than arguments with fixed positions. Arguments are not recognized by their position but by their
 * name:
 *<blockquote><pre>
 * vp(num:Num,neg:Neg) => v(num:Num,neg:Neg,type:tr), np(case:acc).
 * v(neg:plus,type:Type) => [does, not], verb(type:Type).
 *</pre></blockquote>
 * Every feature has the form <code>Name:Value</code> where <code>Name</code> has to be an atom and <code>Value</code>
 * can be a variable or an atom (but not a compound term).
 *<p>
 * Codeco has special support for pre-terminal categories. Such categories are marked with the dollar sign "$" and
 * can expand only to terminal categories:
 *<blockquote><pre>
 * np => [a], $noun(text:Noun).
 * $noun(text:country) => [country].
 *</pre></blockquote>
 *<p>
 * Codeco also provides special support for anaphoric references. Anaphoric references are used in (controlled)
 * natural languages to refer to objects earlier in the sentence. For example, in the sentence
 *<blockquote><i>
 * A country contains an area that is not controlled by the country.
 *</i></blockquote>
 * the anaphoric reference "the country" refers to the antecedent "a country". In Codeco, anaphoric references are
 * defined by the special categories "<code>></code>" and "<code><</code>". "<code>></code>" marks a
 * position in the text to which anaphoric references can refer (such positions are called "antecedents").
 * "<code><</code>" refers back to the closed possible antecedent. An example is shown here:
 *<blockquote><pre>
 * np => [a], $noun(text:Noun), >(type:noun, noun:Noun).
 * ref => [the], $noun(text:Noun), <(type:noun, noun:Noun).
 *</pre></blockquote>
 * Furthermore, the special category "<code>/<</code>" can be used to ensure that there is no matching antecedent.
 * This can be used, for example, for variables to ensure that no variable is introduced twice:
 *<blockquote><pre>
 * np => $var(text:Var), /<(type:var, var:V), >(type:var, var:V).
 *</pre></blockquote>
 * The back-referring categories "<code><</code>" and "<code>/<</code>" have to immediately follow a terminal
 * or pre-terminal category.
 *<p>
 * Codeco has some more features, which are explained in the publication mentioned above.
 * 
 *
 * <h4>Transformations</h4>
 * 
 * Codeco grammars can be translated automatically into a Java class or into a Prolog DCG grammar, using the SWI
 * Prolog programs "generate_java.pl" or "generate_dcg.pl", respectively. These programs can be found in the directory
 * "src/ch/uzh/ifi/attempto/codeco" of the Attempto Java Packages and are linked above. The Java class can be
 * generated like this:
 *<blockquote><pre>
 * swipl -s generate_java.pl -g "generate_java('my_codeco_grammar.pl', 'my.package.MyGrammarClass')" -t halt
 *</pre></blockquote>
 * Note that the SWI Prolog command might be different on your machine (e.g. "<code>plcon</code>" or "<code>pl</code>").
 * The Prolog DCG file can be generated like this:
 *<blockquote><pre>
 * swipl -s generate_dcg.pl -g "generate_dcg('my_codeco_grammar.pl', 'my_dcg_grammar.pl')" -t halt
 *</pre></blockquote>
 *<p>
 * Furthermore, this package provides the programs "generate_html.pl" and "generate_latex.pl", which can be used to
 * generate HTML and LaTeX representations of Codeco grammars. These programs are used as follows:
 *<blockquote><pre>
 * swipl -s generate_html.pl -g "generate_html('my_codeco_grammar.pl', 'my_html_file.html')" -t halt
 * swipl -s generate_latex.pl -g "generate_latex('my_codeco_grammar.pl', 'my_latex_file.tex')" -t halt
 *</pre></blockquote>
 * 
 * 
 * @author Tobias Kuhn
 */
package ch.uzh.ifi.attempto.codeco;
