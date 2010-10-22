% This file is part of AceWiki.
% Copyright 2008-2010, Tobias Kuhn.
% 
% AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
% 
% AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
% even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
% Lesser General Public License for more details.
% 
% You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
% not, see http://www.gnu.org/licenses/.


:- module(generate_java, [
		generate_java/2  % +InputFile, +Class
	]).

/** <module> Generate Java Grammar

This module transfroms from the Codeco format into Prolog DCG grammars. Usage:

==
swipl -s generate_java.pl -g "generate_java('my_codeco_grammar.pl', 'my.package.MyGrammarClass')" -t halt
==

You may have to replace "swipl" by the name of your SWI Prolog executable.

Before transforming a file, you should make sure that it is a well-formed Codeco file:

==
swipl -s validate_codeco.pl -g "validate_codeco('my_codeco_grammar.pl')" -t halt
==

For more information about Codeco, see the following thesis:
http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf 

@author Tobias Kuhn
@version 2010-10-21
*/


% Codeco operator definitions:

:- op(1200, xfx, '=>').
:- op(1200, xfx, '~>').
:- op(600, xfx, ':').
:- op(500, fx, '$').
:- op(500, fx, '#').
:- op(0, xfx, '>').  % Remove declaration
:- op(0, xfx, '<').  % Remove declaration


%% generate_java(+InputFile, +Class)
%
% This predicate reads the Codeco grammar from the input file and transforms it into a Java class
% that contains the grammar.

generate_java(InputFile, Class) :-
	concat_atom(ClassList, '.', Class),
	reverse(ClassList, [ClassName|PackageListR]),
	reverse(PackageListR, PackageList),
	concat_atom(PackageList, '.', Package),
    open(InputFile, read, In),
    atom_concat(ClassName, '.java', OutputFile),
    open(OutputFile, write, Out),
    header(Header),
    replace(Header, '*package*', Package, Header1),
    replace(Header1, '*class*', ClassName, Header2),
    write(Out, Header2),
    (
    	process(In, Out)
    ;
    	write(user_error, 'Error during parsing.\n')
    ),
    close(In),
    write(Out, '\n\t}\n}\n'),
    close(Out).


replace(Input, Search, Replace, Output) :-
	concat_atom(Split, Search, Input),
	concat_atom(Split, Replace, Output).


process(In, Out) :-
	read_term(In, Term, [module(generate_java)]),
	( Term == end_of_file ->
		true
	;
		numbervars(Term, 0, _),
		process_term(Out, Term),
		process(In, Out)
	).


process_term(Out, title:T) :-
	!,
    format(Out, '\t\t\n\t\t\n\t\t/* === ~w === */\n', [T]).

process_term(Out, section:S) :-
	!,
    format(Out, '\t\t\n\t\t\n\t\t/* --- ~w --- */\n', [S]).

process_term(Out, paragraph:P) :-
	!,
    format(Out, '\t\t\n\t\t/* ~w */\n', [P]).

process_term(Out, N:V) :-
	!,
    format(Out, '\t\t\n\t\t/* ~w: ~w */\n', [N,V]).

process_term(Out, $ Head => [Body]) :-
    !,
    write(Out, '\t\t\n\t\t// '),
    write_term(Out, $ Head => [Body], [module(generate_java), numbervars(true), quoted(true)]),
    write(Out, '\n'),
    write(Out, '\t\tl.clear();\n'),
    write(Out, '\t\tfeatureHash.clear();\n'),
    write(Out, '\t\tann = new Annotation();\n'),
    process_head(Out, $ Head),
    process_cat(Out, [Body]),
    write(Out, '\t\taddLexicalRule(new LexicalRule(ann, l));\n').

process_term(Out, Head => Body) :-
    !,
    write(Out, '\t\t\n\t\t// '),
    write_term(Out, Head => Body, [module(generate_java), numbervars(true), quoted(true)]),
    write(Out, '\n'),
    write(Out, '\t\tl.clear();\n'),
    write(Out, '\t\tfeatureHash.clear();\n'),
    write(Out, '\t\tann = new Annotation();\n'),
    process_head(Out, Head),
    process_cats(Out, Body),
    write(Out, '\t\taddGrammarRule(new GrammarRule(ann, l, false));\n').

process_term(Out, Head ~> Body) :-
    !,
    write(Out, '\t\t\n\t\t// '),
    write_term(Out, Head ~> Body, [module(generate_java), numbervars(true), quoted(true)]),
    write(Out, '\n'),
    write(Out, '\t\tl.clear();\n'),
    write(Out, '\t\tfeatureHash.clear();\n'),
    write(Out, '\t\tann = new Annotation();\n'),
    process_head(Out, Head),
    process_cats(Out, Body),
    write(Out, '\t\taddGrammarRule(new GrammarRule(ann, l, true));\n').

process_term(_, Term) :-
    format(user_error, 'WARNING. Cannot process term: ~q\n', Term).


process_head(Out, { Ann } : Cond) :-
    !,
    process_annotations(Out, Ann),
    process_cat(Out, Cond).

process_head(Out, Cond) :-
    process_cat(Out, Cond).


process_annotations(Out, (A , Anns) ) :-
    !,
    process_annotation(Out, A),
    process_annotations(Out, Anns).

process_annotations(Out, A) :-
	process_annotation(Out, A).


process_annotation(Out, N:V) :-
    format(Out, '\t\tann.setItem("~w", ', N),
    process_structure(Out, V),
    write(Out, ');\n').


process_structure(Out, '$VAR'(N)) :-
    !,
    format(Out, 'getStringRef(~w, featureHash)', N).

process_structure(Out, Atom) :-
    atom(Atom),
    !,
    format(Out, '"~w"', [Atom]).

process_structure(Out, Term) :-
    Term =.. [Pred|Args],
    !,
    format(Out, 'new Object[] {"~w"', Pred),
    process_structure_list(Out, Args),
    write(Out, '}').


process_structure_list(_, []) :-
    !.

process_structure_list(Out, [S|Rest]) :-
    write(Out, ', '),
    process_structure(Out, S),
    process_structure_list(Out, Rest).


process_cats(Out, Body) :-
    Body = ','(Cat, BodyRest),
    !,
    process_cat(Out, Cat),
    process_cats(Out, BodyRest).

process_cats(_, []) :-
    !.

process_cats(Out, BodyLast) :-
    process_cat(Out, BodyLast).


process_cat(_, []) :-
    !.

process_cat(Out, [T|Rest]) :-
    !,
    T =.. [Name],
    format(Out, '\t\tterm = new Terminal("~w");\n', Name),
    write(Out, '\t\tl.add(term);\n'),
    process_cat(Out, Rest).

process_cat(Out, $ Cat) :-
	!,
    Cat =.. [Name|Features],
    format(Out, '\t\tpreterm = new Preterminal("~w");\n', Name),
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, Features),
    write(Out, '\t\tpreterm.setFeatureMap(fm);\n'),
    write(Out, '\t\tl.add(preterm);\n').

process_cat(Out, # Pos) :-
	!,
    write(Out, '\t\tnonterm = new Nonterminal("#");\n'),
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, [pos:Pos]),
    write(Out, '\t\tnonterm.setFeatureMap(fm);\n'),
    write(Out, '\t\tl.add(nonterm);\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|BwrefTerms],
	\+ BwrefTerms = [],
	\+ member(_:_, BwrefTerms),
	!,
    write(Out, '\t\tbrefcat = new BackrefCategory();\n'),
    process_bwrefterms(Out, BwrefTerms),
    write(Out, '\t\tl.add(brefcat);\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|Features],
	!,
    write(Out, '\t\tbrefcat = new BackrefCategory();\n'),
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, Features),
    write(Out, '\t\tbrefcat.addPosFeatureMap(fm);\n'),
    write(Out, '\t\tl.add(brefcat);\n').

process_cat(Out, Cat) :-
    Cat =.. [Name|Features],
    format(Out, '\t\tnonterm = new Nonterminal("~w");\n', Name),
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, Features),
    write(Out, '\t\tnonterm.setFeatureMap(fm);\n'),
    write(Out, '\t\tl.add(nonterm);\n').


process_features(_, []).

process_features(Out, [Name:'$VAR'(N)|Rest]) :-
    !,
    format(Out, '\t\tsetFeature(fm, "~w", ~w, featureHash);\n', [Name, N]),
    process_features(Out, Rest).

process_features(Out, [Name:Value|Rest]) :-
    !,
    format(Out, '\t\tfm.setFeature("~w", new StringRef("~w"));\n', [Name, Value]),
    process_features(Out, Rest).


process_bwrefterms(_, []).

process_bwrefterms(Out, [Term|Rest]) :-
	Term =.. ['+'|Features],
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, Features),
    write(Out, '\t\tbrefcat.addPosFeatureMap(fm);\n'),
	process_bwrefterms(Out, Rest).

process_bwrefterms(Out, [Term|Rest]) :-
	Term =.. ['-'|Features],
    write(Out, '\t\tfm = new FeatureMap();\n'),
    process_features(Out, Features),
    write(Out, '\t\tbrefcat.addNegFeatureMap(fm);\n'),
	process_bwrefterms(Out, Rest).


:- style_check(-atom).

header('package *package*;

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
public class *class* extends ch.uzh.ifi.attempto.chartparser.Grammar {

	public static final *class* grammar = new *class*();
	
	/**
	 * Creates a new grammar object.
	 */
	public *class*() {
		List<Category> l = new ArrayList<Category>();
		Terminal term;
		Nonterminal nonterm;
		Preterminal preterm;
		BackrefCategory brefcat;
		FeatureMap fm;
		HashMap<Integer, StringRef> featureHash = new HashMap<Integer, StringRef>();
		Annotation ann;
').
