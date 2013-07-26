% This file is part of AceWiki.
% Copyright 2008-2013, AceWiki developers.
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


:- module(generate_html, [
		generate_html/2  % +InputFile, +OutputFile
	]).

/** <module> Generate HTML Representation

This module transfroms from the Codeco format into an HTML representation. Usage:

==
swipl -s generate_html.pl -g "generate_html('my_codeco_grammar.pl', 'my_html_file.html')" -t halt
==

You may have to replace "swipl" by the name of your SWI Prolog executable.

Before transforming a file, you should make sure that it is a well-formed Codeco file:

==
swipl -s validate_codeco.pl -g "validate_codeco('my_codeco_grammar.pl')" -t halt
==

For more information about Codeco, see the following thesis:
http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf 

@author Tobias Kuhn
@version 2010-10-18
*/


% Codeco operator definitions:

:- op(1200, xfx, '=>').
:- op(1200, xfx, '~>').
:- op(600, xfx, ':').
:- op(500, yfx, '+').
:- op(300, fx, '$').
:- op(300, fx, '#').
:- op(0, fx, '+').  % Remove declaration
:- op(0, xfx, '>').  % Remove declaration
:- op(0, xfx, '<').  % Remove declaration


%% sort_features(?OnOff)
%
% This fact defines whether the features should be shown in alphabetical order ('on') or in the
% same order as they appear in the input file ('off').

%sort_features(off).
sort_features(on).


%% generate_html(+InputFile, +OutputFile)
%
% This predicate reads the Codeco grammar from the input file and transforms it into an HTML file.

generate_html(InputFile, OutputFile) :-
	retractall(next_id(_)),
	assert(next_id(1)),
    open(InputFile, read, In),
    open(OutputFile, write, Out),
    header(Header),
    write(Out, Header),
    (
    	process(In, Out)
    ;
    	write(user_error, 'Error during parsing.\n')
    ),
    close(In),
    footer(Footer),
    write(Out, Footer),
    close(Out).


process(In, Out) :-
	read_term(In, Term, [module(generate_html)]),
	( Term == end_of_file ->
		true
	;
		retractall(var_number(_, _)),
		numbervars(Term, 1, _),
		process_term(Out, Term),
		process(In, Out)
	).


process_term(Out, title:T) :-
	!,
    format(Out, '<h1>~l</h1>\n', [T]).

process_term(Out, section:S) :-
	!,
    format(Out, '<h3>~l</h3>\n', [S]).

process_term(Out, paragraph:P) :-
	!,
    format(Out, '<p>~l</p>\n', [P]).

process_term(Out, Head => Body) :-
    !,
    get_category_name(Head, N),
    write(Out, '<table class="r"><tr>\n'),
    write_id(Out),
    process_head(Out, Head),
    format(Out, '<td class="arrow"><a name="~l"/>=></td>\n', N),
    process_cats(Out, Body),
    write(Out, '</tr></table>\n\n').

process_term(Out, Head ~> Body) :-
    !,
    get_category_name(Head, N),
    write(Out, '<table class="r"><tr>\n'),
    write_id(Out),
    process_head(Out, Head),
    format(Out, '<td class="arrow"><a name="~l"/>~~></td>\n', N),
    process_cats(Out, Body),
    write(Out, '</tr></table>\n\n').

process_term(_, Term) :-
    format(user_error, 'WARNING. Cannot process term: ~q\n', Term).


process_head(Out, _ : Cond) :-
    !,
    process_cat(Out, Cond).

process_head(Out, Cond) :-
    process_cat(Out, Cond).


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
    T =.. [Name],
    !,
    format(Out, '  <td class="cat">[ <span class="term">~l</span> ]</td>\n', Name),
    process_cat(Out, Rest).

process_cat(Out, $ Cat) :-
    Cat =.. [Name],
    !,
    format(Out, '  <td class="preterm"><a href="#_~l">~l</a></td>\n', [Name,Name]).

process_cat(Out, $ Cat) :-
    Cat =.. [Name|Features],
    !,
    format(Out, '  <td class="preterm"><a href="#_~l">~l</a></td><td><table class="f">', [Name,Name]),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').

process_cat(Out, # '$VAR'(V)) :-
    !,
    get_var_number(V, N),
    format(Out, '  <td class="special">#</td><td><span class="var">~l</span></td>\n', N).

process_cat(Out, '//') :-
    !,
    write(Out, '  <td class="special">//</td>\n').

process_cat(Out, Cat) :-
    Cat =.. [Name],
    !,
    format(Out, '  <td class="cat"><a href="#~l">~l</a></td>\n', [Name,Name]).

process_cat(Out, Cat) :-
    Cat =.. ['>'|Features],
    !,
    write(Out, '  <td class="special">&gt;</td><td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').

process_cat(Out, Cat) :-
    Cat =.. ['>>'|Features],
    !,
    write(Out, '  <td class="special">&gt;&gt;</td><td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|BwrefTerms],
	\+ BwrefTerms = [],
	\+ member(_:_, BwrefTerms),
	!,
    write(Out, '  <td class="special">&lt;+</td>'),
    process_bwrefterms(Out, BwrefTerms),
    write(Out, '  <td></td>\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|Features],
    !,
    write(Out, '  <td class="special">&lt;</td><td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').

process_cat(Out, Cat) :-
    Cat =.. ['/<'|Features],
    !,
    write(Out, '  <td class="special">/&lt;</td><td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').

process_cat(Out, Cat) :-
    Cat =.. [Name|Features],
    format(Out, '  <td class="cat"><a href="#~l">~l</a></td><td><table class="f">', [Name,Name]),
    process_features(Out, Features),
    write(Out, '  </table></td> <td></td>\n').


process_bwrefterms(Out, [Term|Rest]) :-
	Term =.. ['+'|Features],
	!,
    write(Out, '  <td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td>'),
	process_bwrefterms(Out, Rest).

process_bwrefterms(Out, Terms) :-
    write(Out, '  <td class="specialx">&ndash;</td>'),
	process_bwrefterms_x(Out, Terms).

process_bwrefterms_x(Out, [Term|Rest]) :-
	Term =.. ['-'|Features],
	!,
    write(Out, '  <td><table class="f">'),
    process_features(Out, Features),
    write(Out, '  </table></td>'),
	process_bwrefterms_x(Out, Rest).

process_bwrefterms_x(_, []).


process_features(Out, Features) :-
	sort_features(on),
	!,
	sort(Features, FeaturesS),
	process_features_x(Out, FeaturesS).

process_features(Out, Features) :-
	process_features_x(Out, Features).


process_features_x(_, []).

process_features_x(Out, [Name:'$VAR'(V)|Rest]) :-
    !,
    get_var_number(V, N),
    format(Out, '<tr><td class="f">~l: <span class="var"> ~l </span></td></tr>', [Name, N]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:plus|Rest]) :-
    !,
    format(Out, '<tr><td class="f">~l: <strong>+</strong></td></tr>', [Name]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:minus|Rest]) :-
    !,
    format(Out, '<tr><td class="f">~l: <strong>&ndash;</strong></td></tr>', [Name]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:Value|Rest]) :-
    !,
    format(Out, '<tr><td class="f">~l: <strong>~l</strong></td></tr>', [Name, Value]),
    process_features_x(Out, Rest).


get_category_name(C, N) :-
	C =.. [$,P],
	!,
	P =.. [NN|_],
	atom_concat('_', NN, N).

get_category_name(_:C, N) :-
    !,
    get_category_name(C, N).

get_category_name(C, N) :-
	C =.. [N|_].


:- dynamic(next_id/1).


write_id(Out) :-
	next_id(ID),
    format(Out, '  <td class="id">(~w)</td>\n', ID),
	retractall(next_id(_)),
	NewID is ID + 1,
	assert(next_id(NewID)).


:- dynamic(var_number/2).


get_var_number(VarID, VarNumber) :-
    var_number(VarID, VarNumber),
    !.

get_var_number(VarID, VarNumber) :-
    var_number(_, LastVarNumber),
    !,
    VarNumber is LastVarNumber + 1,
    asserta(var_number(VarID, VarNumber)).

get_var_number(VarID, 1) :-
    asserta(var_number(VarID, 1)).


replace(Input, Search, Replace, Output) :-
	concat_atom(Split, Search, Input),
	concat_atom(Split, Replace, Output).


:- format_predicate(l, write_html(_Arg, _Term)).


write_html(_, Atom) :-
	replace(Atom, '&', '&amp;', Atom1),
	replace(Atom1, '<', '&lt;', Atom2),
	replace(Atom2, '>', '&gt;', Atom3),
    write(Atom3).


:- style_check(-atom).

header('<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">

<head>
<title>Codeco Grammar</title>

<style type="text/css">
h1, h2, h3, h4, h5, h6 { font-family: sans-serif; padding: 0.5em 0em 0em 0em; }
table td { white-space: nowrap; }
td { padding: 0.05em 0.05em 0.05em 0.05em; }
td.cat { padding: 0.05em 0.05em 0.05em 0.8em; }
td.preterm { text-decoration: underline; padding: 0.05em 0.05em 0.05em 0.8em; }
td.special { color: red; font-weight: bold; padding: 0.05em 0.05em 0.05em 0.8em; }
td.specialx { color: red; font-weight: bold; padding: 0.05em 0.05em 0.05em 0.05em; }
td.arrow { padding: 0.05em 0.2em 0.05em 1em; font-weight: bold; font-size: 120%; }
td.id { font-size: 80%; padding: 0.05em 1em 0.05em 0.2em; color: gray; }
td.f { padding: 0em 0.3em 0em 0.3em; }
table.r { margin: 0.5em 0em 0.5em 0em; empty-cells: show; border-collapse: collapse; }
table.f {
  margin: 0em 0em 0em 0.2em;
  border-style: solid;
  border-left-width: 1px;
  border-right-width: 1px;
  border-top-width: 0px;
  border-bottom-width: 0px;
  font-size: 80%;
  empty-cells: show;
  border-collapse: collapse;
  height: 2em;
  background: #DDDDDD;
}
span.var {
  padding: 0em 0.2em 0em 0.2em;
  border-style: solid;
  border-width: 1px;
  background: #CCCCFF;
  font-size: 80%;
}
span.term { font-family: sans-serif; font-size: 80%; color: gray; }
a[href]:link {color: black; text-decoration: none; }
a[href]:visited {color: black; text-decoration: none; }
a[href]:hover {color: blue; text-decoration: none; }
</style>
</head>

<body>

').

footer('</body>
</html>
').
