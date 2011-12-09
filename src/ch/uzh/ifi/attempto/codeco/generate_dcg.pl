% This file is part of AceWiki.
% Copyright 2008-2011, AceWiki developers.
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


:- module(generate_dcg, [
		generate_dcg/2  % +InputFile, +OutputFile
	]).


/** <module> Generate DCG Grammar

This module transfroms from the Codeco format into Prolog DCG grammars. Usage:

==
swipl -s generate_dcg.pl -g "generate_dcg('my_codeco_grammar.pl', 'my_dcg_grammar.pl')" -t halt
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


%% special_category_name(?CatName)
%
% This predicate stores the names of the categories with a special meaning in Codeco.

special_category_name('>').
special_category_name('>>').
special_category_name('<').
special_category_name('/<').
special_category_name('#').
special_category_name('//').


%% generate_dcg(+InputFile, +OutputFile)
%
% This predicate reads the Codeco grammar from the input file and transforms it into a Prolog DCG grammar.

generate_dcg(InputFile, OutputFile) :-
    open(InputFile, read, In),
    load_terms(In),
    close(In),
    open(OutputFile, write, Out),
    write(Out, '% This code is automatically generated on the basis of a file in Codeco notation.\n%\n'),
    write(Out, '% For more information, see the package ch.uzh.ifi.attempto.codeco of the AceWiki system\n'),
    write(Out, '% (http://attempto.ifi.uzh.ch/acewiki/) and the thesis "Controlled English for Knowledge\n'),
    write(Out, '% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).\n\n'),
    process_terms(Out),
    write(Out, '\n\n'),
	write(Out, '~(I/T/O) --> {append([X,[//|N],I],T), \\+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.\n'),
	write(Out, '~(_/O/O) --> [].\n'),
	write(Out, '//(_, T/[//|T]) --> [].\n'),
    write(Out, '>(F, T/[>(F)|T]) --> [].\n'),
    write(Out, '>>(F, T/[>>(F)|T]) --> [].\n'),
    write(Out, '<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \\+ member(-Q, L), \\+ \\+ member(+Q, L), !, member(+Q, L)}.\n'),
    write(Out, '<(L, [R|T]/[R|T]) --> <(L,T/T).\n'),
    write(Out, '/<(F, T/T) --> {\\+ (member(R,T), R =.. [_,F])}, !.\n'),
    write(Out, '#(#(P),L,L) :- length(L,P).\n'),
    close(Out).


:- dynamic(term/1).
:- dynamic(feature/1).


load_terms(In) :-
	read_term(In, Term, [module(generate_dcg)]),
	( Term == end_of_file ->
		true
	;
		load_term(Term),
		load_terms(In)
	).

load_term(Name:Value) :-
	!,
    assert(term(Name:Value)).

load_term(Head => Body) :-
    load_head(Head),
    load_conds(Body),
    !,
    assert(term(Head => Body)).

load_term(Head ~> Body) :-
    load_head(Head),
    load_conds(Body),
    !,
    assert(term(Head ~> Body)).

load_term(Term) :-
    format(user_error, 'ERROR. Illegal term: ~q\n', Term).


load_head(_ : Cond) :-
    load_cond(Cond),
    !.

load_head(Cond) :-
    load_cond(Cond).


load_conds((Cond,Rest)) :-
    !,
    load_cond(Cond),
    load_conds(Rest).

load_conds(Cond) :-
    load_cond(Cond).


load_cond(List) :-
    is_list(List),
    !.

load_cond($ Cond) :-
	load_cond(Cond),
	!.

load_cond(# _) :-
	!.

load_cond(Cond) :-
    Cond =.. [_|Args],
    load_args(Args).


load_args([]) :-
	!.

load_args([BwrefTerm|Rest]) :-
	BwrefTerm =.. [Op|Args],
	(Op = '+' ; Op = '-'),
	!,
    load_args(Args),
    load_args(Rest).

load_args([Arg|Rest]) :-
	!,
    load_arg(Arg),
    load_args(Rest).

load_args((Arg,Rest)) :-
    !,
    load_arg(Arg),
    load_args(Rest).

load_args(Cond) :-
    load_arg(Cond).


load_arg(Feature:Value) :-
    atomic(Feature),
    \+ compound(Value),
    !,
    load_feature(Feature).

load_arg(Term) :-
    format(user_error, 'ERROR. Illegal argument: ~q\n', Term),
    fail.


load_feature(Feature) :-
    feature(Feature),
    !.

load_feature(Feature) :-
    assert(feature(Feature)).


process_terms(Out) :-
    term(Term),
    process_term(Out, Term),
    fail.

process_terms(_).


process_term(Out, title:T) :-
	!,
    format(Out, '\n/* === ~w === */\n', [T]).

process_term(Out, section:S) :-
	!,
    format(Out, '\n/* --- ~w --- */\n', [S]).

process_term(Out, paragraph:P) :-
	!,
    format(Out, '/* ~w */\n', [P]).

process_term(Out, Name:Value) :-
	format(Out, '/* ~w: ~w */\n', [Name,Value]).

process_term(Out, Head => Body) :-
    transform_head(Head, HeadT, Head => BodyNodes, AnteTreeIn/AnteTreeOut),
    transform_conds(Body, BodyT, BodyNodes, AnteTreeIn/AnteTreeOut, ''),
    output_term(Out, HeadT --> BodyT).

process_term(Out, Head ~> Body) :-
    transform_head(Head, HeadT, Head => BodyNodes, AnteTreeIn/AnteTreeOut),
    transform_conds(Body, BodyT, BodyNodes, AnteTreeIn/AnteTreeTemp, ~(AnteTreeIn/AnteTreeTemp/AnteTreeOut)),
    output_term(Out, HeadT --> BodyT).


transform_head(_ : Cond, CondT, Node, AnteTreeIn/AnteTreeOut) :-
    !,
    transform_cond(Cond, CondT, Node, AnteTreeIn/AnteTreeOut).

transform_head(Cond, CondT, Node, AnteTreeIn/AnteTreeOut) :-
    transform_cond(Cond, CondT, Node, AnteTreeIn/AnteTreeOut).


transform_conds((Cond,Rest), (CondT,RestT), (N,Nodes), AnteTreeIn/AnteTreeOut, Last) :-
    !,
    transform_cond(Cond, CondT, N, AnteTreeIn/AnteTreeTemp),
    transform_conds(Rest, RestT, Nodes, AnteTreeTemp/AnteTreeOut, Last).

transform_conds(Cond, CondT, Node, AnteTreeIn/AnteTreeOut, '') :-
	!,
    transform_cond(Cond, CondT, Node, AnteTreeIn/AnteTreeOut).

transform_conds(Cond, (CondT,Last), Node, AnteTreeIn/AnteTreeOut, Last) :-
    transform_cond(Cond, CondT, Node, AnteTreeIn/AnteTreeOut).


transform_cond(List, List, List, AnteTree/AnteTree) :-
    is_list(List),
    !.

transform_cond($ Cond, $ CondT, Node, AnteTree/AnteTree) :-
	transform_cond(Cond, CondT, Node, AnteTree/AnteTree),
    !.

transform_cond(# Cond, #(Cond), # Cond, AnteTree/AnteTree) :-
    !.

transform_cond(Cond, CondT, Cond, AnteTreeIn/AnteTreeOut) :-
    Cond =.. ['<'|BwrefTerms],
	\+ BwrefTerms = [],
	\+ member(_:_, BwrefTerms),
	!,
	transform_bwrefterms(BwrefTerms, BwrefTermsT),
    CondT =.. ['<',BwrefTermsT,AnteTreeIn/AnteTreeOut].

transform_cond(Cond, CondT, Cond, AnteTreeIn/AnteTreeOut) :-
    Cond =.. ['<'|Args],
	!,
    get_features(Features),
    transform_args(Args, Features),
    transform_features(Features, FeaturesT),
    CondT =.. ['<',[+(FeaturesT)],AnteTreeIn/AnteTreeOut].

transform_cond(Cond, CondT, Cond, AnteTreeIn/AnteTreeOut) :-
    Cond =.. [Pred|Args],
    special_category_name(Pred),
    !,
    get_features(Features),
    transform_args(Args, Features),
    transform_features(Features, FeaturesT),
    CondT =.. [Pred,FeaturesT,AnteTreeIn/AnteTreeOut].

transform_cond(Cond, CondT, Node, AnteTreeIn/AnteTreeOut) :-
    Cond =.. [Pred|Args],
    get_features(Features),
    transform_args(Args, Features),
    transform_features(Features, FeaturesT),
    CondT =.. [Pred,FeaturesT,Node,AnteTreeIn/AnteTreeOut].


transform_args([], _) :-
	!.

transform_args([Arg|Rest], Features) :-
    member(Arg, Features),
    !,
    transform_args(Rest, Features).

transform_args([Arg|_], _) :-
	!,
    format(user_error, 'ERROR. Illegal argument: ~q\n', Arg),
    fail.

transform_args((Arg, Rest), Features) :-
	member(Arg, Features),
	!,
	transform_args(Rest, Features).

transform_args((Arg, _), _) :-
	!,
    format(user_error, 'ERROR. Illegal argument: ~q\n', Arg),
    fail.

transform_args(Arg, Features) :-
	member(Arg, Features),
	!.

transform_args(Arg, _) :-
	!,
    format(user_error, 'ERROR. Illegal argument: ~q\n', Arg),
    fail.


transform_bwrefterms([], []).

transform_bwrefterms([Term|Rest], [TermT|RestT]) :-
	Term =.. [Op|Args],
    get_features(Features),
    transform_args(Args, Features),
    transform_features(Features, FeaturesT),
    TermT =.. [Op,FeaturesT],
	transform_bwrefterms(Rest, RestT).


get_features(Features) :-
    findall(F:_, feature(F), Features).


transform_features([], []).

transform_features([_:Value|Rest], [Value|RestT]) :-
    transform_features(Rest, RestT).


output_term(Out, Term) :-
    numbervars(Term, 0, _),
	write_term(Out, Term, [module(validate_codeco), numbervars(true), quoted(true)]),
    write(Out, '.\n').
