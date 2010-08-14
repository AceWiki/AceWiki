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


:- module(validate_codeco, [
		validate_codeco/1  % +InputFile
	]).


/** <module> Codeco Validator

This module reads a Codeco file and validates it, i.e. reports errors if the file is not
well-formed. Usage:

==
swipl -s validate_codeco.pl -g "validate_codeco('my_codeco_grammar.pl')" -t halt
==

You may have to replace "swipl" by the name of your SWI Prolog executable.

For more information about Codeco, see the following thesis:
http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf 

@author Tobias Kuhn
@version 2009-11-19
*/


% Codeco operator definitions:

:- op(1200, xfx, '=>').
:- op(1200, xfx, '~>').
:- op(600, xfx, ':').
:- op(500, fx, '$').
:- op(500, fx, '#').
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


%% validate_codeco(+InputFile)
%
% This predicate reads the Codeco grammar from the input file and validates it.

validate_codeco(InputFile) :-
    open(InputFile, read, In),
    (
    	process(In)
    ;
    	write(user_error, 'ERROR: The file is not a well-formed Codeco file.\n')
    ),
    close(In).


process(In) :-
	read_term(In, Term, [module(validate_codeco)]),
	( Term == end_of_file ->
		true
	;
		process_term(Term),
		process(In)
	).


process_term(N:_) :-
	atom(N),
	!.

process_term(N:V) :-
	format(user_error, 'ERROR: This is not a valid Codeco term: ~l\n', [N:V]),
	!,
	fail.

process_term(Head => Body) :-
    process_head(Head),
    process_body(Body),
    !.

process_term(Head => Body) :-
	format(user_error, 'ERROR: This is not a valid Codeco rule: ~l\n', [Head => Body]),
	!,
	fail.

process_term(Head ~> Body) :-
    process_head(Head),
    process_body(Body),
    !.

process_term(Head ~> Body) :-
	format(user_error, 'ERROR: This is not a valid Codeco rule: ~l\n', [Head => Body]),
	!,
	fail.

process_term(Term) :-
	format(user_error, 'ERROR: This is not a valid Codeco term: ~l\n', [Term]),
	!,
	fail.


process_head(Head) :-
	var(Head),
	!,
	write(user_error, 'ERROR: The head of a rule cannot be a variable.\n'),
	fail.

process_head(Head) :-
	Head =.. [Name|_],
	special_category_name(Name),
	!,
	format(user_error, 'ERROR: The head of a rule cannot be a special category: ~l\n', [Head]),
	fail.

process_head(Head) :-
	process_cat(Head),
	!.


process_body(Body) :-
	var(Body),
	!,
	write(user_error, 'ERROR: The body of a rule cannot be a variable.\n'),
	fail.

process_body(Body) :-
    Body = ','(Cat, BodyRest),
    !,
    process_cat(Cat),
    process_body(BodyRest).

process_body(BodyLast) :-
    process_cat(BodyLast),
    !.


process_cat(Cat) :-
	var(Cat),
	!,
	write(user_error, 'ERROR: Categories cannot be variables.\n'),
	fail.

process_cat([]) :-
    !.

process_cat([T|_]) :-
	var(T),
	!,
	write(user_error, 'ERROR: Terminal categories cannot be variables.\n'),
	fail.

process_cat([T|Rest]) :-
	atom(T),
    !,
    process_cat(Rest).

process_cat([T|_]) :-
	!,
	format(user_error, 'ERROR: This is an invalid terminal category: ~l\n', [T]),
	fail.

process_cat($ Cat) :-
    var(Cat),
	!,
	format(user_error, 'ERROR: Preterminal categories cannot be variables: ~l\n', [$(Cat)]),
	fail.

process_cat($ Cat) :-
    atom(Cat),
    !.

process_cat($ Cat) :-
    Cat =.. [_|Features],
    !,
    process_features(Features).

process_cat(# V) :-
	var(V),
    !.

process_cat(# V) :-
	!,
	format(user_error, 'ERROR: A position operator requires a variable: ~l\n', [#(V)]),
	fail.

process_cat('//') :-
    !.

process_cat(Cat) :-
    Cat =.. ['//'|_],
	!,
	format(user_error, 'ERROR: Scope-openers cannot take feature structures: ~l\n', [Cat]),
	fail.

process_cat(Cat) :-
	atom(Cat),
	\+ special_category_name(Cat),
	!.

process_cat(Cat) :-
    Cat =.. ['>'|Features],
    !,
    process_features(Features).

process_cat(Cat) :-
    Cat =.. ['>>'|Features],
    !,
    process_features(Features).

process_cat(Cat) :-
    Cat =.. ['<'|BwrefTerms],
	\+ BwrefTerms = [],
	\+ member(_:_, BwrefTerms),
	!,
    process_bwrefterms(BwrefTerms).

process_cat(Cat) :-
    Cat =.. ['<'|Features],
    !,
    process_features(Features).

process_cat(Cat) :-
    Cat =.. ['/<'|Features],
    !,
    process_features(Features).

process_cat(Cat) :-
    Cat =.. [Name|Features],
	\+ special_category_name(Name),
	!,
    process_features(Features).

process_cat(Cat) :-
	!,
	format(user_error, 'ERROR: This is an invalid category: ~l\n', [Cat]),
	fail.


process_bwrefterms([Term|_]) :-
	var(Term),
	!,
	write(user_error, 'ERROR: Positive or negative feature structures cannot be variables.\n'),
	fail.

process_bwrefterms([Term|Rest]) :-
	Term =.. ['+'|Features],
	!,
    process_features(Features),
	process_bwrefterms(Rest).

process_bwrefterms(Terms) :-
	!,
	process_bwrefterms_x(Terms).


process_bwrefterms_x([Term|_]) :-
	var(Term),
	!,
	write(user_error, 'ERROR: Positive or negative feature structures cannot be variables.\n'),
	fail.

process_bwrefterms_x([Term|Rest]) :-
	Term =.. ['-'|Features],
	!,
    process_features(Features),
	process_bwrefterms_x(Rest).

process_bwrefterms_x([Term|_]) :-
	!,
	format(user_error, 'ERROR: This is an invalid positive or negative feature structure: ~l\n', [Term]),
	fail.

process_bwrefterms_x([]) :-
	!.


process_features(F) :-
	var(F),
	!,
	write(user_error, 'ERROR: Feature structures cannot be variables.\n'),
	fail.

process_features([]) :-
	!.

process_features([Name:_|_]) :-
	\+ atom(Name),
	!,
	format(user_error, 'ERROR: Feature names must be atoms: ~l\n', [Name]),
	fail.

process_features([Name:_|Rest]) :-
	member(Name:_, Rest),
	!,
	format(user_error, 'ERROR: Feature name occurs more than once within the same structure: ~l\n', [Name]),
	fail.

process_features([_:V|Rest]) :-
	var(V),
	!,
	process_features(Rest).

process_features([_:V|Rest]) :-
	atom(V),
	!,
	process_features(Rest).

process_features([_:V|_]) :-
	!,
	format(user_error, 'ERROR: Feature values must be variables or atoms: ~l\n', [V]),
	fail.


:- format_predicate(l, write_codeco_term(_Arg, _Term)).


write_codeco_term(_, Term) :-
	current_output(Out),
	write_term(Out, Term, [module(validate_codeco), quoted(true)]).
