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


:- module(extract_syntree, [
		extract_syntree/2  % +ParseTree, -SyntaxTree
	]).


/** <module> Syntax Tree Extractor

This module extracts a syntax tree from the parse tree as produced by the module generate_dcg.

@author Tobias Kuhn
@version 2010-10-15
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


%% extract_syntree(+ParseTree, -SyntaxTree)
%
% This predicate extracts a syntax tree from the parse tree.

extract_syntree(ParseTree, SyntaxTree) :-
    transform_tree(ParseTree, SyntaxTree).


transform_tree(Head => Body, SyntaxTree) :-
    !,
    get_category_name(Head, Name),
    transform_body(Body, BodyList),
    SyntaxTree =.. [Name|BodyList].

transform_tree(Head ~> Body, SyntaxTree) :-
    !,
    get_category_name(Head, Name),
    transform_body(Body, BodyList),
    SyntaxTree =.. [Name|BodyList].


transform_body((B,Rest), BodyT) :-
    !,
    transform_body_element(B, BT),
    transform_body(Rest, RestT),
    append(BT, RestT, BodyT).

transform_body(B, BT) :-
    transform_body_element(B, BT).


transform_body_element([], []) :-
    !.

transform_body_element(B, [B]) :-
    is_list(B),
    !.

transform_body_element(B, []) :-
    B =.. [C|_],
    special_category_name(C),
    !.

transform_body_element(B, [BT]) :-
    transform_tree(B, BT).


get_category_name(C, N) :-
	C =.. [$,P],
	!,
	P =.. [NN|_],
	atom_concat('$', NN, N).

get_category_name(_:C, N) :-
    !,
    get_category_name(C, N).

get_category_name(C, N) :-
	C =.. [N|_].
