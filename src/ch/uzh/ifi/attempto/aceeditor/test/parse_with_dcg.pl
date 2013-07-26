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

%===================================================================================================
% Author: Tobias Kuhn
%===================================================================================================


:- ['../../codeco/extract_syntree'].

:- style_check(-singleton).
:- style_check(-discontiguous).
:- [testgrammar_dcg].
:- style_check(+discontiguous).
:- style_check(+singleton).

:- [sentences].

:- dynamic(count/1).


run :-
	count,
	text(S),
	parse(S),
	count,
	fail.

run :-
	write(user_error, '\n\n').


parse(S) :-
	phrase(test(_, ParseTree, []/_), S),
	extract_syntree(ParseTree, SyntaxTree),
	format(user_output, '~w\n', SyntaxTree),
	!.

parse(S) :-
	format(user_error, 'ERROR: text could not be parsed: ~w~n', [S]).


count :-
	(count(C) ; C = -1),
	!,
	retractall(count(_)),
	NewC is C + 1,
	assert(count(NewC)),
	write_count(NewC).


write_count(C) :-
	0 is C mod 1000,
	!,
	format(user_error, '\n~w', C).

write_count(C) :-
	0 is C mod 100,
	!,
	write(user_error, '.').

write_count(_).


:- run.
