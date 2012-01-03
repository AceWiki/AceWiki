% This file is part of AceWiki.
% Copyright 2008-2012, AceWiki developers.
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


:- style_check(-singleton).
:- style_check(-discontiguous).
:- [testgrammar_dcg].
:- style_check(+discontiguous).
:- style_check(+singleton).

:- dynamic(count/1).


run :-
	count,
	length(S, 8),
	phrase(test(_, _, []/_), S),
	numbervars(S, 0, _),
	format(user_output, '~w  ~w  ~w  ~w  ~w  ~w  ~w  ~w', S),
	count,
	nl,
	fail.

run :-
	write(user_error, '\n\n').


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
