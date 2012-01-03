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
% This is the grammar used by AceWiki in the Codeco format. It covers a subset of ACE.
%
% Author: Tobias Kuhn
%===================================================================================================


title:'AceWiki Grammar'.

paragraph:'- Tobias Kuhn, 10 December 2010 -'.


section:'Texts and Sentences'.

complete_sentence ~>
	//,
	sentence,
	['.'].

complete_sentence ~>
	//,
	simple_sentence_2(qu:plus, whin:minus, whout:plus),
	['?'].

sentence =>
	sentence_coord_1.

sentence ~>
	//,
	['for every'],
	nc(subj:minus, qu:minus),
	sentence_coord_1.

sentence ~>
	//,
	[if],
	sentence_coord_1,
	[then],
	sentence_coord_1.

sentence_coord_1 =>
	sentence_coord_2.

sentence_coord_2 =>
	simple_sentence_1.

sentence_coord_2 =>
	simple_sentence_1,
	[and],
	sentence_coord_2.

simple_sentence_1 ~>
	//,
	['it is false that'],
	simple_sentence_2(qu:minus).

simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus).

simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus),
	['such that'],
	simple_sentence_1.

simple_sentence_1 =>
	['there are'],
	np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, qu:minus).

simple_sentence_1 =>
	simple_sentence_2(qu:minus).

simple_sentence_2(qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:minus, pl:PL, plquant:PLQ, case:nom, qu:Qu, whin:WhIn, whout:WhTemp),
	vp_coord_1(subj:ID, pl:PL, plquant:PLQ, qu:Qu, whin:WhTemp, whout:WhOut).


section:'Verb Phrases'.

vp_coord_1(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhTemp),
	[and],
	vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhTemp, whout:WhOut).

vp(subj:Subj, exist:E, rel:R, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) ~>
	aux(be:Be, exist:E, pl:PL),
	v(subj:Subj, be:Be, exist:E, pl:PL, plquant:PLQ, rel:R, vform:inf, qu:Qu, whin:WhIn, whout:WhOut).

vp(subj:Subj, exist:plus, rel:R, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) ~>
	v(subj:Subj, be:minus, exist:plus, pl:PL, rel:R, vform:fin, qu:Qu, whin:WhIn, whout:WhOut).

v(be:minus, exist:E, pl:PL, vform:VF, copula:minus, whin:Wh, whout:Wh) =>
	verb(vcat:itr, be:minus, pl:PL, exist:E, vform:VF).

v(subj:Subj, be:minus, exist:E, rel:R, pl:PL, vform:VF, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:minus, pl:PL, exist:E, vform:VF),
	np(subj:Subj, rel:R, vcat:tr, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:plus),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:plus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, plquant:minus, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:minus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	$tradj,
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).


section:'Noun Phrases'.

np(id:ID, exist:plus, rel:R, of:minus, def:plus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$propername(human:H, gender:G, text:ID),
	>>(id:ID, human:H, gender:G, type:prop, hasvar:minus),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	#ID,
	newvar(var:Var),
	>(id:ID, type:var, hasvar:plus, var:Var).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$defnoun(noun:Noun),
	$reference(text:Var),
	<(id:ID, type:noun, hasvar:plus, noun:Noun, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$defnoun(noun:Noun),
	<(id:ID, type:noun, noun:Noun, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$reference(text:Var),
	<(id:ID, hasvar:plus, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, subj:Subj, exist:E, rel:R, of:O, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	quant(exist:E, qu:Qu),
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:E, rel:R, of:minus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	#ID,
	ipron(exist:E, human:H, qu:Qu),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, type:ipron, hasvar:HasVar, var:Var),
 	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:plus, of:minus, pl:plus, plquant:plus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	$number,
	#ID,
	$nounpl.

np(id:ID, exist:plus, of:minus, pl:minus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	['1'],
	#ID,
	$noun(human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun).

np(id:ID, exist:plus, of:minus, pl:minus, qu:plus, whin:minus, whout:plus) =>
	#ID,
	[who],
	>(id:ID, human:plus, type:wh, hasvar:minus).

np(id:ID, subj:Subj, exist:plus, rel:R, of:O, embv:EmbV, pl:minus, qu:plus, whin:minus, whout:plus) =>
	[which],
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:plus, whin:plus, whout:plus).

np(id:ID, exist:plus, of:minus, pl:plus, plquant:minus, qu:plus, whin:minus, whout:plus) =>
	[which],
	#ID,
	$nounpl.

nc(id:ID, rel:R, of:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	n(id:ID, human:H, gender:G, text:Noun),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, gender:G, type:noun, hasvar:HasVar, noun:Noun, var:Var),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

nc(subj:Subj, rel:R, of:plus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) ~>
	$nounof,
	np(subj:Subj, rel:R, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

n(id:ID, human:H, gender:G, text:Noun) =>
	#ID,
	$noun(human:H, gender:G, text:Noun).

opt_newvar(hasvar:minus) => [].

opt_newvar(hasvar:plus, var:Var) =>
	newvar(var:Var).

newvar(var:Var) =>
	$variable(text:Var),
	/<(hasvar:plus, var:Var).


section:'Relative Clauses'.

relcl(whin:Wh, whout:Wh) =>
	[].

relcl(subj:ID, rel:plus, embv:plus, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut) =>
	relcl2(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:minus, pl:minus, qu:Qu, whin:WhIn, whout:WhTemp),
	and_relpron(human:H, relpron:RP),
	relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhTemp, whout:WhOut).

relcl2(subj:ID, rel:R, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:R, pl:minus, qu:Qu, whin:WhIn, whout:WhOut).

relcl2(subj:Subj, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhOut),
	aux(be:minus, exist:E, pl:PL),
	verb(vcat:tr, be:minus, exist:E, pl:PL, vform:inf).

relcl2(subj:Subj, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhOut),
	verb(vcat:tr, be:minus, exist:plus, pl:PL, vform:fin).

relpron(human:plus, relpron:who) =>
	[who].

and_relpron(human:H, relpron:RP) =>
	[and],
	relpron(human:H, relpron:RP).


section:'Verbs'.

verb(be:minus, vcat:tr, pl:minus, vform:fin) =>
	$verbsg.

verb(be:minus, vcat:tr, pl:plus, vform:fin) =>
	$verbinf.

verb(be:minus, vcat:tr, vform:inf) =>
	$verbinf.

verb(be:plus, vcat:tr) =>
	$pverb.

aux(be:plus, exist:plus, pl:minus) =>
	[is].

aux(be:plus, exist:minus, pl:minus) =>
	//,
	[is, not].

aux(be:plus, exist:plus, pl:plus) =>
	[are].

aux(be:plus, exist:minus, pl:plus) =>
	//,
	[are, not].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	['does not'].

aux(be:minus, exist:minus, pl:plus) =>
	//,
	['do not'].


section:'Quantifiers'.

quant(exist:plus) =>
	[a].

quant(exist:minus, qu:minus) =>
	//,
	[every].

num_quant =>
	['exactly'].


section:'Indefinite Pronouns'.

ipron(exist:plus, human:plus) =>
	[somebody].

ipron(exist:minus, human:plus, qu:minus) =>
	//,
	[everybody].


section:'Lexicon'.

$propername(text:'Mary') => ['Mary'].
$noun(text:woman) => [woman].
$defnoun(noun:woman) => ['the woman'].
$nounpl => [women].
$nounof => ['friend of'].
$verbsg => [asks].
$verbinf => [ask].
$pverb => ['asked by'].
$tradj => ['mad-about'].
$variable(text:'X') => ['X'].
$reference(text:'X') => ['X'].
$number => ['2'].


section:'Auxiliary Rules for Testing'.

test => complete_sentence, fill.
fill => [].
fill => [''], fill.
