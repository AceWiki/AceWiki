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

%% m t

title:'AceWiki Grammar'.

paragraph:'- Tobias Kuhn, 10 December 2010 -'.

%% m
paragraph:'Below, the grammar rules of the AceWiki grammar are shown:'.

%% m t

section:'Texts and Sentences'.

%% m
paragraph:'''text'' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero):'.

%% m
text =>
	[].

%% m
text =>
	complete_sentence,
	text.

%% m
paragraph:'A complete sentence is represented by the category ''complete_sentence'' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark:'.

%% m t
complete_sentence ~>
	//,
	sentence,
	['.'].

%% m t
complete_sentence ~>
	//,
	simple_sentence_2(qu:plus, whin:minus, whout:plus),
	['?'].

%% m
paragraph:'General sentences are represented by ''sentence'':'.

%% m t
sentence =>
	sentence_coord_1.

%% m t
sentence ~>
	//,
	['for every'],
	nc(subj:minus, qu:minus),
	sentence_coord_1.

%% m t
sentence ~>
	//,
	[if],
	sentence_coord_1,
	[then],
	sentence_coord_1.

%% m
paragraph:'Sentences can be coordinated using "or" (''sentence_coord_1'') and "and"
		(''sentence_coord_2''):'.

%% m t
sentence_coord_1 =>
	sentence_coord_2.

%% m
sentence_coord_1 ~>
	//,
	sentence_coord_2,
	[or],
	sentence_coord_1.

%% m t
sentence_coord_2 =>
	simple_sentence_1.

%% m t
sentence_coord_2 =>
	simple_sentence_1,
	[and],
	sentence_coord_2.

%% m
paragraph:'Uncoordinated sentences are represented in two levels by ''simple_sentence_1'' and
		''simple_sentence_2'':'.

%% m t
simple_sentence_1 ~>
	//,
	['it is false that'],
	simple_sentence_2(qu:minus).

%% m t
simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus).

%% m t
simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, qu:minus),
	['such that'],
	simple_sentence_1.

%% m t
simple_sentence_1 =>
	['there are'],
	np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, qu:minus).

%% m t
simple_sentence_1 =>
	simple_sentence_2(qu:minus).

%% m t
simple_sentence_2(qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:minus, pl:PL, plquant:PLQ, case:nom, qu:Qu, whin:WhIn, whout:WhTemp),
	vp_coord_1(subj:ID, pl:PL, plquant:PLQ, qu:Qu, whin:WhTemp, whout:WhOut).

%% m t

section:'Verb Phrases'.

%% m
paragraph:'Like sentences, verb phrases can be coordinated using "or" (''vp_coord_1'') and "and"
		(''vp_coord_2''):'.

%% m t
vp_coord_1(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut).

%% m
vp_coord_1(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) ~>
	//,
	vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhTemp),
	[or],
	vp_coord_1(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhTemp, whout:WhOut).

%% m t
vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhTemp),
	[and],
	vp_coord_2(subj:Subj, pl:PL, plquant:PLQ, qu:Qu, whin:WhTemp, whout:WhOut).

%% m
paragraph:'Uncoordinated verb phrases represented by ''vp'' can use an auxiliary verb:'.

%% m t
vp(subj:Subj, exist:E, rel:R, pl:PL, plquant:PLQ, qu:Qu, whin:WhIn, whout:WhOut) ~>
	aux(be:Be, exist:E, pl:PL),
	v(subj:Subj, be:Be, exist:E, pl:PL, plquant:PLQ, rel:R, vform:inf, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
vp(subj:Subj, exist:plus, rel:R, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) ~>
	v(subj:Subj, be:minus, exist:plus, pl:PL, rel:R, vform:fin, qu:Qu, whin:WhIn, whout:WhOut).

%% m
paragraph:'The category ''v'' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement:'.

%% m t
v(be:minus, exist:E, pl:PL, vform:VF, copula:minus, whin:Wh, whout:Wh) =>
	verb(vcat:itr, be:minus, pl:PL, exist:E, vform:VF).

%% m t
v(subj:Subj, be:minus, exist:E, rel:R, pl:PL, vform:VF, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:minus, pl:PL, exist:E, vform:VF),
	np(subj:Subj, rel:R, vcat:tr, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:plus),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:plus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, plquant:minus, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:minus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	$tradj,
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m t

section:'Noun Phrases'.

%% m
paragraph:'Noun phrases are represented by ''np'' and can consist of proper names, variables,
		pronouns, and different noun constructs:'.

%% m t
np(id:ID, exist:plus, rel:R, of:minus, def:plus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$propername(human:H, gender:G, text:ID),
	>>(id:ID, human:H, gender:G, type:prop, hasvar:minus),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	#ID,
	newvar(var:Var),
	>(id:ID, type:var, hasvar:plus, var:Var).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$defnoun(noun:Noun),
	$reference(text:Var),
	<(id:ID, type:noun, hasvar:plus, noun:Noun, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$defnoun(noun:Noun),
	<(id:ID, type:noun, noun:Noun, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$reference(text:Var),
	<(id:ID, hasvar:plus, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, subj:Subj, exist:E, rel:R, of:O, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	quant(exist:E, qu:Qu),
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:E, rel:R, of:minus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	#ID,
	ipron(exist:E, human:H, qu:Qu),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, type:ipron, hasvar:HasVar, var:Var),
 	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:plus, of:minus, pl:plus, plquant:plus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	$number,
	#ID,
	$nounpl.

%% m t
np(id:ID, exist:plus, of:minus, pl:minus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	['1'],
	#ID,
	$noun(human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun).

%% m
np(id:ID, exist:plus, of:minus, pl:minus, qu:plus, whin:minus, whout:plus) =>
	#ID,
	[what],
	>(id:ID, human:minus, type:wh, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, pl:minus, qu:plus, whin:minus, whout:plus) =>
	#ID,
	[who],
	>(id:ID, human:plus, type:wh, hasvar:minus).

%% m t
np(id:ID, subj:Subj, exist:plus, rel:R, of:O, embv:EmbV, pl:minus, qu:plus, whin:minus, whout:plus) =>
	[which],
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:plus, whin:plus, whout:plus).

%% m t
np(id:ID, exist:plus, of:minus, pl:plus, plquant:minus, qu:plus, whin:minus, whout:plus) =>
	[which],
	#ID,
	$nounpl.

%% m
paragraph:'The category ''nc'' represents nouns optionally followed by variables, relative clauses,
		and of-constructs:'.

%% m t
nc(id:ID, rel:R, of:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	n(id:ID, human:H, gender:G, text:Noun),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, gender:G, type:noun, hasvar:HasVar, noun:Noun, var:Var),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
nc(subj:Subj, rel:R, of:plus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) ~>
	$nounof,
	np(subj:Subj, rel:R, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

%% m
paragraph:'The category ''n'' stands for nouns:'.

%% m t
n(id:ID, human:H, gender:G, text:Noun) =>
	#ID,
	$noun(human:H, gender:G, text:Noun).

%% m
paragraph:'New variables, optional and mandatory, are represented by ''opt_newvar'' and ''newvar'',
		respectively:'.

%% m t
opt_newvar(hasvar:minus) => [].

%% m t
opt_newvar(hasvar:plus, var:Var) =>
	newvar(var:Var).

%% m t
newvar(var:Var) =>
	$variable(text:Var),
	/<(hasvar:plus, var:Var).

%% m t

section:'Relative Clauses'.

%% m
paragraph:'Relative clauses are represented by ''relcl''. They start with a relative pronoun and
		are always optional:'.

%% m t
relcl(whin:Wh, whout:Wh) =>
	[].

%% m t
relcl(subj:ID, rel:plus, embv:plus, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

%% m
paragraph:'Like sentences and verb phrases, relative clauses can be coordinated by "or"
		(''relcl1'') and "and" (''relcl2''):'.

%% m
relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut) ~>
	//,
	relcl2(subj:ID, human:H, rel:minus, relpron:RP, qu:Qu, whin:WhIn, whout:WhTemp),
	or_relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhTemp, whout:WhOut).

%% m t
relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut) =>
	relcl2(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:minus, pl:minus, qu:Qu, whin:WhIn, whout:WhTemp),
	and_relpron(human:H, relpron:RP),
	relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhTemp, whout:WhOut).

%% m t
relcl2(subj:ID, rel:R, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:R, pl:minus, qu:Qu, whin:WhIn, whout:WhOut).

%% m t
relcl2(subj:Subj, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhOut),
	aux(be:minus, exist:E, pl:PL),
	verb(vcat:tr, be:minus, exist:E, pl:PL, vform:inf).

%% m t
relcl2(subj:Subj, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhOut),
	verb(vcat:tr, be:minus, exist:plus, pl:PL, vform:fin).

%% m
paragraph:'Relative pronouns are represented by ''relpron'' and can be either "that", "who" or
		"which":'.

%% m
relpron(relpron:that) =>
	[that].

%% m t
relpron(human:plus, relpron:who) =>
	[who].

%% m
relpron(human:minus, relpron:which) =>
	[which].

%% m
paragraph:'The categories ''or_relpron'' and ''and_relpron'' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor:'.

%% m
or_relpron(human:H, relpron:RP) =>
	[or],
	relpron(human:H, relpron:RP).

%% m
or_relpron(relpron:that) =>
	['or that'].

%% m
or_relpron(human:plus, relpron:who) =>
	['or who'].

%% m
or_relpron(human:minus, relpron:which) =>
	['or which'].

%% m t
and_relpron(human:H, relpron:RP) =>
	[and],
	relpron(human:H, relpron:RP).

%% m
and_relpron(relpron:that) =>
	['and that'].

%% m
and_relpron(human:plus, relpron:who) =>
	['and who'].

%% m
and_relpron(human:minus, relpron:which) =>
	['and which'].

%% m t

section:'Verbs'.

%% m
paragraph:'The category ''verb'' represents main verbs:'.

%% m t
verb(be:minus, vcat:tr, pl:minus, vform:fin) =>
	$verbsg.

%% m t
verb(be:minus, vcat:tr, pl:plus, vform:fin) =>
	$verbinf.

%% m t
verb(be:minus, vcat:tr, vform:inf) =>
	$verbinf.

%% m t
verb(be:plus, vcat:tr) =>
	$pverb.

%% m
paragraph:'Auxiliary verbs are represented by ''aux'', which includes negation markers:'.

%% m t
aux(be:plus, exist:plus, pl:minus) =>
	[is].

%% m
aux(be:plus, exist:minus, pl:minus) =>
	//,
	['is not'].

%% m t
aux(be:plus, exist:minus, pl:minus) =>
	//,
	[is, not].

%% m t
aux(be:plus, exist:plus, pl:plus) =>
	[are].

%% m
aux(be:plus, exist:minus, pl:plus) =>
	//,
	['are not'].

%% m t
aux(be:plus, exist:minus, pl:plus) =>
	//,
	[are, not].

%% m t
aux(be:minus, exist:minus, pl:minus) =>
	//,
	['does not'].

%% m t
aux(be:minus, exist:minus, pl:plus) =>
	//,
	['do not'].

%% m t

section:'Quantifiers'.

%% m
paragraph:'Existential and universal quantifiers are represented by ''quant'':'.

%% m t
quant(exist:plus) =>
	[a].

%% m
quant(exist:plus) =>
	[an].

%% m t
quant(exist:minus, qu:minus) =>
	//,
	[every].

%% m
quant(exist:minus) =>
	//,
	[no].

%% m
paragraph:'The category ''num_quant'' stands for numerical quantifiers:'.

%% m
num_quant =>
	['at least'].

%% m
num_quant =>
	['at most'].

%% m
num_quant =>
	['less than'].

%% m
num_quant =>
	['more than'].

%% m t
num_quant =>
	['exactly'].

%% m t

section:'Indefinite Pronouns'.

%% m
paragraph:'Indefinite pronouns are represented by ''ipron'':'.

%% m
ipron(exist:plus, human:minus) =>
	[something].

%% m t
ipron(exist:plus, human:plus) =>
	[somebody].

%% m
ipron(exist:minus, human:minus, qu:minus) =>
	//,
	[everything].

%% m t
ipron(exist:minus, human:plus, qu:minus) =>
	//,
	[everybody].

%% m
ipron(exist:minus, human:minus) =>
	//,
	[nothing].

%% m
ipron(exist:minus, human:plus) =>
	//,
	[nobody].

%% t

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

%% t

section:'Auxiliary Rules for Testing'.

test => complete_sentence, fill.
fill => [].
fill => [''], fill.
