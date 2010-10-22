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

%===================================================================================================
% This is the grammar used by AceWiki in the Codeco format. It covers a subset of ACE.
%
% Author: Tobias Kuhn
%===================================================================================================


title:'AceWiki Grammar'.

paragraph:'- Tobias Kuhn, 2 August 2010 -'.

paragraph:'Below, the grammar rules of the AceWiki grammar are shown:'.


section:'Texts and Sentences'.

paragraph:'''text'' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero):'.

text =>
	[].

text =>
	complete_sentence,
	text.

paragraph:'A complete sentence is represented by the category ''complete_sentence'' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark:'.

complete_sentence ~>
	//,
	sentence,
	['.'].

complete_sentence ~>
	//,
	simple_sentence_2(whin:minus, whout:plus),
	['?'].

paragraph:'General sentences are represented by ''sentence'':'.

sentence =>
	sentence_coord_1.

sentence ~>
	//,
	['for every'],
	nc(subj:minus),
	sentence_coord_1.

sentence ~>
	//,
	[if],
	sentence_coord_1,
	[then],
	sentence_coord_1.

paragraph:'Sentences can be coordinated using "or" (''sentence_coord_1'') and "and"
		(''sentence_coord_2''):'.

sentence_coord_1 =>
	sentence_coord_2.

sentence_coord_1 ~>
	//,
	sentence_coord_2,
	[or],
	sentence_coord_1.

sentence_coord_2 =>
	simple_sentence_1.

sentence_coord_2 =>
	simple_sentence_1,
	[and],
	sentence_coord_2.

paragraph:'Uncoordinated sentences are represented in two levels by ''simple_sentence_1'' and
		''simple_sentence_2'':'.

simple_sentence_1 ~>
	//,
	['it is false that'],
	simple_sentence_2(whin:minus, whout:minus).

simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus).

simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus),
	['such that'],
	simple_sentence_1.

simple_sentence_1 =>
	['there are'],
	np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, whin:minus, whout:minus).

simple_sentence_1 =>
	simple_sentence_2(whin:minus, whout:minus).

simple_sentence_2(whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:minus, pl:PL, case:nom, whin:WhIn, whout:WhTemp),
	vp_coord_1(subj:ID, pl:PL, whin:WhTemp, whout:WhOut).


section:'Verb Phrases'.

paragraph:'Like sentences, verb phrases can be coordinated using "or" (''vp_coord_1'') and "and"
		(''vp_coord_2''):'.

vp_coord_1(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut).

vp_coord_1(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) ~>
	//,
	vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhTemp),
	[or],
	vp_coord_1(subj:Subj, pl:PL, whin:WhTemp, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, whin:WhIn, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, whin:WhIn, whout:WhTemp),
	[and],
	vp_coord_2(subj:Subj, pl:PL, whin:WhTemp, whout:WhOut).

paragraph:'Uncoordinated verb phrases represented by ''vp'' can use an auxiliary verb:'.

vp(subj:Subj, exist:E, rel:R, pl:PL, whin:WhIn, whout:WhOut) ~>
	aux(be:Be, exist:E, pl:PL),
	v(subj:Subj, be:Be, exist:E, pl:PL, rel:R, vform:inf, whin:WhIn, whout:WhOut).

vp(subj:Subj, exist:plus, rel:R, pl:PL, whin:WhIn, whout:WhOut) ~>
	v(subj:Subj, be:minus, exist:plus, pl:PL, rel:R, vform:fin, whin:WhIn, whout:WhOut).

paragraph:'The category ''v'' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement:'.

v(be:minus, exist:E, pl:PL, vform:VF, copula:minus, whin:Wh, whout:Wh) =>
	verb(vcat:itr, be:minus, pl:PL, exist:E, vform:VF).

v(subj:Subj, be:minus, exist:E, rel:R, pl:PL, vform:VF, embv:EmbV, copula:minus, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:minus, pl:PL, exist:E, vform:VF),
	np(subj:Subj, rel:R, vcat:tr, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:minus, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:plus),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:plus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, pl:minus, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:minus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	$tradj,
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).


section:'Noun Phrases'.

paragraph:'Noun phrases are represented by ''np'' and can consist of proper names, variables,
		pronouns, and different noun constructs:'.

np(id:ID, exist:plus, rel:R, of:minus, def:plus, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$propername(human:H, gender:G, text:ID),
	>>(id:ID, human:H, gender:G, type:prop, hasvar:minus),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

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

np(id:ID, subj:Subj, exist:E, rel:R, of:O, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	quant(exist:E),
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, whin:WhIn, whout:WhOut).

np(id:ID, exist:E, rel:R, of:minus, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	#ID,
	ipron(exist:E, human:H),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, type:ipron, hasvar:HasVar, var:Var),
 	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

np(id:ID, exist:plus, of:minus, pl:plus, copula:minus, whin:Wh, whout:Wh) =>
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

np(id:ID, exist:plus, of:minus, pl:minus, whout:plus) =>
	#ID,
	[what],
	>(id:ID, human:minus, type:wh, hasvar:minus).

np(id:ID, exist:plus, of:minus, pl:minus, whout:plus) =>
	#ID,
	[who],
	>(id:ID, human:plus, type:wh, hasvar:minus).

np(id:ID, subj:Subj, exist:plus, rel:R, of:O, embv:EmbV, pl:minus, whout:plus) =>
	[which],
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, whin:plus, whout:plus).

np(id:ID, exist:plus, of:minus, pl:plus, whout:plus) =>
	[which],
	#ID,
	$nounpl.

paragraph:'The category ''nc'' represents nouns optionally followed by variables, relative clauses,
		and of-constructs:'.

nc(id:ID, rel:R, of:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	n(id:ID, human:H, gender:G, text:Noun),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, gender:G, type:noun, hasvar:HasVar, noun:Noun, var:Var),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

nc(subj:Subj, rel:R, of:plus, embv:EmbV, whin:WhIn, whout:WhOut) ~>
	$nounof,
	np(subj:Subj, rel:R, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

paragraph:'The category ''n'' stands for nouns:'.

n(id:ID, human:H, gender:G, text:Noun) =>
	#ID,
	$noun(human:H, gender:G, text:Noun).

paragraph:'New variables, optional and mandatory, are represented by ''opt_newvar'' and ''newvar'',
		respectively:'.

opt_newvar(hasvar:minus) => [].

opt_newvar(hasvar:plus, var:Var) =>
	newvar(var:Var).

newvar(var:Var) =>
	$variable(text:Var),
	/<(hasvar:plus, var:Var).


section:'Relative Clauses'.

paragraph:'Relative clauses are represented by ''relcl''. They start with a relative pronoun and
		are always optional:'.

relcl(whin:Wh, whout:Wh) =>
	[].

relcl(subj:ID, rel:plus, embv:plus, human:H, whin:WhIn, whout:WhOut) =>
	relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut).

paragraph:'Like sentences and verb phrases, relative clauses can be coordinated by "or"
		(''relcl1'') and "and" (''relcl2''):'.

relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut) ~>
	//,
	relcl2(subj:ID, human:H, rel:minus, relpron:RP, whin:WhIn, whout:WhTemp),
	or_relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, whin:WhTemp, whout:WhOut).

relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut) =>
	relcl2(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut).

relcl2(subj:ID, rel:R, relpron:RP, human:H, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:minus, pl:minus, whin:WhIn, whout:WhTemp),
	and_relpron(human:H, relpron:RP),
	relcl2(subj:ID, rel:R, relpron:RP, human:H, whin:WhTemp, whout:WhOut).

relcl2(subj:ID, rel:R, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:R, pl:minus, whin:WhIn, whout:WhOut).

relcl2(subj:Subj, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, whin:WhIn, whout:WhOut),
	aux(be:minus, exist:E, pl:PL),
	verb(vcat:tr, be:minus, exist:E, pl:PL, vform:inf).

relcl2(subj:Subj, whin:WhIn, whout:WhOut) ~>
	np(subj:Subj, rel:minus, copula:minus, pl:PL, case:nom, refl:minus, whin:WhIn, whout:WhOut),
	verb(vcat:tr, be:minus, exist:plus, pl:PL, vform:fin).

paragraph:'Relative pronouns are represented by ''relpron'' and can be either "that", "who" or
		"which":'.

relpron(relpron:that) =>
	[that].

relpron(human:plus, relpron:who) =>
	[who].

relpron(human:minus, relpron:which) =>
	[which].

paragraph:'The categories ''or_relpron'' and ''and_relpron'' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor:'.

or_relpron(human:H, relpron:RP) =>
	[or],
	relpron(human:H, relpron:RP).

or_relpron(relpron:that) =>
	['or that'].

or_relpron(human:plus, relpron:who) =>
	['or who'].

or_relpron(human:minus, relpron:which) =>
	['or which'].

and_relpron(human:H, relpron:RP) =>
	[and],
	relpron(human:H, relpron:RP).

and_relpron(relpron:that) =>
	['and that'].

and_relpron(human:plus, relpron:who) =>
	['and who'].

and_relpron(human:minus, relpron:which) =>
	['and which'].


section:'Verbs'.

paragraph:'The category ''verb'' represents main verbs:'.

verb(be:minus, vcat:tr, pl:minus, vform:fin) =>
	$verbsg.

verb(be:minus, vcat:tr, pl:plus, vform:fin) =>
	$verbinf.

verb(be:minus, vcat:tr, vform:inf) =>
	$verbinf.

verb(be:plus, vcat:tr) =>
	$pverb.

paragraph:'Auxiliary verbs are represented by ''aux'', which includes negation markers:'.

aux(be:plus, exist:plus, pl:minus) =>
	[is].

aux(be:plus, exist:minus, pl:minus) =>
	//,
	['is not'].

aux(be:plus, exist:minus, pl:minus) =>
	//,
	[is, not].

aux(be:plus, exist:plus, pl:plus) =>
	[are].

aux(be:plus, exist:minus, pl:plus) =>
	//,
	['are not'].

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

paragraph:'Existential and universal quantifiers are represented by ''quant'':'.

quant(exist:plus) =>
	[a].

quant(exist:plus) =>
	[an].

quant(exist:minus) =>
	//,
	[every].

quant(exist:minus) =>
	//,
	[no].

paragraph:'The category ''num_quant'' stands for numerical quantifiers:'.

num_quant =>
	['at least'].

num_quant =>
	['at most'].

num_quant =>
	['less than'].

num_quant =>
	['more than'].

num_quant =>
	['exactly'].


section:'Indefinite Pronouns'.

paragraph:'Indefinite pronouns are represented by ''ipron'':'.

ipron(exist:plus, human:minus) =>
	[something].

ipron(exist:plus, human:plus) =>
	[somebody].

ipron(exist:minus, human:minus) =>
	//,
	[everything].

ipron(exist:minus, human:plus) =>
	//,
	[everybody].

ipron(exist:minus, human:minus) =>
	//,
	[nothing].

ipron(exist:minus, human:plus) =>
	//,
	[nobody].

