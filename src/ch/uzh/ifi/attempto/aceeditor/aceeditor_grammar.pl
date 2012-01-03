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
% This is the grammar used by the ACE Editor in the Codeco format. It covers a subset of ACE.
%
% Author: Tobias Kuhn
%===================================================================================================


title:'ACE Editor Grammar'.

paragraph:'- Tobias Kuhn, 26 November 2010 -'.

paragraph:'Below, the grammar rules of the ACE Editor grammar are shown:'.


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

complete_sentence =>
	sentence,
	['.'].

complete_sentence ~>
	//,
	simple_sentence_2(qu:plus, whin:minus, whout:plus),
	['?'].

paragraph:'General sentences are represented by ''sentence'':'.

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
	np(id:ID, subj:minus, pl:PL, case:nom, qu:Qu, whin:WhIn, whout:WhTemp),
	vp_coord_1(subj:ID, pl:PL, qu:Qu, whin:WhTemp, whout:WhOut).


section:'Verb Phrases'.

paragraph:'Like sentences, verb phrases can be coordinated using "or" (''vp_coord_1'') and "and"
		(''vp_coord_2''):'.

vp_coord_1(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp_coord_2(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut).

vp_coord_1(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) ~>
	//,
	vp_coord_2(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhTemp),
	[or],
	vp_coord_1(subj:Subj, pl:PL, qu:Qu, whin:WhTemp, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut).

vp_coord_2(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, qu:Qu, whin:WhIn, whout:WhTemp),
	[and],
	vp_coord_2(subj:Subj, pl:PL, qu:Qu, whin:WhTemp, whout:WhOut).

paragraph:'Uncoordinated verb phrases represented by ''vp'' can use an auxiliary verb and can have
		verb phrase modifiers:'.

vp(subj:Subj, exist:E, rel:R, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) ~>
	aux(be:Be, exist:E, pl:PL),
	v(subj:Subj, be:Be, exist:E, pl:PL, rel:R, vform:inf, embv:EmbV, copula:Cop, qu:Qu, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, embv:EmbV, copula:Cop, qu:Qu, whin:WhTemp, whout:WhOut).

vp(subj:Subj, exist:plus, rel:R, pl:PL, qu:Qu, whin:WhIn, whout:WhOut) ~>
	v(subj:Subj, be:minus, exist:plus, pl:PL, rel:R, vform:fin, embv:EmbV, copula:Cop, qu:Qu, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, embv:EmbV, copula:Cop, qu:Qu, whin:WhTemp, whout:WhOut).

paragraph:'The category ''v'' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement:'.

v(be:minus, pl:PL, vform:VF, copula:minus, whin:Wh, whout:Wh) =>
	verb(vcat:itr, be:minus, pl:PL, vform:VF).

v(subj:Subj, be:minus, rel:R, pl:PL, vform:VF, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:minus, pl:PL, vform:VF),
	np(subj:Subj, rel:R, vcat:tr, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:minus, qu:Qu, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:plus),
	[by],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:plus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(subj:Subj, be:plus, rel:R, pl:minus, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:minus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

v(be:plus, copula:plus, whin:Wh, whout:Wh) =>
	adj_coord.

v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, qu:Qu, whin:WhIn, whout:WhOut) =>
	adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut).


section:'Noun Phrases'.

paragraph:'Noun phrases are represented by ''np'' and can consist of proper names, variables,
		pronouns, and different noun constructs:'.

np(id:ID, exist:plus, rel:R, of:minus, def:plus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	prop(id:ID, human:H, gender:G),
	>>(id:ID, human:H, gender:G, type:prop, hasvar:minus),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	#ID,
	newvar(var:Var),
	>(id:ID, type:var, hasvar:plus, var:Var).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$def_noun_sg(noun:Noun),
	$ref(text:Var),
	<(id:ID, type:noun, hasvar:plus, noun:Noun, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$def_noun_sg(noun:Noun),
	<(id:ID, type:noun, noun:Noun, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$ref(text:Var),
	<(id:ID, hasvar:plus, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

np(id:ID, subj:ID, exist:plus, of:minus, def:plus, pl:minus, refl:plus, whin:Wh, whout:Wh) =>
	$pron(refl:plus, human:H, gender:G),
	<(id:ID, human:H, gender:G).

np(id:ID, subj:Subj, exist:plus, of:minus, def:plus, pl:minus, refl:minus, case:C, whin:Wh, whout:Wh) =>
	$pron(refl:minus, case:C, human:H, gender:G),
	<(+(id:ID, human:H, gender:G), -(id:Subj)),
	>(id:ID, human:H, gender:G, type:pron, hasvar:minus).

np(id:ID, subj:Subj, exist:E, rel:R, of:O, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	quant(exist:E),
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:E, rel:R, of:minus, pl:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	#ID,
	ipron(exist:E, human:H),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, type:ipron, hasvar:HasVar, var:Var),
 	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

np(id:ID, exist:plus, of:minus, pl:plus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	$num,
	opt_adj_coord,
	#ID,
	$noun_pl.

np(id:ID, exist:plus, of:minus, pl:minus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	['1'],
	#ID,
	opt_adj_coord,
	$noun_sg(human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun).

np(id:ID, exist:plus, of:minus, pl:minus, qu:plus, whout:plus) =>
	#ID,
	[what],
	>(id:ID, human:minus, type:wh, hasvar:minus).

np(id:ID, exist:plus, of:minus, pl:minus, qu:plus, whout:plus) =>
	#ID,
	[who],
	>(id:ID, human:plus, type:wh, hasvar:minus).

np(id:ID, subj:Subj, exist:plus, rel:R, of:O, embv:EmbV, pl:minus, qu:plus, whout:plus) =>
	[which],
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, qu:plus, whin:plus, whout:plus).

np(id:ID, exist:plus, of:minus, pl:plus, qu:plus, whout:plus) =>
	[which],
	opt_adj_coord,
	#ID,
	$noun_pl.

paragraph:'The category ''nc'' represents nouns optionally followed by variables, relative clauses,
		and prepositional phrases using "of":'.

nc(id:ID, rel:R, of:minus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	n(id:ID, human:H, gender:G, text:Noun),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, gender:G, type:noun, hasvar:HasVar, noun:Noun, var:Var),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, qu:Qu, whin:WhIn, whout:WhOut).

nc(id:ID, subj:Subj, rel:R, of:plus, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) ~>
	n(id:ID, human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun),
	[of],
	np(subj:Subj, rel:R, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

paragraph:'The category ''n'' stands for nouns that are preceded by an optional adjective
		coordination:'.

n(id:ID, human:H, gender:G, text:Noun) =>
	opt_adj_coord,
	#ID,
	$noun_sg(human:H, gender:G, text:Noun).

paragraph:'New variables, optional and mandatory, are represented by ''opt_newvar'' and ''newvar'',
		respectively:'.

opt_newvar(hasvar:minus) => [].

opt_newvar(hasvar:plus, var:Var) =>
	newvar(var:Var).

newvar(var:Var) =>
	$var(text:Var),
	/<(hasvar:plus, var:Var).

paragraph:'Proper names can either require the definite article "the" or not, and are represented
		by the category ''prop'':'.

prop(id:P, human:H, gender:G) =>
	$prop_sg(human:H, gender:G, text:P).

prop(id:P, human:H, gender:G) =>
	$propdef_sg(human:H, gender:G, text:P).


section:'Adjectives'.

paragraph:'Adjectives can be only coordinated by "and", and are represented by ''opt_adj_coord''
		for the optional case and by ''adj_coord'' if mandatory:'.

opt_adj_coord =>
	[].

opt_adj_coord =>
	adj_coord.

adj_coord =>
	adj.

adj_coord =>
	adj,
	[and],
	adj_coord.

paragraph:'Uncoordinated adjectives are represented by ''adj'' and can be used in positive,
		comparative and superlative forms:'.

adj =>
	$adj_itr.

adj =>
	[more],
	$adj_itr.

adj =>
	$adj_itr_comp.

adj =>
	[most],
	$adj_itr.

adj =>
	$adj_itr_sup.

paragraph:'The category ''adjc'' stands for more complicated adjective constructions including
		nested noun phrases that represent a comparison object:'.

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_itr,
	[as],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$adj_itr_comp,
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_itr,
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[most],
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_tr(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[as],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_tr(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[as],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$adj_tr_comp(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[than],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).

adjc(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$adj_tr_comp(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhTemp),
	[than],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, qu:Qu, whin:WhTemp, whout:WhOut).


section:'Relative Clauses'.

paragraph:'Relative clauses are represented by ''relcl''. They start with a relative pronoun and
		are always optional:'.

relcl(whin:Wh, whout:Wh) =>
	[].

relcl(subj:ID, rel:plus, embv:plus, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

paragraph:'Like sentences and verb phrases, relative clauses can be coordinated by "or"
		(''relcl1'') and "and" (''relcl2''):'.

relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut) ~>
	//,
	relcl2(subj:ID, human:H, rel:minus, relpron:RP, qu:Qu, whin:WhIn, whout:WhTemp),
	or_relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhTemp, whout:WhOut).

relcl1(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut) =>
	relcl2(subj:ID, human:H, relpron:RP, qu:Qu, whin:WhIn, whout:WhOut).

relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:minus, pl:minus, qu:Qu, whin:WhIn, whout:WhTemp),
	and_relpron(human:H, relpron:RP),
	relcl2(subj:ID, rel:R, relpron:RP, human:H, qu:Qu, whin:WhTemp, whout:WhOut).

relcl2(subj:ID, rel:R, qu:Qu, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:R, pl:minus, qu:Qu, whin:WhIn, whout:WhOut).

relcl2(subj:Subj, rel:R, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:Subj, rel:minus, copula:minus, pl:PL, embv:EmbV, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhTemp),
	aux(be:minus, pl:PL),
	verb(vcat:tr, be:minus, pl:PL, vform:inf),
	vmod(subj:ID, rel:R, embv:EmbV, copula:minus, qu:Qu, whin:WhTemp, whout:WhOut).

relcl2(subj:Subj, rel:R, qu:Qu, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:Subj, rel:minus, copula:minus, pl:PL, embv:EmbV, case:nom, refl:minus, qu:Qu, whin:WhIn, whout:WhTemp),
	verb(vcat:tr, be:minus, pl:PL, vform:fin),
	vmod(subj:ID, rel:R, embv:EmbV, copula:minus, qu:Qu, whin:WhTemp, whout:WhOut).

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


section:'Verb Phrase Modifiers'.

% m
paragraph:'Verb phrase modifiers are represented by ''vmod'' and the auxiliary category ''vmod_x'',
		and are always optional:'.

vmod(whin:Wh, whout:Wh) =>
	[].

vmod(subj:Subj, rel:R, embv:minus, copula:Cop, qu:Qu, whin:WhIn, whout:WhOut) =>
	adv_coord(copula:Cop),
	vmod_x(subj:Subj, rel:R, copula:Cop, qu:Qu, whin:WhIn, whout:WhOut).

vmod(subj:Subj, rel:R, embv:minus, copula:Cop, qu:Qu, whin:WhIn, whout:WhOut) =>
	pp(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, rel:R, embv:EmbV, copula:Cop, qu:Qu, whin:WhTemp, whout:WhOut).

vmod_x(whin:Wh, whout:Wh) =>
	[].

vmod_x(subj:Subj, rel:R, copula:Cop, qu:Qu, whin:WhIn, whout:WhOut) =>
	pp(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, rel:R, embv:EmbV, copula:Cop, qu:Qu, whin:WhTemp, whout:WhOut).

paragraph:'The category ''pp'' represents prepositional phrases:'.

pp(subj:Subj, rel:R, embv:EmbV, qu:Qu, whin:WhIn, whout:WhOut) =>
	$prep,
	np(subj:Subj, rel:R, embv:EmbV, case:acc, qu:Qu, whin:WhIn, whout:WhOut).

paragraph:'Adverbs can be coordinated by "and", which is represented by ''adv_coord'':'.

adv_coord(copula:minus) =>
	adv_phrase.

adv_coord(copula:minus) =>
	adv_phrase,
	[and],
	adv_coord.

paragraph:'Adverbial phrases are represented by ''adv_phrase'', and can be in positive, comparative
		or superlative form:'.

adv_phrase =>
	$adv.

adv_phrase =>
	[more],
	$adv.

adv_phrase =>
	$adv_comp.

adv_phrase =>
	[most],
	$adv.

adv_phrase =>
	$adv_sup.


section:'Verbs'.

paragraph:'The category ''verb'' represents main verbs that can be intransitive or transitive:'.

verb(be:minus, vcat:itr, pl:minus, vform:fin) =>
	$iv_finsg.

verb(be:minus, vcat:itr, pl:plus, vform:fin) =>
	$iv_infpl.

verb(be:minus, vcat:itr, vform:inf) =>
	$iv_infpl.

verb(be:minus, vcat:tr, pl:minus, vform:fin) =>
	$tv_finsg.

verb(be:minus, vcat:tr, pl:plus, vform:fin) =>
	$tv_infpl.

verb(be:minus, vcat:tr, vform:inf) =>
	$tv_infpl.

verb(be:plus, vcat:tr) =>
	$tv_pp.

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

aux(be:minus, exist:minus) =>
	//,
	[can].

aux(be:minus, exist:minus) =>
	//,
	[should].

aux(be:minus, exist:minus) =>
	//,
	[must].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	['has to'].

aux(be:minus, exist:minus, pl:plus) =>
	//,
	['have to'].

aux(be:plus, exist:minus) =>
	//,
	[can, be].

aux(be:plus, exist:minus) =>
	//,
	[should, be].

aux(be:plus, exist:minus) =>
	//,
	[must, be].

aux(be:plus, exist:minus, pl:minus) =>
	//,
	['has to', be].

aux(be:plus, exist:minus, pl:plus) =>
	//,
	['have to', be].

aux(be:plus, exist:minus) =>
	//,
	[cannot, be].

aux(be:plus, exist:minus) =>
	//,
	[can, not, be].

aux(be:plus, exist:minus) =>
	//,
	[should, not, be].

aux(be:plus, exist:minus, pl:minus) =>
	//,
	['does not', 'have to', be].

aux(be:plus, exist:minus, pl:plus) =>
	//,
	['do not', 'have to', be].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	[cannot].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	[can, not].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	[should, not].

aux(be:minus, exist:minus, pl:minus) =>
	//,
	['does not', 'have to'].

aux(be:minus, exist:minus, pl:plus) =>
	//,
	['do not', 'have to'].


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


section:'Anaphoric Pronouns'.

paragraph:'The category ''pron'' represents reflexive and irreflexive anaphoric pronouns:'.

$pron(refl:plus, human:minus) => [itself].

$pron(refl:plus, human:plus, gender:masc) => [himself].

$pron(refl:plus, human:plus, gender:fem) => [herself].

$pron(refl:minus, human:minus) => [it].

$pron(refl:minus, case:nom, human:plus, gender:masc) => [he].

$pron(refl:minus, case:acc, human:plus, gender:masc) => [him].

$pron(refl:minus, case:nom, human:plus, gender:fem) => [she].

$pron(refl:minus, case:acc, human:plus, gender:fem) => [her].

