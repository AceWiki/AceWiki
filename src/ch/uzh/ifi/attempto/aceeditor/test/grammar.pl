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
% This is the grammar used by the ACE Editor in the Codeco format. It covers a subset of ACE.
%
% Author: Tobias Kuhn
%===================================================================================================

%% m t

title:'ACE Editor Grammar'.

paragraph:'- Tobias Kuhn, 2 August 2010 -'.

%% m
paragraph:'Below, the grammar rules of the ACE Editor grammar are shown:'.

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
complete_sentence =>
	sentence,
	['.'].

%% m t
complete_sentence ~>
	//,
	simple_sentence_2(whin:minus, whout:plus),
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
	nc(subj:minus),
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
	simple_sentence_2(whin:minus, whout:minus).

%% m t
simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus).

%% m t
simple_sentence_1 =>
	['there is'],
	np(subj:minus, exist:plus, def:minus, pl:minus, case:nom, whin:minus, whout:minus),
	['such that'],
	simple_sentence_1.

%% m t
simple_sentence_1 =>
	['there are'],
	np(subj:minus, exist:plus, def:minus, pl:plus, case:nom, whin:minus, whout:minus).

%% m t
simple_sentence_1 =>
	simple_sentence_2(whin:minus, whout:minus).

%% m t
simple_sentence_2(whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:minus, pl:PL, case:nom, whin:WhIn, whout:WhTemp),
	vp_coord_1(subj:ID, pl:PL, whin:WhTemp, whout:WhOut).

%% m t

section:'Verb Phrases'.

%% m
paragraph:'Like sentences, verb phrases can be coordinated using "or" (''vp_coord_1'') and "and"
		(''vp_coord_2''):'.

%% m t
vp_coord_1(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut).

%% m
vp_coord_1(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) ~>
	//,
	vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhTemp),
	[or],
	vp_coord_1(subj:Subj, pl:PL, whin:WhTemp, whout:WhOut).

%% m t
vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, whin:WhIn, whout:WhOut).

%% m t
vp_coord_2(subj:Subj, pl:PL, whin:WhIn, whout:WhOut) =>
	vp(subj:Subj, pl:PL, whin:WhIn, whout:WhTemp),
	[and],
	vp_coord_2(subj:Subj, pl:PL, whin:WhTemp, whout:WhOut).

%% m
paragraph:'Uncoordinated verb phrases represented by ''vp'' can use an auxiliary verb and can have
		verb phrase modifiers:'.

%% m t
vp(subj:Subj, exist:E, rel:R, pl:PL, whin:WhIn, whout:WhOut) ~>
	aux(be:Be, exist:E, pl:PL),
	v(subj:Subj, be:Be, exist:E, pl:PL, rel:R, vform:inf, embv:EmbV, copula:Cop, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, embv:EmbV, copula:Cop, whin:WhTemp, whout:WhOut).

%% m t
vp(subj:Subj, exist:plus, rel:R, pl:PL, whin:WhIn, whout:WhOut) ~>
	v(subj:Subj, be:minus, exist:plus, pl:PL, rel:R, vform:fin, embv:EmbV, copula:Cop, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, embv:EmbV, copula:Cop, whin:WhTemp, whout:WhOut).

%% m
paragraph:'The category ''v'' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement:'.

%% m t
v(be:minus, exist:E, pl:PL, vform:VF, copula:minus, whin:Wh, whout:Wh) =>
	verb(vcat:itr, be:minus, pl:PL, exist:E, vform:VF).

%% m t
v(subj:Subj, be:minus, exist:E, rel:R, pl:PL, vform:VF, embv:EmbV, copula:minus, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:minus, pl:PL, exist:E, vform:VF),
	np(subj:Subj, rel:R, vcat:tr, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:minus, whin:WhIn, whout:WhOut) =>
	verb(vcat:tr, be:plus, pl:PL, exist:E, vform:VF),
	[by],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:plus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
v(subj:Subj, be:plus, rel:R, pl:minus, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	np(subj:Subj, of:minus, rel:R, pl:minus, copula:plus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
v(be:plus, rel:R, copula:plus, whin:Wh, whout:Wh) =>
	adj_coord.

%% m t
v(subj:Subj, be:plus, rel:R, embv:EmbV, copula:plus, whin:WhIn, whout:WhOut) =>
	adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut).

%% m t

section:'Noun Phrases'.

%% m
paragraph:'Noun phrases are represented by ''np'' and can consist of proper names, variables,
		pronouns, and different noun constructs:'.

%% m t
np(id:ID, exist:plus, rel:R, of:minus, def:plus, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	prop(id:ID, human:H, gender:G),
	>>(id:ID, human:H, gender:G, type:prop, hasvar:minus),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	#ID,
	newvar(var:Var),
	>(id:ID, type:var, hasvar:plus, var:Var).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$def_noun_sg(noun:Noun),
	$ref(text:Var),
	<(id:ID, type:noun, hasvar:plus, noun:Noun, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$def_noun_sg(noun:Noun),
	<(id:ID, type:noun, noun:Noun, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, def:plus, pl:minus, whin:Wh, whout:Wh) =>
	$ref(text:Var),
	<(id:ID, hasvar:plus, var:Var, human:H, gender:G),
	>(id:ID, human:H, gender:G, type:ref, hasvar:minus).

%% m t
np(id:ID, subj:ID, exist:plus, of:minus, def:plus, pl:minus, refl:plus, whin:Wh, whout:Wh) =>
	$pron(refl:plus, human:H, gender:G),
	<(id:ID, human:H, gender:G).

%% m t
np(id:ID, subj:Subj, exist:plus, of:minus, def:plus, pl:minus, refl:minus, case:C, whin:Wh, whout:Wh) =>
	$pron(refl:minus, case:C, human:H, gender:G),
	<(+(id:ID, human:H, gender:G), -(id:Subj)),
	>(id:ID, human:H, gender:G, type:pron, hasvar:minus).

%% m t
np(id:ID, subj:Subj, exist:E, rel:R, of:O, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	quant(exist:E),
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:E, rel:R, of:minus, pl:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	#ID,
	ipron(exist:E, human:H),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, type:ipron, hasvar:HasVar, var:Var),
 	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

%% m t
np(id:ID, exist:plus, of:minus, pl:plus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	$num,
	opt_adj_coord,
	#ID,
	$noun_pl.

%% m t
np(id:ID, exist:plus, of:minus, pl:minus, copula:minus, whin:Wh, whout:Wh) =>
	num_quant,
	['1'],
	#ID,
	opt_adj_coord,
	$noun_sg(human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun).

%% m
np(id:ID, exist:plus, of:minus, pl:minus, whout:plus) =>
	#ID,
	[what],
	>(id:ID, human:minus, type:wh, hasvar:minus).

%% m t
np(id:ID, exist:plus, of:minus, pl:minus, whout:plus) =>
	#ID,
	[who],
	>(id:ID, human:plus, type:wh, hasvar:minus).

%% m t
np(id:ID, subj:Subj, exist:plus, rel:R, of:O, embv:EmbV, pl:minus, whout:plus) =>
	[which],
	nc(id:ID, subj:Subj, rel:R, of:O, embv:EmbV, whin:plus, whout:plus).

%% m t
np(id:ID, exist:plus, of:minus, pl:plus, whout:plus) =>
	[which],
	opt_adj_coord,
	#ID,
	$noun_pl.

%% m
paragraph:'The category ''nc'' represents nouns optionally followed by variables, relative clauses,
		and prepositional phrases using "of":'.

%% m t
nc(id:ID, rel:R, of:minus, embv:EmbV, whin:WhIn, whout:WhOut) =>
	n(id:ID, human:H, gender:G, text:Noun),
	opt_newvar(hasvar:HasVar, var:Var),
	>(id:ID, human:H, gender:G, type:noun, hasvar:HasVar, noun:Noun, var:Var),
	relcl(subj:ID, rel:R, embv:EmbV, human:H, whin:WhIn, whout:WhOut).

%% m t
nc(id:ID, subj:Subj, rel:R, of:plus, embv:EmbV, whin:WhIn, whout:WhOut) ~>
	n(id:ID, human:H, gender:G, text:Noun),
	>(id:ID, human:H, gender:G, type:noun, hasvar:minus, noun:Noun),
	[of],
	np(subj:Subj, rel:R, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m
paragraph:'The category ''n'' stands for nouns that are preceded by an optional adjective
		coordination:'.

%% m t
n(id:ID, human:H, gender:G, text:Noun) =>
	opt_adj_coord,
	#ID,
	$noun_sg(human:H, gender:G, text:Noun).

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
	$var(text:Var),
	/<(hasvar:plus, var:Var).

%% m
paragraph:'Proper names can either require the definite article "the" or not, and are represented
		by the category ''prop'':'.

%% m t
prop(id:P, human:H, gender:G) =>
	$prop_sg(human:H, gender:G, text:P).

%% m
prop(id:P, human:H, gender:G) =>
	$propdef_sg(human:H, gender:G, text:P).

%% m t

section:'Adjectives'.

%% m
paragraph:'Adjectives can be only coordinated by "and", and are represented by ''opt_adj_coord''
		for the optional case and by ''adj_coord'' if mandatory:'.

%% m t
opt_adj_coord =>
	[].

%% m t
opt_adj_coord =>
	adj_coord.

%% m t
adj_coord =>
	adj.

%% m t
adj_coord =>
	adj,
	[and],
	adj_coord.

%% m
paragraph:'Uncoordinated adjectives are represented by ''adj'' and can be used in positive,
		comparative and superlative forms:'.

%% m t
adj =>
	$adj_itr.

%% m
adj =>
	[more],
	$adj_itr.

%% m
adj =>
	$adj_itr_comp.

%% m
adj =>
	[most],
	$adj_itr.

%% m
adj =>
	$adj_itr_sup.

%% m
paragraph:'The category ''adjc'' stands for more complicated adjective constructions including
		nested noun phrases that represent a comparison object:'.

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_itr,
	[as],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$adj_itr_comp,
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_itr,
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[most],
	$adj_tr(prep:_),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_tr(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[as],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[as],
	$adj_tr(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[as],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$adj_tr_comp(prep:_),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[than],
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	[more],
	$adj_tr(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[than],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m t
adjc(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$adj_tr_comp(prep:P),
	np(subj:Subj, rel:minus, copula:minus, embv:EmbV, case:acc, whin:WhIn, whout:WhTemp),
	[than],
	$adj_prep(prep:P),
	np(subj:Subj, rel:R, copula:minus, embv:EmbV, case:acc, whin:WhTemp, whout:WhOut).

%% m t

section:'Relative Clauses'.

%% m
paragraph:'Relative pronouns are represented by ''relpron'' and can be either "that", "who" or
		"which":'.

%% m t
relcl(whin:Wh, whout:Wh) =>
	[].

%% m t
relcl(subj:ID, rel:plus, embv:plus, human:H, whin:WhIn, whout:WhOut) =>
	relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut).

%% m
paragraph:'Like sentences and verb phrases, relative clauses can be coordinated by "or"
		(''relcl1'') and "and" (''relcl2''):'.

%% m
relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut) ~>
	//,
	relcl2(subj:ID, human:H, rel:minus, relpron:RP, whin:WhIn, whout:WhTemp),
	or_relpron(human:H, relpron:RP),
	relcl1(subj:ID, human:H, relpron:RP, whin:WhTemp, whout:WhOut).

%% m t
relcl1(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut) =>
	relcl2(subj:ID, human:H, relpron:RP, whin:WhIn, whout:WhOut).

%% m t
relcl2(subj:ID, rel:R, relpron:RP, human:H, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:minus, pl:minus, whin:WhIn, whout:WhTemp),
	and_relpron(human:H, relpron:RP),
	relcl2(subj:ID, rel:R, relpron:RP, human:H, whin:WhTemp, whout:WhOut).

%% m t
relcl2(subj:ID, rel:R, whin:WhIn, whout:WhOut) =>
	vp(subj:ID, rel:R, pl:minus, whin:WhIn, whout:WhOut).

%% m t
relcl2(subj:Subj, rel:R, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:Subj, rel:minus, copula:minus, pl:PL, embv:EmbV, case:nom, refl:minus, whin:WhIn, whout:WhTemp),
	aux(be:minus, exist:E, pl:PL),
	verb(vcat:tr, be:minus, exist:E, pl:PL, vform:inf),
	vmod(subj:ID, rel:R, embv:EmbV, copula:minus, whin:WhTemp, whout:WhOut).

%% m t
relcl2(subj:Subj, rel:R, whin:WhIn, whout:WhOut) ~>
	np(id:ID, subj:Subj, rel:minus, copula:minus, pl:PL, embv:EmbV, case:nom, refl:minus, whin:WhIn, whout:WhTemp),
	verb(vcat:tr, be:minus, exist:plus, pl:PL, vform:fin),
	vmod(subj:ID, rel:R, embv:EmbV, copula:minus, whin:WhTemp, whout:WhOut).

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

section:'Verb Phrase Modifiers'.

% m
paragraph:'Verb phrase modifiers are represented by ''vmod'' and the auxiliary category ''vmod_x'',
		and are always optional:'.

%% m t
vmod(whin:Wh, whout:Wh) =>
	[].

%% m t
vmod(subj:Subj, rel:R, embv:minus, copula:Cop, whin:WhIn, whout:WhOut) =>
	adv_coord(copula:Cop),
	vmod_x(subj:Subj, rel:R, copula:Cop, whin:WhIn, whout:WhOut).

%% m t
vmod(subj:Subj, rel:R, embv:minus, copula:Cop, whin:WhIn, whout:WhOut) =>
	pp(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, rel:R, embv:EmbV, copula:Cop, whin:WhTemp, whout:WhOut).

%% m t
vmod_x(whin:Wh, whout:Wh) =>
	[].

%% m t
vmod_x(subj:Subj, rel:R, copula:Cop, whin:WhIn, whout:WhOut) =>
	pp(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhTemp),
	vmod(subj:Subj, rel:R, embv:EmbV, copula:Cop, whin:WhTemp, whout:WhOut).

%% m
paragraph:'The category ''pp'' represents prepositional phrases:'.

%% m t
pp(subj:Subj, rel:R, embv:EmbV, whin:WhIn, whout:WhOut) =>
	$prep,
	np(subj:Subj, rel:R, embv:EmbV, case:acc, whin:WhIn, whout:WhOut).

%% m
paragraph:'Adverbs can be coordinated by "and", which is represented by ''adv_coord'':'.

%% m t
adv_coord(copula:minus) =>
	adv_phrase.

%% m t
adv_coord(copula:minus) =>
	adv_phrase,
	[and],
	adv_coord.

%% m
paragraph:'Adverbial phrases are represented by ''adv_phrase'', and can be in positive, comparative
		or superlative form:'.

%% m t
adv_phrase =>
	$adv.

%% m
adv_phrase =>
	[more],
	$adv.

%% m
adv_phrase =>
	$adv_comp.

%% m
adv_phrase =>
	[most],
	$adv.

%% m
adv_phrase =>
	$adv_sup.

%% m t

section:'Verbs'.

%% m
paragraph:'The category ''verb'' represents main verbs that can be intransitive or transitive:'.

%% m t
verb(be:minus, vcat:itr, pl:minus, vform:fin) =>
	$iv_finsg.

%% m t
verb(be:minus, vcat:itr, pl:plus, vform:fin) =>
	$iv_infpl.

%% m t
verb(be:minus, vcat:itr, vform:inf) =>
	$iv_infpl.

%% m t
verb(be:minus, vcat:tr, pl:minus, vform:fin) =>
	$tv_finsg.

%% m t
verb(be:minus, vcat:tr, pl:plus, vform:fin) =>
	$tv_infpl.

%% m t
verb(be:minus, vcat:tr, vform:inf) =>
	$tv_infpl.

%% m t
verb(be:plus, vcat:tr) =>
	$tv_pp.

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

%% m
aux(be:minus, exist:minus) =>
	//,
	[can].

%% m
aux(be:minus, exist:minus) =>
	//,
	[should].

%% m
aux(be:minus, exist:minus) =>
	//,
	[must].

%% m
aux(be:minus, exist:minus, pl:minus) =>
	//,
	['has to'].

%% m
aux(be:minus, exist:minus, pl:plus) =>
	//,
	['have to'].

%% m
aux(be:plus, exist:minus) =>
	//,
	[can, be].

%% m
aux(be:plus, exist:minus) =>
	//,
	[should, be].

%% m
aux(be:plus, exist:minus) =>
	//,
	[must, be].

%% m
aux(be:plus, exist:minus, pl:minus) =>
	//,
	['has to', be].

%% m
aux(be:plus, exist:minus, pl:plus) =>
	//,
	['have to', be].

%% m
aux(be:plus, exist:minus) =>
	//,
	[cannot, be].

%% m
aux(be:plus, exist:minus) =>
	//,
	[can, not, be].

%% m
aux(be:plus, exist:minus) =>
	//,
	[should, not, be].

%% m
aux(be:plus, exist:minus, pl:minus) =>
	//,
	['does not', 'have to', be].

%% m
aux(be:plus, exist:minus, pl:plus) =>
	//,
	['do not', 'have to', be].

%% m
aux(be:minus, exist:minus, pl:minus) =>
	//,
	[cannot].

%% m
aux(be:minus, exist:minus, pl:minus) =>
	//,
	[can, not].

%% m
aux(be:minus, exist:minus, pl:minus) =>
	//,
	[should, not].

%% m
aux(be:minus, exist:minus, pl:minus) =>
	//,
	['does not', 'have to'].

%% m
aux(be:minus, exist:minus, pl:plus) =>
	//,
	['do not', 'have to'].

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
quant(exist:minus) =>
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
ipron(exist:minus, human:minus) =>
	//,
	[everything].

%% m t
ipron(exist:minus, human:plus) =>
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

%% m t

section:'Anaphoric Pronouns'.

%% m
paragraph:'The category ''pron'' represents reflexive and irreflexive anaphoric pronouns:'.

%% m
$pron(refl:plus, human:minus) => [itself].

%% m
$pron(refl:plus, human:plus, gender:masc) => [himself].

%% m t
$pron(refl:plus, human:plus, gender:fem) => [herself].

%% m
$pron(refl:minus, human:minus) => [it].

%% m
$pron(refl:minus, case:nom, human:plus, gender:masc) => [he].

%% m
$pron(refl:minus, case:acc, human:plus, gender:masc) => [him].

%% m t
$pron(refl:minus, case:nom, human:plus, gender:fem) => [she].

%% m t
$pron(refl:minus, case:acc, human:plus, gender:fem) => [her].

%% t

section:'Lexicon'.

$prop_sg(human:plus, gender:fem, text:'Mary') => ['Mary'].
$def_noun_sg(noun:woman) => ['the woman'].
$ref(text:'X') => ['X'].
$num => ['2'].
$noun_pl => [women].
$noun_sg(text:woman, human:plus, gender:fem) => [woman].
$var(text:'X') => ['X'].
$iv_finsg => [waits].
$iv_infpl => [wait].
$tv_finsg => [asks].
$tv_infpl => [ask].
$tv_pp => [asked].
$adj_itr => [young].
$adj_itr_comp => [younger].
$adj_tr(prep:about) => ['mad-about'].
$adj_tr_comp(prep:about) => ['madder-about'].
$adj_prep(prep:about) => [about].
$prep => [for].
$adv => [early].

%% t

section:'Auxiliary Rules for Testing'.

test => complete_sentence, fill.
fill => [].
fill => [''], fill.
