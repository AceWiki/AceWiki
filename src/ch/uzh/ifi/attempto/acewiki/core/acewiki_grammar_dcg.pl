% This code is automatically generated on the basis of a file in Codeco notation.
%
% For more information, see the package ch.uzh.ifi.attempto.codeco of the AceWiki system
% (http://attempto.ifi.uzh.ch/acewiki/) and the thesis "Controlled English for Knowledge
% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).


/* === AceWiki Grammar === */
/* - Tobias Kuhn, 2 August 2010 - */
/* Below, the grammar rules of the AceWiki grammar are shown: */

/* --- Texts and Sentences --- */
/* 'text' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero): */
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(text, []), Y/Z)-->[], ~(Y/Y/Z).
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(text, (Y, Z)), A1/B1)-->complete_sentence([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Y, A1/A2), text([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], Z, A2/Z2), ~(A1/Z2/B1).
/* A complete sentence is represented by the category 'complete_sentence' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark: */
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(complete_sentence, (Y, ['.'])), Z/A1)-->sentence([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y, Z/Z1), ['.'], ~(Z/Z1/A1).
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(complete_sentence, (//, Y, [?])), Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (Z/Z1), simple_sentence_2([minus, plus, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Y, Z1/W2), [?], ~(Z/W2/A1).
/* General sentences are represented by 'sentence': */
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence, Y), Z/A1)-->sentence_coord_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y, Z/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence, (//, ['for every'], Y, Z)), A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (A1/A2), ['for every'], nc([B2, C2, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], Y, A2/Y2), sentence_coord_1([Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3], Z, Y2/X3), ~(A1/X3/B1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence, (//, [if], Y, [then], Z)), A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (A1/A2), [if], sentence_coord_1([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], Y, A2/Z2), [then], sentence_coord_1([A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3], Z, Z2/Y3), ~(A1/Y3/B1).
/* Sentences can be coordinated using "or" ('sentence_coord_1') and "and"
		('sentence_coord_2'): */
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence_coord_1, Y), Z/A1)-->sentence_coord_2([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y, Z/A1).
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence_coord_1, (//, Y, [or], Z)), A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (A1/A2), sentence_coord_2([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], Y, A2/Z2), [or], sentence_coord_1([A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3], Z, Z2/Y3), ~(A1/Y3/B1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence_coord_2, Y), Z/A1)-->simple_sentence_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y, Z/A1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(sentence_coord_2, (Y, [and], Z)), A1/B1)-->simple_sentence_1([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Y, A1/A2), [and], sentence_coord_2([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], Z, A2/B1).
/* Uncoordinated sentences are represented in two levels by 'simple_sentence_1' and
		'simple_sentence_2': */
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_1, (//, ['it is false that'], Y)), Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (Z/Z1), ['it is false that'], simple_sentence_2([minus, minus, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Y, Z1/W2), ~(Z/W2/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_1, (['there is'], Y)), Z/A1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], Y, Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_1, (['there is'], Y, ['such that'], Z)), A1/B1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Y, A1/T1), ['such that'], simple_sentence_1([U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z, T1/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_1, (['there are'], Y)), Z/A1)-->['there are'], np([minus, minus, minus, plus, minus, plus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], Y, Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_1, Y), Z/A1)-->simple_sentence_2([minus, minus, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Y, Z/A1).
simple_sentence_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(simple_sentence_2(whin:A, whout:B), (Y, Z)), A1/B1)-->np([A, C1, minus, D1, E1, F1, nom, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Y, A1/X1), vp_coord_1([C1, B, G1, Y1, Z1, F1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z, X1/S2), ~(A1/S2/B1).

/* --- Verb Phrases --- */
/* Like sentences, verb phrases can be coordinated using "or" ('vp_coord_1') and "and"
		('vp_coord_2'): */
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(vp_coord_1(subj:C, pl:F, whin:A, whout:B), Y), Z/A1)-->vp_coord_2([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y, Z/A1).
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(vp_coord_1(subj:C, pl:F, whin:A, whout:B), (//, Y, [or], Z)), A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (A1/A2), vp_coord_2([A, B2, C, C2, D2, F, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Y, A2/W2), [or], vp_coord_1([B2, B, C, X2, Y2, F, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3], Z, W2/R3), ~(A1/R3/B1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(vp_coord_2(subj:C, pl:F, whin:A, whout:B), Y), Z/A1)-->vp([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y, Z/A1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(vp_coord_2(subj:C, pl:F, whin:A, whout:B), (Y, [and], Z)), A1/B1)-->vp([A, C1, C, D1, E1, F, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Y, A1/X1), [and], vp_coord_2([C1, B, C, Y1, Z1, F, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z, X1/B1).
/* Uncoordinated verb phrases represented by 'vp' can use an auxiliary verb: */
vp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(vp(subj:C, exist:D, rel:I, pl:F, whin:A, whout:B), (Y, Z)), A1/B1)-->aux([C1, D1, E1, D, F1, F, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y, A1/Y1), v([A, B, C, D, Z1, F, A2, B2, I, J1, inf, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], Z, Y1/P2), ~(A1/P2/B1).
vp([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(vp(subj:C, exist:plus, rel:H, pl:E, whin:A, whout:B), X), Y/Z)-->v([A, B, C, plus, A1, E, B1, C1, H, minus, fin, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1], X, Y/Q1), ~(Y/Q1/Z).
/* The category 'v' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement: */
v([A, A, B, C, D, E, F, G, H, minus, I, minus, J, K, L, M, N, O, P, Q, R, S, T, U], =>(v(be:minus, exist:C, pl:E, vform:I, copula:minus, whin:A, whout:A), V), W/X)-->verb([Y, Z, A1, C, B1, E, C1, D1, E1, minus, I, F1, itr, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], V, W/X).
v([A, B, C, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V], =>(v(subj:C, be:minus, exist:D, rel:I, pl:F, vform:J, embv:L, copula:minus, whin:A, whout:B), (W, X)), Y/Z)-->verb([A1, B1, C1, D, D1, F, E1, F1, G1, minus, J, H1, tr, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], W, Y/T1), np([A, B, C, U1, V1, W1, acc, X1, I, Y1, Z1, A2, tr, L, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2], X, T1/Z).
v([A, B, C, D, E, F, G, H, I, plus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V], =>(v(subj:C, be:plus, rel:I, embv:L, copula:minus, whin:A, whout:B), (W, X)), Y/Z)-->verb([A1, B1, C1, D1, E1, F1, G1, H1, I1, plus, J1, K1, tr, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W, Y/W1), np([A, B, C, X1, Y1, Z1, acc, A2, I, B2, C2, minus, D2, L, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2], X, W1/Z).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], =>(v(subj:C, be:plus, rel:I, embv:L, copula:plus, whin:A, whout:B), W), X/Y)-->np([A, B, C, Z, A1, minus, acc, B1, I, C1, D1, plus, E1, L, plus, F1, G1, H1, I1, J1, K1, L1, M1, N1], W, X/Y).
v([A, B, C, D, E, minus, F, G, H, plus, I, plus, J, K, L, M, N, O, P, Q, R, S, T, U], =>(v(subj:C, be:plus, rel:H, pl:minus, embv:K, copula:plus, whin:A, whout:B), V), W/X)-->np([A, B, C, Y, Z, minus, acc, A1, H, B1, C1, plus, D1, K, minus, E1, F1, G1, H1, I1, J1, K1, L1, M1], V, W/X).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], =>(v(subj:C, be:plus, rel:I, embv:L, copula:plus, whin:A, whout:B), (W, X)), Y/Z)--> $tradj([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], W, Y/Y), np([A, B, C, Y1, Z1, A2, acc, B2, I, C2, D2, minus, E2, L, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], X, Y/Z).

/* --- Noun Phrases --- */
/* Noun phrases are represented by 'np' and can consist of proper names, variables,
		pronouns, and different noun constructs: */
np([A, B, C, plus, plus, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], =>(np(id:E, exist:plus, rel:F, of:minus, def:plus, pl:minus, embv:K, whin:A, whout:B), (U, >>(id:E, human:V, gender:W, type:prop, hasvar:minus), X)), Y/Z)--> $propername([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, V, W, E, P1, Q1, R1, S1, T1, U1], U, Y/Y), [V1, W1, X1, Y1, Z1, A2, B2, E, C2, D2, E2, F2, G2, H2, I2, V, W, J2, prop, minus, K2, L2, M2, N2]>> (Y/O2), relcl([A, B, E, P2, Q2, R2, S2, T2, F, U2, V2, W2, X2, K, Y2, V, Z2, A3, B3, C3, D3, E3, F3, G3], X, O2/Z).
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:D, exist:plus, of:minus, def:plus, pl:minus, whin:A, whout:A), (#(D), T, >(id:D, type:var, hasvar:plus, var:U))), V/W)--> #(D), newvar([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, U, R1, S1, T1], T, V/U1), [V1, W1, X1, Y1, Z1, A2, B2, D, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, var, plus, U, M2, N2, O2]>U1/W.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:D, exist:plus, of:minus, def:plus, pl:minus, whin:A, whout:A), (T, U, <(id:D, type:noun, hasvar:plus, noun:V, var:W, human:X, gender:Y), >(id:D, human:X, gender:Y, type:ref, hasvar:minus))), Z/A1)--> $defnoun([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, V, W1, X1], T, Z/Z), $reference([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, W, P2, Q2, R2, S2, T2, U2], U, Z/Z), [+[V2, W2, X2, Y2, Z2, A3, B3, D, C3, D3, E3, F3, G3, H3, I3, X, Y, J3, noun, plus, W, V, K3, L3]]<Z/M3, [N3, O3, P3, Q3, R3, S3, T3, D, U3, V3, W3, X3, Y3, Z3, A4, X, Y, B4, ref, minus, C4, D4, E4, F4]>M3/A1.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:D, exist:plus, of:minus, def:plus, pl:minus, whin:A, whout:A), (T, <(id:D, type:noun, noun:U, human:V, gender:W), >(id:D, human:V, gender:W, type:ref, hasvar:minus))), X/Y)--> $defnoun([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U, U1, V1], T, X/X), [+[W1, X1, Y1, Z1, A2, B2, C2, D, D2, E2, F2, G2, H2, I2, J2, V, W, K2, noun, L2, M2, U, N2, O2]]<X/P2, [Q2, R2, S2, T2, U2, V2, W2, D, X2, Y2, Z2, A3, B3, C3, D3, V, W, E3, ref, minus, F3, G3, H3, I3]>P2/Y.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:D, exist:plus, of:minus, def:plus, pl:minus, whin:A, whout:A), (T, <(id:D, hasvar:plus, var:U, human:V, gender:W), >(id:D, human:V, gender:W, type:ref, hasvar:minus))), X/Y)--> $reference([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, U, Q1, R1, S1, T1, U1, V1], T, X/X), [+[W1, X1, Y1, Z1, A2, B2, C2, D, D2, E2, F2, G2, H2, I2, J2, V, W, K2, L2, plus, U, M2, N2, O2]]<X/P2, [Q2, R2, S2, T2, U2, V2, W2, D, X2, Y2, Z2, A3, B3, C3, D3, V, W, E3, ref, minus, F3, G3, H3, I3]>P2/Y.
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(np(id:G, subj:C, exist:D, rel:H, of:N, pl:minus, embv:M, whin:A, whout:B), (X, Y)), Z/A1)-->quant([B1, C1, D1, D, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], X, Z/Y1), nc([A, B, C, Z1, A2, B2, C2, G, H, D2, E2, F2, G2, M, N, H2, I2, J2, K2, L2, M2, N2, O2, P2], Y, Y1/A1).
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, minus, N, O, P, Q, R, S, T, U, V], =>(np(id:G, exist:D, rel:H, of:minus, pl:minus, embv:M, whin:A, whout:B), (#(G), W, X, >(id:G, human:Y, type:ipron, hasvar:Z, var:A1), B1)), C1/D1)--> #(G), ipron([E1, F1, G1, D, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, Y, S1, T1, U1, V1, W1, X1, Y1, Z1], W, C1/A2), opt_newvar([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, Z, A1, U2, V2, W2], X, A2/X2), [Y2, Z2, A3, B3, C3, D3, E3, G, F3, G3, H3, I3, J3, K3, L3, Y, M3, N3, ipron, Z, A1, O3, P3, Q3]>X2/R3, relcl([A, B, G, S3, T3, U3, V3, W3, H, X3, Y3, Z3, A4, M, B4, Y, C4, D4, E4, F4, G4, H4, I4, J4], B1, R3/D1).
np([A, A, B, plus, C, plus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:E, exist:plus, of:minus, pl:plus, copula:minus, whin:A, whout:A), (T, U, #(E), V)), W/X)-->num_quant([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], T, W/X), $number([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], U, X/X), #(E), $nounpl([U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3], V, X/X).
np([A, A, B, plus, C, minus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S], =>(np(id:E, exist:plus, of:minus, pl:minus, copula:minus, whin:A, whout:A), (T, ['1'], #(E), U, >(id:E, human:V, gender:W, type:noun, hasvar:minus, noun:X))), Y/Z)-->num_quant([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], T, Y/Y1), ['1'], #(E), $noun([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, V, W, X, O2, P2, Q2, R2, S2, T2], U, Y1/Y1), [U2, V2, W2, X2, Y2, Z2, A3, E, B3, C3, D3, E3, F3, G3, H3, V, W, I3, noun, minus, J3, X, K3, L3]>Y1/Z.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], =>(np(id:E, exist:plus, of:minus, pl:minus, whout:plus), (#(E), [what], >(id:E, human:minus, type:wh, hasvar:minus))), U/V)--> #(E), [what], [W, X, Y, Z, A1, B1, C1, E, D1, E1, F1, G1, H1, I1, J1, minus, K1, L1, wh, minus, M1, N1, O1, P1]>U/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], =>(np(id:E, exist:plus, of:minus, pl:minus, whout:plus), (#(E), [who], >(id:E, human:plus, type:wh, hasvar:minus))), U/V)--> #(E), [who], [W, X, Y, Z, A1, B1, C1, E, D1, E1, F1, G1, H1, I1, J1, plus, K1, L1, wh, minus, M1, N1, O1, P1]>U/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(np(id:E, subj:B, exist:plus, rel:F, of:L, embv:K, pl:minus, whout:plus), ([which], V)), W/X)-->[which], nc([plus, plus, B, Y, Z, A1, B1, E, F, C1, D1, E1, F1, K, L, G1, H1, I1, J1, K1, L1, M1, N1, O1], V, W/X).
np([A, plus, B, plus, C, plus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], =>(np(id:E, exist:plus, of:minus, pl:plus, whout:plus), ([which], #(E), U)), V/V)-->[which], #(E), $nounpl([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], U, V/V).
/* The category 'nc' represents nouns optionally followed by variables, relative clauses,
		and of-constructs: */
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W], =>(nc(id:H, rel:I, of:minus, embv:N, whin:A, whout:B), (X, Y, >(id:H, human:Z, gender:A1, type:noun, hasvar:B1, noun:C1, var:D1), E1)), F1/G1)-->n([H1, I1, J1, K1, L1, M1, N1, H, O1, P1, Q1, R1, S1, T1, U1, Z, A1, C1, V1, W1, X1, Y1, Z1, A2], X, F1/B2), opt_newvar([C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, B1, D1, V2, W2, X2], Y, B2/Y2), [Z2, A3, B3, C3, D3, E3, F3, H, G3, H3, I3, J3, K3, L3, M3, Z, A1, N3, noun, B1, D1, C1, O3, P3]>Y2/Q3, relcl([A, B, H, R3, S3, T3, U3, V3, I, W3, X3, Y3, Z3, N, A4, Z, B4, C4, D4, E4, F4, G4, H4, I4], E1, Q3/G1).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], =>(nc(subj:C, rel:I, of:plus, embv:N, whin:A, whout:B), (X, Y)), Z/A1)--> $nounof([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], X, Z/Z), np([A, B, C, Z1, A2, B2, acc, C2, I, D2, E2, F2, G2, N, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], Y, Z/R2), ~(Z/R2/A1).
/* The category 'n' stands for nouns: */
n([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(n(id:H, human:P, gender:Q, text:R), (#(H), Y)), Z/Z)--> #(H), $noun([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, Q, R, P1, Q1, R1, S1, T1, U1], Y, Z/Z).
/* New variables, optional and mandatory, are represented by 'opt_newvar' and 'newvar',
		respectively: */
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, minus, T, U, V, W], =>(opt_newvar(hasvar:minus), []), X/X)-->[].
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, plus, T, U, V, W], =>(opt_newvar(hasvar:plus, var:T), X), Y/Z)-->newvar([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, T, U1, V1, W1], X, Y/Z).
newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(newvar(var:U), (Y, /<(hasvar:plus, var:U))), Z/A1)--> $variable([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, U, S1, T1, U1, V1, W1, X1], Y, Z/Z), /<([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, plus, U, R2, S2, T2], Z/A1).

/* --- Relative Clauses --- */
/* Relative clauses are represented by 'relcl'. They start with a relative pronoun and
		are always optional: */
relcl([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(relcl(whin:A, whout:A), []), X/X)-->[].
relcl([A, B, C, D, E, F, G, H, plus, I, J, K, L, plus, M, N, O, P, Q, R, S, T, U, V], =>(relcl(subj:C, rel:plus, embv:plus, human:N, whin:A, whout:B), (W, X)), Y/Z)-->relpron([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, N, P1, Q1, R1, S1, T1, U1, V1, W1], W, Y/X1), relcl1([A, B, C, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, N, K2, L2, M2, N2, O2, P2, V1, Q2], X, X1/Z).
/* Like sentences and verb phrases, relative clauses can be coordinated by "or"
		('relcl1') and "and" ('relcl2'): */
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl1(subj:C, human:P, relpron:W, whin:A, whout:B), (//, Y, Z, A1)), B1/C1)-->[D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2]// (B1/B2), relcl2([A, C2, C, D2, E2, F2, G2, H2, minus, I2, J2, K2, L2, M2, N2, P, O2, P2, Q2, R2, S2, T2, W, U2], Y, B2/V2), or_relpron([W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, P, L3, M3, N3, O3, P3, Q3, W, R3], Z, V2/S3), relcl1([C2, B, C, T3, U3, V3, W3, X3, Y3, Z3, A4, B4, C4, D4, E4, P, F4, G4, H4, I4, J4, K4, W, L4], A1, S3/M4), ~(B1/M4/C1).
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl1(subj:C, human:P, relpron:W, whin:A, whout:B), Y), Z/A1)-->relcl2([A, B, C, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, P, N1, O1, P1, Q1, R1, S1, W, T1], Y, Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl2(subj:C, rel:I, relpron:W, human:P, whin:A, whout:B), (Y, Z, A1)), B1/C1)-->vp([A, D1, C, E1, F1, minus, G1, H1, minus, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Y, B1/X1), and_relpron([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, P, N2, O2, P2, Q2, R2, S2, W, T2], Z, X1/U2), relcl2([D1, B, C, V2, W2, X2, Y2, Z2, I, A3, B3, C3, D3, E3, F3, P, G3, H3, I3, J3, K3, L3, W, M3], A1, U2/C1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl2(subj:C, rel:I, whin:A, whout:B), Y), Z/A1)-->vp([A, B, C, B1, C1, minus, D1, E1, I, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], Y, Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl2(subj:C, whin:A, whout:B), (Y, Z, A1)), B1/C1)-->np([A, B, C, D1, E1, F1, nom, G1, minus, H1, I1, minus, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, minus], Y, B1/U1), aux([V1, W1, X1, Y1, Z1, F1, A2, B2, C2, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], Z, U1/R2), verb([S2, T2, U2, Y1, V2, F1, W2, X2, Y2, minus, inf, Z2, tr, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3], A1, R2/L3), ~(B1/L3/C1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(relcl2(subj:C, whin:A, whout:B), (Y, Z)), A1/B1)-->np([A, B, C, C1, D1, E1, nom, F1, minus, G1, H1, minus, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, minus], Y, A1/T1), verb([U1, V1, W1, plus, X1, E1, Y1, Z1, A2, minus, fin, B2, tr, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2], Z, T1/N2), ~(A1/N2/B1).
/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], =>(relpron(relpron:that), [that]), X/X)-->[that].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], =>(relpron(human:plus, relpron:who), [who]), W/W)-->[who].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], =>(relpron(human:minus, relpron:which), [which]), W/W)-->[which].
/* The categories 'or_relpron' and 'and_relpron' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor: */
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(or_relpron(human:P, relpron:W), ([or], Y)), Z/A1)-->[or], relpron([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q1, R1, S1, T1, U1, V1, W, W1], Y, Z/A1).
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], =>(or_relpron(relpron:that), ['or that']), X/X)-->['or that'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], =>(or_relpron(human:plus, relpron:who), ['or who']), W/W)-->['or who'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], =>(or_relpron(human:minus, relpron:which), ['or which']), W/W)-->['or which'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(and_relpron(human:P, relpron:W), ([and], Y)), Z/A1)-->[and], relpron([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q1, R1, S1, T1, U1, V1, W, W1], Y, Z/A1).
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], =>(and_relpron(relpron:that), ['and that']), X/X)-->['and that'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], =>(and_relpron(human:plus, relpron:who), ['and who']), W/W)-->['and who'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], =>(and_relpron(human:minus, relpron:which), ['and which']), W/W)-->['and which'].

/* --- Verbs --- */
/* The category 'verb' represents main verbs: */
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T], =>(verb(be:minus, vcat:tr, pl:minus, vform:fin), U), V/V)--> $verbsg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], U, V/V).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T], =>(verb(be:minus, vcat:tr, pl:plus, vform:fin), U), V/V)--> $verbinf([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], U, V/V).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, tr, K, L, M, N, O, P, Q, R, S, T, U], =>(verb(be:minus, vcat:tr, vform:inf), V), W/W)--> $verbinf([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V, W/W).
verb([A, B, C, D, E, F, G, H, I, plus, J, K, tr, L, M, N, O, P, Q, R, S, T, U, V], =>(verb(be:plus, vcat:tr), W), X/X)--> $pverb([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W, X/X).
/* Auxiliary verbs are represented by 'aux', which includes negation markers: */
aux([A, B, C, plus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:plus, pl:minus), [is]), V/V)-->[is].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:minus, pl:minus), (//, ['is not'])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['is not'].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:minus, pl:minus), (//, [is, not])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), [is, not].
aux([A, B, C, plus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:plus, pl:plus), [are]), V/V)-->[are].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:minus, pl:plus), (//, ['are not'])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['are not'].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:plus, exist:minus, pl:plus), (//, [are, not])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), [are, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:minus, exist:minus, pl:minus), (//, ['does not'])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['does not'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], =>(aux(be:minus, exist:minus, pl:plus), (//, ['do not'])), V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['do not'].

/* --- Quantifiers --- */
/* Existential and universal quantifiers are represented by 'quant': */
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(quant(exist:plus), [a]), X/X)-->[a].
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(quant(exist:plus), [an]), X/X)-->[an].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(quant(exist:minus), (//, [every])), X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (X/Y), [every].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], =>(quant(exist:minus), (//, [no])), X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (X/Y), [no].
/* The category 'num_quant' stands for numerical quantifiers: */
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(num_quant, ['at least']), Y/Y)-->['at least'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(num_quant, ['at most']), Y/Y)-->['at most'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(num_quant, ['less than']), Y/Y)-->['less than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(num_quant, ['more than']), Y/Y)-->['more than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], =>(num_quant, [exactly]), Y/Y)-->[exactly].

/* --- Indefinite Pronouns --- */
/* Indefinite pronouns are represented by 'ipron': */
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], =>(ipron(exist:plus, human:minus), [something]), W/W)-->[something].
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], =>(ipron(exist:plus, human:plus), [somebody]), W/W)-->[somebody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], =>(ipron(exist:minus, human:minus), (//, [everything])), W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [everything].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], =>(ipron(exist:minus, human:plus), (//, [everybody])), W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [everybody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], =>(ipron(exist:minus, human:minus), (//, [nothing])), W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [nothing].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], =>(ipron(exist:minus, human:plus), (//, [nobody])), W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [nobody].


~(I/T/O) --> {append([X,[//|N],I],T), \+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.
~(_/O/O) --> [].
//(_, T/[//|T]) --> [].
>(F, T/[>(F)|T]) --> [].
>>(F, T/[>>(F)|T]) --> [].
<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \+ member(-Q, L), \+ \+ member(+Q, L), !, member(+Q, L)}.
<(L, [R|T]/[R|T]) --> <(L,T/T).
/<(F, T/T) --> {\+ (member(R,T), R =.. [_,F])}, !.
#(#(P),L,L) :- length(L,P).
