% This code is automatically generated on the basis of a file in Codeco notation.
%
% For more information, see the package ch.uzh.ifi.attempto.codeco of the Attempto Java Packages
% (http://attempto.ifi.uzh.ch/site/downloads/) and the thesis "Controlled English for Knowledge
% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).


/* === AceWiki Grammar === */
/* - Tobias Kuhn, 2 August 2010 - */
/* Below, the grammar rules of the AceWiki grammar are shown: */

/* --- Texts and Sentences --- */
/* 'text' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero): */
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[], ~(Y/Y/Z).
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->complete_sentence([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), text([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], Y1/X2), ~(Y/X2/Z).
/* A complete sentence is represented by the category 'complete_sentence' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark: */
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->sentence([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), ['.'], ~(Y/Y1/Z).
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), simple_sentence_2([minus, plus, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], Y1/V2), [?], ~(Y/V2/Z).
/* General sentences are represented by 'sentence': */
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->sentence_coord_1([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Z).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), ['for every'], nc([Z1, A2, minus, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Y1/W2), sentence_coord_1([X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3], W2/V3), ~(Y/V3/Z).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), [if], sentence_coord_1([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], Y1/X2), [then], sentence_coord_1([Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3], X2/W3), ~(Y/W3/Z).
/* Sentences can be coordinated using "or" ('sentence_coord_1') and "and"
		('sentence_coord_2'): */
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->sentence_coord_2([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Z).
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), sentence_coord_2([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], Y1/X2), [or], sentence_coord_1([Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3], X2/W3), ~(Y/W3/Z).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->simple_sentence_1([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Z).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->simple_sentence_1([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), [and], sentence_coord_2([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], Y1/Z).
/* Uncoordinated sentences are represented in two levels by 'simple_sentence_1' and
		'simple_sentence_2': */
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), ['it is false that'], simple_sentence_2([minus, minus, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], Y1/V2), ~(Y/V2/Z).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], Y/Z).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], Y/R1), ['such that'], simple_sentence_1([S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], R1/Z).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->['there are'], np([minus, minus, minus, plus, minus, plus, nom, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], Y/Z).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->simple_sentence_2([minus, minus, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Y/Z).
simple_sentence_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->np([A, A1, minus, B1, C1, D1, nom, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y/V1), vp_coord_1([A1, B, E1, W1, X1, D1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], V1/Q2), ~(Y/Q2/Z).

/* --- Verb Phrases --- */
/* Like sentences, verb phrases can be coordinated using "or" ('vp_coord_1') and "and"
		('vp_coord_2'): */
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->vp_coord_2([A, B, C, A1, B1, F, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], Y/Z).
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), vp_coord_2([A, Z1, C, A2, B2, F, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], Y1/U2), [or], vp_coord_1([Z1, B, C, V2, W2, F, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3], U2/P3), ~(Y/P3/Z).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->vp([A, B, C, A1, B1, F, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], Y/Z).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->vp([A, A1, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y/V1), [and], vp_coord_2([A1, B, C, W1, X1, F, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], V1/Z).
/* Uncoordinated verb phrases represented by 'vp' can use an auxiliary verb: */
vp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->aux([A1, B1, C1, D, D1, F, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Y/W1), v([A, B, C, D, X1, F, Y1, Z1, I, H1, inf, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2], W1/N2), ~(Y/N2/Z).
vp([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->v([A, B, C, plus, Z, E, A1, B1, H, minus, fin, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1], X/P1), ~(X/P1/Y).
/* The category 'v' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement: */
v([A, A, B, C, D, E, F, G, H, minus, I, minus, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->verb([X, Y, Z, C, A1, E, B1, C1, D1, minus, I, E1, itr, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1], V/W).
v([A, B, C, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->verb([Y, Z, A1, D, B1, F, C1, D1, E1, minus, J, F1, tr, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], W/R1), np([A, B, C, S1, T1, U1, acc, V1, I, W1, X1, Y1, tr, L, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2], R1/X).
v([A, B, C, D, E, F, G, H, I, plus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->verb([Y, Z, A1, B1, C1, D1, E1, F1, G1, plus, H1, I1, tr, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], W/U1), np([A, B, C, V1, W1, X1, acc, Y1, I, Z1, A2, minus, B2, L, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2], U1/X).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->np([A, B, C, Y, Z, minus, acc, A1, I, B1, C1, plus, D1, L, plus, E1, F1, G1, H1, I1, J1, K1, L1, M1], W/X).
v([A, B, C, D, E, minus, F, G, H, plus, I, plus, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->np([A, B, C, X, Y, minus, acc, Z, H, A1, B1, plus, C1, K, minus, D1, E1, F1, G1, H1, I1, J1, K1, L1], V/W).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)--> $tradj([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W/W), np([A, B, C, W1, X1, Y1, acc, Z1, I, A2, B2, minus, C2, L, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2], W/X).

/* --- Noun Phrases --- */
/* Noun phrases are represented by 'np' and can consist of proper names, variables,
		pronouns, and different noun constructs: */
np([A, B, C, plus, plus, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], U/V)--> $propername([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, E, N1, O1, P1, Q1, R1, S1], U/U), [T1, U1, V1, W1, X1, Y1, Z1, E, A2, B2, C2, D2, E2, F2, G2, L1, M1, H2, prop, minus, I2, J2, K2, L2]>> (U/M2), relcl([A, B, E, N2, O2, P2, Q2, R2, F, S2, T2, U2, V2, K, W2, L1, X2, Y2, Z2, A3, B3, C3, D3, E3], M2/V).
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)--> #(D), newvar([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/T1), [U1, V1, W1, X1, Y1, Z1, A2, D, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, var, plus, P1, L2, M2, N2]>T1/U.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)--> $defnoun([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/T), $reference([T1, U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], T/T), [+[R2, S2, T2, U2, V2, W2, X2, D, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, noun, plus, K2, Q1, I3, J3]]<T/K3, [L3, M3, N3, O3, P3, Q3, R3, D, S3, T3, U3, V3, W3, X3, Y3, F3, G3, Z3, ref, minus, A4, B4, C4, D4]>K3/U.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)--> $defnoun([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/T), [+[T1, U1, V1, W1, X1, Y1, Z1, D, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, noun, K2, L2, Q1, M2, N2]]<T/O2, [P2, Q2, R2, S2, T2, U2, V2, D, W2, X2, Y2, Z2, A3, B3, C3, H2, I2, D3, ref, minus, E3, F3, G3, H3]>O2/U.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)--> $reference([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/T), [+[T1, U1, V1, W1, X1, Y1, Z1, D, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, plus, M1, L2, M2, N2]]<T/O2, [P2, Q2, R2, S2, T2, U2, V2, D, W2, X2, Y2, Z2, A3, B3, C3, H2, I2, D3, ref, minus, E3, F3, G3, H3]>O2/U.
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->quant([Z, A1, B1, D, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], X/W1), nc([A, B, C, X1, Y1, Z1, A2, G, H, B2, C2, D2, E2, M, N, F2, G2, H2, I2, J2, K2, L2, M2, N2], W1/Y).
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, minus, N, O, P, Q, R, S, T, U, V], W/X)--> #(G), ipron([Y, Z, A1, D, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], W/V1), opt_newvar([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], V1/U2), [V2, W2, X2, Y2, Z2, A3, B3, G, C3, D3, E3, F3, G3, H3, I3, M1, J3, K3, ipron, P2, Q2, L3, M3, N3]>U2/O3, relcl([A, B, G, P3, Q3, R3, S3, T3, H, U3, V3, W3, X3, M, Y3, M1, Z3, A4, B4, C4, D4, E4, F4, G4], O3/X).
np([A, A, B, plus, C, plus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)-->num_quant([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/U), $number([T1, U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], U/U), #(E), $nounpl([R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3], U/U).
np([A, A, B, plus, C, minus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S], T/U)-->num_quant([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], T/T1), ['1'], #(E), $noun([U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], T1/T1), [S2, T2, U2, V2, W2, X2, Y2, E, Z2, A3, B3, C3, D3, E3, F3, J2, K2, G3, noun, minus, H3, L2, I3, J3]>T1/U.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], U/V)--> #(E), [what], [W, X, Y, Z, A1, B1, C1, E, D1, E1, F1, G1, H1, I1, J1, minus, K1, L1, wh, minus, M1, N1, O1, P1]>U/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], U/V)--> #(E), [who], [W, X, Y, Z, A1, B1, C1, E, D1, E1, F1, G1, H1, I1, J1, plus, K1, L1, wh, minus, M1, N1, O1, P1]>U/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[which], nc([plus, plus, B, X, Y, Z, A1, E, F, B1, C1, D1, E1, K, L, F1, G1, H1, I1, J1, K1, L1, M1, N1], V/W).
np([A, plus, B, plus, C, plus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T], U/U)-->[which], #(E), $nounpl([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], U/U).
/* The category 'nc' represents nouns optionally followed by variables, relative clauses,
		and of-constructs: */
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W], X/Y)-->n([Z, A1, B1, C1, D1, E1, F1, H, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], X/W1), opt_newvar([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], W1/V2), [W2, X2, Y2, Z2, A3, B3, C3, H, D3, E3, F3, G3, H3, I3, J3, N1, O1, K3, noun, Q2, R2, P1, L3, M3]>V2/N3, relcl([A, B, H, O3, P3, Q3, R3, S3, I, T3, U3, V3, W3, N, X3, N1, Y3, Z3, A4, B4, C4, D4, E4, F4], N3/Y).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/Y)--> $nounof([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X), np([A, B, C, X1, Y1, Z1, acc, A2, I, B2, C2, D2, E2, N, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], X/P2), ~(X/P2/Y).
/* The category 'n' stands for nouns: */
n([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)--> #(H), $noun([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, P, Q, R, O1, P1, Q1, R1, S1, T1], Y/Y).
/* New variables, optional and mandatory, are represented by 'opt_newvar' and 'newvar',
		respectively: */
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, minus, T, U, V, W], X/X)-->[].
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, plus, T, U, V, W], X/Y)-->newvar([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T, T1, U1, V1], X/Y).
newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)--> $variable([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, U, R1, S1, T1, U1, V1, W1], Y/Y), /<([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, plus, U, Q2, R2, S2], Y/Z).

/* --- Relative Clauses --- */
/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
relcl([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)-->[].
relcl([A, B, C, D, E, F, G, H, plus, I, J, K, L, plus, M, N, O, P, Q, R, S, T, U, V], W/X)-->relpron([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N, N1, O1, P1, Q1, R1, S1, T1, U1], W/V1), relcl1([A, B, C, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, N, I2, J2, K2, L2, M2, N2, T1, O2], V1/X).
/* Like sentences and verb phrases, relative clauses can be coordinated by "or"
		('relcl1') and "and" ('relcl2'): */
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (Y/Y1), relcl2([A, Z1, C, A2, B2, C2, D2, E2, minus, F2, G2, H2, I2, J2, K2, P, L2, M2, N2, O2, P2, Q2, W, R2], Y1/S2), or_relpron([T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, P, I3, J3, K3, L3, M3, N3, W, O3], S2/P3), relcl1([Z1, B, C, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3, A4, B4, P, C4, D4, E4, F4, G4, H4, W, I4], P3/J4), ~(Y/J4/Z).
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->relcl2([A, B, C, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, P, M1, N1, O1, P1, Q1, R1, W, S1], Y/Z).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->vp([A, A1, C, B1, C1, minus, D1, E1, minus, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], Y/U1), and_relpron([V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, P, K2, L2, M2, N2, O2, P2, W, Q2], U1/R2), relcl2([A1, B, C, S2, T2, U2, V2, W2, I, X2, Y2, Z2, A3, B3, C3, P, D3, E3, F3, G3, H3, I3, W, J3], R2/Z).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->vp([A, B, C, A1, B1, minus, C1, D1, I, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Y/Z).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->np([A, B, C, A1, B1, C1, nom, D1, minus, E1, F1, minus, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, minus], Y/R1), aux([S1, T1, U1, V1, W1, C1, X1, Y1, Z1, minus, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2], R1/O2), verb([P2, Q2, R2, V1, S2, C1, T2, U2, V2, minus, inf, W2, tr, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3], O2/I3), ~(Y/I3/Z).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->np([A, B, C, A1, B1, C1, nom, D1, minus, E1, F1, minus, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, minus], Y/R1), verb([S1, T1, U1, plus, V1, C1, W1, X1, Y1, minus, fin, Z1, tr, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2], R1/L2), ~(Y/L2/Z).
/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], X/X)-->[that].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], W/W)-->[who].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], W/W)-->[which].
/* The categories 'or_relpron' and 'and_relpron' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor: */
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[or], relpron([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, P1, Q1, R1, S1, T1, U1, W, V1], Y/Z).
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], X/X)-->['or that'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], W/W)-->['or who'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], W/W)-->['or which'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[and], relpron([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, P1, Q1, R1, S1, T1, U1, W, V1], Y/Z).
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, that, W], X/X)-->['and that'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, who, V], W/W)-->['and who'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, which, V], W/W)-->['and which'].

/* --- Verbs --- */
/* The category 'verb' represents main verbs: */
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T], U/U)--> $verbsg([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], U/U).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T], U/U)--> $verbinf([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], U/U).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, tr, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $verbinf([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], V/V).
verb([A, B, C, D, E, F, G, H, I, plus, J, K, tr, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $pverb([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], W/W).
/* Auxiliary verbs are represented by 'aux', which includes negation markers: */
aux([A, B, C, plus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/V)-->[is].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['is not'].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), [is, not].
aux([A, B, C, plus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/V)-->[are].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['are not'].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), [are, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['does not'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->[X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1]// (V/W), ['do not'].

/* --- Quantifiers --- */
/* Existential and universal quantifiers are represented by 'quant': */
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)-->[a].
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)-->[an].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (X/Y), [every].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (X/Y), [no].
/* The category 'num_quant' stands for numerical quantifiers: */
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->['at least'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->['at most'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->['less than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->['more than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[exactly].

/* --- Indefinite Pronouns --- */
/* Indefinite pronouns are represented by 'ipron': */
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], W/W)-->[something].
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], W/W)-->[somebody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [everything].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [everybody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [nothing].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1]// (W/X), [nobody].


~(I/T/O) --> {append([X,[//|N],I],T), \+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.
~(_/O/O) --> [].
//(_, T/[//|T]) --> [].
>(F, T/[>(F)|T]) --> [].
>>(F, T/[>>(F)|T]) --> [].
<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \+ member(-Q, L), \+ \+ member(+Q, L), !, member(+Q, L)}.
<(L, [R|T]/[R|T]) --> <(L,T/T).
/<(F, T/T) --> {\+ (member(R,T), R =.. [_,F])}, !.
#(#(P),L,L) :- length(L,P).
