% This code is automatically generated on the basis of a file in Codeco notation.
%
% For more information, see the package ch.uzh.ifi.attempto.codeco of the Attempto Java Packages
% (http://attempto.ifi.uzh.ch/site/downloads/) and the thesis "Controlled English for Knowledge
% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).


/* === ACE Editor Grammar === */
/* - Tobias Kuhn, 17 November 2009 - */
/* Below, the grammar rules of the ACE Editor grammar are shown: */

/* --- Texts and Sentences --- */
/* 'text' stands for a complete text consisting of an arbitrary number of complete
		sentences (including zero): */
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
text([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->complete_sentence([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), text([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A1).
/* A complete sentence is represented by the category 'complete_sentence' and is either
		a declarative sentence that ends with a full stop or a question ending with a question mark: */
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1), ['.'].
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_2([minus, plus, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/A1), [?].
/* General sentences are represented by 'sentence': */
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence_coord_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), ['for every'], nc([B2, C2, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], A2/Z2), sentence_coord_1([A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3], Z2/Z3), ~(Z/Z3/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), [if], sentence_coord_1([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A3), [then], sentence_coord_1([B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3], A3/A4), ~(Z/A4/A1).
/* Sentences can be coordinated using "or" ('sentence_coord_1') and "and"
		('sentence_coord_2'): */
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence_coord_2([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), sentence_coord_2([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A3), [or], sentence_coord_1([B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3], A3/A4), ~(Z/A4/A1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), [and], sentence_coord_2([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A1).
/* Uncoordinated sentences are represented in two levels by 'simple_sentence_1' and
		'simple_sentence_2': */
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), ['it is false that'], simple_sentence_2([minus, minus, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], A2/Y2), ~(Z/Y2/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/T1), ['such that'], simple_sentence_1([U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], T1/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there are'], np([minus, minus, minus, plus, minus, plus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_2([minus, minus, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/A1).
simple_sentence_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B1, minus, C1, D1, E1, nom, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Z/X1), vp_coord_1([B1, B, F1, Y1, Z1, E1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], X1/T2), ~(Z/T2/A1).

/* --- Verb Phrases --- */
/* Like sentences, verb phrases can be coordinated using "or" ('vp_coord_1') and "and"
		('vp_coord_2'): */
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp_coord_2([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/A1).
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), vp_coord_2([A, B2, C, C2, D2, F, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], A2/X2), [or], vp_coord_1([B2, B, C, Y2, Z2, F, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3], X2/T3), ~(Z/T3/A1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/A1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B1, C, C1, D1, F, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Z/X1), [and], vp_coord_2([B1, B, C, Y1, Z1, F, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], X1/A1).
/* Uncoordinated verb phrases represented by 'vp' can use an auxiliary verb and can have
		verb phrase modifiers: */
vp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->aux([B1, C1, D1, D, E1, F, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/Y1), v([A, Z1, C, D, A2, F, B2, C2, I, I1, inf, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], Y1/R2), vmod([Z1, B, C, S2, T2, U2, V2, W2, X2, Y2, Z2, D2, E2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], R2/M3), ~(Z/M3/A1).
vp([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->v([A, A1, C, plus, B1, E, C1, D1, H, minus, fin, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], Y/S1), vmod([A1, B, C, T1, U1, V1, W1, X1, Y1, Z1, A2, E1, F1, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2], S1/N2), ~(Y/N2/Z).
/* The category 'v' represents the main verb or - if "be" is used as a copula verb - the
		complementing noun phrase or adjective complement: */
v([A, A, B, C, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->verb([Y, Z, A1, C, B1, E, C1, D1, E1, minus, I, F1, G1, itr, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], W/X).
v([A, B, C, D, E, F, G, H, I, minus, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->verb([Z, A1, B1, D, C1, F, D1, E1, F1, minus, J, G1, H1, tr, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], X/T1), np([A, B, C, U1, V1, W1, acc, X1, I, Y1, Z1, K, A2, tr, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2], T1/Y).
v([A, B, C, D, E, F, G, H, I, plus, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->verb([Z, A1, B1, C1, D1, E1, F1, G1, H1, plus, I1, J1, K1, tr, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], X/W1), [by], np([A, B, C, X1, Y1, Z1, acc, A2, I, B2, C2, K, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], W1/Y).
v([A, B, C, D, E, F, G, H, I, plus, J, K, plus, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->np([A, B, C, Z, A1, minus, acc, B1, I, C1, D1, K, plus, E1, plus, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1], X/Y).
v([A, B, C, D, E, minus, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->np([A, B, C, Y, Z, minus, acc, A1, H, B1, C1, J, plus, D1, minus, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1], W/X).
v([A, A, B, C, D, E, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->adj_coord([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/X).
v([A, B, C, D, E, F, G, H, I, plus, J, K, plus, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->adjc([A, B, C, Z, A1, B1, C1, D1, I, E1, F1, K, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], X/Y).

/* --- Noun Phrases --- */
/* Noun phrases are represented by 'np' and can consist of proper names, variables,
		pronouns, and different noun constructs: */
np([A, B, C, plus, plus, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)-->prop([X, Y, Z, A1, B1, C1, D1, E, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V1), [W1, X1, Y1, Z1, A2, B2, C2, E, D2, E2, F2, G2, H2, I2, J2, L1, M1, prop, minus, K2, L2, M2, N2, O2, P2]>> (V1/Q2), relcl([A, B, E, R2, S2, T2, U2, V2, F, W2, X2, I, Y2, Z2, A3, L1, B3, C3, D3, E3, F3, G3, H3, I3, J3], Q2/W).
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> #(D), newvar([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/V1), [W1, X1, Y1, Z1, A2, B2, C2, D, D2, E2, F2, G2, H2, I2, J2, K2, L2, var, plus, P1, M2, N2, O2, P2, Q2]>V1/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $def_noun_sg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), $ref([V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], U/U), [+[U2, V2, W2, X2, Y2, Z2, A3, D, B3, C3, D3, E3, F3, G3, H3, I3, J3, noun, plus, Q2, Q1, K3, L3, M3, N3]]<U/O3, [P3, Q3, R3, S3, T3, U3, V3, D, W3, X3, Y3, Z3, A4, B4, C4, I3, J3, ref, minus, D4, E4, F4, G4, H4, I4]>O3/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $def_noun_sg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), [+[V1, W1, X1, Y1, Z1, A2, B2, D, C2, D2, E2, F2, G2, H2, I2, J2, K2, noun, L2, M2, Q1, N2, O2, P2, Q2]]<U/R2, [S2, T2, U2, V2, W2, X2, Y2, D, Z2, A3, B3, C3, D3, E3, F3, J2, K2, ref, minus, G3, H3, I3, J3, K3, L3]>R2/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $ref([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), [+[V1, W1, X1, Y1, Z1, A2, B2, D, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, plus, R1, M2, N2, O2, P2, Q2]]<U/R2, [S2, T2, U2, V2, W2, X2, Y2, D, Z2, A3, B3, C3, D3, E3, F3, J2, K2, ref, minus, G3, H3, I3, J3, K3, L3]>R2/V.
np([A, A, B, plus, plus, minus, C, B, D, E, F, G, H, I, minus, J, K, L, M, N, O, P, plus, Q, R], S/T)--> $pron([U, V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, plus, Q1, R1], S/S), [+[S1, T1, U1, V1, W1, X1, Y1, B, Z1, A2, B2, C2, D2, E2, F2, J1, K1, G2, H2, I2, J2, K2, L2, M2, N2]]<S/T.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, minus, R, S], T/U)--> $pron([V, W, X, Y, Z, A1, C, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, minus, Q1, R1], T/T), [+[S1, T1, U1, V1, W1, X1, Y1, D, Z1, A2, B2, C2, D2, E2, F2, J1, K1, G2, H2, I2, J2, K2, L2, M2, N2], -[O2, P2, Q2, R2, S2, T2, U2, B, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3]]<T/M3, [N3, O3, P3, Q3, R3, S3, T3, D, U3, V3, W3, X3, Y3, Z3, A4, J1, K1, pron, minus, B4, C4, D4, E4, F4, G4]>M3/U.
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->quant([A1, B1, C1, D, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), nc([A, B, C, Z1, A2, B2, C2, G, H, D2, E2, K, F2, G2, N, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], Y1/Z).
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, minus, N, O, P, Q, R, S, T, U, V, W], X/Y)--> #(G), ipron([Z, A1, B1, D, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X1), opt_newvar([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], X1/X2), [Y2, Z2, A3, B3, C3, D3, E3, G, F3, G3, H3, I3, J3, K3, L3, N1, M3, ipron, Q2, R2, N3, O3, P3, Q3, R3]>X2/S3, relcl([A, B, G, T3, U3, V3, W3, X3, H, Y3, Z3, K, A4, B4, C4, N1, D4, E4, F4, G4, H4, I4, J4, K4, L4], S3/Y).
np([A, A, B, plus, C, plus, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)-->num_quant([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/V1), $num([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], V1/V1), opt_adj_coord([V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3], V1/V), #(E), $noun_pl([U3, V3, W3, X3, Y3, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4, K4, L4, M4, N4, O4, P4, Q4, R4, S4], V/V).
np([A, A, B, plus, C, minus, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)-->num_quant([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/V1), ['1'], #(E), opt_adj_coord([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], V1/V2), $noun_sg([W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3], V2/V2), [V3, W3, X3, Y3, Z3, A4, B4, E, C4, D4, E4, F4, G4, H4, I4, L3, M3, noun, minus, J4, R3, K4, L4, M4, N4]>V2/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)--> #(E), [what], [X, Y, Z, A1, B1, C1, D1, E, E1, F1, G1, H1, I1, J1, K1, minus, L1, wh, minus, M1, N1, O1, P1, Q1, R1]>V/W.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)--> #(E), [who], [X, Y, Z, A1, B1, C1, D1, E, E1, F1, G1, H1, I1, J1, K1, plus, L1, wh, minus, M1, N1, O1, P1, Q1, R1]>V/W.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[which], nc([plus, plus, B, Y, Z, A1, B1, E, F, C1, D1, I, E1, F1, L, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1], W/X).
np([A, plus, B, plus, C, plus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)-->[which], opt_adj_coord([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], V/W), #(E), $noun_pl([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], W/W).
/* The category 'nc' represents nouns optionally followed by variables, relative clauses,
		and prepositional phrases using "of": */
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->n([A1, B1, C1, D1, E1, F1, G1, H, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), opt_newvar([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], Y1/Y2), [Z2, A3, B3, C3, D3, E3, F3, H, G3, H3, I3, J3, K3, L3, M3, O1, P1, noun, R2, S2, U1, N3, O3, P3, Q3]>Y2/R3, relcl([A, B, H, S3, T3, U3, V3, W3, I, X3, Y3, L, Z3, A4, B4, O1, C4, D4, E4, F4, G4, H4, I4, J4, K4], R3/Z).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->n([A1, B1, C1, D1, E1, F1, G1, H, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), [Z1, A2, B2, C2, D2, E2, F2, H, G2, H2, I2, J2, K2, L2, M2, O1, P1, noun, minus, N2, U1, O2, P2, Q2, R2]>Y1/S2, [of], np([A, B, C, T2, U2, V2, acc, W2, I, X2, Y2, L, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], S2/M3), ~(Y/M3/Z).
/* The category 'n' stands for nouns that are preceded by an optional adjective
		coordination: */
n([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->opt_adj_coord([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1), #(H), $noun_sg([A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P, Q, P2, Q2, R2, S2, V, T2, U2, V2], A1/A1).
/* New variables, optional and mandatory, are represented by 'opt_newvar' and 'newvar',
		respectively: */
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, minus, S, T, U, V, W, X], Y/Y)-->[].
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, plus, S, T, U, V, W, X], Y/Z)-->newvar([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, S, T1, U1, V1, W1, X1], Y/Z).
newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $var([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, T, W1, X1, Y1], Z/Z), /<([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, plus, T, R2, S2, T2, U2, V2], Z/A1).
/* Proper names can either require the definite article "the" or not, and are represented
		by the category 'prop': */
prop([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $prop_sg([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, Q, P1, Q1, R1, S1, H, T1, U1, V1], Z/Z).
prop([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $propdef_sg([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, Q, P1, Q1, R1, S1, H, T1, U1, V1], Z/Z).

/* --- Adjectives --- */
/* Adjectives can be only coordinated by "and", and are represented by 'opt_adj_coord'
		for the optional case and by 'adj_coord' if mandatory: */
opt_adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
opt_adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adj_coord([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adj([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adj([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), [and], adj_coord([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A1).
/* Uncoordinated adjectives are represented by 'adj' and can be used in positive,
		comparative and superlative forms: */
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adj_itr([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[more], $adj_itr([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adj_itr_comp([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[most], $adj_itr([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adj_itr_sup([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
/* The category 'adjc' stands for more complicated adjective constructions including
		nested noun phrases that represent a comparison object: */
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[as], $adj_itr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), [as], np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $adj_itr_comp([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), [than], np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[more], $adj_itr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), [than], np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[more], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[most], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, minus, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Z/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[as], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [as], np([A2, B, C, U2, V2, W2, acc, X2, I, Y2, Z2, L, minus, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], T2/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[as], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [as], $adj_prep([U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, Y1, R3], T2/T2), np([A2, B, C, S3, T3, U3, acc, V3, I, W3, X3, L, minus, Y3, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4], T2/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[more], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [than], np([A2, B, C, U2, V2, W2, acc, X2, I, Y2, Z2, L, minus, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], T2/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $adj_tr_comp([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [than], np([A2, B, C, U2, V2, W2, acc, X2, I, Y2, Z2, L, minus, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], T2/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[more], $adj_tr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [than], $adj_prep([U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, Y1, R3], T2/T2), np([A2, B, C, S3, T3, U3, acc, V3, I, W3, X3, L, minus, Y3, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4], T2/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $adj_tr_comp([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, A2, C, B2, C2, D2, acc, E2, minus, F2, G2, L, minus, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/T2), [than], $adj_prep([U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, Y1, R3], T2/T2), np([A2, B, C, S3, T3, U3, acc, V3, I, W3, X3, L, minus, Y3, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4], T2/A1).

/* --- Relative Clauses --- */
/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
relcl([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[].
relcl([A, B, C, D, E, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->relpron([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, N, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X1), relcl1([A, B, C, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, N, K2, L2, M2, N2, O2, P2, Q2, R2, W1], X1/Y).
/* Like sentences and verb phrases, relative clauses can be coordinated by "or"
		('relcl1') and "and" ('relcl2'): */
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), relcl2([A, B2, C, C2, D2, E2, F2, G2, minus, H2, I2, J2, K2, L2, M2, P, N2, O2, P2, Q2, R2, S2, T2, U2, Y], A2/V2), or_relpron([W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, P, L3, M3, N3, O3, P3, Q3, R3, S3, Y], V2/T3), relcl1([B2, B, C, U3, V3, W3, X3, Y3, Z3, A4, B4, C4, D4, E4, F4, P, G4, H4, I4, J4, K4, L4, M4, N4, Y], T3/O4), ~(Z/O4/A1).
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->relcl2([A, B, C, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, P, N1, O1, P1, Q1, R1, S1, T1, U1, Y], Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B1, C, C1, D1, minus, E1, F1, minus, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/W1), and_relpron([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, P, M2, N2, O2, P2, Q2, R2, S2, T2, Y], W1/U2), relcl2([B1, B, C, V2, W2, X2, Y2, Z2, I, A3, B3, C3, D3, E3, F3, P, G3, H3, I3, J3, K3, L3, M3, N3, Y], U2/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B, C, B1, C1, minus, D1, E1, I, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B1, C, C1, D1, E1, nom, F1, minus, G1, H1, I1, minus, J1, K1, L1, M1, N1, O1, P1, Q1, R1, minus, S1, T1], Z/U1), aux([V1, W1, X1, Y1, Z1, E1, A2, B2, C2, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], U1/S2), verb([T2, U2, V2, Y1, W2, E1, X2, Y2, Z2, minus, inf, A3, B3, tr, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3], S2/N3), vmod([B1, B, F1, O3, P3, Q3, R3, S3, I, T3, U3, I1, minus, V3, W3, X3, Y3, Z3, A4, B4, C4, D4, E4, F4, G4], N3/H4), ~(Z/H4/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B1, C, C1, D1, E1, nom, F1, minus, G1, H1, I1, minus, J1, K1, L1, M1, N1, O1, P1, Q1, R1, minus, S1, T1], Z/U1), verb([V1, W1, X1, plus, Y1, E1, Z1, A2, B2, minus, fin, C2, D2, tr, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], U1/P2), vmod([B1, B, F1, Q2, R2, S2, T2, U2, I, V2, W2, I1, minus, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3], P2/J3), ~(Z/J3/A1).
/* Relative pronouns are represented by 'relpron' and can be either "that", "who" or
		"which": */
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, that], Y/Y)-->[that].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, V, W, who], X/X)-->[who].
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, V, W, which], X/X)-->[which].
/* The categories 'or_relpron' and 'and_relpron' define shortcuts - like "or that" as
		one token - for better usability inside of the predictive editor: */
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[or], relpron([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q1, R1, S1, T1, U1, V1, W1, X1, Y], Z/A1).
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, that], Y/Y)-->['or that'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, V, W, who], X/X)-->['or who'].
or_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, V, W, which], X/X)-->['or which'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[and], relpron([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q1, R1, S1, T1, U1, V1, W1, X1, Y], Z/A1).
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, that], Y/Y)-->['and that'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, V, W, who], X/X)-->['and who'].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, V, W, which], X/X)-->['and which'].

/* --- Verb Phrase Modifiers --- */
/* Verb phrase modifiers are represented by 'vmod' and the auxiliary category 'vmod_x',
		and are always optional: */
vmod([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[].
vmod([A, B, C, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->adv_coord([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, L, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), vmod_x([A, B, C, Z1, A2, B2, C2, D2, I, E2, F2, G2, L, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Y1/Z).
vmod([A, B, C, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->pp([A, A1, C, B1, C1, D1, E1, F1, I, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Y/W1), vmod([A1, B, C, X1, Y1, Z1, A2, B2, I, C2, D2, I1, L, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], W1/Z).
vmod_x([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[].
vmod_x([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->pp([A, B1, C, C1, D1, E1, F1, G1, I, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Z/X1), vmod([B1, B, C, Y1, Z1, A2, B2, C2, I, D2, E2, J1, M, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], X1/A1).
/* The category 'pp' represents prepositional phrases: */
pp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $prep([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/Z), np([A, B, C, A2, B2, C2, acc, D2, I, E2, F2, L, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Z/A1).
/* Adverbs can be coordinated by "and", which is represented by 'adv_coord': */
adv_coord([A, B, C, D, E, F, G, H, I, J, K, L, minus, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->adv_phrase([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Z).
adv_coord([A, B, C, D, E, F, G, H, I, J, K, L, minus, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->adv_phrase([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Z1), [and], adv_coord([A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], Z1/Z).
/* Adverbial phrases are represented by 'adv_phrase', and can be in positive, comparative
		or superlative form: */
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adv([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[more], $adv([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adv_comp([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[most], $adv([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> $adv_sup([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Z/Z).

/* --- Verbs --- */
/* The category 'verb' represents main verbs that can be intransitive or transitive: */
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, J, itr, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $iv_finsg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, J, itr, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $iv_infpl([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, K, itr, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $iv_infpl([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W/W).
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, J, tr, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $tv_finsg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, J, tr, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $tv_infpl([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, K, tr, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $tv_infpl([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W/W).
verb([A, B, C, D, E, F, G, H, I, plus, J, K, L, tr, M, N, O, P, Q, R, S, T, U, V, W], X/X)--> $tv_pp([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X).
/* Auxiliary verbs are represented by 'aux', which includes negation markers: */
aux([A, B, C, plus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)-->[is].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['is not'].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [is, not].
aux([A, B, C, plus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)-->[are].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['are not'].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [are, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['does not'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['do not'].
aux([A, B, C, minus, D, E, F, G, H, minus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [can].
aux([A, B, C, minus, D, E, F, G, H, minus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [should].
aux([A, B, C, minus, D, E, F, G, H, minus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [must].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['has to'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['have to'].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [can, be].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [should, be].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [must, be].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['has to', be].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['have to', be].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [cannot, be].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [can, not, be].
aux([A, B, C, minus, D, E, F, G, H, plus, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [should, not, be].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['does not', 'have to', be].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['do not', 'have to', be].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [cannot].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [can, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [should, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['does not', 'have to'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['do not', 'have to'].

/* --- Quantifiers --- */
/* Existential and universal quantifiers are represented by 'quant': */
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[a].
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[an].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (Y/Z), [every].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (Y/Z), [no].
/* The category 'num_quant' stands for numerical quantifiers: */
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['at least'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['at most'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['less than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['more than'].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[exactly].

/* --- Indefinite Pronouns --- */
/* Indefinite pronouns are represented by 'ipron': */
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W], X/X)-->[something].
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/X)-->[somebody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [everything].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [everybody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [nothing].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [nobody].

/* --- Anaphoric Pronouns --- */
/* The category 'pron' represents reflexive and irreflexive anaphoric pronouns: */
$pron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, plus, V, W], X/X)-->[itself].
$pron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, masc, P, Q, R, S, T, plus, U, V], W/W)-->[himself].
$pron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, fem, P, Q, R, S, T, plus, U, V], W/W)-->[herself].
$pron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, minus, P, Q, R, S, T, U, minus, V, W], X/X)-->[it].
$pron([A, B, C, D, E, F, nom, G, H, I, J, K, L, M, N, plus, masc, O, P, Q, R, S, minus, T, U], V/V)-->[he].
$pron([A, B, C, D, E, F, acc, G, H, I, J, K, L, M, N, plus, masc, O, P, Q, R, S, minus, T, U], V/V)-->[him].
$pron([A, B, C, D, E, F, nom, G, H, I, J, K, L, M, N, plus, fem, O, P, Q, R, S, minus, T, U], V/V)-->[she].
$pron([A, B, C, D, E, F, acc, G, H, I, J, K, L, M, N, plus, fem, O, P, Q, R, S, minus, T, U], V/V)-->[her].


~(I/T/O) --> {append([X,[//|N],I],T), \+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.
~(_/O/O) --> [].
//(_, T/[//|T]) --> [].
>(F, T/[>(F)|T]) --> [].
>>(F, T/[>>(F)|T]) --> [].
<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \+ member(-Q, L), \+ \+ member(+Q, L), !, member(+Q, L)}.
<(L, [R|T]/[R|T]) --> <(L,T/T).
/<(F, T/T) --> {\+ (member(R,T), R =.. [_,F])}, !.
#(#(P),L,L) :- length(L,P).
