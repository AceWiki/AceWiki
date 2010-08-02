% This code is automatically generated on the basis of a file in Codeco notation.
%
% For more information, see the package ch.uzh.ifi.attempto.codeco of the Attempto Java Packages
% (http://attempto.ifi.uzh.ch/site/downloads/) and the thesis "Controlled English for Knowledge
% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).


/* === ACE Editor Grammar === */
/* - Tobias Kuhn, 2 August 2010 - */

/* --- Texts and Sentences --- */
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->sentence([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1), ['.'].
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2]// (A1/C2), simple_sentence_2([minus, plus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3], C2/B3), [?], ~(A1/B3/B1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->sentence_coord_1([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2]// (A1/C2), ['for every'], nc([D2, E2, minus, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3], C2/C3), sentence_coord_1([D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3, A4, B4, C4], C3/D4), ~(A1/D4/B1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2]// (A1/C2), [if], sentence_coord_1([D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3], C2/D3), [then], sentence_coord_1([E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3, A4, B4, C4, D4], D3/E4), ~(A1/E4/B1).
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->sentence_coord_2([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->simple_sentence_1([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->simple_sentence_1([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/C2), [and], sentence_coord_2([D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3], C2/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2]// (A1/C2), ['it is false that'], simple_sentence_2([minus, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3], C2/B3), ~(A1/B3/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], A1/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], A1/V1), ['such that'], simple_sentence_1([W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], V1/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->['there are'], np([minus, minus, minus, plus, minus, plus, nom, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], A1/B1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->simple_sentence_2([minus, minus, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], A1/B1).
simple_sentence_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->np([A, C1, minus, D1, E1, F1, nom, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], A1/Z1), vp_coord_1([C1, B, G1, A2, B2, F1, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Z1/W2), ~(A1/W2/B1).

/* --- Verb Phrases --- */
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->vp_coord_2([A, B, C, C1, D1, F, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], A1/B1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->vp([A, B, C, C1, D1, F, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], A1/B1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->vp([A, C1, C, D1, E1, F, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], A1/Z1), [and], vp_coord_2([C1, B, C, A2, B2, F, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], Z1/B1).
vp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->aux([C1, D1, E1, D, F1, F, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], A1/A2), v([A, B2, C, D, C2, F, D2, E2, I, J1, inf, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], A2/U2), vmod([B2, B, C, V2, W2, X2, Y2, Z2, A3, B3, C3, F2, G2, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3], U2/Q3), ~(A1/Q3/B1).
vp([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->v([A, B1, C, plus, C1, E, D1, E1, H, minus, fin, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], Z/U1), vmod([B1, B, C, V1, W1, X1, Y1, Z1, A2, B2, C2, F1, G1, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], U1/Q2), ~(Z/Q2/A1).
v([A, A, B, C, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->verb([Z, A1, B1, C, C1, E, D1, E1, F1, minus, I, G1, H1, itr, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1], X/Y).
v([A, B, C, D, E, F, G, H, I, minus, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->verb([A1, B1, C1, D, D1, F, E1, F1, G1, minus, J, H1, I1, tr, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y/V1), np([A, B, C, W1, X1, Y1, acc, Z1, I, A2, B2, K, C2, tr, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], V1/Z).
v([A, B, C, D, E, F, G, H, I, plus, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->verb([A1, B1, C1, D1, E1, F1, G1, H1, I1, plus, J1, K1, L1, tr, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), [by], np([A, B, C, Z1, A2, B2, acc, C2, I, D2, E2, K, minus, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Y1/Z).
v([A, B, C, D, E, F, G, H, I, plus, J, K, plus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->np([A, B, C, A1, B1, minus, acc, C1, I, D1, E1, K, plus, F1, plus, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], Y/Z).
v([A, B, C, D, E, minus, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->np([A, B, C, Z, A1, minus, acc, B1, H, C1, D1, J, plus, E1, minus, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1], X/Y).
v([A, A, B, C, D, E, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->adj_coord([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], X/Y).
v([A, B, C, D, E, F, G, H, I, plus, J, K, plus, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->adjc([A, B, C, A1, B1, C1, D1, E1, I, F1, G1, K, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Y/Z).

/* --- Noun Phrases --- */
np([A, B, C, plus, plus, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->prop([Y, Z, A1, B1, C1, D1, E1, E, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/X1), [Y1, Z1, A2, B2, C2, D2, E2, E, F2, G2, H2, I2, J2, K2, L2, M1, N1, prop, minus, M2, N2, O2, P2, Q2, R2, S2]>> (X1/T2), relcl([A, B, E, U2, V2, W2, X2, Y2, F, Z2, A3, I, B3, C3, D3, M1, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3], T2/X).
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)--> #(D), newvar([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], V/X1), [Y1, Z1, A2, B2, C2, D2, E2, D, F2, G2, H2, I2, J2, K2, L2, M2, N2, var, plus, Q1, O2, P2, Q2, R2, S2, T2]>X1/W.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)--> $def_noun_sg([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], V/V), $ref([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], V/V), [+[X2, Y2, Z2, A3, B3, C3, D3, D, E3, F3, G3, H3, I3, J3, K3, L3, M3, noun, plus, S2, R1, N3, O3, P3, Q3, R3]]<V/S3, [T3, U3, V3, W3, X3, Y3, Z3, D, A4, B4, C4, D4, E4, F4, G4, L3, M3, ref, minus, H4, I4, J4, K4, L4, M4, N4]>S3/W.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)--> $def_noun_sg([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], V/V), [+[X1, Y1, Z1, A2, B2, C2, D2, D, E2, F2, G2, H2, I2, J2, K2, L2, M2, noun, N2, O2, R1, P2, Q2, R2, S2, T2]]<V/U2, [V2, W2, X2, Y2, Z2, A3, B3, D, C3, D3, E3, F3, G3, H3, I3, L2, M2, ref, minus, J3, K3, L3, M3, N3, O3, P3]>U2/W.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)--> $ref([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], V/V), [+[X1, Y1, Z1, A2, B2, C2, D2, D, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, plus, S1, O2, P2, Q2, R2, S2, T2]]<V/U2, [V2, W2, X2, Y2, Z2, A3, B3, D, C3, D3, E3, F3, G3, H3, I3, L2, M2, ref, minus, J3, K3, L3, M3, N3, O3, P3]>U2/W.
np([A, A, B, plus, plus, minus, C, B, D, E, F, G, H, I, minus, J, K, L, M, N, O, P, plus, Q, R, S], T/U)--> $pron([V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, plus, R1, S1, T1], T/T), [+[U1, V1, W1, X1, Y1, Z1, A2, B, B2, C2, D2, E2, F2, G2, H2, K1, L1, I2, J2, K2, L2, M2, N2, O2, P2, Q2]]<T/U.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, minus, R, S, T], U/V)--> $pron([W, X, Y, Z, A1, B1, C, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, minus, R1, S1, T1], U/U), [+[U1, V1, W1, X1, Y1, Z1, A2, D, B2, C2, D2, E2, F2, G2, H2, K1, L1, I2, J2, K2, L2, M2, N2, O2, P2, Q2], -[R2, S2, T2, U2, V2, W2, X2, B, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3]]<U/Q3, [R3, S3, T3, U3, V3, W3, X3, D, Y3, Z3, A4, B4, C4, D4, E4, K1, L1, pron, minus, F4, G4, H4, I4, J4, K4, L4]>Q3/V.
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->quant([B1, C1, D1, D, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), nc([A, B, C, B2, C2, D2, E2, G, H, F2, G2, K, H2, I2, N, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], A2/A1).
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, minus, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)--> #(G), ipron([A1, B1, C1, D, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Z1), opt_newvar([A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], Z1/A3), [B3, C3, D3, E3, F3, G3, H3, G, I3, J3, K3, L3, M3, N3, O3, O1, P3, ipron, S2, T2, Q3, R3, S3, T3, U3, V3]>A3/W3, relcl([A, B, G, X3, Y3, Z3, A4, B4, H, C4, D4, K, E4, F4, G4, O1, H4, I4, J4, K4, L4, M4, N4, O4, P4, Q4], W3/Z).
np([A, A, B, plus, C, plus, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->num_quant([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], V/X1), $num([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], X1/X1), opt_adj_coord([Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3], X1/W), #(E), $noun_pl([Y3, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4, K4, L4, M4, N4, O4, P4, Q4, R4, S4, T4, U4, V4, W4, X4], W/W).
np([A, A, B, plus, C, minus, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T, U], V/W)-->num_quant([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, plus, U1, V1], V/W1), ['1'], #(E), opt_adj_coord([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], W1/X2), $noun_sg([Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3], X2/X2), [Y3, Z3, A4, B4, C4, D4, E4, E, F4, G4, H4, I4, J4, K4, L4, N3, O3, noun, minus, M4, T3, N4, O4, P4, Q4, R4]>X2/W.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V], W/X)--> #(E), [who], [Y, Z, A1, B1, C1, D1, E1, E, F1, G1, H1, I1, J1, K1, L1, plus, M1, wh, minus, N1, O1, P1, Q1, R1, S1, T1]>W/X.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[which], nc([plus, plus, B, Z, A1, B1, C1, E, F, D1, E1, I, F1, G1, L, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], X/Y).
np([A, plus, B, plus, C, plus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[which], opt_adj_coord([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], W/X), #(E), $noun_pl([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], X/X).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->n([B1, C1, D1, E1, F1, G1, H1, H, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), opt_newvar([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3], A2/B3), [C3, D3, E3, F3, G3, H3, I3, H, J3, K3, L3, M3, N3, O3, P3, P1, Q1, noun, T2, U2, V1, Q3, R3, S3, T3, U3]>B3/V3, relcl([A, B, H, W3, X3, Y3, Z3, A4, I, B4, C4, L, D4, E4, F4, P1, G4, H4, I4, J4, K4, L4, M4, N4, O4, P4], V3/A1).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->n([B1, C1, D1, E1, F1, G1, H1, H, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), [B2, C2, D2, E2, F2, G2, H2, H, I2, J2, K2, L2, M2, N2, O2, P1, Q1, noun, minus, P2, V1, Q2, R2, S2, T2, U2]>A2/V2, [of], np([A, B, C, W2, X2, Y2, acc, Z2, I, A3, B3, L, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3], V2/Q3), ~(Z/Q3/A1).
n([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->opt_adj_coord([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1), #(H), $noun_sg([C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, P, Q, R2, S2, T2, U2, V, V2, W2, X2, Y2], B1/B1).
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, minus, S, T, U, V, W, X, Y], Z/Z)-->[].
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, plus, S, T, U, V, W, X, Y], Z/A1)-->newvar([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, S, U1, V1, W1, X1, Y1, Z1], Z/A1).
newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $var([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, T, X1, Y1, Z1, A2], A1/A1), /<([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, plus, T, T2, U2, V2, W2, X2, Y2], A1/B1).
prop([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)--> $prop_sg([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q, Q1, R1, S1, T1, H, U1, V1, W1, X1], A1/A1).

/* --- Adjectives --- */
opt_adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[].
opt_adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->adj_coord([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).
adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->adj([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).
adj_coord([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->adj([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/C2), [and], adj_coord([D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3], C2/B1).
adj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)--> $adj_itr([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2], A1/A1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[as], $adj_itr([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), [as], np([A, B, C, C2, D2, E2, acc, F2, I, G2, H2, L, minus, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], A1/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $adj_itr_comp([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), [than], np([A, B, C, C2, D2, E2, acc, F2, I, G2, H2, L, minus, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], A1/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $adj_tr([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, B, C, C2, D2, E2, acc, F2, I, G2, H2, L, minus, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], A1/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[as], $adj_tr([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, C2, C, D2, E2, F2, acc, G2, minus, H2, I2, L, minus, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A1/W2), [as], np([C2, B, C, X2, Y2, Z2, acc, A3, I, B3, C3, L, minus, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3], W2/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[as], $adj_tr([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, C2, C, D2, E2, F2, acc, G2, minus, H2, I2, L, minus, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A1/W2), [as], $adj_prep([X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, A2, V3], W2/W2), np([C2, B, C, W3, X3, Y3, acc, Z3, I, A4, B4, L, minus, C4, D4, E4, F4, G4, H4, I4, J4, K4, L4, M4, N4, O4], W2/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $adj_tr_comp([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, C2, C, D2, E2, F2, acc, G2, minus, H2, I2, L, minus, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A1/W2), [than], np([C2, B, C, X2, Y2, Z2, acc, A3, I, B3, C3, L, minus, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3], W2/B1).
adjc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $adj_tr_comp([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, C2, C, D2, E2, F2, acc, G2, minus, H2, I2, L, minus, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A1/W2), [than], $adj_prep([X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, A2, V3], W2/W2), np([C2, B, C, W3, X3, Y3, acc, Z3, I, A4, B4, L, minus, C4, D4, E4, F4, G4, H4, I4, J4, K4, L4, M4, N4, O4], W2/B1).

/* --- Relative Clauses --- */
relcl([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
relcl([A, B, C, D, E, F, G, H, plus, I, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->relpron([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, N, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Z1), relcl1([A, B, C, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, N, M2, N2, O2, P2, Q2, R2, S2, T2, U2, Y1], Z1/Z).
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->relcl2([A, B, C, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, P, O1, P1, Q1, R1, S1, T1, U1, V1, W1, Z], A1/B1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->vp([A, C1, C, D1, E1, minus, F1, G1, minus, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], A1/Y1), and_relpron([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, P, O2, P2, Q2, R2, S2, T2, U2, V2, W2, Z], Y1/X2), relcl2([C1, B, C, Y2, Z2, A3, B3, C3, I, D3, E3, F3, G3, H3, I3, P, J3, K3, L3, M3, N3, O3, P3, Q3, R3, Z], X2/B1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->vp([A, B, C, C1, D1, minus, E1, F1, I, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], A1/B1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->np([A, C1, C, D1, E1, F1, nom, G1, minus, H1, I1, J1, minus, K1, L1, M1, N1, O1, P1, Q1, R1, S1, minus, T1, U1, V1], A1/W1), aux([X1, Y1, Z1, A2, B2, F1, C2, D2, E2, minus, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2], W1/V2), verb([W2, X2, Y2, A2, Z2, F1, A3, B3, C3, minus, inf, D3, E3, tr, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3], V2/R3), vmod([C1, B, G1, S3, T3, U3, V3, W3, I, X3, Y3, J1, minus, Z3, A4, B4, C4, D4, E4, F4, G4, H4, I4, J4, K4, L4], R3/M4), ~(A1/M4/B1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->np([A, C1, C, D1, E1, F1, nom, G1, minus, H1, I1, J1, minus, K1, L1, M1, N1, O1, P1, Q1, R1, S1, minus, T1, U1, V1], A1/W1), verb([X1, Y1, Z1, plus, A2, F1, B2, C2, D2, minus, fin, E2, F2, tr, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], W1/S2), vmod([C1, B, G1, T2, U2, V2, W2, X2, I, Y2, Z2, J1, minus, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3], S2/N3), ~(A1/N3/B1).
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, V, W, X, who], Y/Y)-->[who].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[and], relpron([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, P, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, Z], A1/B1).

/* --- Verb Phrase Modifiers --- */
/* Verb phrase modifiers are represented by 'vmod' and the auxiliary category 'vmod_x',
		and are always optional: */
vmod([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
vmod([A, B, C, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adv_coord([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, L, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), vmod_x([A, B, C, B2, C2, D2, E2, F2, I, G2, H2, I2, L, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A2/A1).
vmod([A, B, C, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->pp([A, B1, C, C1, D1, E1, F1, G1, I, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/Y1), vmod([B1, B, C, Z1, A2, B2, C2, D2, I, E2, F2, J1, L, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], Y1/A1).
vmod_x([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
vmod_x([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->pp([A, C1, C, D1, E1, F1, G1, H1, I, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], A1/Z1), vmod([C1, B, C, A2, B2, C2, D2, E2, I, F2, G2, K1, M, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], Z1/B1).
pp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)--> $prep([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/A1), np([A, B, C, C2, D2, E2, acc, F2, I, G2, H2, L, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2], A1/B1).
adv_coord([A, B, C, D, E, F, G, H, I, J, K, L, minus, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adv_phrase([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2], Z/A1).
adv_coord([A, B, C, D, E, F, G, H, I, J, K, L, minus, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->adv_phrase([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2], Z/B2), [and], adv_coord([C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3], B2/A1).
adv_phrase([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)--> $adv([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2], A1/A1).

/* --- Verbs --- */
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, J, itr, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $iv_finsg([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/W).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, J, itr, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $iv_infpl([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/W).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, K, itr, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)--> $iv_infpl([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], X/X).
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, J, tr, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $tv_finsg([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/W).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, J, tr, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $tv_infpl([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], W/W).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, K, tr, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)--> $tv_infpl([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], X/X).
verb([A, B, C, D, E, F, G, H, I, plus, J, K, L, tr, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)--> $tv_pp([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Y).
aux([A, B, C, plus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)-->[is].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (X/Y), [is, not].
aux([A, B, C, plus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)-->[are].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (X/Y), [are, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (X/Y), ['does not'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (X/Y), ['do not'].

/* --- Quantifiers --- */
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[a].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2]// (Z/A1), [every].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, plus, X, Y], Z/Z)-->[exactly].

/* --- Indefinite Pronouns --- */
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[somebody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Y/Z), [everybody].

/* --- Anaphoric Pronouns --- */
$pron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, fem, P, Q, R, S, T, plus, U, V, W], X/X)-->[herself].
$pron([A, B, C, D, E, F, nom, G, H, I, J, K, L, M, N, plus, fem, O, P, Q, R, S, minus, T, U, V], W/W)-->[she].
$pron([A, B, C, D, E, F, acc, G, H, I, J, K, L, M, N, plus, fem, O, P, Q, R, S, minus, T, U, V], W/W)-->[her].

/* --- Lexicon --- */
$prop_sg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, fem, P, Q, R, S, 'Mary', T, U, V, W], X/X)-->['Mary'].
$def_noun_sg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, woman, U, V, W, X, Y], Z/Z)-->['the woman'].
$ref([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, 'X', V, W, X, Y], Z/Z)-->['X'].
$num([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->['2'].
$noun_pl([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[women].
$noun_sg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, fem, P, Q, R, S, woman, T, U, V, W], X/X)-->[woman].
$var([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, 'X', V, W, X, Y], Z/Z)-->['X'].
$iv_finsg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[waits].
$iv_infpl([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[wait].
$tv_finsg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[asks].
$tv_infpl([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[ask].
$tv_pp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[asked].
$adj_itr([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[young].
$adj_itr_comp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[younger].
$adj_tr([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, about, Y], Z/Z)-->['mad-about'].
$adj_tr_comp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, about, Y], Z/Z)-->['madder-about'].
$adj_prep([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, about, Y], Z/Z)-->[about].
$prep([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[for].
$adv([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[early].

/* --- Auxiliary Rules for Testing --- */
test([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->complete_sentence([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/C2), fill([D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2, A3, B3, C3], C2/B1).
fill([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/A1)-->[].
fill([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z], A1/B1)-->[''], fill([C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2], A1/B1).


~(I/T/O) --> {append([X,[//|N],I],T), \+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.
~(_/O/O) --> [].
//(_, T/[//|T]) --> [].
>(F, T/[>(F)|T]) --> [].
>>(F, T/[>>(F)|T]) --> [].
<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \+ member(-Q, L), \+ \+ member(+Q, L), !, member(+Q, L)}.
<(L, [R|T]/[R|T]) --> <(L,T/T).
/<(F, T/T) --> {\+ (member(R,T), R =.. [_,F])}, !.
#(#(P),L,L) :- length(L,P).
