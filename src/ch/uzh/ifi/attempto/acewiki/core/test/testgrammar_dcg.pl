% This code is automatically generated on the basis of a file in Codeco notation.
%
% For more information, see the package ch.uzh.ifi.attempto.codeco of the Attempto Java Packages
% (http://attempto.ifi.uzh.ch/site/downloads/) and the thesis "Controlled English for Knowledge
% Representation" (http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf).


/* === AceWiki Grammar === */
/* - Tobias Kuhn, 2 August 2010 - */

/* --- Texts and Sentences --- */
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), ['.'], ~(Z/A2/A1).
complete_sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), simple_sentence_2([minus, plus, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], A2/Y2), [?], ~(Z/Y2/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence_coord_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), ['for every'], nc([B2, C2, minus, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2], A2/Z2), sentence_coord_1([A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3], Z2/Z3), ~(Z/Z3/A1).
sentence([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), [if], sentence_coord_1([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A3), [then], sentence_coord_1([B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3, T3, U3, V3, W3, X3, Y3, Z3], A3/A4), ~(Z/A4/A1).
sentence_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->sentence_coord_2([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).
sentence_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_1([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), [and], sentence_coord_2([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1]// (Z/A2), ['it is false that'], simple_sentence_2([minus, minus, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], A2/Y2), ~(Z/Y2/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there is'], np([minus, minus, minus, plus, minus, minus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/T1), ['such that'], simple_sentence_1([U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], T1/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->['there are'], np([minus, minus, minus, plus, minus, plus, nom, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], Z/A1).
simple_sentence_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->simple_sentence_2([minus, minus, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/A1).
simple_sentence_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B1, minus, C1, D1, E1, nom, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Z/X1), vp_coord_1([B1, B, F1, Y1, Z1, E1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], X1/T2), ~(Z/T2/A1).

/* --- Verb Phrases --- */
vp_coord_1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp_coord_2([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/A1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B, C, B1, C1, F, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/A1).
vp_coord_2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B1, C, C1, D1, F, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], Z/X1), [and], vp_coord_2([B1, B, C, Y1, Z1, F, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2], X1/A1).
vp([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->aux([B1, C1, D1, D, E1, F, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Z/Y1), v([A, B, C, D, Z1, F, A2, B2, I, I1, inf, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], Y1/Q2), ~(Z/Q2/A1).
vp([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->v([A, B, C, plus, A1, E, B1, C1, H, minus, fin, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1], Y/R1), ~(Y/R1/Z).
v([A, A, B, C, D, E, F, G, H, minus, I, minus, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->verb([Y, Z, A1, C, B1, E, C1, D1, E1, minus, I, F1, itr, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1], W/X).
v([A, B, C, D, E, F, G, H, I, minus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->verb([Z, A1, B1, D, C1, F, D1, E1, F1, minus, J, G1, tr, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1], X/T1), np([A, B, C, U1, V1, W1, acc, X1, I, Y1, Z1, A2, tr, L, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2], T1/Y).
v([A, B, C, D, E, F, G, H, I, plus, J, minus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->verb([Z, A1, B1, C1, D1, E1, F1, G1, H1, plus, I1, J1, tr, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], X/W1), np([A, B, C, X1, Y1, Z1, acc, A2, I, B2, C2, minus, D2, L, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2], W1/Y).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->np([A, B, C, Z, A1, minus, acc, B1, I, C1, D1, plus, E1, L, plus, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1], X/Y).
v([A, B, C, D, E, minus, F, G, H, plus, I, plus, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->np([A, B, C, Y, Z, minus, acc, A1, H, B1, C1, plus, D1, K, minus, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1], W/X).
v([A, B, C, D, E, F, G, H, I, plus, J, plus, K, L, M, N, O, P, Q, R, S, T, U, V, W], X/Y)--> $tradj([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], X/X), np([A, B, C, Y1, Z1, A2, acc, B2, I, C2, D2, minus, E2, L, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2], X/Y).

/* --- Noun Phrases --- */
np([A, B, C, plus, plus, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)--> $propername([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, E, O1, P1, Q1, R1, S1, T1, U1], V/V), [V1, W1, X1, Y1, Z1, A2, B2, E, C2, D2, E2, F2, G2, H2, I2, M1, N1, J2, prop, minus, K2, L2, M2, N2, O2]>> (V/P2), relcl([A, B, E, Q2, R2, S2, T2, U2, F, V2, W2, X2, Y2, K, Z2, M1, A3, B3, C3, D3, E3, F3, G3, H3, I3], P2/W).
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> #(D), newvar([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/V1), [W1, X1, Y1, Z1, A2, B2, C2, D, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, var, plus, Q1, N2, O2, P2, Q2]>V1/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $defnoun([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), $reference([V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], U/U), [+[U2, V2, W2, X2, Y2, Z2, A3, D, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, noun, plus, M2, R1, L3, M3, N3]]<U/O3, [P3, Q3, R3, S3, T3, U3, V3, D, W3, X3, Y3, Z3, A4, B4, C4, I3, J3, D4, ref, minus, E4, F4, G4, H4, I4]>O3/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $defnoun([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), [+[V1, W1, X1, Y1, Z1, A2, B2, D, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, noun, M2, N2, R1, O2, P2, Q2]]<U/R2, [S2, T2, U2, V2, W2, X2, Y2, D, Z2, A3, B3, C3, D3, E3, F3, J2, K2, G3, ref, minus, H3, I3, J3, K3, L3]>R2/V.
np([A, A, B, plus, plus, minus, C, D, E, F, G, H, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)--> $reference([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/U), [+[V1, W1, X1, Y1, Z1, A2, B2, D, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, plus, N1, N2, O2, P2, Q2]]<U/R2, [S2, T2, U2, V2, W2, X2, Y2, D, Z2, A3, B3, C3, D3, E3, F3, J2, K2, G3, ref, minus, H3, I3, J3, K3, L3]>R2/V.
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->quant([A1, B1, C1, D, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), nc([A, B, C, Z1, A2, B2, C2, G, H, D2, E2, F2, G2, M, N, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], Y1/Z).
np([A, B, C, D, E, minus, F, G, H, I, J, K, L, M, minus, N, O, P, Q, R, S, T, U, V, W], X/Y)--> #(G), ipron([Z, A1, B1, D, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X1), opt_newvar([Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2], X1/X2), [Y2, Z2, A3, B3, C3, D3, E3, G, F3, G3, H3, I3, J3, K3, L3, N1, M3, N3, ipron, R2, S2, O3, P3, Q3, R3]>X2/S3, relcl([A, B, G, T3, U3, V3, W3, X3, H, Y3, Z3, A4, B4, M, C4, N1, D4, E4, F4, G4, H4, I4, J4, K4, L4], S3/Y).
np([A, A, B, plus, C, plus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)-->num_quant([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], U/V), $number([V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], V/V), #(E), $nounpl([U2, V2, W2, X2, Y2, Z2, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3, M3, N3, O3, P3, Q3, R3, S3], V/V).
np([A, A, B, plus, C, minus, D, E, F, G, H, minus, I, J, minus, K, L, M, N, O, P, Q, R, S, T], U/V)-->num_quant([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, plus, S1, T1], U/U1), ['1'], #(E), $noun([V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2], U1/U1), [U2, V2, W2, X2, Y2, Z2, A3, E, B3, C3, D3, E3, F3, G3, H3, K2, L2, I3, noun, minus, J3, M2, K3, L3, M3]>U1/V.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/W)--> #(E), [who], [X, Y, Z, A1, B1, C1, D1, E, E1, F1, G1, H1, I1, J1, K1, plus, L1, M1, wh, minus, N1, O1, P1, Q1, R1]>V/W.
np([A, plus, B, plus, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[which], nc([plus, plus, B, Y, Z, A1, B1, E, F, C1, D1, E1, F1, K, L, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1], W/X).
np([A, plus, B, plus, C, plus, D, E, F, G, H, I, J, K, minus, L, M, N, O, P, Q, R, S, T, U], V/V)-->[which], #(E), $nounpl([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, minus, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->n([A1, B1, C1, D1, E1, F1, G1, H, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1], Y/Y1), opt_newvar([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2], Y1/Y2), [Z2, A3, B3, C3, D3, E3, F3, H, G3, H3, I3, J3, K3, L3, M3, O1, P1, N3, noun, S2, T2, Q1, O3, P3, Q3]>Y2/R3, relcl([A, B, H, S3, T3, U3, V3, W3, I, X3, Y3, Z3, A4, N, B4, O1, C4, D4, E4, F4, G4, H4, I4, J4, K4], R3/Z).
nc([A, B, C, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W, X], Y/Z)--> $nounof([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1], Y/Y), np([A, B, C, Z1, A2, B2, acc, C2, I, D2, E2, F2, G2, N, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2], Y/S2), ~(Y/S2/Z).
n([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)--> #(H), $noun([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P, Q, R, P1, Q1, R1, S1, T1, U1, V1], Z/Z).
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, minus, T, U, V, W, X], Y/Y)-->[].
opt_newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, plus, T, U, V, W, X], Y/Z)-->newvar([A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, T, U1, V1, W1, X1], Y/Z).
newvar([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)--> $variable([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, U, S1, T1, U1, V1, W1, X1, Y1], Z/Z), /<([Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, plus, U, S2, T2, U2, V2], Z/A1).

/* --- Relative Clauses --- */
relcl([A, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[].
relcl([A, B, C, D, E, F, G, H, plus, I, J, K, L, plus, M, N, O, P, Q, R, S, T, U, V, W], X/Y)-->relpron([Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, N, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X1), relcl1([A, B, C, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, N, K2, L2, M2, N2, O2, P2, Q2, V1, R2], X1/Y).
relcl1([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->relcl2([A, B, C, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, P, N1, O1, P1, Q1, R1, S1, T1, X, U1], Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B1, C, C1, D1, minus, E1, F1, minus, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], Z/W1), and_relpron([X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, P, M2, N2, O2, P2, Q2, R2, S2, X, T2], W1/U2), relcl2([B1, B, C, V2, W2, X2, Y2, Z2, I, A3, B3, C3, D3, E3, F3, P, G3, H3, I3, J3, K3, L3, M3, X, N3], U2/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->vp([A, B, C, B1, C1, minus, D1, E1, I, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], Z/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B, C, B1, C1, D1, nom, E1, minus, F1, G1, minus, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, minus], Z/T1), aux([U1, V1, W1, X1, Y1, D1, Z1, A2, B2, minus, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2], T1/R2), verb([S2, T2, U2, X1, V2, D1, W2, X2, Y2, minus, inf, Z2, tr, A3, B3, C3, D3, E3, F3, G3, H3, I3, J3, K3, L3], R2/M3), ~(Z/M3/A1).
relcl2([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->np([A, B, C, B1, C1, D1, nom, E1, minus, F1, G1, minus, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, minus], Z/T1), verb([U1, V1, W1, plus, X1, D1, Y1, Z1, A2, minus, fin, B2, tr, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2], T1/O2), ~(Z/O2/A1).
relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, plus, P, Q, R, S, T, U, V, who, W], X/X)-->[who].
and_relpron([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[and], relpron([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, P, Q1, R1, S1, T1, U1, V1, W1, X, X1], Z/A1).

/* --- Verbs --- */
verb([A, B, C, D, E, minus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $verbsg([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, plus, F, G, H, minus, fin, I, tr, J, K, L, M, N, O, P, Q, R, S, T, U], V/V)--> $verbinf([W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1], V/V).
verb([A, B, C, D, E, F, G, H, I, minus, inf, J, tr, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)--> $verbinf([X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1], W/W).
verb([A, B, C, D, E, F, G, H, I, plus, J, K, tr, L, M, N, O, P, Q, R, S, T, U, V, W], X/X)--> $pverb([Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1], X/X).
aux([A, B, C, plus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)-->[is].
aux([A, B, C, minus, D, minus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [is, not].
aux([A, B, C, plus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/W)-->[are].
aux([A, B, C, minus, D, plus, E, F, G, plus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), [are, not].
aux([A, B, C, minus, D, minus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['does not'].
aux([A, B, C, minus, D, plus, E, F, G, minus, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V], W/X)-->[Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1]// (W/X), ['do not'].

/* --- Quantifiers --- */
quant([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Y)-->[a].
quant([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X], Y/Z)-->[A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1]// (Y/Z), [every].
num_quant([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, plus, W, X], Y/Y)-->[exactly].

/* --- Indefinite Pronouns --- */
ipron([A, B, C, plus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/X)-->[somebody].
ipron([A, B, C, minus, D, E, F, G, H, I, J, K, L, M, N, plus, O, P, Q, R, S, T, U, V, W], X/Y)-->[Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1]// (X/Y), [everybody].

/* --- Lexicon --- */
$propername([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, 'Mary', R, S, T, U, V, W, X], Y/Y)-->['Mary'].
$noun([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, woman, R, S, T, U, V, W, X], Y/Y)-->[woman].
$defnoun([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, woman, V, W, X], Y/Y)-->['the woman'].
$nounpl([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[women].
$nounof([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['friend of'].
$verbsg([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[asks].
$verbinf([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[ask].
$pverb([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['asked by'].
$tradj([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['mad-about'].
$variable([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, 'X', R, S, T, U, V, W, X], Y/Y)-->['X'].
$reference([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, 'X', R, S, T, U, V, W, X], Y/Y)-->['X'].
$number([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->['2'].

/* --- Auxiliary Rules for Testing --- */
test([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->complete_sentence([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A2), fill([B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2, W2, X2, Y2, Z2], A2/A1).
fill([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/Z)-->[].
fill([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y], Z/A1)-->[''], fill([B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1], Z/A1).


~(I/T/O) --> {append([X,[//|N],I],T), \+ member(//,N), findall(>>(R),member(>>(R),X),Y), append([Y,N,I],O)}, !.
~(_/O/O) --> [].
//(_, T/[//|T]) --> [].
>(F, T/[>(F)|T]) --> [].
>>(F, T/[>>(F)|T]) --> [].
<(L, [R|T]/[R|T]) --> {R =.. [_,Q], \+ member(-Q, L), \+ \+ member(+Q, L), !, member(+Q, L)}.
<(L, [R|T]/[R|T]) --> <(L,T/T).
/<(F, T/T) --> {\+ (member(R,T), R =.. [_,F])}, !.
#(#(P),L,L) :- length(L,P).
