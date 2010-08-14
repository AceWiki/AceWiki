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


:- module(generate_latex, [
		generate_latex/2  % +InputFile, +OutputFile
	]).

/** <module> Generate Latex Representation

This module transfroms from the Codeco format into a Latex representation. Usage:

==
swipl -s generate_latex.pl -g "generate_latex('my_codeco_grammar.pl', 'my_latex_file.tex')" -t halt
==

You may have to replace "swipl" by the name of your SWI Prolog executable.

Before transforming a file, you should make sure that it is a well-formed Codeco file:

==
swipl -s validate_codeco.pl -g "validate_codeco('my_codeco_grammar.pl')" -t halt
==

For more information about Codeco, see the following thesis:
http://attempto.ifi.uzh.ch/site/pubs/papers/doctoral_thesis_kuhn.pdf 

@author Tobias Kuhn
@version 2009-11-19
*/


% Codeco operator definitions:

:- op(1200, xfx, '=>').
:- op(1200, xfx, '~>').
:- op(600, xfx, ':').
:- op(500, fx, '$').
:- op(500, fx, '#').
:- op(0, xfx, '>').  % Remove declaration
:- op(0, xfx, '<').  % Remove declaration


%% sort_features(?OnOff)
%
% This fact defines whether the features should be shown in alphabetical order ('on') or in the
% same order as they appear in the input file ('off').

%sort_features(off).
sort_features(on).


%% generate_latex(+InputFile, +OutputFile)
%
% This predicate reads the Codeco grammar from the input file and transforms it into a Latex file.

generate_latex(InputFile, OutputFile) :-
    open(InputFile, read, In),
    open(OutputFile, write, Out),
    header(Header),
    write(Out, Header),
    (
    	process(In, Out)
    ;
    	write(user_error, 'Error during parsing.\n')
    ),
    close(In),
    footer(Footer),
    write(Out, Footer),
    close(Out).


process(In, Out) :-
	read_term(In, Term, [module(generate_latex)]),
	( Term == end_of_file ->
		true
	;
		numbervars(Term, 1, _),
		process_term(Out, Term),
		process(In, Out)
	).


process_term(Out, title:T) :-
	!,
    format(Out, '\\section*{~l}\n\n', [T]).

process_term(Out, section:S) :-
	!,
    format(Out, '\\subsection*{~l}\n\n', [S]).

process_term(Out, paragraph:P) :-
	!,
    format(Out, '\\noindent ~l \\vspace{2mm}\n\n', [P]).

process_term(Out, Head => Body) :-
    !,
    write(Out, '{\\scriptsize\n'),
    write(Out, '\\noindent$\n'),
    write(Out, '\\ruleid\n'),
    write(Out, '\\nrule{\n'),
    process_cat(Out, Head),
    write(Out, '}{\n'),
    process_cats(Out, Body),
    write(Out, '}$\n\\vspace{2mm}\n\n}\n').

process_term(Out, Head ~> Body) :-
    !,
    write(Out, '{\\scriptsize\n'),
    write(Out, '\\noindent$\n'),
    write(Out, '\\ruleid\n'),
    write(Out, '\\scrule{\n'),
    process_cat(Out, Head),
    write(Out, '}{\n'),
    process_cats(Out, Body),
    write(Out, '}$\n\\vspace{2mm}\n\n}\n').

process_term(_, Term) :-
    format(user_error, 'WARNING. Cannot process term: ~q\n', Term).


process_cats(Out, Body) :-
    Body = ','(Cat, BodyRest),
    !,
    process_cat(Out, Cat),
    process_cats(Out, BodyRest).

process_cats(_, []) :-
    !.

process_cats(Out, BodyLast) :-
    process_cat(Out, BodyLast).


process_cat(_, []) :-
    !.

process_cat(Out, [T|Rest]) :-
    T =.. [Name],
    !,
    format(Out, '  \\term{~l}\n', Name),
    process_cat(Out, Rest).

process_cat(Out, $ Cat) :-
    Cat =.. [Name],
    !,
    format(Out, '  \\spreterm{~l}\n', Name).

process_cat(Out, $ Cat) :-
    Cat =.. [Name|Features],
    !,
    format(Out, '  \\preterm{~l}{', Name),
    process_features(Out, Features),
    write(Out, '}\n').

process_cat(Out, # '$VAR'(N)) :-
    !,
    format(Out, '  \\pos{~l}\n', N).

process_cat(Out, '//') :-
    !,
    write(Out, '  \\scopeopensymb{}\n').

process_cat(Out, Cat) :-
    Cat =.. [Name],
    !,
    format(Out, '  \\scat{~l}\n', Name).

process_cat(Out, Cat) :-
    Cat =.. ['>'|Features],
    !,
    write(Out, '  \\fwref{'),
    process_features(Out, Features),
    write(Out, '}\n').

process_cat(Out, Cat) :-
    Cat =.. ['>>'|Features],
    !,
    write(Out, '  \\sfwref{'),
    process_features(Out, Features),
    write(Out, '}\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|BwrefTerms],
	\+ BwrefTerms = [],
	\+ member(_:_, BwrefTerms),
	!,
    write(Out, '  \\cbwref{'),
    process_bwrefterms(Out, BwrefTerms),
    write(Out, ' }\n').

process_cat(Out, Cat) :-
    Cat =.. ['<'|Features],
    !,
    write(Out, '  \\bwref{'),
    process_features(Out, Features),
    write(Out, '}\n').

process_cat(Out, Cat) :-
    Cat =.. ['/<'|Features],
    !,
    write(Out, '  \\nbwref{'),
    process_features(Out, Features),
    write(Out, '}\n').

process_cat(Out, Cat) :-
    Cat =.. [Name|Features],
    format(Out, '  \\cat{~l}{', Name),
    process_features(Out, Features),
    write(Out, '}\n').


process_bwrefterms(Out, [Term|Rest]) :-
	Term =.. ['+'|Features],
	!,
    write(Out, ' \\fs{'),
    process_features(Out, Features),
    write(Out, '}'),
	process_bwrefterms(Out, Rest).

process_bwrefterms(Out, Terms) :-
    write(Out, ' }{'),
	process_bwrefterms_x(Out, Terms).

process_bwrefterms_x(Out, [Term|Rest]) :-
	Term =.. ['-'|Features],
	!,
    write(Out, ' \\fs{'),
    process_features(Out, Features),
    write(Out, '}'),
	process_bwrefterms_x(Out, Rest).

process_bwrefterms_x(_, []).


process_features(Out, Features) :-
	sort_features(on),
	!,
	sort(Features, FeaturesS),
	process_features_x(Out, FeaturesS).

process_features(Out, Features) :-
	process_features_x(Out, Features).


process_features_x(_, []).

process_features_x(Out, [Name:'$VAR'(N)|Rest]) :-
    !,
    format(Out, '\\featv{~l}{~l}', [Name, N]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:plus|Rest]) :-
    !,
    format(Out, '\\featc{~l}{+}', [Name]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:minus|Rest]) :-
    !,
    format(Out, '\\featc{~l}{--}', [Name]),
    process_features_x(Out, Rest).

process_features_x(Out, [Name:Value|Rest]) :-
    !,
    format(Out, '\\featc{~l}{~l}', [Name, Value]),
    process_features_x(Out, Rest).


replace(Input, Search, Replace, Output) :-
	concat_atom(Split, Search, Input),
	concat_atom(Split, Replace, Output).


:- format_predicate(l, write_latex(_Arg, _Term)).


write_latex(_, Atom) :-
	replace(Atom, '\\', '\\\\', Atom1),
	replace(Atom1, '_', '\\_', Atom2),
	replace(Atom2, '$', '\\$', Atom3),
	replace(Atom3, '{', '\\{', Atom4),
	replace(Atom4, '}', '\\}', Atom5),
    write(Atom5).


:- style_check(-atom).

header('\\documentclass[a4paper]{article}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{stmaryrd}
\\usepackage{graphicx}
\\usepackage{color}
\\usepackage[left=1cm,top=1cm,right=1cm,nohead,nofoot]{geometry}
\\textwidth 19cm
\\pagestyle{empty}

\\newcounter{ruleid}
\\newcommand{\\ruleid}{{\\addtocounter{ruleid}{1} \\:\\:\\: (\\arabic{ruleid})} \\:\\:\\: }
\\newcommand{\\ruleideval}{{\\addtocounter{ruleid}{1} \\:\\:\\: (\\underline{\\arabic{ruleid}})} \\:\\:\\: }

\\newcommand{\\possymb}{\\ensuremath{\\mathord{\\#}}}
\\newcommand{\\scopeopensymb}{\\ensuremath{\\mathord{\\sslash}}}
\\newcommand{\\fwrefsymb}{\\ensuremath{\\mathord{>}}}
\\newcommand{\\sfwrefsymb}{\\ensuremath{\\mathord{\\gg}}}
\\newcommand{\\bwrefsymb}{\\ensuremath{\\mathord{<}}}
\\newcommand{\\nbwrefsymb}{\\ensuremath{\\mathord{\\nless}}}
\\newcommand{\\cbwrefplus}{\\ensuremath{\\mathord{^+}}}
\\newcommand{\\cbwrefminus}{\\ensuremath{\\mathord{^-}}}
\\newcommand{\\cbwrefplain}[2]{\\ensuremath{\\bwrefsymb\\cbwrefplus #1 \\cbwrefminus #2 }}
\\newcommand{\\nrulesymb}[0]{\\mathrel{:}}
\\newcommand{\\scrulesymb}[0]{\\mathrel{\\sim}}
\\newcommand{\\genrulesymb}[1]{\\mathrel{#1}}

\\newcommand{\\fs}[1]{\\!\\! \\left( \\!\\!\\! \\scalebox{0.75}{$\\begin{array}{l} \\\\[-2ex] #1 \\\\[-2ex] \\end{array}$} \\!\\!\\! \\right)}
\\newcommand{\\nrule}[2]{#1 \\: \\xrightarrow{\\displaystyle \\: \\nrulesymb \\:} \\: #2}
\\newcommand{\\scrule}[2]{#1 \\: \\xrightarrow{\\displaystyle \\: \\scrulesymb \\:} \\: #2}
\\newcommand{\\lrule}[2]{#1 \\: \\rightarrow \\: #2}
\\newcommand{\\scat}[1]{\\:\\: \\mbox{\\itshape #1} \\:\\:}
\\newcommand{\\cat}[2]{\\:\\: \\mbox{\\itshape #1} \\, \\fs{#2} }
\\newcommand{\\fwref}[1]{\\:\\: \\fwrefsymb \\fs{#1} }
\\newcommand{\\sfwref}[1]{\\:\\: \\sfwrefsymb \\fs{#1} }
\\newcommand{\\bwref}[1]{\\:\\: \\bwrefsymb \\fs{#1} }
\\newcommand{\\nbwref}[1]{\\:\\: \\nbwrefsymb \\fs{#1} }
\\newcommand{\\cbwref}[2]{\\:\\: \\bwrefsymb\\cbwrefplus #1 \\!\\! \\cbwrefminus #2 }
\\newcommand{\\cbwrefp}[1]{\\:\\: \\bwrefsymb\\cbwrefplus #1 }
\\newcommand{\\term}[1]{\\:\\: [\\,\\mbox{#1}\\,] \\:\\:}
\\newcommand{\\spreterm}[1]{\\:\\: \\mbox{\\underline{\\itshape #1}} \\:\\:}
\\newcommand{\\preterm}[2]{\\:\\: \\mbox{\\underline{\\itshape #1}} \\, \\fs{#2} }
\\newcommand{\\pos}[1]{\\:\\: \\possymb {\\fboxsep 0.5mm \\framebox{\\scalebox{0.6}{#1}}} \\:\\:}
\\newcommand{\\scopeopener}[0]{\\:\\: \\scopeopensymb \\:\\:}

\\newcommand{\\featv}[2]{\\mbox{#1:}\\:\\fboxsep 0.5mm \\framebox{\\scalebox{0.8}{#2}}\\:\\\\}
\\newcommand{\\featc}[2]{\\mbox{#1:}\\:\\mbox{#2}\\\\}

\\newcommand{\\edge}[3]{#1 \\: \\xrightarrow{\\displaystyle #2} \\: #3}
\\newcommand{\\edot}[0]{\\:\\mathord{\\bullet}\\:}
\\newcommand{\\epos}[2]{\\langle#1,#2\\rangle\\:\\:\\:\\:}

\\begin{document}

').

footer('\\end{document}').
