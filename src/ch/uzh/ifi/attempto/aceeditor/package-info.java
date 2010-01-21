// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

/**
 * This package contains the ACE Editor which is a web-based editor for ACE texts. The ACE Editor
 * makes use of the Echo Web Framework. {@link ch.uzh.ifi.attempto.aceeditor.ACEEditorServlet} is
 * the Java Servlet class to be launched by the web server.
 * <p>
 * The grammar used by the ACE Editor describes a subset of ACE and is defined in the Codeco
 * notation. This grammar is available in different formats:
 * <ul>
 * <li>Codeco: <a href="aceeditor_grammar.pl" target="_top">aceeditor_grammar.pl</a></li>
 * <li>Prolog DCG: <a href="aceeditor_grammar_dcg.pl" target="_top">aceeditor_grammar_dcg.pl</a></li>
 * <li>HTML: <a href="aceeditor_grammar.html" target="_top">aceeditor_grammar.html</a></li>
 * <li>PDF: <a href="aceeditor_grammar.pdf" target="_top">aceeditor_grammar.pdf</a></li>
 * <li>LaTeX: <a href="aceeditor_grammar.tex" target="_top">aceeditor_grammar.tex</a></li>
 * </ul>
 * Furthermore, the {@link ch.uzh.ifi.attempto.chartparser.Grammar}-class
 * {@link ch.uzh.ifi.attempto.aceeditor.ACEEditorGrammar} is automatically generated from the
 * Codeco grammar.
 * 
 * @author Tobias Kuhn
 */
package ch.uzh.ifi.attempto.aceeditor;
