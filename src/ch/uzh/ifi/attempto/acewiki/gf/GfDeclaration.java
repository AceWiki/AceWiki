// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.owl.OWLSentence;

public class GfDeclaration extends GfSentence implements Declaration, OWLSentence {

	public GfDeclaration(GfGrammar gfGrammar, GfWikiEntry gfWikiEntry) {
		super(gfGrammar, gfWikiEntry);
	}

	public GfDeclaration(GfGrammar grammar, String language, String tokenText) {
		super(grammar, language, tokenText);
	}

	public GfSentence copyFor(Article article) {
		GfSentence c = new GfDeclaration(getGfGrammar(), mGfWikiEntry);
		c.init(getOntology(), article);
		c.setIntegrated(isIntegrated());
		return c;
	}

	public Sentence unambiguousCopyFor(Article article, int index) {
		List<String> trees = new ArrayList<>();
		trees.add(mGfWikiEntry.getTrees().getTrees().get(0));
		GfWikiEntry wikiEntry = new GfWikiEntry(mGfWikiEntry.getLanguage(), mGfWikiEntry.getText(), new TreeList(trees));
		GfSentence d = new GfDeclaration(mGfGrammar, wikiEntry);
		d.init(getOntology(), article);
		return d;
	}

}