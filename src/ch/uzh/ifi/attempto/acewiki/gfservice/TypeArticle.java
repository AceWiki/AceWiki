// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.aceowl.ACEOWLOntoElement;
import ch.uzh.ifi.attempto.acewiki.owl.OWLRelation;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;

// TODO: this is a temporary hack,
// extend some general ontology element instead
public class TypeArticle extends OWLRelation implements ACEOWLOntoElement {

	private String word;

	/**
	 * Creates a new relation that is represented by a transitive adjective.
	 */
	public TypeArticle() {
		word = "";
	}

	public String[] getWords() {
		return new String[] {word};
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		word = words[0];
	}

	public String serializeWords() {
		return word + ";";
	}

	public String getIRISuffix() {
		return getWord(0);
	}

	public List<LexiconEntry> getLexiconEntries() {
		List<LexiconEntry> entries = new ArrayList<LexiconEntry>();
		entries.add(LexiconEntry.createTrAdjEntry(word, word, ""));
		return entries;
	}

	public String getType() {
		return "Article";
	}

	public String getInternalType() {
		return "article";
	}

	public void collectLexicalRules(String catName, Collection<LexicalRule> lexRules) {
		if (catName == null || catName.equals("tradj")) {
			lexRules.add(new LexicalRule("tradj", getWord(0)));
		}
	}

}
