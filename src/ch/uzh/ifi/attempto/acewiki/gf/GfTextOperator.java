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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.PrettyTextElement;
import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This is the text operator used to handle the GF service input/output.
 *
 * TODO: can this class be used to present the GF binding character (&+) on the
 * surface as an almost invisible dot that is glued to its neighbors?
 *
 * @author Kaarel Kaljurand
 */
// Some quick and dirty hack to show sentences in a nice way. (Tobias)
// TODO Do this properly (using GF features)
public class GfTextOperator extends DefaultTextOperator {

	//private static final String SPACE = " ";
	private static final String EMPTY = "";
	private static final String GF_BIND = "&+";
	//private static final String GF_BIND_PRETTY = "·";

	private Ontology mOntology;


	public GfTextOperator(Ontology ontology) {
		mOntology = ontology;
	}


	public TextElement createTextElement(String text) {
		OntologyTextElement ote = createOntologyTextElement(text);
		if (ote == null) {
			return new PrettyTextElement(text);
		}
		return ote;
	}


	public String getTextInContext(TextElement textElement, String preceding, String following) {
		String text = textElement.getOriginalText();
		if (GF_BIND.equals(text)) {
			return EMPTY;
			//return GF_BIND_PRETTY;
		}
		if (preceding == null || preceding.matches("[.?!]")) {
			return firstCharToUpperCase(text);
		}
		return text;
	}


	public List<String> splitIntoTokens(String text) {
		/*
		List<String> tokens = new ArrayList<String>();
		Iterables.addAll(tokens, Splitter.on(SPACE).omitEmptyStrings().split(text));
		return tokens;
		 */
		List<String> preTokens = super.splitIntoTokens(text);
		List<String> tokens = new ArrayList<>();
		for (String t : preTokens) {
			if (t.matches("[0-9]+")) {
				for (int i = 0 ; i < t.length() ; i++) {
					tokens.add(t.substring(i, i+1));
				}
			} else {
				tokens.add(t);
			}
		}
		return tokens;
	}


	public String getGlue(TextElement left, TextElement right) {
		if (GF_BIND.equals(right.getOriginalText()) || GF_BIND.equals(left.getOriginalText())) {
			return EMPTY;
		}
		if (right.getText().matches("[0-9]") && left.getText().matches("[0-9]")) {
			return EMPTY;
		}
		return super.getGlue(left, right);
	}


	/**
	 * OntologyTextElement requires the index of the wordform, so we scan
	 * all the words and return the index of the first form that matches.
	 *
	 * TODO: use this also for the ACETextOperator
	 */
	private OntologyTextElement createOntologyTextElement(String text) {
		OntologyElement oe = mOntology.getElement(text);
		if (oe != null) {
			int index = 0;
			for (String word : oe.getWords()) {
				if (text.equals(word))
					return new OntologyTextElement(oe, index);
				index++;
			}
		}
		return null;
	}

}
