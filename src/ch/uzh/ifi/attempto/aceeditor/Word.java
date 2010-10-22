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

package ch.uzh.ifi.attempto.aceeditor;

import ch.uzh.ifi.attempto.chartparser.LexicalRule;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.preditor.MenuEntry;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class manages words for the lexicon of the ACE Editor.
 * 
 * @author Tobias Kuhn
 */
class Word {
	
	private String wordForm, symbol, entry;
	private Preterminal category;

	/**
	 * Generates a new word on the basis of a lexicon entry according to the ACE lexicon
	 * specification.
	 * 
	 * @param lexiconEntry A lexicon entry in the ACE lexicon format.
	 */
	public Word(String lexiconEntry) {
		if (lexiconEntry.matches("[a-z_]+\\('?[A-Za-z0-9-_]+'?,\\s*'?[A-Za-z0-9-_]+'?.*\\)\\.\\s*")) {
			entry = lexiconEntry.replaceFirst("^(.*)\\.\\s*$", "$1");
			category = new Preterminal(lexiconEntry.replaceFirst("^([a-z_]+)\\(.*$", "$1"));
			if (category.getName().equals("pn_sg")) category = new Preterminal("prop_sg");
			if (category.getName().equals("pndef_sg")) category = new Preterminal("propdef_sg");
			wordForm = lexiconEntry.replaceFirst("^[a-z_]+\\('?([A-Za-z0-9-_]+)'?,.*$", "$1");
			symbol = lexiconEntry.replaceFirst("^[a-z_]+\\('?[A-Za-z0-9-_]+'?,\\s*'?([A-Za-z0-9-_]+)'?.*$", "$1");
			readFeatures(lexiconEntry.replaceFirst("^[a-z_]+\\('?[A-Za-z0-9-_]+'?,\\s*'?[A-Za-z0-9-_]+'?(.*)\\)\\.\\s*", "$1"));
			category.setFeature("text", wordForm);
		} else if (!lexiconEntry.equals("") && !lexiconEntry.matches("\\s*%.*")) {
			System.err.println("WARNING: Invalid lexicon entry: " + lexiconEntry);
		}
	}

	/**
	 * Returns the word form how it appears in ACE texts.
	 * 
	 * @return The word form.
	 */
	public String getWordForm() {
		return wordForm;
	}
	
	/**
	 * Returns the text of a token representing this word. For proper names with definite articles,
	 * the article "the" is part of the token but not part of the word form. Otherwise, the token
	 * text is the same as the word form.
	 * 
	 * @return The token text.
	 */
	public String getTokenText() {
		if (category.getName().equals("propdef_sg")) {
			return "the " + wordForm;
		} else {
			return wordForm;
		}
	}

	/**
	 * Returns the symbol of this word how it appears in the logical representation.
	 * 
	 * @return The symbol of this word.
	 */
	public String getSymbol() {
		return symbol;
	}

	/**
	 * Returns this word as a lexicon entry accoring to the ACE lexicon format.
	 * 
	 * @return The lexicon entry in the ACE lexicon format.
	 */
	public String getEntry() {
		return entry;
	}

	/**
	 * Returns the pre-terminal category for this word form.
	 * 
	 * @return The pre-terminal category.
	 */
	public Preterminal getCategory() {
		return category;
	}
	
	/**
	 * Returns the lexical rule for this word form.
	 * 
	 * @return The lexical rule.
	 */
	public LexicalRule getLexicalRule() {
		return new LexicalRule(category, wordForm);
	}
	
	/**
	 * Creates a text element containing this word.
	 * 
	 * @return A new text element.
	 */
	public TextElement getTextElement() {
		return new TextElement(getTokenText());
	}
	
	/**
	 * Creates a menu entry containing this word.
	 * 
	 * @param menuGroup The menu group of the menu entry to be created.
	 * @return A new menu entry.
	 */
	public MenuEntry getMenuEntry(String menuGroup) {
		return new MenuEntry(getTextElement(), menuGroup);
	}

	private void readFeatures(String featureString) {
		String[] s = featureString
				.replaceAll("\\s+", "")
				.replaceFirst("^,", "")
				.replaceAll("^'", "")
				.replaceAll("'$", "")
				.replaceAll(",'", "")
				.replaceAll("',", "")
				.split(",");

		String n = category.getName();
		if (n.equals("adj_tr") || n.equals("adj_tr_comp")) {
			category.setFeature("prep", s[0]);
		} else if (n.equals("noun_sg") || n.equals("noun_pl") || n.equals("prop_sg") || n.equals("propdef_sg")) {
			if (s[0].equals("masc") || s[0].equals("fem")) {
				category.setFeature("human", "plus");
				category.setFeature("gender", s[0]);
			} else if (s[0].equals("human")) {
				category.setFeature("human", "plus");
			} else if (s[0].equals("neutr")) {
				category.setFeature("human", "minus");
			}
		}
	}

}
