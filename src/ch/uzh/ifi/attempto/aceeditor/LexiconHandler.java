// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;
import ch.uzh.ifi.attempto.base.AbstractOption;
import ch.uzh.ifi.attempto.chartparser.DynamicLexicon;
import ch.uzh.ifi.attempto.chartparser.LexicalRule;
import ch.uzh.ifi.attempto.chartparser.Preterminal;

/**
 * This class manages the lexicon of the ACE Editor.
 * 
 * @author Tobias Kuhn
 */
class LexiconHandler implements DynamicLexicon {

	private Lexicon lexicon = new Lexicon();
	private Map<String,List<Word>> categoryMap = new HashMap<String,List<Word>>();
	private Map<String,List<Word>> textMap = new HashMap<String,List<Word>>();

	/**
	 * Creates a new lexicon handler object and loads the given lexicon file.
	 * 
	 * @param lexiconFile The lexicon file to be loaded.
	 */
	public LexiconHandler(String lexiconFile) {
		if (lexiconFile == null || lexiconFile.equals("")) {
			return;
		}
		try {
			BufferedReader in = new BufferedReader(new FileReader(lexiconFile));
			String line = in.readLine();
			while (line != null) {
				addWord(line);
				line = in.readLine();
			}
			in.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * Adds the word to the lexicon.
	 * 
	 * @param lexiconEntry A lexicon entry representing a word to be added.
	 */
	public void addWord(String lexiconEntry) {
		addWord(new Word(lexiconEntry));
	}
	
	/**
	 * Adds the word to the lexicon.
	 * 
	 * @param word The word to be added.
	 */
	public void addWord(Word word) {
		if (word == null) return;
		
		String w = word.getWordForm();
		if (w == null) return;
		String catName = word.getCategory().getName();

		lexicon.addEntry(LexiconEntry.createEntry(word.getEntry()));

		if (categoryMap.get(catName) == null) {
			categoryMap.put(catName, new ArrayList<Word>());
		}
		categoryMap.get(catName).add(word);

		if (textMap.get(w) == null) {
			textMap.put(w, new ArrayList<Word>());
		}
		textMap.get(w).add(word);
	}
	
	/**
	 * Removes all lexicon entries.
	 */
	public void clear() {
		lexicon.removeAllEntries();
		categoryMap.clear();
		textMap.clear();
	}

	/**
	 * Returns the lexicon object.
	 * 
	 * @return The lexicon object.
	 */
	public Lexicon getLexicon() {
		return lexicon;
	}

	/**
	 * Returns the words for the given category name.
	 * 
	 * @param category The name of the category.
	 * @return A list of all words of the given category.
	 */
	public List<Word> getWordsByCategory(String category) {
		List<Word> l = categoryMap.get(category);
		if (l == null) l = new ArrayList<Word>();
		return l;
	}

	/**
	 * Returns the words with the given word form.
	 * 
	 * @param text The word form.
	 * @return A list of all words with the given word form.
	 */
	public List<Word> getWordsByText(String text) {
		List<Word> l = textMap.get(text);
		if (l == null) l = new ArrayList<Word>();
		return l;
	}
	
	/**
	 * Returns the content of the complete lexicon as a string.
	 * 
	 * @return The content of the lexicon.
	 */
	public String getLexiconFileContent() {
		StringBuffer sb = new StringBuffer();
		for (LexiconEntry entry : lexicon.getEntries()) {
			sb.append(entry.toString());
			sb.append(".\n");
		}
		return sb.toString();
	}

	public Collection<LexicalRule> getLexRules(AbstractOption option) {
		String catName = option.getCategoryName();
		Collection<LexicalRule> lexRules = new ArrayList<LexicalRule>();
		if (catName.equals("def_noun_sg")) {
			for (Word w : getWordsByCategory("noun_sg")) {
				Preterminal cat = new Preterminal("def_noun_sg");
				cat.setFeature("noun", w.getWordForm());
				cat.setFeature("text", "the " + w.getWordForm());
				lexRules.add(new LexicalRule(cat, "the " + w.getWordForm()));
			}
		} else if (catName.equals("var")) {
			addVariableEntries(lexRules, "var");
		} else if (catName.equals("ref")) {
			addVariableEntries(lexRules, "ref");
		} else if (catName.equals("num")) {
			for (int i = 2 ; i < 100 ; i++) {
				lexRules.add(new LexicalRule("num", i + ""));
			}
		} else if (catName.equals("adj_prep")) {
			for (Word w : getWordsByCategory("adj_tr")) {
				Preterminal p = new Preterminal("adj_prep");
				String prep = w.getCategory().getFeature("prep").getString();
				p.setFeature("prep", prep);
				lexRules.add(new LexicalRule(p, prep));
			}
		} else {
			for (Word w : getWordsByCategory(catName)) {
				lexRules.add(w.getLexicalRule());
			}
		}
		return lexRules;
	}

	public Collection<LexicalRule> getLexRules(String word) {
		Collection<LexicalRule> lexRules = new ArrayList<LexicalRule>();
		if (word.startsWith("the ")) {
			for (Word w : getWordsByText(word.substring(4))) {
				if (w.getCategory().getName().equals("noun_sg")) {
					Preterminal cat = new Preterminal("def_noun_sg");
					cat.setFeature("noun", w.getWordForm());
					cat.setFeature("text", "the " + w.getWordForm());
					lexRules.add(new LexicalRule(cat, "the " + w.getWordForm()));
				}
			}
		} else if (word.matches("[XYZ][0-9]*")) {
			Preterminal p = new Preterminal("var");
			p.setFeature("text", word);
			lexRules.add(new LexicalRule(p, word));
			p = new Preterminal("ref");
			p.setFeature("text", word);
			lexRules.add(new LexicalRule(p, word));
		} else if (word.matches("[1-9][0-9]+|[2-9]")) {
			lexRules.add(new LexicalRule("num", word));
		} else {
			for (Word w : getWordsByText(word)) {
				lexRules.add(w.getLexicalRule());
			}
		}
		Preterminal p = new Preterminal("adj_prep");
		p.setFeature("prep", word);
		lexRules.add(new LexicalRule(p, word));
		
		return lexRules;
	}
	
	private static void addVariableEntries(Collection<LexicalRule> entries, String cat) {
		for (String s : new String[] {"X", "Y", "Z"}) {
			for (int i = 0 ; i <= 2 ; i++) {
				Preterminal p = new Preterminal(cat);
				String t = s;
				if (i != 0) t += i;
				p.setFeature("text", t);
				entries.add(new LexicalRule(p, t));
			}
		}
	}

}
