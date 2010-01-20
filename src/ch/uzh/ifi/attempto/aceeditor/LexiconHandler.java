// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.aceeditor;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.LexiconEntry;

/**
 * This class manages the lexicon of the ACE Editor.
 * 
 * @author Tobias Kuhn
 */
class LexiconHandler {

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
			FileInputStream in = new FileInputStream(lexiconFile);
			byte[] bytes = new byte[in.available()];
			in.read(bytes);
			in.close();
			String[] lines = (new String(bytes)).split("\n");
			for (String s : lines) {
				addWord(s);
			}
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

}
