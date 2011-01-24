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

package ch.uzh.ifi.attempto.chartparser;

import java.util.HashMap;
import java.util.List;

/**
 * This class represents a lexical rule. Lexical rules can also be called "lexicon entries". A
 * lexical rule consists of a word (in the form of a terminal category) and a pre-terminal
 * category. In the pre-terminal category of a lexical rule is simply called its "category".
 * 
 * @author Tobias Kuhn
 */
public class LexicalRule {
	
	private final Preterminal category;
	private final Terminal word;
	private final Annotation annotation;
	
	/**
	 * Creates a new lexical rule.
	 * 
	 * @param annotation The annotation object.
	 * @param category The category of the lexical rule.
	 * @param word The word of the lexical rule as a terminal category.
	 */
	public LexicalRule(Annotation annotation, Preterminal category, Terminal word) {
		this.category = category;
		this.word = word;
		if (annotation == null) {
			this.annotation = new Annotation();
		} else {
			this.annotation = annotation;
		}
	}

	/**
	 * Creates a new lexical rule.
	 * 
	 * @param category The category of the lexical rule.
	 * @param word The word of the lexical rule as a terminal category.
	 */
	public LexicalRule(Preterminal category, Terminal word) {
		this(null, category, word);
	}

	/**
	 * Creates a new lexical rule.
	 * 
	 * @param category The category of the lexical rule.
	 * @param word The word of the lexical rule.
	 */
	public LexicalRule(Preterminal category, String word) {
		this(category, new Terminal(word));
	}

	/**
	 * Creates a new lexical rule.
	 * 
	 * @param categoryName The category name of the lexical rule.
	 * @param word The word of the lexical rule.
	 */
	public LexicalRule(String categoryName, String word) {
		this(new Preterminal(categoryName), new Terminal(word));
	}
	
	/**
	 * Creates a new lexical rule.
	 * 
	 * @param annotation The annotation object.
	 * @param categories This list must contain exactly two elements. The first one must be a
	 *     pre-terminal category representing the category of the lexical rule. The second one
	 *     must be a terminal category representing the word of the lexical rule.
	 */
	public LexicalRule(Annotation annotation, List<Category> categories) {
		this.category = (Preterminal) categories.get(0);
		this.word = (Terminal) categories.get(1);
		if (annotation == null) {
			this.annotation = new Annotation();
		} else {
			this.annotation = annotation;
		}
	}
	
	/**
	 * Returns the category of the lexical rule.
	 * 
	 * @return The category of the lexical rule.
	 */
	public Preterminal getCategory() {
		return category;
	}
	
	/**
	 * Returns the word of the lexical rule.
	 * 
	 * @return The word as a terminal category.
	 */
	public Terminal getWord() {
		return word;
	}

	/**
	 * Returns the annotation object of this rule.
	 * 
	 * @return The annotation object.
	 */
	public Annotation getAnnotation() {
		return annotation;
	}
	
	/**
	 * Creates a deep copy of this lexicon entry.
	 * 
	 * @return A deep copy.
	 */
	public LexicalRule deepCopy() {
		return deepCopy(new HashMap<Integer, StringObject>());
	}
	
	/**
	 * Creates a deep copy of this lexicon entry using the given string objects.
	 * This method is usually called form another deepCopy-method.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	LexicalRule deepCopy(HashMap<Integer, StringObject> stringObjs) {
		Preterminal categoryC = (Preterminal) category.deepCopy(stringObjs);
		Terminal wordC = (Terminal) word.deepCopy(stringObjs);
		Annotation annotationC = annotation.deepCopy(stringObjs);
		return new LexicalRule(annotationC, categoryC, wordC);
	}
	
	public String toString() {
		return category + " => " + word;
	}

}
