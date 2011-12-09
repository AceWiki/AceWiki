// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
 * This class represents a grammar rule. A grammar rule consists of a nonterminal category (called
 * "head") and a sequence of categories (called "body"). Furthermore, a grammar rule can be
 * scope-closing, which means that all scopes opened by its children are closed at the position
 * after the last child category.
 * 
 * @author Tobias Kuhn
 */
public class GrammarRule {
	
	private final Nonterminal head;
	private final boolean scopeclosing;
	private final Category[] body;
	private final Annotation annotation;
	
	/**
	 * Creates a new grammar rule.
	 * 
	 * @param annotation The annotation object.
	 * @param head The head category.
	 * @param scopeclosing Defines whether the rule is scope-closing or not.
	 * @param body The category sequence of the body.
	 */
	public GrammarRule(Annotation annotation, Nonterminal head, boolean scopeclosing, Category... body) {
		this.head = head;
		this.body = body;
		this.scopeclosing = scopeclosing;
		if (annotation == null) {
			this.annotation = new Annotation();
		} else {
			this.annotation = annotation;
		}
	}
	
	/**
	 * Creates a new grammar rule.
	 * 
	 * @param head The head category.
	 * @param scopeclosing Defines whether the rule is scope-closing or not.
	 * @param body The category sequence of the body.
	 */
	public GrammarRule(Nonterminal head, boolean scopeclosing, Category... body) {
		this(null, head, scopeclosing, body);
	}
	
	/**
	 * Creates a new grammar rule that is not scope-closing.
	 * 
	 * @param head The head category.
	 * @param body The body category sequence.
	 */
	public GrammarRule(Nonterminal head, Category... body) {
		this(null, head, false, body);
	}
	
	/**
	 * Creates a new grammar rule.
	 * 
	 * @param annotation The annotation object.
	 * @param categories The first category of this list stands for the head category (it has to be
	 *     a Nonterminal object). The rest stands for the body categories.
	 * @param scopeclosing Defines whether the rule is scope-closing or not.
	 */
	public GrammarRule(Annotation annotation, List<Category> categories, boolean scopeclosing) {
		this.scopeclosing = scopeclosing;
		this.head = (Nonterminal) categories.get(0);
		categories.remove(0);
		this.body = categories.toArray(new Category[0]);
		if (annotation == null) {
			this.annotation = new Annotation();
		} else {
			this.annotation = annotation;
		}
	}
	
	/**
	 * Returns the head category of this grammar rule.
	 * 
	 * @return The head category.
	 */
	public Nonterminal getHead() {
		return head;
	}
	
	/**
	 * Returns the body category sequence of this grammar rule.
	 * 
	 * @return The body category sequence.
	 */
	public Category[] getBody() {
		return body;
	}
	
	/**
	 * Returns true if the body is empty. Such grammar rules are called "epsilon rules".
	 * 
	 * @return true if the body is empty.
	 */
	public boolean hasEmptyBody() {
		return body.length == 0;
	}
	
	/**
	 * Returns the first category of the body.
	 * 
	 * @return The first category of the body.
	 */
	public Category getFirst() {
		return body[0];
	}
	
	/**
	 * Returns true if the rule is scope-closing.
	 * 
	 * @return true if the rule is scope-closing.
	 */
	public boolean isScopeClosing() {
		return scopeclosing;
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
	 * Creates a deep copy of this rule.
	 * 
	 * @return A deep copy.
	 */
	public GrammarRule deepCopy() {
		return deepCopy(new HashMap<Integer, StringObject>());
	}
	
	/**
	 * Creates a deep copy of this rule using the given string objects. This method is
	 * usually called form another deepCopy-method.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	GrammarRule deepCopy(HashMap<Integer, StringObject> stringObjs) {
		Nonterminal headC = (Nonterminal) head.deepCopy(stringObjs);
		Category[] bodyC = new Category[body.length];
		for (int i=0; i < body.length ; i++) {
			bodyC[i] = body[i].deepCopy(stringObjs);
		}
		Annotation annotationC = annotation.deepCopy(stringObjs);
		return new GrammarRule(annotationC, headC, scopeclosing, bodyC);
	}
	
	public String toString() {
		String s = head + " ";
		if (scopeclosing) {
			s += "~>";
		} else {
			s += "=>";
		}
		for (Category c : body) {
			s += " " + c;
		}
		return s;
	}

}
