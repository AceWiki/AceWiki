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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.TextElement;

/**
 * This is the text operator used for the ACE/OWL language engine for AceWiki.
 * 
 * @author Tobias Kuhn
 */
public class ACETextOperator extends DefaultTextOperator {
	
	private Ontology ontology;
	
	/**
	 * Creates a new text operator.
	 * 
	 * @param ontology The ontology object.
	 */
	public ACETextOperator(Ontology ontology) {
		this.ontology = ontology;
	}
	
	public TextElement createTextElement(String text) {
		OntologyTextElement ote;
		if (ACEGrammar.grammar.containsTerminalSymbol(text)) {
			return new TextElement(text);
		}
		if (ACEGrammar.grammar.containsTerminalSymbol(text.toLowerCase())) {
			return new TextElement(text.toLowerCase());
		}
		String t = text.replaceAll("[ _]+", "_");
		ote = createOntologyTextElement(t);
		if (ote != null) return ote;
		if (t.toLowerCase().startsWith("the_")) {
			ote = createOntologyTextElement("the " + t.substring(4));
			if (ote != null) {
				return ote;
			}
			ote = createOntologyTextElement(t.substring(4));
			if (ote != null) {
				ote.setPreText("the ");
				return ote;
			}
		}
		if (t.toLowerCase().endsWith("_by")) {
			ote = createOntologyTextElement(t.replaceFirst("_by$", " by"));
			if (ote != null) return ote;
		}
		if (t.toLowerCase().endsWith("_of")) {
			ote = createOntologyTextElement(t.replaceFirst("_of$", " of"));
			if (ote != null) return ote;
		}
		return new TextElement(text);
	}
	
	private OntologyTextElement createOntologyTextElement(String text) {
		OntologyElement oe = ontology.getElement(text);
		if (oe != null) {
			int wn = -1;
			String[] words = oe.getWords();
			for (int i = 0 ; i < words.length ; i++) {
				if (text.equals(words[i])) wn = i;
			}
			if (wn > -1) {
				return new OntologyTextElement(oe, wn);
			}
		}
		return null;
	}
	
	public String getTextInContext(TextElement textElement, String preceding, String following) {
		String text = textElement.getOriginalText();
		String t;
		if (textElement instanceof OntologyTextElement) {
			OntologyElement oe = ((OntologyTextElement) textElement).getOntologyElement();
			boolean capitalize = false;
			if (preceding == null || preceding.matches("[.?!]")) {
				ProperNameIndividual i = null;
				if (oe instanceof ProperNameIndividual) {
					i = (ProperNameIndividual) oe;
				}
				if (oe == null) {
					capitalize = true;
				} else if (i != null && i.hasDefiniteArticle()) {
					capitalize = true;
				} else {
					capitalize = false;
				}
			}
			if (capitalize && text.length() > 0) {
				String f = text.substring(0, 1);
				t = f.toUpperCase() + text.substring(1);
			} else {
				t = text;
			}
		} else {
			boolean capitalize = false;
			if (preceding == null || preceding.matches("[.?!]")) {
				capitalize = true;
			}
			if (capitalize && text.length() > 0) {
				String f = text.substring(0, 1);
				t = f.toUpperCase() + text.substring(1);
			} else {
				t = text;
			}
			
			if (following != null && t.matches("(A|a)n?")) {
				if (ACEUtils.useIndefiniteArticleAn(following)) {
					t = t.substring(0, 1) + "n";
				} else {
					t = t.substring(0, 1);
				}
			}
		}
		return t;
	}

}
