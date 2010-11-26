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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class is used to tokenize an ACE text.
 * 
 * @author Tobias Kuhn
 */
class Tokenizer {
	
	// no instances allowed
	private Tokenizer() {}

	/**
	 * Tokenizes the given ACE text. The tokens are returned as a text container. Ontology elements
	 * are identified and linked.
	 * 
	 * @param aceText The ACE text to tokenize.
	 * @param ontology The ontology used to link the ontology elements.
	 * @return The tokenized ACE text as a text container.
	 */
	public static TextContainer tokenize(String aceText, Ontology ontology) {
		TextContainer textContainer = new TextContainer(ontology.getTextOperator());
		
		String t = "&" + aceText + "&";
		t = t.replaceAll(" ", "&");
		t = t.replaceAll("\\.", "&.&");
		t = t.replaceAll("\\?", "&?&");
		t = t.replaceAll("&of&", " of&");
		t = t.replaceAll("&by&", " by&");
		
		List<String> tokens = new ArrayList<String>(Arrays.asList(t.split("&")));
		
		while (tokens.contains("")) {
			tokens.remove("");
		}
		
		for (String s : tokens) {
			if (s.startsWith("<")) {
				OntologyTextElement te;
				try {
					long oeId = new Long(s.substring(1, s.indexOf(",")));
					int wordNumber = new Integer(s.substring(s.indexOf(",")+1, s.indexOf(">")));
					OntologyElement oe = ontology.get(oeId);
					te = new OntologyTextElement(oe, wordNumber);
				} catch (Exception ex) {
					throw new RuntimeException("Could not resolve link: " + s, ex);
				}
				if (te != null) {
					textContainer.addElement(te);
				} else {
					throw new RuntimeException("Could not resolve link: " + s);
				}
			} else {
				OntologyElement oe = ontology.getElement(s);
				
				if (oe == null) {
					textContainer.addElement(new TextElement(s));
				} else {
					// TODO: not 100% clean solution (several word forms of the same word can be
					// identical):
					int wordId = Arrays.asList(oe.getWords()).indexOf(s);
					if (oe instanceof Individual) {
						// TODO: this should probably be done at a different place...
						Individual ind = (Individual) oe;
						if (ind.hasDefiniteArticle(wordId-1) && textContainer.getTextElementsCount() > 0) {
							String precedingText = textContainer.
									getTextElement(textContainer.getTextElementsCount()-1).
									getText();
							if (precedingText.equals("the") || precedingText.equals("The")) {
								textContainer.removeLastElement();
								wordId--;
							}
						}
					}
					textContainer.addElement(new OntologyTextElement(oe, wordId));
				}
			}
		}
		
		return textContainer;
	}

}
