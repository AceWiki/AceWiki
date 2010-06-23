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
		TextContainer textContainer = new TextContainer(Sentence.contextChecker);
		
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
					te = OntologyTextElement.createTextElement(oe, wordNumber);
				} catch (Exception ex) {
					throw new RuntimeException("Could not resolve link: " + s, ex);
				}
				if (te != null) {
					textContainer.addElement(te);
				} else {
					throw new RuntimeException("Could not resolve link: " + s);
				}
			} else {
				OntologyElement oe = ontology.get(s);
				
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
					textContainer.addElement(OntologyTextElement.createTextElement(oe, wordId));
				}
			}
		}
		
		return textContainer;
	}

}
