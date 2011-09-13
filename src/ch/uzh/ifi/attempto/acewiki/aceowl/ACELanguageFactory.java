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

import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageFactory;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.ACEUtils;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This is a language factory implementation for ACE.
 * 
 * @author Tobias Kuhn
 */
public class ACELanguageFactory implements LanguageFactory {
	
	private TextOperator textOperator;
	
	public void init(Ontology ontology) {
		textOperator = new ACETextOperator(ontology);
	}
	
	public TextOperator getTextOperator() {
		return textOperator;
	}

	public OntologyElement createOntologyElement(String type) {
		if (type.equals("propername")) {
			return new ProperNameIndividual();
		} else if (type.equals("noun")) {
			return new NounConcept();
		} else if (type.equals("nounof")) {
			return new OfRelation();
		} else if (type.equals("trverb")) {
			return new VerbRelation();
		} else if (type.equals("tradj")) {
			return new TrAdjRelation();
		}
		return null;
	}
	
	public Sentence createSentence(String serialized) {
		// remove leading and trailing blank spaces.
		String s = serialized.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (s.substring(s.length()-1).equals("?")) {
			return new ACEQuestion(s);
		} else {
			return new ACEDeclaration(s);
		}
	}
	
	public Sentence createAssignmentSentence(Individual ind, Concept concept) {
		return createSentence(ind.getWord(2) + " is a " + concept.getWord() + ".");
	}
	
	public Sentence createHierarchySentence(Concept subConcept, Concept superConcept) {
		return createSentence("Every " + subConcept.getWord() + " is a " +
				superConcept.getWord() + ".");
	}
	
	public TextContainer createAnswerItem(OntologyElement el) {
		if (el instanceof NounConcept) {
			boolean an = ACEUtils.useIndefiniteArticleAn(el.getWord());
			TextElement det = new TextElement(an ? "an" : "a");
			return new TextContainer(det, new OntologyTextElement(el, 0));
		} else if (el instanceof ProperNameIndividual) {
			return new TextContainer(new OntologyTextElement(el, 1));
		}
		return new TextContainer();
	}

}
