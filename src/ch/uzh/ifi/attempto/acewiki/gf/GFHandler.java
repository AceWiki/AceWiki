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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.AbstractLanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.base.DefaultTextOperator;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This is a language handler for languages written in GF.
 * 
 * @author Tobias Kuhn
 */
public class GFHandler extends AbstractLanguageHandler {
	
//	private Ontology ontology;
	private String language;
	private TextOperator textOperator = new DefaultTextOperator();
	EditorController editorController = new EditorController();
	
	/**
	 * Creates a new GF handler for the given language.
	 * 
	 * @param language The name of the language.
	 */
	public GFHandler(String language) {
		this.language = language;
	}

	public String getLanguage() {
		return language;
	}

	public void init(Ontology ontology) {
//		this.ontology = ontology;
	}
	
	public TextOperator getTextOperator() {
		return textOperator;
	}
	
	public List<Sentence> extractSentences(TextContainer tc, PredictiveParser parser) {
		List<Sentence> l = new ArrayList<Sentence>();
		l.add(new GFDeclaration(tc.getText()));
		return l;
	}
	
	public PredictiveParser getPredictiveParser() {
		return new JPGFParser("ch/uzh/ifi/attempto/acewiki/gf/Foods.pgf", "Foods" + language);
		//return new JPGFParser("ch/uzh/ifi/attempto/acewiki/gf/TestAttempto.pgf", "TestAttempto" + language);
	}
	
	public EditorController getEditorController() {
		return editorController;
	}

}
