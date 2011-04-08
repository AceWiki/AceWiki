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

import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.LanguageFactory;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

public class ACELanguageFactory implements LanguageFactory {

	public OntologyElement createOntologyElement(String type) {
		if (type.equals("propername")) {
			return new ProperNameIndividual();
		} else if (type.equals("noun")) {
			return new NounConcept();
		} else if (type.equals("nounof")) {
			return new OfRole();
		} else if (type.equals("trverb")) {
			return new VerbRole();
		} else if (type.equals("tradj")) {
			return new TrAdjRole();
		}
		return null;
	}
	
	public Sentence createSentence(String text) {
		// remove leading and trailing blank spaces.
		text = text.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (text.substring(text.length()-1).equals("?")) {
			return new Question(text);
		} else {
			return new Declaration(text);
		}
	}

}
