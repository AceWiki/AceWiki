// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.acewiki.core.InvalidWordException;
import ch.uzh.ifi.attempto.acewiki.core.LexiconChanger;
import ch.uzh.ifi.attempto.acewiki.core.LexiconDetail;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;

/**
 * This class is used to modify or create pages that do not have
 * an ontological type.
 * 
 * TODO: move this class to a more general place
 * 
 * @author Kaarel Kaljurand
 */
public class ArticleChanger implements LexiconChanger {

	public String getDescription() {
		return "Every article is a sequence of statements and comments.";
	}

	public List<LexiconDetail> getDetails(OntologyElement el) {
		TypeArticle concept = (TypeArticle) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				"Name of the article",
				"",
				concept.getWord(0)
				));
		return l;
	}

	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		TypeArticle concept = (TypeArticle) el;
		ontology.change(concept, newValues.get(0).toString());
	}

}