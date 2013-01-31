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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.echocomp.LocaleResources;

/**
 * This class is used to modify or create topics.
 * 
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public class TopicChanger implements LexiconChanger {

	public String getDescription() {
		return LocaleResources.getString("acewiki_ontoelement_topicdesc");
	}

	public List<LexiconDetail> getDetails(OntologyElement el) {
		GeneralTopic topic = (GeneralTopic) el;
		List<LexiconDetail> l = new ArrayList<LexiconDetail>();
		l.add(new LexiconDetail(
				LocaleResources.getString("acewiki_ontoelement_topicname"),
				LocaleResources.getString("acewiki_ontoelement_topiceg"),
				topic.getWord()
				));
		return l;
	}

	public void save(OntologyElement el, int wordNumber, List<Object> newValues, Ontology ontology)
			throws InvalidWordException {
		GeneralTopic topic = (GeneralTopic) el;
		String word = (String) newValues.get(0);
		OntologyElement oe = ontology.getElement(word);
		if (oe != null && oe != topic) {
			throw new InvalidWordException(LocaleResources.getString("acewiki_error_wordexists"));
		}
		if (word.isEmpty()) {
			throw new InvalidWordException(LocaleResources.getString("acewiki_error_emptyrequiredfield"));
		}
		ontology.change(topic, newValues.get(0).toString());
	}

}
