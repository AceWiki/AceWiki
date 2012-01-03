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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This dummy class is a reasoner without reasoning features.
 * 
 * @author Tobias Kuhn
 */
public class DummyReasoner implements AceWikiReasoner {

	public void init(Ontology ontology) {
	}
	
	public String getReasonerName() {
		return "";
	}
	
	public String getReasonerVersion() {
		return "";
	}
	
	public String getReasonerType() {
		return "No reasoner";
	}
	
	public Map<String, String> getInfo() {
		return new HashMap<String, String>();
	}
	
	public void load() {
	}
	
	public void loadElement(OntologyElement element) {
	}
	
	public void unloadElement(OntologyElement element) {
	}
	
	public void flushElements() {
	}
	
	public List<Concept> getConcepts(Individual ind) {
		return new ArrayList<Concept>();
	}
	
	public List<Individual> getIndividuals(Concept concept) {
		return new ArrayList<Individual>();
	}
	
	public List<Concept> getSuperConcepts(Concept concept) {
		return new ArrayList<Concept>();
	}
	
	public List<Concept> getSubConcepts(Concept concept) {
		return new ArrayList<Concept>();
	}
	
	public List<AnswerElement> getAnswer(Question question) {
		return new ArrayList<AnswerElement>();
	}
	
	public boolean isConsistent() {
		return true;
	}
	
	public boolean isSatisfiable(Concept concept) {
		return true;
	}
	
	public void loadSentence(Sentence sentence) {
	}
	
	public void unloadSentence(Sentence sentence) {
	}

}
