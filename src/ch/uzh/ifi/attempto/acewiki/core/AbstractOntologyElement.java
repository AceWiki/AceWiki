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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This is a partial implementation of an ontology element.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractOntologyElement implements OntologyElement {
	
	private Ontology ontology;
	private Article article;
	
	private long id = -1;
	
	/**
	 * Initializes the ontology element.
	 */
	protected AbstractOntologyElement() {
	}
	
	public void initId(long id) {
		this.id = id;
	}
	
	public void initOntology(Ontology ontology) {
		if (this.ontology != null && this.ontology != ontology) {
			throw new RuntimeException("Cannot change the ontology for element " + toString());
		}
		this.ontology = ontology;
	}
	
	public void initArticle(Article article) {
		this.article = article;
	}
	
	public String[] getExternalWordList() {
		return getWords();
	}
	
	public String getWord(int n) {
		return getWords()[n];
	}
	
	public String getWord() {
		return getWord(0);
	}
	
	public String serializeWords() {
		String s = "";
		for (String w : getWords()) {
			s += ";" + w;
		}
		if (s.length() > 0) {
			s = s.substring(1);
		}
		return s;
	}
	
	public int getIndexOfWord(String word) {
		String[] words = getWords();
		for (int i = 0 ; i < words.length ; i++) {
			if (word.equals(words[i])) return i;
		}
		return -1;
	}
	
	public String getPrettyWord(int n) {
		String w = getWord(n);
		if (w == null) return null;
		return w.replace("_", " ");
	}
	
	public String getHeadword() {
		return getPrettyWord(0);
	}
	
	public String[] getIndexEntries() {
		return new String[] {getHeadword()};
	}
	
	public long getId() {
		return id;
	}
	
	public Ontology getOntology() {
		return ontology;
	}
	
	public Article getArticle() {
		if (article == null) {
			article = new Article(this);
		}
		return article;
	}
	
	/**
	 * Writes the text to the log file.
	 * 
	 * @param text The text to be written to the log file.
	 */
	protected void log(String text) {
		if (ontology != null) {
			ontology.log(text);
		}
	}

	public int compareTo(OntologyElement e) {
		if (this instanceof DummyOntologyElement && !(e instanceof DummyOntologyElement)) {
			return -1;
		} else if (!(this instanceof DummyOntologyElement) && e instanceof DummyOntologyElement) {
			return 1;
		} else {
			return getHeadword().compareToIgnoreCase(e.getHeadword());
		}
	}
	
	public String toString() {
		String l = "";
		for (String s : getWords()) {
			if (s == null) {
				l += ",";
			} else {
				l += s + ",";
			}
		}
		if (l.length() > 0) l = l.substring(0, l.length()-1);
		return getType() + "{" + l + "}";
	}

}
