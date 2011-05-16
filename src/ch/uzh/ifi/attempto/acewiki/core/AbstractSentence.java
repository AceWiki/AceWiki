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

import ch.uzh.ifi.attempto.preditor.TextContainer;
import ch.uzh.ifi.attempto.preditor.TextElement;

/**
 * This class represents an ACE sentence, which can be either a declaration (declarative sentence)
 * or a question.
 *<p>
 * ACE sentences can either be part of an article (in the case of asserted sentences or
 * questions) or can be independent statements with no article (in the case of inferred sentences).
 *<p>
 * Parsing of the sentence is done lazily, i.e. at the first time when a parsing result is
 * required. Parsing fails silently. No exceptions are thrown if a sentence is not ACE compliant.
 * 
 * @author Tobias Kuhn
 */
public abstract class AbstractSentence extends AbstractStatement implements Sentence {
	
	private boolean integrated = false;
	
	public boolean isIntegrated() {
		return integrated;
	}
	
	public void setIntegrated(boolean integrated) {
		this.integrated = integrated;
	}
	
	public static String getUnderscoredText(TextContainer textContainer) {
		String t = "";
		for (TextElement te : textContainer.getTextElements()) {
			if (te instanceof OntologyTextElement) {
				t += " " + ((OntologyTextElement) te).getUnderscoredText();
			} else if (te.getText().matches("[.?]")) {
				t += te.getText();
			} else {
				t += " " + te.getText();
			}
		}
		if (t.length() > 0) {
			t = t.substring(1);
		}
		return t;
	}
	
	public boolean isReadOnly() {
		return getArticle() == null;
	}
	
	public String toString() {
		return getText();
	}

}
