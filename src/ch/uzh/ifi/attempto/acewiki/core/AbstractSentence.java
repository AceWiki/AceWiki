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

import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;

/**
 * This class is a partial implementation of a sentence.
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
	
	public boolean isImmutable() {
		return getArticle() == null;
	}
	
	/**
	 * Returns a text container with the text of this sentence in the given language.
	 * 
	 * @param language The language.
	 * @return The text container.
	 */
	protected abstract TextContainer getTextContainer(String language);
	
	public String getText(String language) {
		String t = "";
		TextElement prev = null;
		TextOperator textOperator = getTextOperator(language);
		for (TextElement te : getTextContainer(language).getTextElements()) {
			String glue = "";
			if (prev != null) {
				glue = textOperator.getGlue(prev, te);
			}
			if (te instanceof OntologyTextElement) {
				t += glue + ((OntologyTextElement) te).getUnderscoredText();
			} else {
				t += glue + te.getText();
			}
			prev = te;
		}
		return t;
	}

}
