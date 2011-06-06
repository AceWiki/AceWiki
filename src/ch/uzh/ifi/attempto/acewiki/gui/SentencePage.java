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

package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import echopoint.DirectHtml;

/**
 * This class represents a page that shows the details of an ACE sentence.
 * 
 * @author Tobias Kuhn
 */
public class SentencePage extends WikiPage {

	private static final long serialVersionUID = -1550505465878272821L;

	private Sentence sentence;

	/**
	 * Creates a new sentence page.
	 * 
	 * @param wiki The wiki instance.
	 * @param sentence The sentence to be shown in the page.
	 */
	public SentencePage(Wiki wiki, Sentence sentence) {
		super(wiki);
		this.sentence = sentence;
		
		addSelectedTab("Sentence");
		
		add(new Title(sentence.getPrettyText(), false));
		addHorizontalLine();
		add(new VSpace(15));
		
		List<SentenceDetail> l = sentence.getDetails();
		
		if (l.isEmpty()) {
			add(new SolidLabel("(no detail information available)", Font.ITALIC, 10));
		}
		
		for (SentenceDetail si : l) {
			addHeadline(si.getName());
			Column infoColumn = new Column();
			infoColumn.setInsets(new Insets(10, 5, 5, 15));
			infoColumn.add(new DirectHtml(si.getRichText()));
			add(infoColumn);
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof SentencePage) {
			return sentence == ((SentencePage) obj).sentence;
		}
		return false;
	}
	
	public String toString() {
		return sentence.getText();
	}

}
