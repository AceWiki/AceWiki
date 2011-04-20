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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import static ch.uzh.ifi.attempto.ape.OutputType.DRSPP;

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceInfo;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import echopoint.DirectHtml;
import echopoint.util.HtmlKit;

/**
 * This class represents a page that shows the logical representation of an ACE sentence.
 * 
 * @author Tobias Kuhn
 */
public class LogicPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 61958212885537L;

	private Sentence sentence;

	/**
	 * Creates a new logic page.
	 * 
	 * @param wiki The wiki instance.
	 * @param sentence The sentence whose logical representation should be shown in the page.
	 */
	public LogicPage(Wiki wiki, Sentence sentence) {
		super(wiki);
		this.sentence = sentence;
		
		addTab("Sentence", this);
		addSelectedTab("Logic");
		
		add(new Title(sentence.getPrettyText(), "- Logic"));
		addHorizontalLine();
		add(new VSpace(15));
		
		List<SentenceInfo> l = sentence.getLogicInfo();
		
		if (l.isEmpty()) {
			add(new SolidLabel("(no logic information available)", Font.ITALIC, 10));
		}
		
		for (SentenceInfo si : l) {
			addHeadline(si.getName());
			Column infoColumn = new Column();
			infoColumn.setInsets(new Insets(10, 5, 5, 15));
			infoColumn.add(new DirectHtml(si.getRichText()));
			add(infoColumn);
		}
	}

	public void actionPerformed(ActionEvent e) {
		if ("Sentence".equals(e.getActionCommand())) {
			getWiki().showPage(new SentencePage(getWiki(), sentence));
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof LogicPage) {
			return sentence == ((LogicPage) obj).sentence;
		}
		return false;
	}
	
	public String toString() {
		return "Logic: " + sentence.getText();
	}

}
