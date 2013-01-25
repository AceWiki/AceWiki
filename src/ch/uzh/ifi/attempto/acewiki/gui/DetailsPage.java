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

package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
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
public class DetailsPage extends SentencePage implements ActionListener {

	private static final long serialVersionUID = -1550505465878272821L;

	private int selectedIndex = 0;
	private IndexBar indexBar;

	/**
	 * Creates a new details page.
	 * 
	 * @param wiki The wiki instance.
	 * @param sentence The sentence to be shown in the page.
	 */
	public DetailsPage(Wiki wiki, Sentence sentence) {
		super(sentence, wiki);
	}

	protected void doUpdate() {
		removeAll();

		removeAllTabs();
		addSelectedTab("acewiki_page_sentence");
		if (getWiki().isMultilingual()) {
			addTab("acewiki_page_translations", this);
		}

		String t = LanguageUtils.getPrettyPrinted(sentence.getText(getWiki().getLanguage()));
		add(new Title(t, false));
		addHorizontalLine();
		add(new VSpace(15));
		
		int n = sentence.getNumberOfRepresentations();
		if (n > 1) {
			indexBar = new IndexBar(n, "acewiki_details_representation", this);
			indexBar.setActiveButton(selectedIndex);
			add(indexBar);
			add(new VSpace(10));
		}
		
		List<SentenceDetail> l = sentence.getDetails(getWiki().getLanguage(), selectedIndex);
		boolean empty = true;

		if (l != null) {
			for (SentenceDetail si : l) {
				empty = false;
				addHeadline(si.getName());
				Column infoColumn = new Column();
				infoColumn.setInsets(new Insets(10, 5, 5, 15));
				infoColumn.add(new DirectHtml(si.getRichText()));
				add(infoColumn);
			}
		}

		if (empty) {
			Column col = new Column();
			col.setInsets(new Insets(10, 5, 5, 15));
			col.add(new SolidLabel(getWiki().getGUIText("acewiki_details_empty"), Font.ITALIC, 10));
			add(col);
		}
	}

	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		Object src = e.getSource();

		if ("acewiki_page_translations".equals(c)) {
			getWiki().showPage(new TranslationsPage(getWiki(), sentence));
		} else if (src == indexBar) {
			selectedIndex = Integer.parseInt(e.getActionCommand()) - 1;
			update();
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof DetailsPage) {
			return sentence == ((DetailsPage) obj).sentence;
		}
		return false;
	}
	
	public String toString() {
		return sentence.getText(getWiki().getEngine().getLanguages()[0]);
	}

}
