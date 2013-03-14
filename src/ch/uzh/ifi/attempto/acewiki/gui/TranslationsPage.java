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

import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.VSpace;

public class TranslationsPage extends SentencePage implements ActionListener {

	private static final long serialVersionUID = 8682814239383999827L;

	public TranslationsPage(Wiki wiki, Sentence sentence) {
		super(sentence, wiki);
	}

	protected void doUpdate() {
		removeAll();

		setTabRow(TabRow.getSentenceTabRow(sentence, TabRow.TAB_TRANSLATIONS, getWiki()));

		String t = LanguageUtils.getPrettyPrinted(sentence.getText(getWiki().getLanguage()));
		String p = "- " + getWiki().getGUIText("acewiki_page_translations");
		add(new Title(t, p));
		addHorizontalLine();
		add(new VSpace(15));

		AceWikiEngine e = getWiki().getEngine();
		for (String l : e.getLanguages()) {
			// Uncomment to hide current language:
			//if (l.equals(getWiki().getLanguage())) continue;
			addHeadline(e.getLanguageHandler(l).getLanguageName());
			Row r = new Row();
			r.setInsets(new Insets(10, 5, 5, 5));
			r.add(new Label(sentence.getText(l)));
			int a = sentence.getTextContainer(l).size();
			if (a > 1) {
				// The sentence has more than one alternative
				r.add(new HSpace(10));
				String s = getWiki().getGUIText("acewiki_statement_alternatives");
				SmallButton b = new SmallButton("(" + a + " " + s + ")", this);
				b.setActionCommand(l);
				r.add(b);
			}
			add(r);
		}
	}

	public void actionPerformed(ActionEvent e) {
		Object src = e.getSource();

		if (src instanceof SmallButton) {
			String lang = ((SmallButton) src).getActionCommand();
			getWiki().showWindow(new AlternativesWindow(sentence, lang, getWiki()));
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof TranslationsPage) {
			return sentence == ((TranslationsPage) obj).sentence;
		}
		return false;
	}
}
