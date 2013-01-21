package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.VSpace;

public class TranslationsPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 8682814239383999827L;

	private final Sentence mSentence;

	public TranslationsPage(Wiki wiki, Sentence sentence) {
		super(wiki);
		mSentence = sentence;
	}

	protected void doUpdate() {
		removeAll();

		removeAllTabs();
		addTab("acewiki_page_sentence", this);
		addSelectedTab("acewiki_page_translations");

		String t = LanguageUtils.getPrettyPrinted(mSentence.getText(getWiki().getLanguage()));
		String p = "- " + getWiki().getGUIText("acewiki_page_translations");
		add(new Title(t, p));
		addHorizontalLine();
		add(new VSpace(15));

		AceWikiEngine e = getWiki().getEngine();
		for (String l : e.getLanguages()) {
			if (l.equals(getWiki().getLanguage())) continue;
			addHeadline(e.getLanguageHandler(l).getLanguageName());
			Row r = new Row();
			r.setInsets(new Insets(10, 5, 5, 5));
			r.add(new Label(mSentence.getText(l)));
			add(r);
		}
	}

	public void actionPerformed(ActionEvent e) {
		if ("acewiki_page_sentence".equals(e.getActionCommand())) {
			getWiki().showPage(new SentencePage(getWiki(), mSentence));
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof TranslationsPage) {
			return mSentence == ((TranslationsPage) obj).mSentence;
		}
		return false;
	}
}
