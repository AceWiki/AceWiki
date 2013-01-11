package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.List;

import com.google.common.collect.ImmutableSet;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.MultilingualSentence;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import echopoint.DirectHtml;

public class TranslationsPage extends WikiPage {

	private static final String TAB_TRANSLATIONS = "Translations";

	private static final long serialVersionUID = 8682814239383999827L;

	private final Sentence mSentence;

	public TranslationsPage(Wiki wiki, MultilingualSentence sentence) {
		super(wiki);
		mSentence = sentence;

		addSelectedTab(TAB_TRANSLATIONS);

		add(new Title(TAB_TRANSLATIONS, false));
		addHorizontalLine();
		add(new VSpace(15));

		List<SentenceDetail> l = sentence.getLins(ImmutableSet.of(wiki.getLanguage()));

		if (l == null || l.isEmpty()) {
			Column col = new Column();
			col.setInsets(new Insets(10, 5, 5, 15));
			col.add(new SolidLabel("(no translations)", Font.ITALIC, 10));
			add(col);
		} else {
			for (SentenceDetail si : l) {
				// addHeadline(si.getName());
				add(GuiUtils.getNameComponent(wiki, si.getName()));
				Column infoColumn = new Column();
				infoColumn.setInsets(new Insets(10, 5, 5, 15));
				infoColumn.add(new DirectHtml(si.getRichText()));
				add(infoColumn);
			}
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