package ch.uzh.ifi.attempto.acewiki.gui;

import com.google.common.collect.ImmutableList;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;

public abstract class AbstractNavigationPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = -7034147244015215624L;

	protected static final String TAB_MAIN_PAGE = "Main Page";
	protected static final String TAB_INDEX = "Index";
	protected static final String TAB_SEARCH = "Search";
	protected static final String TAB_ABOUT = "About";
	protected static final String TAB_ABOUT_GRAMMAR = "About Grammar";

	private static final ImmutableList<String> TABS = ImmutableList.of(
			TAB_MAIN_PAGE,
			TAB_INDEX,
			TAB_SEARCH,
			TAB_ABOUT,
			TAB_ABOUT_GRAMMAR
			);

	public AbstractNavigationPage(Wiki wiki) {
		super(wiki);

		for (String tab : TABS) {
			if (isSelected(tab)) {
				addSelectedTab(tab);
			} else {
				addTab(tab, this);
			}
		}
	}

	public abstract boolean isSelected(String tabName);

	public void actionPerformed(ActionEvent e) {
		if (TAB_MAIN_PAGE.equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if (TAB_INDEX.equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if (TAB_SEARCH.equals(e.getActionCommand())) {
			getWiki().showSearchPage();
		} else if (TAB_ABOUT.equals(e.getActionCommand())) {
			getWiki().showAboutPage();
		} else if (TAB_ABOUT_GRAMMAR.equals(e.getActionCommand())) {
			getWiki().showAboutGrammarPage();
		}
	}

}