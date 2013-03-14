package ch.uzh.ifi.attempto.acewiki.gui;

import com.google.common.collect.ImmutableList;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;

public abstract class AbstractNavigationPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = -7034147244015215624L;

	protected static final String TAB_MAIN_PAGE = "acewiki_page_main";
	protected static final String TAB_INDEX = "acewiki_page_index";
	protected static final String TAB_SEARCH = "acewiki_page_search";
	protected static final String TAB_ABOUT = "acewiki_page_about";
	protected static final String TAB_GRAMMAR = "acewiki_page_grammar";

	private final String selectedTab;

	private static final ImmutableList<String> TABS = ImmutableList.of(
			TAB_MAIN_PAGE,
			TAB_INDEX,
			TAB_SEARCH,
			TAB_ABOUT,
			TAB_GRAMMAR
			);

	public AbstractNavigationPage(Wiki wiki, String selectedTab) {
		super(wiki);
		this.selectedTab = selectedTab;
	}

	protected void doUpdate() {
		removeAllTabs();
		for (String tab : TABS) {
			if (tab.equals(selectedTab)) {
				addSelectedTab(tab);
			} else {
				addTab(tab, this);
			}
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (TAB_MAIN_PAGE.equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if (TAB_INDEX.equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if (TAB_SEARCH.equals(e.getActionCommand())) {
			getWiki().showSearchPage();
		} else if (TAB_ABOUT.equals(e.getActionCommand())) {
			getWiki().showAboutPage();
		} else if (TAB_GRAMMAR.equals(e.getActionCommand())) {
			getWiki().showAboutGrammarPage();
		}
	}

}