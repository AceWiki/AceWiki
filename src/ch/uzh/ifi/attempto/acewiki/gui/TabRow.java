package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.List;

import nextapp.echo.app.Border;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.TopicElement;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SmallButton;

import com.google.common.collect.ImmutableList;

public class TabRow extends Row implements ActionListener {

	private static final long serialVersionUID = -8239909541142956202L;

	public static final String TAB_MAIN = "acewiki_page_main";
	public static final String TAB_INDEX = "acewiki_page_index";
	public static final String TAB_SEARCH = "acewiki_page_search";
	public static final String TAB_ABOUT = "acewiki_page_about";
	public static final String TAB_GRAMMAR = "acewiki_page_grammar";
	public static final String TAB_LEXICON = "acewiki_page_lexicon";

	public static final String TAB_ARTICLE = "acewiki_page_article";
	public static final String TAB_REFERENCES = "acewiki_page_references";
	public static final String TAB_INDIVIDUALS = "acewiki_page_individuals";
	public static final String TAB_HIERARCHY = "acewiki_page_hierarchy";
	public static final String TAB_ASSIGNMENTS = "acewiki_page_assignments";

	public static final String TAB_SENTENCE = "acewiki_page_sentence";
	public static final String TAB_TRANSLATIONS = "acewiki_page_translations";

	private static final ImmutableList<String> TABS_MAIN_SIMPLE = ImmutableList.of(
			TAB_MAIN, TAB_INDEX, TAB_SEARCH, TAB_ABOUT);

	private static final ImmutableList<String> TABS_MAIN_ADVANCED = ImmutableList.of(
			TAB_MAIN, TAB_INDEX, TAB_SEARCH, TAB_ABOUT, TAB_GRAMMAR, TAB_LEXICON);

	private static final ImmutableList<String> TABS_ARTICLE_PLAIN = ImmutableList.of(
			TAB_ARTICLE);

	private static final ImmutableList<String> TABS_ARTICLE_REFS = ImmutableList.of(
			TAB_ARTICLE, TAB_REFERENCES);

	private static final ImmutableList<String> TABS_ARTICLE_CONCEPT = ImmutableList.of(
			TAB_ARTICLE, TAB_REFERENCES, TAB_INDIVIDUALS, TAB_HIERARCHY);

	private static final ImmutableList<String> TABS_ARTICLE_INDIVIDUAL = ImmutableList.of(
			TAB_ARTICLE, TAB_REFERENCES, TAB_ASSIGNMENTS);

	private static final ImmutableList<String> TABS_SENTENCE_PLAIN = ImmutableList.of(
			TAB_SENTENCE);

	private static final ImmutableList<String> TABS_SENTENCE_TRANSLATIONS = ImmutableList.of(
			TAB_SENTENCE, TAB_TRANSLATIONS);

	public static TabRow getMainTabRow(String selectedTab, Wiki wiki) {
		if (wiki.getConfig().isGrammarIntegrationEnabled()) {
			return new TabRow(TABS_MAIN_ADVANCED, selectedTab, wiki);
		} else {
			return new TabRow(TABS_MAIN_SIMPLE, selectedTab, wiki);
		}
	}

	public static TabRow getArticleTabRow(OntologyElement oe, String selectedTab, Wiki wiki) {
		if (oe instanceof Concept) {
			return new TabRow(TABS_ARTICLE_CONCEPT, selectedTab, oe, wiki);
		} else if (oe instanceof Individual) {
			return new TabRow(TABS_ARTICLE_INDIVIDUAL, selectedTab, oe, wiki);
		} else if (oe instanceof TopicElement) {
			return new TabRow(TABS_ARTICLE_PLAIN, selectedTab, oe, wiki);
		} else {
			return new TabRow(TABS_ARTICLE_REFS, selectedTab, oe, wiki);
		}
	}

	public static TabRow getSentenceTabRow(Sentence sentence, String selectedTab, Wiki wiki) {
		if (wiki.isMultilingual() && wiki.isTranslationsPageActivated()) {
			return new TabRow(TABS_SENTENCE_TRANSLATIONS, selectedTab, sentence, wiki);
		} else {
			return new TabRow(TABS_SENTENCE_PLAIN, selectedTab, sentence, wiki);
		}
	}

	public static TabRow getEmptyTabRow() {
		return new TabRow(ImmutableList.<String>of(), (String) null, (Wiki) null);
	}

	private Object object;
	private Wiki wiki;

	private TabRow(List<String> tabs, String selectedTab, Object object, Wiki wiki) {
		this.object = object;
		this.wiki = wiki;

		setInsets(new Insets(10, 0, 0, 0));

		if (tabs != null) {
			for (String tab : tabs) {
				if (tab.equals(selectedTab)) {
					addSelectedTab(tab);
				} else {
					addTab(tab);
				}
			}
		}
	}

	public TabRow(List<String> tabs, String selectedTab, Wiki wiki) {
		this(tabs, selectedTab, null, wiki);
	}

	/**
	 * Adds a new tab to the tab row.
	 * 
	 * @param text Either a text key or the text itself.
	 */
	protected void addTab(String text) {
		add(new SmallButton(text, this));
		add(new HSpace(8));
		add(createTabSeparator());
		add(new HSpace(8));
	}

	/**
	 * Adds a new tab to the tab row that is currently selected.
	 * 
	 * @param text Either a text key or the text itself.
	 */
	protected void addSelectedTab(String text) {
		SmallButton b = new SmallButton(text, null);
		b.setEnabled(false);
		add(b);
		add(new HSpace(8));
		add(createTabSeparator());
		add(new HSpace(8));
	}

	private Button createTabSeparator() {
		Button tabSeparator = new Button();
		tabSeparator.setBorder(new Border(1, Color.DARKGRAY, Border.STYLE_SOLID));
		tabSeparator.setHeight(new Extent(12));
		return tabSeparator;
	}

	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		if (TAB_MAIN.equals(c)) {
			wiki.showStartPage();
		} else if (TAB_INDEX.equals(c)) {
			wiki.showIndexPage();
		} else if (TAB_SEARCH.equals(c)) {
			wiki.showSearchPage();
		} else if (TAB_ABOUT.equals(c)) {
			wiki.showAboutPage();
		} else if (TAB_GRAMMAR.equals(c)) {
			wiki.showGrammarPage();
		} else if (TAB_LEXICON.equals(c)) {
			wiki.showLexiconEditorPage();
		} else if (TAB_ARTICLE.equals(c)) {
			wiki.showPage((OntologyElement) object);
		} else if (TAB_REFERENCES.equals(c)) {
			wiki.showPage(new ReferencesPage((OntologyElement) object, wiki));
		} else if (TAB_INDIVIDUALS.equals(c)) {
			wiki.showPage(new IndividualsPage((Concept) object, wiki));
		} else if (TAB_HIERARCHY.equals(c)) {
			wiki.showPage(new HierarchyPage((Concept) object, wiki));
		} else if (TAB_ASSIGNMENTS.equals(c)) {
			wiki.showPage(new AssignmentsPage((Individual) object, wiki));
		} else if (TAB_SENTENCE.equals(c)) {
			wiki.showPage(new DetailsPage(wiki, (Sentence) object));
		} else if (TAB_TRANSLATIONS.equals(c)) {
			wiki.showPage(new TranslationsPage(wiki, (Sentence) object));
		}
	}

}
