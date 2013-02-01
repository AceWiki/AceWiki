// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Joiner;
import com.google.common.collect.Multimap;

import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.acewiki.gf.GFGrammar;
import ch.uzh.ifi.attempto.acewiki.gf.TypeGfModule;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.TextAreaWindow;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultGrammar;


public class GrammarPage extends AbstractNavigationPage implements ActionListener {

	// TODO: localize
	private static final boolean ADMIN_MODE = false;
	private static final String ACTION_GRAMMAR_PUSH = "acewiki_action_grammar_push";
	private static final String ACTION_GRAMMAR_PULL = "acewiki_action_grammar_pull";

	private static final long serialVersionUID = -2031690219932377941L;
	private static final Joiner JOINER_SPACE = Joiner.on(' ');
	private static final Joiner JOINER_COMMA = Joiner.on(", ");
	private final CompTable table1, table2, table3, table4;
	private final CompTable mTableTokens;
	private final Label mTableTokensLabel = new Label();
	private final GFGrammar mGrammar;
	private final GfServiceResultGrammar mInfo;
	private final Wiki mWiki;

	public GrammarPage(Wiki wiki, GFGrammar grammar) {
		super(wiki);
		mWiki = wiki;

		mGrammar = grammar;
		mInfo = grammar.getGrammar();

		add(new Title("About grammar", true));
		addHorizontalLine();
		add(new VSpace(10));

		table1 = new CompTable();
		table1.setInsets(new Insets(10, 10, 10, 15));
		add(table1);

		addHeadline("Top-level modules");
		table2 = new CompTable();
		table2.setInsets(new Insets(10, 10, 10, 15));
		add(table2);

		addHeadline("Categories and producer functions");
		table3 = new CompTable();
		table3.setInsets(new Insets(10, 10, 10, 15));
		add(table3);

		addHeadline("Categories and consumer functions");
		table4 = new CompTable();
		table4.setInsets(new Insets(10, 10, 10, 15));
		add(table4);

		addHeadline("Tokens and their categories for language " + mWiki.getLanguage());
		add(mTableTokensLabel);
		mTableTokens = new CompTable();
		mTableTokens.setInsets(new Insets(10, 10, 10, 15));
		add(mTableTokens);

		add(new VSpace(20));

		// TODO: these should be buttons (not tabs) as they do not open a new page
		// TODO: admin mode should be a runtime thing
		if (ADMIN_MODE) {
			addTab(ACTION_GRAMMAR_PUSH, this);
			addTab(ACTION_GRAMMAR_PULL, this);
		}
	}


	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if (ACTION_GRAMMAR_PUSH.equals(e.getActionCommand())) {
			actionGrammarPush();
		} else if (ACTION_GRAMMAR_PULL.equals(e.getActionCommand())) {
			actionGrammarPull();
		}
	}

	protected void doUpdate() {
		if (mInfo == null) {
			return;
		}
		table1.clear();
		table2.clear();
		table3.clear();
		table4.clear();
		mTableTokens.clear();

		Map<String, Set<String>> langs = mInfo.getLanguages();
		table1.addEntry("Name", GuiUtils.getNameComponent(mWiki, mInfo.getName()));
		table1.addEntry("Startcat", mInfo.getStartcat());
		table1.addEntry("Categories", mInfo.getCategories().size() + "");
		table1.addEntry("Functions", mInfo.getFunctions().size() + "");
		table1.addEntry("Languages", langs.keySet().size() + "");

		for (String lang : asSortedList(langs.keySet())) {
			table2.addEntry(GuiUtils.getNameComponent(mWiki, lang), JOINER_SPACE.join(langs.get(lang)));
		}

		for (String cat : asSortedList(mInfo.getCategories())) {
			table3.addEntry(cat, JOINER_COMMA.join(mGrammar.getProducers(cat)));
			table4.addEntry(cat, JOINER_COMMA.join(mGrammar.getConsumers(cat)));
		}


		// Make a token -> categories table
		Multimap<String, String> tokenToCats = null;
		try {
			tokenToCats = mGrammar.getTokenToCats(mWiki.getLanguage());
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (tokenToCats != null) {
			mTableTokensLabel.setText("There are " + tokenToCats.keySet().size() + " tokens.\n" +
					"Note that this list includes only tokens that are generated by " +
					"functions that produce categories but do not consume them.");
			for (String tok : asSortedList(tokenToCats.keySet())) {
				mTableTokens.addEntry(tok, JOINER_COMMA.join(tokenToCats.get(tok)));
			}
		}

	}


	public boolean equals(Object obj) {
		return obj instanceof GrammarPage;
	}

	public String toString() {
		return "-GRAMMAR-";
	}

	@Override
	public boolean isSelected(String tabName) {
		return TAB_ABOUT_GRAMMAR.equals(tabName);
	}


	// TODO: visual progress monitor
	private void actionGrammarPush() {
		StringBuilder sb = new StringBuilder();
		int countEmpty = 0;
		int countOk = 0;
		int countErr = 0;
		for (TypeGfModule module : mWiki.getOntology().getOntologyElements(TypeGfModule.class)) {
			sb.append(module.getWord());
			sb.append(": ");
			String content = getModuleContent(module.getArticle());
			if (content == null) {
				countEmpty++;
				sb.append("EMPTY");
			} else {
				try {
					mGrammar.upload(new GfModule(module.getWord(), content));
					sb.append("OK");
					countOk++;
				} catch (GfServiceException e) {
					sb.append("FAIL\n");
					sb.append(e.getMessage());
					countErr++;
				}
			}
			sb.append("\n\n");
		}
		TextAreaWindow resultsWindow = new TextAreaWindow(ACTION_GRAMMAR_PUSH + " " + countOk + "/" + countEmpty + "/" + countErr, this);
		resultsWindow.setText(sb.toString());
		mWiki.showWindow(resultsWindow);
	}


	private void actionGrammarPull() {
		StringBuilder sb = new StringBuilder();
		int countFile = 0; // larger or equal to countOld + countNew
		int countOld = 0;
		int countNew = 0;
		int countClash = 0;
		int countChanged = 0; // larger or equal to countNew
		Ontology ont = mWiki.getOntology();

		try {
			// Iterate over the list GF source files
			for (String filename : mGrammar.ls(GFGrammar.EXTENSION_GF)) {
				countFile++;
				String moduleName = filename.substring(0, filename.length() - GFGrammar.EXTENSION_GF.length());
				sb.append(moduleName);
				sb.append(':');
				OntologyElement el = ont.getElement(moduleName);

				if (el != null && ! (el instanceof TypeGfModule)) {
					// TODO: name clash, this would be avoided if we had namespaces
					// for different types of pages
					sb.append(" NAME CLASH\n");
					countClash++;
					continue;
				}

				String newContent = mGrammar.downloadAsString(filename);
				String oldContent = null;

				if (el == null) {
					el = new TypeGfModule();
					// TODO: verify that this is correct
					el.setWords(moduleName);
					ont.register(el);
					countNew++;
					sb.append(" CREATED");
				} else {
					countOld++;
					oldContent = getModuleContent(el.getArticle());
				}

				if (! newContent.equals(oldContent)) {
					replaceModuleContent(el.getArticle(), newContent);
					countChanged++;
					sb.append(" UPDATED");
				}
				sb.append("\n\n");
			}
		} catch (GfServiceException e) {
			sb.append(e.getMessage());
		}

		sb.append("----\n\n");
		sb.append("Downloaded: " + countFile);
		sb.append('\n');
		sb.append("New files: " + countNew);
		sb.append('\n');
		sb.append("Changed old files: " + (countChanged - countNew));
		sb.append('\n');
		sb.append("Name clashes: " + countClash);
		sb.append('\n');
		TextAreaWindow resultsWindow = new TextAreaWindow(ACTION_GRAMMAR_PULL + " " +
				countFile + "/" + countOld + "/" + countChanged + "/" + countNew, this);
		resultsWindow.setText(sb.toString());
		mWiki.showWindow(resultsWindow);
	}


	// TODO: move to Utils
	public static <T extends Comparable<? super T>> List<T> asSortedList(Collection<T> c) {
		List<T> list = new ArrayList<T>(c);
		java.util.Collections.sort(list);
		return list;
	}


	// TODO: the following methods assume that a GF module is an article
	// which contains at most one Comment whose text is the module's GF source.
	private static String getModuleContent(Article article) {
		if (article == null) {
			return null;
		}
		List<Statement> statements = article.getStatements();
		if (statements == null || statements.isEmpty()) {
			return null;
		}
		return statements.get(0).getText(null);
	}


	public static void replaceModuleContent(Article article, String newContent) {
		Statement newStatement = new Comment(newContent);
		newStatement.init(article.getOntology(), article); // TODO: verify that this is correct
		List<Statement> statements = article.getStatements();
		if (statements == null || statements.isEmpty()) {
			article.add(null, newStatement);
		} else {
			article.edit(statements.get(0), newStatement);
		}
	}

}