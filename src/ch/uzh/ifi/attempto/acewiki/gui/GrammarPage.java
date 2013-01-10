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

import java.util.Map.Entry;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Joiner;

import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.gfservice.GFGrammar;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfServiceResultGrammar;


public class GrammarPage extends AbstractNavigationPage implements ActionListener {

	final Logger logger = LoggerFactory.getLogger(GrammarPage.class);

	private static final long serialVersionUID = -2031690219932377941L;
	private static final Joiner JOINER_SPACE = Joiner.on(' ');
	private static final Joiner JOINER_COMMA = Joiner.on(", ");
	private final CompTable table1, table2, table3, table4;
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

		addHeadline("Languages");
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

		add(new VSpace(20));
	}

	protected void doUpdate() {
		if (mInfo == null) {
			return;
		}
		table1.clear();
		table2.clear();
		table3.clear();
		table4.clear();

		table1.addEntry("Name", GuiUtils.getNameComponent(mWiki, mInfo.getName()));
		table1.addEntry("Startcat", mInfo.getStartcat());
		table1.addEntry("Functions", JOINER_COMMA.join(mInfo.getFunctions()));

		for (Entry<String, Set<String>> entry : mInfo.getLanguages().entrySet()) {
			table2.addEntry(GuiUtils.getNameComponent(mWiki, entry.getKey()), JOINER_SPACE.join(entry.getValue()));
		}

		try {
			for (String cat : mInfo.getCategories()) {
				table3.addEntry(cat, JOINER_COMMA.join(mGrammar.getProducers(cat)));
				table4.addEntry(cat, JOINER_COMMA.join(mGrammar.getConsumers(cat)));
			}
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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

}