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

import java.util.Map;

import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page with information about the respective wiki.
 * 
 * @author Tobias Kuhn
 */
public class AboutPage extends WikiPage implements ActionListener {
	
	private static final long serialVersionUID = -5184590884798735077L;
	
	/**
	 * Creates a new about page.
	 * 
	 * @param wiki The wiki instance.
	 */
	public AboutPage(Wiki wiki) {
		super(wiki);
	}
	
	protected void doUpdate() {
		Wiki w = getWiki();
		Ontology o = w.getOntology();

		removeAll();

		addTab("acewiki_page_main", this);
		addTab("acewiki_page_index", this);
		addTab("acewiki_page_search", this);
		addSelectedTab("acewiki_page_about");
		
		add(new Title(w.getGUIText("acewiki_page_about"), true));
		addHorizontalLine();
		add(new VSpace(10));

		NameValueTable table = new NameValueTable();
		
		addHeadline("acewiki_about_systemheading");
		table = new NameValueTable();
		table.setInsets(new Insets(10, 10, 10, 15));
		table.addEntry(
				w.getGUIText("acewiki_about_version"),
				Wiki.getInfo("acewiki-version") + " (" + Wiki.getInfo("acewiki-release-stage") + ")"
			);
		table.addEntry(w.getGUIText("acewiki_about_builddate"), Wiki.getInfo("acewiki-build-date"));
		add(table);
		
		addHeadline("acewiki_about_ontologyheading");
		table = new NameValueTable();
		table.setInsets(new Insets(10, 10, 10, 15));
		table.addEntry(w.getGUIText("acewiki_about_ontologyname"), o.getName());
		table.addEntry(w.getGUIText("acewiki_about_ontologysize"), o.getOntologyElements().size() + "");
		table.addEntry(w.getGUIText("acewiki_about_ontologyuri"), o.getURI());
		add(table);

		AceWikiReasoner r = getWiki().getEngine().getReasoner();
		if (r != null) {
			addHeadline("acewiki_about_reasonerheading");
			table = new NameValueTable();
			table.setInsets(new Insets(10, 10, 10, 15));
			table.addEntry(w.getGUIText("acewiki_about_reasonertype"), r.getReasonerType());
			table.addEntry(w.getGUIText("acewiki_about_reasonername"), r.getReasonerName());
			table.addEntry(w.getGUIText("acewiki_about_reasonerversion"), r.getReasonerVersion());
			Map<String, String> info = r.getInfo();
			if (info != null) {
				for (String s : info.keySet()) {
					table.addEntry(s, info.get(s));
				}
			}
			add(table);
		}

		addHeadline("acewiki_about_usersheading");
		table = new NameValueTable();
		table.setInsets(new Insets(10, 10, 10, 15));
		String yes = w.getGUIText("acewiki_about_yes");
		String no = w.getGUIText("acewiki_about_no");
		table.addEntry(w.getGUIText("acewiki_about_usersnumber"), w.getUserBase().getUserCount() + "");
		table.addEntry(w.getGUIText("acewiki_about_loginenabled"), (w.isLoginEnabled() ? yes : no));
		table.addEntry(w.getGUIText("acewiki_about_loginforview"), (w.isLoginRequiredForViewing() ? yes : no));
		table.addEntry(w.getGUIText("acewiki_about_loginforedit"), (w.isLoginRequiredForEditing() ? yes : no));
		table.addEntry(w.getGUIText("acewiki_about_openregistr"), (w.isUserRegistrationOpen() ? yes : no));
		add(table);
		
		add(new VSpace(20));
	}

	public void actionPerformed(ActionEvent e) {
		if ("acewiki_page_main".equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if ("acewiki_page_index".equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if ("acewiki_page_search".equals(e.getActionCommand())) {
			getWiki().showSearchPage();
		}
	}

	public boolean equals(Object obj) {
		return obj instanceof AboutPage;
	}
	
	public String toString() {
		return "-ABOUT-";
	}

}
