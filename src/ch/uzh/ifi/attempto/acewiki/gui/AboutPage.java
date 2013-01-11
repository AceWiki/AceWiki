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

		addTab("acewiki_specialpage_main", this);
		addTab("acewiki_specialpage_index", this);
		addTab("acewiki_specialpage_search", this);
		addSelectedTab("acewiki_specialpage_about");
		
		add(new Title(w.getGUIText("acewiki_specialpage_about"), true));
		addHorizontalLine();
		add(new VSpace(10));
		
		addHeadline("System");
		NameValueTable table1 = new NameValueTable();
		table1.setInsets(new Insets(10, 10, 10, 15));
		add(table1);

		addHeadline("Ontology");
		NameValueTable table2 = new NameValueTable();
		table2.setInsets(new Insets(10, 10, 10, 15));
		add(table2);

		addHeadline("Reasoner");
		NameValueTable table3 = new NameValueTable();
		table3.setInsets(new Insets(10, 10, 10, 15));
		add(table3);

		addHeadline("Users");
		NameValueTable table4 = new NameValueTable();
		table4.setInsets(new Insets(10, 10, 10, 15));
		add(table4);
		
		add(new VSpace(20));
		
		table1.addEntry("AceWiki version", Wiki.getInfo("acewiki-version"));
		table1.addEntry("AceWiki release stage", Wiki.getInfo("acewiki-release-stage"));
		table1.addEntry("AceWiki build date", Wiki.getInfo("acewiki-build-date"));
		
		table2.addEntry("ontology name", o.getName());
		table2.addEntry("number of ontology elements", o.getOntologyElements().size() + "");
		table2.addEntry("ontology URI", o.getURI());
		
		AceWikiReasoner r = o.getReasoner();
		table3.addEntry("reasoner type", r.getReasonerType());
		table3.addEntry("reasoner name", r.getReasonerName());
		table3.addEntry("reasoner version", r.getReasonerVersion());
		Map<String, String> info = r.getInfo();
		if (info != null) {
			for (String s : info.keySet()) {
				table3.addEntry(s, info.get(s));
			}
		}
		
		table4.addEntry("number of registered users", w.getUserBase().getUserCount() + "");
		table4.addEntry("login enabled", (w.isLoginEnabled() ? "yes" : "no"));
		table4.addEntry("login required for viewing", (w.isLoginRequiredForViewing() ? "yes" : "no"));
		table4.addEntry("login required for editing", (w.isLoginRequiredForEditing() ? "yes" : "no"));
		table4.addEntry("open user registration", (w.isUserRegistrationOpen() ? "yes" : "no"));
	}

	public void actionPerformed(ActionEvent e) {
		if ("acewiki_specialpage_main".equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if ("acewiki_specialpage_index".equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if ("acewiki_specialpage_search".equals(e.getActionCommand())) {
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
