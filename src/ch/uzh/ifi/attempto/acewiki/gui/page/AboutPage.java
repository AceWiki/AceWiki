// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Ontology;
import ch.uzh.ifi.attempto.acewiki.gui.NameValueTable;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page with information about the respective wiki.
 * 
 * @author Tobias Kuhn
 */
public class AboutPage extends WikiPage implements ActionListener {
	
	private static final long serialVersionUID = -5184590884798735077L;
	
	private NameValueTable table1, table2, table3;
	
	/**
	 * Creates a new about page.
	 * 
	 * @param wiki The wiki instance.
	 */
	public AboutPage(Wiki wiki) {
		super(wiki, new Title("About", true));

		addTab("Main Page", this);
		addTab("Index", this);
		addTab("Search", this);
		addSelectedTab("About");
		
		add(new VSpace(10));
		
		addHeadline("System");
		table1 = new NameValueTable();
		table1.setInsets(new Insets(10, 10, 0, 15));
		add(table1);

		addHeadline("Ontology");
		table2 = new NameValueTable();
		table2.setInsets(new Insets(10, 10, 0, 15));
		add(table2);

		addHeadline("Reasoner");
		table3 = new NameValueTable();
		table3.setInsets(new Insets(10, 10, 0, 15));
		add(table3);
		
		add(new VSpace(20));
	}
	
	protected void doUpdate() {
		table1.clear();
		table2.clear();
		table3.clear();
		
		Wiki w = getWiki();
		Ontology o = w.getOntology();
		
		table1.addEntry("AceWiki version", Wiki.getInfo("acewiki-version"));
		table1.addEntry("AceWiki release stage", Wiki.getInfo("acewiki-release-stage"));
		table1.addEntry("AceWiki developer", Wiki.getInfo("acewiki-developer"));
		table1.addEntry("AceWiki build date", Wiki.getInfo("acewiki-build-date"));
		
		table2.addEntry("ontology name", o.getName());
		table2.addEntry("number of ontology elements", o.getOntologyElements().size() + "");
		table2.addEntry("ontology URI", o.getURI());
		table2.addEntry("global restrictions policy", o.getGlobalRestrictionsPolicy());
		table2.addEntry("OWL profile", o.getOWLProfileName());
		
		table3.addEntry("reasoner type", o.getReasonerType());
		table3.addEntry("reasoner name", o.getReasonerName());
		table3.addEntry("reasoner version", o.getReasonerVersion());
	}

	public void actionPerformed(ActionEvent e) {
		if ("Main Page".equals(e.getActionCommand())) {
			getWiki().showStartPage();
		} else if ("Index".equals(e.getActionCommand())) {
			getWiki().showIndexPage();
		} else if ("Search".equals(e.getActionCommand())) {
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
