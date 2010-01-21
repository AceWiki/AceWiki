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

import nextapp.echo2.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;

/**
 * This class stands for an article page showing the article of an individual. Individuals
 * are represented by proper names.
 * 
 * @author Tobias Kuhn
 */
public class IndividualPage extends ArticlePage {
	
	private static final long serialVersionUID = 8411734605383577958L;

	private Individual ind;
	
	/**
	 * Creates a new article page for an individual.
	 * 
	 * @param ind The individual.
	 * @param wiki The wiki instance.
	 */
	public IndividualPage(Individual ind, Wiki wiki) {
		super(wiki, ind);
		this.ind = ind;
		
		addTab("Assignments", this);
	}
	
	public OntologyElement getOntologyElement() {
		return ind;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if ("Assignments".equals(e.getActionCommand())) {
			getWiki().showPage(new AssignmentsPage(this));
		}
	}
	
	protected void doUpdate() {
		super.doUpdate();
		
		getTitle().setText(ind.getHeadword());
	}

}
